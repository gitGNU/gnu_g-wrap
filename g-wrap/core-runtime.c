#define _GNU_SOURCE

#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "g-wrap/core-runtime.h"

static int nregistered_wrapsets = 0;
static int nallocated_wrapsets = 0;
static GWWrapSet **registered_wrapsets = NULL;

static GWLangSupport *gw_lang = NULL;

void *
gw_malloc (GWLangArena arena, size_t size)
{
  return gw_lang->malloc (arena, size);
}

void *
gw_realloc (GWLangArena arena, void *mem, size_t size)
{
  return gw_lang->realloc (arena, mem, size);
}

void
gw_raise_error (GWLangArena arena, const char *proc, const char *fmt, ...)
{
  char *message = NULL;
  va_list args;

  va_start (args, fmt);
  vasprintf (&message, fmt, args);
  va_end (args);
  
  gw_lang->raise_error (arena, proc, message);
  
  free (message);
}

void
gw_handle_wrapper_error (GWLangArena arena,
                         GWError *error,
                         const char *func_name,
                         unsigned int arg_pos)
{
  gw_lang->handle_wrapper_error (arena, error, func_name, arg_pos);
}

GWWrapSet *
gw_wrapset_new (GWLangArena arena,
                const char *name,
                const char *dependency, ...)
{
  GWWrapSet *ws;
  GWWrapSet **ws_deps;
  int i, ndeps;
  const int start_size = 4;
  va_list args;

  for (i = 0; i < nregistered_wrapsets; i++)
    if (strcmp (registered_wrapsets[i]->name, name) == 0)
    {
      gw_raise_error (arena, "%gw:wrapset-new",
                      "tried to double-register wrapset %s",
                      name);
    }
  
  va_start (args, dependency);
  for (ndeps = 0, ws_deps = NULL; dependency != NULL; ndeps++)
  {
    ws = NULL;
    for (i = 0; i < nregistered_wrapsets; i++)
      if (strcmp (registered_wrapsets[i]->name, dependency) == 0)
      {
        ws = registered_wrapsets[i];
        break;
      }
    if (ws == NULL)
      gw_raise_error (arena, "%gw:wrapset-new",
                      "dependency on nonexisting wrapset: %s", dependency);
    
    ws_deps = gw_realloc (arena, ws_deps, (ndeps + 1) * sizeof (GWWrapSet *));
    ws_deps[ndeps] = ws;

    dependency = va_arg (args, const char *);
  }
  va_end (args);
  
  ws = gw_malloc (arena, sizeof (GWWrapSet));
  ws->name = name;
  
  ws->ndependencies = ndeps;
  ws->dependencies = ws_deps;
  
  ws->types = gw_malloc (arena, start_size * sizeof (GWTypeInfo));
  ws->ntypes = 0;
  ws->ntypes_allocated = start_size;
  ws->types_sorted = 0;

  ws->functions = gw_malloc (arena, start_size * sizeof (GWFunctionInfo));
  ws->nfunctions = 0;
  ws->nfuncs_allocated = start_size;
  
  return ws;
}

void
gw_wrapset_add_type (GWWrapSet *ws,
                     const char *name,
                     const char *class_name,
                     ffi_type *type,
                     const char **subtypes,
                     GWWrapValueFunc wrap_value,
                     GWUnWrapValueFunc unwrap_value,
                     GWDestructValueFunc destruct_value)
{
  GWTypeInfo *ti;
  
  if (ws->ntypes >= ws->ntypes_allocated)
  {
    ws->ntypes_allocated <<= 1;
    ws->types = gw_realloc (ws->arena, ws->types, ws->ntypes_allocated *
                             sizeof (GWTypeInfo));
  }
  
  ti = &ws->types[ws->ntypes++];

  assert (!(type != NULL && subtypes != NULL));
  
  if (subtypes)
  {
    int nsubtypes, i;
    ffi_type **type_elements;
    
    for (nsubtypes = 0; subtypes[nsubtypes] != NULL; nsubtypes++)
      ;
    
    type = gw_malloc (ws->arena, sizeof (ffi_type)
                      + ((nsubtypes + 1) * sizeof (ffi_type *)));
    type_elements = (ffi_type **)((unsigned char *)type + sizeof (ffi_type));
    
    for (i = 0; i < nsubtypes; i++)
    {
      GWTypeInfo *subtype_info = gw_wrapset_lookup_type(ws, subtypes[i]);
      assert (subtype_info != NULL && subtype_info->type != NULL);
      type_elements[i] = subtype_info->type;
    }
    type_elements[nsubtypes] = NULL;

    type->type = FFI_TYPE_STRUCT;
    type->size = type->alignment = 0;
    type->elements = type_elements;
  }
  
  ti->name = name;
  ti->class_name = class_name;
  ti->type = type;
  ti->wrap_value = wrap_value;
  ti->unwrap_value = unwrap_value;
  ti->destruct_value = destruct_value;
  
  ws->types_sorted = 0;
}

static int
typeinfo_cmp (const void *a, const void *b)
{
  return strcmp (((GWTypeInfo *) a)->name, ((GWTypeInfo *) b)->name);
}

GWTypeInfo *
gw_wrapset_lookup_type (GWWrapSet *ws, const char *name)
{
  GWTypeInfo key;
  GWTypeInfo *result;

  if (!ws->types_sorted)
  {
    qsort (ws->types, ws->ntypes, sizeof (GWTypeInfo), typeinfo_cmp);
    ws->types_sorted = 1;
  }

  key.name = name;
  result = (GWTypeInfo *) bsearch (&key, ws->types, ws->ntypes,
                                   sizeof (GWTypeInfo), typeinfo_cmp);
  if (result == NULL)
  {
    int i;
    /* Recursivly search wrapsets we depend on. */
    for (i = 0; i < ws->ndependencies; i++)
    {
      result = gw_wrapset_lookup_type (ws->dependencies[i], name);
      if (result)
        break;
    }
  }
  return result;
}

void
gw_wrapset_add_function (GWWrapSet *ws,
                         void *proc,
                         int n_args,
                         const char *ret_type,
                         GWTypeSpec ret_typespec,
                         const char **arg_types,
                         GWTypeSpec *arg_typespecs,
                         const char *proc_name,
                         const char *generic_name)
{
  GWFunctionInfo *fi;
  ffi_type **arg_ffi = NULL;
  ffi_status status;
  int i;

  if (ws->nfunctions >= ws->nfuncs_allocated)
  {
    ws->nfuncs_allocated <<= 1;
    ws->functions = gw_realloc (ws->arena,
                                ws->functions, ws->nfuncs_allocated *
                                sizeof (GWFunctionInfo));
  }
  fi = &ws->functions[ws->nfunctions];
  fi->proc = proc;
  fi->n_args = n_args;
  fi->proc_name = proc_name;
  fi->generic_name = generic_name;

  assert ((!arg_types && !ret_type)
          || ((n_args == 0 || arg_types) && ret_type));
  
  if (arg_types != NULL)
  {
    if (fi->n_args > 0)
      fi->arg_types = gw_malloc (ws->arena,
                                 fi->n_args * sizeof (GWTypeInfo *));
    else
      fi->arg_types = NULL;

    for (i = 0; i < fi->n_args; i++)
    {
      fi->arg_types[i] = gw_wrapset_lookup_type (ws, arg_types[i]);
      if (fi->arg_types[i] == NULL)
        gw_raise_error (ws->arena, "%gw:wrapset-add-function",
                        "invalid argument type reference %s "
                        "in argument list of %s",
                        arg_types[i], fi->proc_name);
    }
  }
  else
    fi->arg_types = NULL;
  
  /* argument must be static */
  fi->arg_typespecs = arg_typespecs;

  fi->ret_type = ret_type ? gw_wrapset_lookup_type (ws, ret_type) : NULL;
  fi->ret_typespec = ret_typespec;
  
  fi->data_area_size = fi->n_args * sizeof (void *);
  
  if (fi->n_args > 0)
  {
    /* Data is used by ffi_call, so don't free it */
    arg_ffi = (ffi_type **) gw_malloc (ws->arena,
                                       sizeof (ffi_type *) * fi->n_args);
    for (i = 0; i < fi->n_args; i++)
    {
      arg_ffi[i] = fi->arg_types[i]->type;
      assert (arg_ffi[i] != NULL);
    }
  }

  if (fi->ret_type)
  {
    status = ffi_prep_cif (&fi->cif, FFI_DEFAULT_ABI, fi->n_args,
                           fi->ret_type->type, arg_ffi);
    assert (status == FFI_OK);
  
    /* now we know the sizes of the types and calculate the data
     * area size where we store the arguments' values */
    for (i = 0; i < fi->n_args; i++)
      fi->data_area_size += arg_ffi[i]->size;
    fi->data_area_size += fi->ret_type->type->size;
  }
  
  ws->nfunctions++;
}

/* Note that the wrapset must not be modified once this function has
 * been called. */
void
gw_wrapset_register (GWWrapSet *ws)
{
  gw_lang->register_wrapset (ws);
  
  if (nallocated_wrapsets <= nregistered_wrapsets)
  {
    if (nallocated_wrapsets > 0)
      nallocated_wrapsets <<= 1;
    else
      nallocated_wrapsets = 4;
    registered_wrapsets =
      gw_realloc (ws->arena, registered_wrapsets,
                  nallocated_wrapsets * sizeof (GWWrapSet *));
  }
  registered_wrapsets[nregistered_wrapsets++] = ws;
}

int
gw_runtime_init (GWLangSupport *lang)
{
  static int initialized = 0;
  
  if (initialized)
    return 0;

  gw_lang = lang;
  
  initialized = 1;
  return 1;
}
