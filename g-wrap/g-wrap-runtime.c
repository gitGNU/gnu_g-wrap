/**********************************************************************
Copyright (C) 2003-2004 Andreas Rottmann
 
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this software; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
USA.
**********************************************************************/

#if HAVE_CONFIG_H
#  include "config.h"
#endif

/* AIX requires this to be the first thing in the file. The #pragma
   directive is indented so pre-ANSI compilers will ignore it, rather
   than choke on it. */
#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>

#include "g-wrap-compatibility.h"
#include "g-wrap-runtime.h"

void
gw_runtime_get_version_info(int *major, int *revision, int *age)
{
  *major = GW_RUNTIME_INTERFACE_MAJOR_VER;
  *revision = GW_RUNTIME_INTERFACE_REVISION;
  *age = GW_RUNTIME_INTERFACE_AGE;
}


SCM
gw_enum_val2sym(GWEnumPair enum_pairs[], SCM scm_val, SCM scm_show_all_p)
{
  int enum_val;
  SCM scm_result;
  GWEnumPair *epair;
  int return_all_syms = SCM_NFALSEP (scm_show_all_p);

  if (return_all_syms)
    scm_result = SCM_EOL;
  else
    scm_result = SCM_BOOL_F;

  if (SCM_SYMBOLP (scm_val))
  {
    SCM scm_int_value = gw_enum_val2int (enum_pairs, scm_val);
    if (SCM_FALSEP (scm_int_value))
      return SCM_EOL;
    if (!return_all_syms)
      return scm_val;
    enum_val = scm_num2long (scm_int_value, 0, "gw:enum-val->sym");
  }
  else
  {
    /* this better be an int */
    enum_val = scm_num2long (scm_val, 0, "gw:enum-val->sym");
  }
  
  for (epair = enum_pairs; epair->sym != NULL; epair++)
  {
    if (enum_val == epair->val)
    {
      if (!return_all_syms) 
        return scm_str2symbol (epair->sym);
      
      scm_result = scm_cons (scm_str2symbol(epair->sym), scm_result);
    }
  }
  return scm_result;
}

SCM
gw_enum_val2int (GWEnumPair enum_pairs[], SCM scm_val)
{
  char *symstr = NULL;
  GWEnumPair *epair;

  if (SCM_NFALSEP(scm_integer_p (scm_val)))
  {
    SCM scm_existing_sym = gw_enum_val2sym (enum_pairs, scm_val, SCM_BOOL_F);
    if(SCM_FALSEP (scm_existing_sym))
      return SCM_BOOL_F;
    else
      return scm_val;
  }

  /* Flag support */
  if (scm_val == SCM_EOL || SCM_CONSP (scm_val))
  {
    SCM tail, s_val;
    int value = 0;
    
    for (tail = scm_val; tail != SCM_EOL; tail = SCM_CDR (tail))
    {
      if (!SCM_CONSP (tail) || !SCM_SYMBOLP (SCM_CAR (tail)))
        scm_wrong_type_arg("gw:enum-val->int", 1, scm_val);
      
      s_val = gw_enum_val2int (enum_pairs, SCM_CAR (tail));
      if (SCM_FALSEP (s_val))
        return s_val;
      
      value |= scm_num2long (s_val, 1, "gw:enum-val->int");
    }
    return scm_long2num (value);
  }
  
  if (!SCM_SYMBOLP (scm_val))
  {
    scm_wrong_type_arg("gw:enum-val->int", 1, scm_val);
    return SCM_UNDEFINED;
  }
  
  symstr = SCM_SYMBOL_CHARS(scm_val);

  for (epair = enum_pairs; epair->sym != NULL; epair++)
  {
    if (strcmp (symstr, epair->sym) == 0)
       return scm_long2num (epair->val);
  }

  return SCM_BOOL_F;
}

static SCM the_root_module = SCM_UNSPECIFIED;
static SCM is_a_p_proc = SCM_UNSPECIFIED;
static SCM module_add_x = SCM_UNSPECIFIED;
static SCM scm_sym_make = SCM_UNSPECIFIED;

/* TODO: Use snarfer for kewords & symbols */
static SCM k_specializers = SCM_UNSPECIFIED;
static SCM k_procedure = SCM_UNSPECIFIED;
static SCM k_name = SCM_UNSPECIFIED;
static SCM k_default = SCM_UNSPECIFIED;
static SCM sym_object = SCM_UNSPECIFIED;
static SCM sym_args = SCM_UNSPECIFIED;
static scm_t_bits dynproc_smob_tag = 0;

/* 1. methods of generic functions can come from any module.
 *    eg gst_props_entry_get and g_object_get.
 *
 * 2. the generic function can only be defined in one place, or it loses
 *    all knowledge of other methods (gst_props_entry_get replaces all
 *    definitions from other modules, eg g_object_get.)
 *
 * 3. therefore, we export the bindings for generics to the root module */

static void
gw_function_to_method_public (SCM proc, int nargs, SCM specializers,
                              SCM generic_name)
{
  SCM method_formals, method_args;
  SCM default_val = SCM_BOOL_F;
  SCM generic;
  SCM procm;
  SCM meth;
  clock_t start, end;
  int i;
  char buffer[32];
  int is_generic = 0;
  
  if (SCM_FALSEP (scm_procedure_p (proc)))
    return;

  generic =
    scm_sym2var (generic_name, scm_module_lookup_closure (the_root_module),
                 SCM_BOOL_F);

    
  if (SCM_NFALSEP (generic))
  {
    generic = SCM_VARIABLE_REF (generic);
    is_generic = SCM_NFALSEP (scm_call_2 (is_a_p_proc, generic,
                                          scm_class_generic));
  }
  
  if (!is_generic && SCM_NFALSEP (scm_procedure_p (generic))
      && (nargs > 0 && !SCM_EQ_P (SCM_CAR (specializers), scm_class_top)))
  {
    default_val = generic;
    generic = SCM_BOOL_F;
  }
  
  /* try not to shadow other bindings */
  if (!is_generic && SCM_NFALSEP (generic))
  {
    int old_len = SCM_SYMBOL_LENGTH (generic_name);
    char *new_name = (char *) scm_malloc (old_len + 2);
    new_name[0] = '.';
    memcpy (new_name + 1, SCM_SYMBOL_CHARS (generic_name), old_len);
    new_name[old_len + 1] = '\0';
    generic_name = scm_str2symbol (new_name);
    free (new_name);
    generic = SCM_BOOL_F;
  }

  if (SCM_FALSEP (generic))
  {
    /* Note that scm_make is *not* the same as calling "make". */
    if (SCM_FALSEP (default_val))
      generic = scm_call_3 (scm_sym_make, scm_class_generic,
                            k_name, generic_name);
    else
      generic = scm_apply_0 (scm_sym_make,
                             scm_list_5 (scm_class_generic,
                                         k_name, generic_name,
                                         k_default, default_val));

    scm_call_3 (module_add_x, the_root_module, generic_name,
                scm_make_variable (generic));
  }

  method_formals = SCM_EOL;
  for (i = nargs; i > 0; i--)
  {
    sprintf (buffer, "arg%d", i);
    method_formals = scm_cons (scm_str2symbol (buffer), method_formals);
  }
  
  method_args = scm_cons (proc, method_formals);

  procm = scm_closure (scm_list_2 (method_formals, method_args),
                           scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));
  
  meth = scm_apply_0 (scm_sym_make,
                      scm_list_5 (scm_class_method,
                                  k_specializers, specializers,
                                  k_procedure, procm));
  
  scm_add_method (generic, meth);
}

static int nregistered_wrapsets = 0;
static int nallocated_wrapsets = 0;
static GWWrapSet **registered_wrapsets = NULL;

GWWrapSet *
gw_wrapset_new (const char *name, const char *dependency, ...)
{
  GWWrapSet *ws;
  GWWrapSet **ws_deps;
  int i, ndeps;
  const int start_size = 4;
  va_list args;

  /* TODO: We use linear searching now (stuff would be easier when we
   * could use GLib). */
  
  for (i = 0; i < nregistered_wrapsets; i++)
    if (strcmp (registered_wrapsets[i]->name, name) == 0)
    {
      scm_misc_error ("%gw:wrapset-new", "Tried to double-register wrapset",
                      scm_makfrom0str (name));
                      
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
      scm_misc_error ("%gw:wrapset-new",
                      "Dependency on nonexisting wrapset: ~S.",
                      scm_list_1 (scm_makfrom0str (dependency)));
      
    ws_deps = scm_realloc (ws_deps, (ndeps + 1) * sizeof (GWWrapSet *));
    ws_deps[ndeps] = ws;

    dependency = va_arg (args, const char *);
  }
  va_end (args);
  
  ws = scm_malloc (sizeof (GWWrapSet));
  ws->name = name;
  
  ws->ndependencies = ndeps;
  ws->dependencies = ws_deps;
  
  ws->types = scm_malloc (start_size * sizeof (GWTypeInfo));
  ws->ntypes = 0;
  ws->ntypes_allocated = start_size;
  ws->types_sorted = 0;

  ws->functions = scm_malloc (start_size * sizeof (GWFunctionInfo));
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
                     GWFromScmFunc from_scm,
                     GWToScmFunc to_scm,
                     GWDestructorFunc destructor)
{
  GWTypeInfo *ti;
  
  if (ws->ntypes >= ws->ntypes_allocated)
  {
    ws->ntypes_allocated <<= 1;
    ws->types = scm_realloc (ws->types, ws->ntypes_allocated *
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
    
    type = scm_malloc (sizeof (ffi_type)
                       + (nsubtypes + 1) * sizeof (ffi_type *));
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
  ti->from_scm = from_scm;
  ti->to_scm = to_scm;
  ti->destructor = destructor;
  
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
                         int dynamic,
                         void *proc,
                         int n_req_args,
                         int n_opt_args,
                         int use_extra_args,
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
    ws->functions = scm_realloc (ws->functions, ws->nfuncs_allocated *
                                 sizeof (GWFunctionInfo));
  }
  fi = &ws->functions[ws->nfunctions];
  fi->dynamic = dynamic;
  fi->proc = proc;
  fi->n_required_args = n_req_args;
  fi->n_optional_args = n_opt_args;
  fi->use_extra_args = use_extra_args;
  fi->proc_name = proc_name;
  fi->generic_name = generic_name;
  
  assert ((arg_types && ret_type) || !dynamic);
  
  if (arg_types != NULL && fi->dynamic)
  {
    for (fi->nargs = 0; arg_types[fi->nargs] != NULL; fi->nargs++)
      ;
    
    if (fi->nargs > 0)
      fi->arg_types = scm_malloc (fi->nargs * sizeof (GWTypeInfo *));
    else
      fi->arg_types = NULL;
    
    for (i = 0; i < fi->nargs; i++)
    {
      fi->arg_types[i] = gw_wrapset_lookup_type (ws, arg_types[i]);
      if (fi->arg_types[i] == NULL)
        scm_misc_error ("%gw:wrapset-add-function",
                        "invalid argument type reference ~S "
                        "in argument list of ~S\n",
                        scm_list_2 (scm_makfrom0str (arg_types[i]),
                                    scm_makfrom0str (fi->proc_name)));
    }
  }
  else
  {
    fi->nargs = 0;
    fi->arg_types = NULL;
  }
  
  /* argument must be static */
  fi->arg_typespecs = arg_typespecs;

  fi->ret_type = ret_type ? gw_wrapset_lookup_type (ws, ret_type) : NULL;
  fi->ret_typespec = ret_typespec;
  
  fi->data_area_size = fi->nargs * sizeof (void *);
  
  if (fi->nargs > 0)
  {
    /* Data is used by ffi_call, so don't free it */
    arg_ffi = (ffi_type **) scm_malloc (sizeof (ffi_type *) * fi->nargs);
    for (i = 0; i < fi->nargs; i++)
    {
      arg_ffi[i] = fi->arg_types[i]->type;
      assert (arg_ffi[i] != NULL);
    }
  }

  if (fi->dynamic)
  {
    status = ffi_prep_cif (&fi->cif, FFI_DEFAULT_ABI, fi->nargs,
                           fi->ret_type->type, arg_ffi);
    assert (status == FFI_OK);
  
    /* now we know the sizes of the types and calculate the data
     * area size where we store the arguments' values */
    for (i = 0; i < fi->nargs; i++)
      fi->data_area_size += arg_ffi[i]->size;
    fi->data_area_size += fi->ret_type->type->size;
  }
  
  ws->nfunctions++;
}

static SCM
dynproc_smob_apply (SCM smob, SCM args)
{
  GWFunctionInfo *fi = (GWFunctionInfo *) SCM_SMOB_DATA (smob);
  SCM result;
  ffi_cif *cif;
  void **values;
  void *rvalue;
  int i;
  unsigned offset;
  void *data;
  GWError error;

  data = alloca (fi->data_area_size);
  values = (void **) data;

  error.status = GW_ERR_NONE;
  
  offset = fi->nargs * sizeof (void *);
  for (i = 0; i < fi->nargs; i++)
  {
    values[i] = (void *) ((unsigned char *) data + offset);
    if (!SCM_CONSP (args))
      scm_wrong_num_args (smob);
    fi->arg_types[i]->from_scm (values[i], &fi->arg_typespecs[i],
                                SCM_CAR (args), &error);
    if (error.status != GW_ERR_NONE)
      gw_handle_wrapper_error (&error, fi->proc_name, i + 1);
    offset += fi->arg_types[i]->type->size;
    args = SCM_CDR (args);
  }
  rvalue = (void *) ((unsigned char *) data + offset);
  
  ffi_call (&fi->cif, fi->proc, rvalue, values);

  result = fi->ret_type->to_scm (rvalue, &fi->ret_typespec, &error);
  if (error.status != GW_ERR_NONE)
    gw_handle_wrapper_error (&error, fi->proc_name, 0);

  fi->ret_type->destructor (rvalue, &fi->ret_typespec, 0, &error);
  if (error.status != GW_ERR_NONE)
    gw_handle_wrapper_error (&error, fi->proc_name, 0);

  /* call the destructors in the reverse orders, as done by the
   * traditional glue. */
  for (i = fi->nargs - 1; i >= 0; i--)
  {
    fi->arg_types[i]->destructor (values[i], &fi->arg_typespecs[i], 0, &error);
    if (error.status != GW_ERR_NONE)
      gw_handle_wrapper_error (&error, fi->proc_name, i + 1);
  }
  
  return result;
}

static int
dynproc_smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  GWFunctionInfo *fi = (GWFunctionInfo *)SCM_SMOB_DATA (smob);

  scm_display (scm_makfrom0str ("#<gw:dynproc "), port);
  scm_display (scm_makfrom0str (fi->proc_name), port);
  scm_display (scm_makfrom0str (" ("), port);
  scm_display (SCM_MAKINUM (fi->nargs), port);
  scm_display (scm_makfrom0str (")>"), port);
  
  return 1;
}

void
gw_runtime_init (void)
{
  static int initialized = 0;
  
  if (!initialized)
  {
    scm_load_goops();

    scm_sym_make = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_module_lookup (scm_module_goops,
                                                   "make")));
    is_a_p_proc = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_module_lookup (scm_module_goops,
                                                   "is-a?")));
    the_root_module = scm_permanent_object (
            SCM_VARIABLE_REF ( scm_c_lookup ("the-root-module")));
    module_add_x = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_lookup ("module-add!")));
    k_specializers = scm_permanent_object (
            scm_c_make_keyword ("specializers"));
    k_procedure = scm_permanent_object(
            scm_c_make_keyword ("procedure"));
    k_name = scm_permanent_object( scm_c_make_keyword ("name"));
    k_default = scm_permanent_object (scm_c_make_keyword ("default"));
    sym_object = scm_permanent_object (scm_str2symbol("object"));
    sym_args = scm_permanent_object (scm_str2symbol("args"));
    
    dynproc_smob_tag = scm_make_smob_type("%gw:dynamic-procedure",
                                          sizeof(GWFunctionInfo *));
    scm_set_smob_free (dynproc_smob_tag, NULL);
    scm_set_smob_apply (dynproc_smob_tag,
                        (SCM (*)())dynproc_smob_apply, 0, 0, 1);
    scm_set_smob_print (dynproc_smob_tag, dynproc_smob_print);
    
    initialized = 1;
  }
}

/* Performance Note: blocking GC improves performance considerably, at
 * the cost of increased memory usage.
 *
 * It was suggested that the GC slowness is caused through inproper
 * use of some _gc_ functions, but I don't think so. Recent
 * experiments showed that the GC is triggered by the add_method
 * invocation.  --rotty
 *
 */
  

/* Note that the wrapset must not be modified once this function has
 * been called. */
void
gw_wrapset_register (GWWrapSet *ws)
{
  int i;

  for (i = 0; i < ws->nfunctions; i++)
  {
    SCM subr;
    GWFunctionInfo *fi = &ws->functions[i];
    
    if (fi->dynamic)
    {
      SCM_NEWSMOB(subr, dynproc_smob_tag, fi);
      scm_c_define (fi->proc_name, subr);
    }
    else
      subr = scm_c_define_gsubr (fi->proc_name, 
                                 fi->n_required_args,
                                 fi->n_optional_args,
                                 fi->use_extra_args,
                                 (SCM (*)())fi->proc);
    
    if (fi->generic_name)
    {
      SCM specializers = SCM_EOL;
      int j;
      
      for (j = fi->nargs - 1; j >= 0; j--)
      {
        SCM klass = scm_class_top;
        const char *class_name = fi->arg_types[j]->class_name;

        /* we specialize only on the first parameter, since the others
         * don't work with gobject/<gvalue> */
        if (j == 0 && class_name)
          klass = SCM_VARIABLE_REF (scm_c_lookup (class_name));
        
        if (SCM_FALSEP (scm_call_2 (is_a_p_proc, klass, scm_class_class)))
          scm_misc_error ("%gw:wrapset-init", "specializer is not a class: ~A",
                          scm_list_1 (klass));
        specializers = scm_cons (klass, specializers);
      }
      
      gw_function_to_method_public (subr, fi->nargs, specializers,
                                    scm_str2symbol (fi->generic_name));
    }
  }
  
  if (nallocated_wrapsets <= nregistered_wrapsets)
  {
    if (nallocated_wrapsets > 0)
      nallocated_wrapsets <<= 1;
    else
      nallocated_wrapsets = 4;
    registered_wrapsets =
      scm_realloc (registered_wrapsets,
                   nallocated_wrapsets * sizeof (GWWrapSet *));
  }
  registered_wrapsets[nregistered_wrapsets++] = ws;
}

void
gw_handle_wrapper_error(GWError *error,
                        const char *func_name,
                        unsigned int arg_pos)
{
  static SCM out_of_range_key = SCM_BOOL_F;
  static SCM wrong_type_key = SCM_BOOL_F;

  if (SCM_FALSEP (out_of_range_key))
    out_of_range_key = scm_permanent_object (
            scm_c_make_keyword("out-of-range"));
  if (SCM_FALSEP (wrong_type_key))
    wrong_type_key = scm_permanent_object (
            scm_c_make_keyword("wrong-type"));
  
  switch (error->status)
  {
    case GW_ERR_NONE:
      scm_misc_error(func_name,
                     "asked to handle error when there wasn't one",
                     SCM_EOL);
      break;
    case GW_ERR_MISC:
      /* scm_data is a list of format args for misc_msg */
      scm_misc_error(func_name, error->message, error->data);
      break;
    case GW_ERR_MEMORY:
      scm_memory_error(func_name);
      break;
    case GW_ERR_RANGE:
      scm_error (out_of_range_key,
                 func_name,
                 "Out of range: ~S",
                 scm_cons (error->data, SCM_EOL),
                 SCM_BOOL_F);
      break;
    case GW_ERR_TYPE:
      scm_error(wrong_type_key,
                func_name,
                "Wrong type: ",
                scm_cons (error->data, SCM_EOL),
                SCM_BOOL_F);
      break;
    case GW_ERR_ARGC:
      scm_wrong_num_args(scm_makfrom0str(func_name)); break;
    case GW_ERR_ARG_RANGE:
      /* scm_data is the bad arg */
      scm_out_of_range(func_name, error->data); break;
    case GW_ERR_ARG_TYPE:
      /* scm_data is the bad arg */
      scm_wrong_type_arg(func_name, arg_pos, error->data); break;
    default:
      scm_misc_error(func_name,
                     "asked to handle nonexistent gw:error type: ~S",
                     scm_cons(scm_long2num(error->status), SCM_EOL));    
    break;
  };
  exit(1);
}
