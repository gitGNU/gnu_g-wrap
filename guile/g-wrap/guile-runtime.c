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

#include "g-wrap/guile-compatibility.h"
#include "g-wrap/guile-runtime.h"

#define ARENA NULL /* Guile has no concept of an arena */

static SCM the_scm_module = SCM_UNSPECIFIED;
static SCM is_a_p_proc = SCM_UNSPECIFIED;
static SCM module_add_x = SCM_UNSPECIFIED;
static SCM scm_sym_make = SCM_UNSPECIFIED;

static SCM latent_variables_hash_hash = SCM_BOOL_F;

static SCM latent_generics_hash = SCM_BOOL_F;
static SCM old_binder_proc = SCM_BOOL_F;

/* TODO: Use snarfer for kewords & symbols */
static SCM k_specializers = SCM_UNSPECIFIED;
static SCM k_procedure = SCM_UNSPECIFIED;
static SCM k_name = SCM_UNSPECIFIED;
static SCM k_default = SCM_UNSPECIFIED;
static SCM sym_object = SCM_UNSPECIFIED;
static SCM sym_args = SCM_UNSPECIFIED;
static scm_t_bits dynproc_smob_tag = 0;

#if 0 // not useed ATM
void
gw_guile_runtime_get_version_info(int *major, int *revision, int *age)
{
  *major = GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER;
  *revision = GW_GUILE_RUNTIME_INTERFACE_REVISION;
  *age = GW_GUILE_RUNTIME_INTERFACE_AGE;
}
#endif

SCM
gw_guile_enum_val2sym(GWEnumPair enum_pairs[], SCM scm_val, SCM scm_show_all_p)
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
    SCM scm_int_value = gw_guile_enum_val2int (enum_pairs, scm_val);
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
gw_guile_enum_val2int (GWEnumPair enum_pairs[], SCM scm_val)
{
  char *symstr = NULL;
  GWEnumPair *epair;

  if (SCM_NFALSEP(scm_integer_p (scm_val)))
  {
    SCM scm_existing_sym = gw_guile_enum_val2sym (enum_pairs, scm_val,
                                                  SCM_BOOL_F);
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
      
      s_val = gw_guile_enum_val2int (enum_pairs, SCM_CAR (tail));
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

static SCM gw_user_module_binder_proc (SCM module, SCM sym, SCM definep)
{
  SCM latent_variables_hash, pair, val, var;

  latent_variables_hash =
    scm_hashq_ref (latent_variables_hash_hash, module, SCM_BOOL_F);
  if (SCM_FALSEP (latent_variables_hash))
    abort ();
    
  pair = scm_hashq_ref (latent_variables_hash, sym, SCM_BOOL_F);
  if (SCM_FALSEP (pair))
    return SCM_BOOL_F;
  
  val = scm_call_1 (SCM_CAR (pair), SCM_CDR (pair));
  var = scm_make_variable (val);
  scm_call_3 (module_add_x, module, sym, var);
  return var;
}

void gw_guile_make_latent_variable (SCM sym, SCM proc, SCM arg)
{
  SCM latent_variables_hash;
  SCM module = scm_current_module ();
  
  /* Unlike generics, variables are hashed per-module. */
  if (SCM_FALSEP (latent_variables_hash_hash))
    latent_variables_hash_hash = scm_permanent_object (scm_c_make_hash_table (31));

  latent_variables_hash =
    scm_hashq_ref (latent_variables_hash_hash, module, SCM_BOOL_F);
  if (SCM_FALSEP (latent_variables_hash)) {
    latent_variables_hash = scm_c_make_hash_table (31);
    scm_hashq_create_handle_x (latent_variables_hash_hash, module,
                               latent_variables_hash);
    /* Also need to hack the module: */
    if (SCM_FALSEP (SCM_MODULE_BINDER (module)))
      scm_struct_set_x (module, SCM_MAKINUM (scm_module_index_binder),
                        scm_c_make_gsubr ("%gw-user-module-binder", 3, 0, 0,
                                          gw_user_module_binder_proc));
  }
  
  if (SCM_NFALSEP (scm_hashq_ref (latent_variables_hash, sym, SCM_BOOL_F))) {
    gw_raise_error (NULL, "Latent var already registered: %s", SCM_SYMBOL_CHARS (sym));
    return;
  }

  scm_hashq_create_handle_x (latent_variables_hash, sym,
                             scm_cons (proc, arg));
}

/* 1. methods of generic functions can come from any module.
 *    eg gst_props_entry_get and g_object_get.
 *
 * 2. the generic function can only be defined in one place, or it loses
 *    all knowledge of other methods (gst_props_entry_get replaces all
 *    definitions from other modules, eg g_object_get.)
 *
 * 3. therefore, we export the bindings for generics to the root module */

/* Making generics takes a lot of time. Our strategy is to offload generic
   creation until they are needed. First, check if the root module already has a
   definition for generic_name. In that case, we need to make the generic.
   Otherwise, add the proc and specializers to a hash table for the module
   binder proc to instantiate as needed. */

/* Grr. Seems subrs can't be methods. */
static void 
gw_guile_add_subr_method (SCM generic, SCM subr, SCM class_name, SCM module,
                          int n_req_args, int use_optional_args)
{
  int i;
  char buffer[32];
  SCM specializers, formals, method_args, procm, meth;
  
  if (use_optional_args)
  {
    formals = SCM_EOL; //scm_str2symbol ("opt-args");
    specializers = scm_class_top;
  }
  else
    formals = specializers = SCM_EOL;
  
  for (i = n_req_args; i > 0; i--)
  {
    sprintf (buffer, "arg%d", i);
    formals = scm_cons (scm_str2symbol (buffer), formals);
    if (i == 1)
      specializers =
        scm_cons (SCM_VARIABLE_REF (scm_module_lookup (module, class_name)),
                  specializers);
    else
      specializers = scm_cons (scm_class_top, specializers);
  }

  method_args = scm_cons (subr, formals);
  procm = scm_closure (scm_list_2 (formals, method_args),
                       scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));
  meth = scm_apply_0 (scm_sym_make,
                      scm_list_5 (scm_class_method,
                                  k_specializers, specializers,
                                  k_procedure, procm));
  scm_add_method (generic, meth);
} 


static SCM gw_scm_module_binder_proc (SCM module, SCM sym, SCM definep)
{
  SCM proc_list, generic, var;

  /* We hack the scm module because it's the interface to the root module. The
   * scm module and the root module share the same obarray. */

  proc_list = scm_hashq_ref (latent_generics_hash, sym, SCM_BOOL_F);

  if (SCM_FALSEP (proc_list)) {
    if (SCM_FALSEP (old_binder_proc))
      return SCM_BOOL_F;
    else
      return scm_call_3 (old_binder_proc, module, sym, definep);
  }
  
  /* We need to make the generic now. Because the binder proc is
   * called, we know there's nothing else in the root module to
   * collide with our name. */
  generic = scm_apply_0 (scm_sym_make,
                         scm_list_3 (scm_class_generic, k_name, sym));

  while (!SCM_NULLP (proc_list))
  {
    SCM entry, *velts;
    entry = SCM_CAR (proc_list);
    /* entry := #(proc class_name module n_req_args use_optional_args) */
    velts = SCM_VELTS (entry);

    gw_guile_add_subr_method (generic,
                              velts[0], velts[1], velts[2],
                              SCM_INUM (velts[3]), SCM_NFALSEP (velts[4]));

    proc_list = SCM_CDR (proc_list);
  }

  var = scm_make_variable (generic);
  scm_call_3 (module_add_x, module, sym, var);

  return var;
}

void
gw_guile_procedure_to_method_public (SCM proc, SCM class_name,
                                     SCM generic_name,
                                     SCM n_req_args, SCM use_optional_args)
#define FUNC_NAME "%gw:procedure-to-method-public!"
{
  static int scm_module_hacked = 0;
  SCM generic = SCM_BOOL_F;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_SYMBOL (2, class_name);
  SCM_VALIDATE_SYMBOL (3, generic_name);
  SCM_VALIDATE_INUM (4, n_req_args);
  /* the fifth is a bool */
  
  if (!scm_module_hacked)
  {
    scm_module_hacked = 1;
    old_binder_proc = scm_permanent_object (
            SCM_MODULE_BINDER (the_scm_module));
    scm_struct_set_x (the_scm_module, SCM_MAKINUM (scm_module_index_binder),
                      scm_c_make_gsubr ("%gw-scm-module-binder", 3, 0,
                                        0, gw_scm_module_binder_proc));
  }

  if (SCM_FALSEP (latent_generics_hash))
    latent_generics_hash = scm_permanent_object (scm_c_make_hash_table (53));

  if (SCM_FALSEP (scm_hashq_ref (latent_generics_hash, generic_name,
                                 SCM_BOOL_F)))
    generic = scm_sym2var (generic_name,
                           scm_module_lookup_closure (the_scm_module),
                           SCM_BOOL_F);

  if (SCM_FALSEP (generic))
  {
    /* Handle the common case when there's not already a symbol in the
     * root. */
    SCM entry, *velts;
    SCM handle = scm_hashq_create_handle_x (latent_generics_hash, generic_name,
                                            SCM_EOL);
    entry = scm_c_make_vector (5, SCM_BOOL_F);
    /* entry := #(proc class_name module n_req_args use_optional_args) */
    velts = SCM_VELTS (entry);
    velts[0] = proc;
    velts[1] = class_name;
    velts[2] = scm_current_module ();
    velts[3] = n_req_args;
    velts[4] = use_optional_args;
    SCM_SETCDR (handle, scm_cons (entry, SCM_CDR (handle)));
    return;
  }
  else
  {
    int is_generic = 0;
    /* Otherwise, we have to make the generic. */

    generic = SCM_VARIABLE_REF (generic);
    /* I seem to remember this is_a_p thing is a hack around GOOPS's deficient
       macros, but I don't remember */
    is_generic = SCM_NFALSEP (scm_call_2 (is_a_p_proc, generic,
                                          scm_class_generic));

    if (!is_generic)
    {
      if (SCM_NFALSEP (scm_procedure_p (generic)))
      {
        /* We need to fall back on the original binding. */
        SCM default_val = generic;
        generic = scm_apply_0 (scm_sym_make,
                               scm_list_5 (scm_class_generic,
                                           k_name, generic_name,
                                           k_default, default_val));
      }
      else
      {
        /* We can't extend the binding, have to use a different name. */
        int old_len = SCM_SYMBOL_LENGTH (generic_name);
        char *new_name = (char *) scm_malloc (old_len + 2);
        new_name[0] = '.';
        memcpy (new_name + 1, SCM_SYMBOL_CHARS (generic_name), old_len);
        new_name[old_len + 1] = '\0';
        generic_name = scm_str2symbol (new_name);
        free (new_name);
        generic = scm_call_3 (scm_sym_make, scm_class_generic,
                              k_name, generic_name);
      }
      /* a rash and uncalled-for act */
      scm_call_3 (module_add_x, the_scm_module, generic_name,
                  scm_make_variable (generic));
    }
  }

  gw_guile_add_subr_method (generic, proc, class_name, scm_current_module(),
                            SCM_INUM (n_req_args),
                            SCM_NFALSEP (use_optional_args));
}
#undef FUNC_NAME

static SCM
dynproc_smob_apply (SCM smob, SCM args)
{
  GWFunctionInfo *fi = (GWFunctionInfo *) SCM_SMOB_DATA (smob);
  SCM result;
  void **values;
  void *rvalue;
  int i;
  unsigned offset;
  void *data;
  GWError error;
  
  /* TODO: Most of this should be factored out into the core; but how
   * to deal with the arg list? */
  
  data = alloca (fi->data_area_size);
  values = (void **) data;

  error.status = GW_ERR_NONE;

  offset = fi->n_req_args * sizeof (void *);
  for (i = 0; i < fi->n_req_args; i++)
  {
    SCM arg;
    values[i] = (void *) ((unsigned char *) data + offset);
    if (!SCM_CONSP (args))
      scm_wrong_num_args (smob);
    arg = SCM_CAR (args);
    fi->arg_types[i]->unwrap_value (values[i], ARENA,
                                    &fi->arg_typespecs[i], &arg, &error);
    if (error.status != GW_ERR_NONE)
      gw_handle_wrapper_error (ARENA, &error, fi->proc_name, i + 1);
    offset += fi->arg_types[i]->type->size;
    args = SCM_CDR (args);
  }
  rvalue = (void *) ((unsigned char *) data + offset);
  
  ffi_call (&fi->cif, fi->proc, rvalue, values);

  fi->ret_type->wrap_value (&result, ARENA, &fi->ret_typespec, rvalue, &error);
  if (error.status != GW_ERR_NONE)
    gw_handle_wrapper_error (ARENA, &error, fi->proc_name, 0);

  fi->ret_type->destruct_value (ARENA, rvalue, &fi->ret_typespec, &error);
  if (error.status != GW_ERR_NONE)
    gw_handle_wrapper_error (ARENA, &error, fi->proc_name, 0);

  /* call the destructors in the reverse orders, as done by the
   * traditional glue. */
  for (i = fi->n_req_args - 1; i >= 0; i--)
  {
    fi->arg_types[i]->destruct_value (ARENA, values[i], &fi->arg_typespecs[i],
                                      &error);
    if (error.status != GW_ERR_NONE)
      gw_handle_wrapper_error (ARENA, &error, fi->proc_name, i + 1);
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
  scm_display (SCM_MAKINUM (fi->n_req_args), port);
  scm_display (scm_makfrom0str (")>"), port);
  
  return 1;
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
  

static void
gw_guile_handle_wrapper_error(GWLangArena arena,
                              GWError *error,
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

static void
gw_guile_raise_error (GWLangArena arena, const char *proc, const char *error)
{
  scm_misc_error (proc, error, SCM_EOL);
}

static void
gw_guile_register_wrapset (GWWrapSet *ws)
{
  int i;

  for (i = 0; i < ws->nfunctions; i++)
  {
    SCM subr;
    GWFunctionInfo *fi = &ws->functions[i];

    if (fi->ret_type)
    {
      SCM_NEWSMOB (subr, dynproc_smob_tag, fi);
      scm_c_define (fi->proc_name, subr);
    }
    else
    {
      int n_req_args = fi->n_req_args;
      int n_optional_args = fi->n_optional_args;
      int use_extra_args = 0;
      
      if (n_req_args > SCM_GSUBR_MAX)
      {
        n_req_args = SCM_GSUBR_MAX - 1;
        use_extra_args = 1;
      }
      if (!use_extra_args && n_optional_args + n_req_args >= SCM_GSUBR_MAX)
      {
        n_optional_args = SCM_GSUBR_MAX - 1 - n_req_args;
        use_extra_args = 1;
      }
      subr = scm_c_define_gsubr (fi->proc_name, n_req_args, n_optional_args,
                                 use_extra_args, (SCM (*)())fi->proc);
    }
    
    if (fi->generic_name && fi->arg_types && fi->arg_types[0]->class_name)
    {
      gw_guile_procedure_to_method_public (
              subr,
              scm_str2symbol (fi->arg_types[0]->class_name),
              scm_str2symbol (fi->generic_name),
              SCM_MAKINUM (fi->n_req_args),
              (fi->n_optional_args ? SCM_BOOL_T : SCM_BOOL_F));
    }
  }
}

static void *
gw_guile_malloc (GWLangArena arena, size_t size)
{
  return scm_malloc (size);
}

static void *
gw_guile_realloc (GWLangArena arena, void *mem, size_t size)
{
  return scm_realloc (mem, size);
}

void
gw_guile_runtime_init (void)
{
  static GWLangSupport guile_support = {
    .register_wrapset = gw_guile_register_wrapset,
    .malloc = gw_guile_malloc,
    .realloc = gw_guile_realloc,
    .raise_error = gw_guile_raise_error,
    .handle_wrapper_error = gw_guile_handle_wrapper_error
  };
  
  if (gw_runtime_init (&guile_support))
  {
    scm_load_goops();

    scm_sym_make = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_module_lookup (scm_module_goops,
                                                   "make")));
    is_a_p_proc = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_module_lookup (scm_module_goops,
                                                   "is-a?")));
    the_scm_module = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_lookup ("the-scm-module")));
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

    gw_wct_initialize ();
  }
}

