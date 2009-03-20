/**********************************************************************
Copyright (C) 2003-2005 Andreas Rottmann
 
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

#include <alloca.h>

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>

#include "g-wrap/guile-compatibility.h"
#include "g-wrap/guile-runtime.h"

#define ARENA NULL /* Guile has no concept of an arena */

static SCM is_a_p_proc = SCM_UNSPECIFIED;
static SCM module_add_x = SCM_UNSPECIFIED;
static SCM scm_sym_make = SCM_UNSPECIFIED;

/* TODO: Use snarfer for kewords & symbols */
static SCM k_specializers = SCM_UNSPECIFIED;
static SCM k_procedure = SCM_UNSPECIFIED;
static SCM k_name = SCM_UNSPECIFIED;
static SCM k_default = SCM_UNSPECIFIED;
static SCM sym_generic = SCM_UNSPECIFIED;
static SCM sym_class = SCM_UNSPECIFIED;
static SCM sym_sys_gw_latent_variables_hash = SCM_UNSPECIFIED;
static scm_t_bits dynproc_smob_tag = 0;

static void gw_guile_handle_wrapper_error(GWLangArena arena,
                                          GWError *error,
                                          const char *func_name,
                                          unsigned int arg_pos) GW_NORETURN;
static void gw_guile_raise_error (GWLangArena arena, const char *proc,
                                  const char *error) GW_NORETURN;

#if 0 /* not used ATM */
void
gw_guile_runtime_get_version_info(int *major, int *revision, int *age)
{
  *major = GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER;
  *revision = GW_GUILE_RUNTIME_INTERFACE_REVISION;
  *age = GW_GUILE_RUNTIME_INTERFACE_AGE;
}
#endif

SCM
gw_guile_enum_val2sym(const GWEnumPair enum_pairs[], SCM scm_val,
		      SCM scm_show_all_p)
{
  int enum_val;
  SCM scm_result;
  const GWEnumPair *epair;
  int return_all_syms = scm_is_true (scm_show_all_p);

  if (return_all_syms)
    scm_result = SCM_EOL;
  else
    scm_result = SCM_BOOL_F;

  if (scm_is_symbol (scm_val))
  {
    SCM scm_int_value = gw_guile_enum_val2int (enum_pairs, scm_val);
    if (scm_is_false (scm_int_value))
      return SCM_EOL;
    if (!return_all_syms)
      return scm_val;
    enum_val = scm_to_long (scm_int_value);
  }
  else
  {
    /* this better be an int */
    enum_val = scm_to_long (scm_val);
  }
  
  for (epair = enum_pairs; epair->sym != NULL; epair++)
  {
    if (enum_val == epair->val)
    {
      if (!return_all_syms)
        return scm_from_locale_symbol (epair->sym);

      scm_result = scm_cons (scm_from_locale_symbol (epair->sym),
			     scm_result);
    }
  }
  return scm_result;
}

SCM
gw_guile_enum_val2int (const GWEnumPair enum_pairs[], SCM scm_val)
{
  char *symstr = NULL;
  const GWEnumPair *epair;

  if (scm_is_true(scm_integer_p (scm_val)))
  {
    SCM scm_existing_sym = gw_guile_enum_val2sym (enum_pairs, scm_val,
                                                  SCM_BOOL_F);
    if(scm_is_false (scm_existing_sym))
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
      if (!SCM_CONSP (tail) || (!scm_is_symbol (SCM_CAR (tail))))
        scm_wrong_type_arg("gw:enum-val->int", 1, scm_val);
      
      s_val = gw_guile_enum_val2int (enum_pairs, SCM_CAR (tail));
      if (scm_is_false (s_val))
        return s_val;
      
      value |= scm_to_long (s_val);
    }
    return scm_from_long (value);
  }
  
  if (!scm_is_symbol (scm_val))
  {
    scm_wrong_type_arg("gw:enum-val->int", 1, scm_val);
    return SCM_UNDEFINED;
  }

  GW_ACCESS_SYMBOL (symstr, scm_val);

  for (epair = enum_pairs; epair->sym != NULL; epair++)
  {
    if (strcmp (symstr, epair->sym) == 0)
       return scm_from_long (epair->val);
  }

  return SCM_BOOL_F;
}

/* Helper method, because it seems scm_add_method doesn't work for subrs */
static void 
gw_guile_add_subr_method (SCM generic, SCM subr, SCM all_specializers,
                          SCM module, int n_req_args, int use_optional_args)
{
  int i;
  char buffer[32];
  SCM specializers, formals, procm, meth, rest_sym = SCM_BOOL_F;
  
  specializers = SCM_EOL;
  for (i = n_req_args; i > 0 && SCM_CONSP (all_specializers); i--)
  {
    SCM class_name = SCM_CAR (all_specializers);
    if (scm_is_true (class_name)) {
        SCM var = scm_module_lookup (module, class_name);
        specializers = scm_cons (SCM_VARIABLE_REF (var), specializers);
    } else {
        specializers = scm_cons (scm_class_top, specializers);
    }
    all_specializers = SCM_CDR (all_specializers);
  }
  specializers = scm_reverse (specializers);
  
  if (use_optional_args)
  {
    rest_sym = scm_from_locale_symbol ("rest");
    specializers = scm_append_x (scm_list_2 (specializers, scm_class_top));
  }
  
  formals = SCM_EOL;
  for (i = n_req_args; i > 0; i--)
  {
    sprintf (buffer, "arg%d", i);
    formals = scm_cons (scm_from_locale_symbol (buffer), formals);
  }

  if (use_optional_args)
  {
    SCM f_apply = scm_c_eval_string ("apply");
    procm = scm_closure (
            scm_list_2 (scm_append (scm_list_2 (formals, rest_sym)),
                        scm_append (scm_list_3
                                    (scm_list_2 (f_apply, subr),
                                     formals,
                                     scm_cons (rest_sym, SCM_EOL)))),
            scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));
  }
  else
  {
    procm = scm_closure (scm_list_2 (formals, scm_cons (subr, formals)),
                         scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE));
  }

  meth = scm_apply_0 (scm_sym_make,
                      scm_list_5 (scm_class_method,
                                  k_specializers, specializers,
                                  k_procedure, procm));
  scm_add_method (generic, meth);
} 

/* What's going on here?
 *
 * Workarounds for two problems, that's what. They are (1) a problem with
 * object-oriented design based on generic functions, and (2) the fact that
 * making generic functions and classes in Guile is very slow.
 *
 * For the first problem, consider the case of an application that imports two
 * unrelated modules, `foo' and `bar', which define `foo-frob' and `bar-frob',
 * respectively. The obvious thing would be to define a generic function `frob',
 * such that it does the right thing on both foo and bar objects.
 * 
 * However this is not possible with GOOPS, guile's object system, unless the
 * foo and bar methods are defined against the same generic function object[0].
 * G-Wrap supports this mode of operation by allowing the user to specify a
 * generics module programmatically, via gw_guile_set_generics_module_x. G-Wrap
 * will default to installing generics in a newly created submodule of the
 * current module, %generics.
 *
 * This case is most common with libraries for which generic functions are not
 * seen as having any intrinsic meaning, when instead they are just used as
 * abbreviations of the full function name. For example, gtk-bin-add and
 * gst-bin-add both abbreviate to `add' for g-wrap, although they are not
 * related actions.
 * 
 * The second problem is that guile's classes and generic functions take a long
 * time to create. (This is because goops.scm is a lot of scheme code, and guile
 * does not have a compiler.) This affects large class libraries like
 * guile-gnome, making it so that loading a module with many generics and
 * classes can take upwards of 15 seconds.
 *
 * G-Wrap works around this by observing that only a small portion of such a
 * large class library would ever be used by any given application. Therefore it
 * supports delaying the instantiation of the classes and generic functions to
 * until they are accessed.
 *
 * However this raises an issue: is the behavior of a guile system with lazy
 * binding the same as if all of the symbols are immediately bound?
 *
 * To answer this question, we have to look at the implementation of
 * symbol->variable mapping, and see if the implementation of lazy binding
 * affects the symbol->variable resolution process.
 *
 * When guile evalutes an expression for the first time, it memoizes it.
 * Memoization will look up the variables for all of the values that the
 * expression needs. For example, memoization of (sin x) would memoize the
 * variables for the symbols `sin' and `x'. Variables are located by traversing
 * the list of used modules, in order, and calling scm_sym2var on the module.
 *
 * scm_sym2var eventually dispatches down to module_variable in modules.c, which
 * searches for a variable in this order: (1) the module's obarray; (2) the
 * module's binder proc, if any; (3) recursively calling module_variable on
 * modules in the module's use list. So we see that since a module's obarray and
 * binder proc are always called together, for a given set of generics being
 * exported by a module, the question "is X a member of this set" will always be
 * answered in the same way, because the set of exports is a union of the
 * obarray and the symbols to which the binder proc will return a variable.
 *
 * If that memoized expression is evaluated again, it will not run through the
 * lookup procedure, relying instead on the memoized variables. This can be
 * demonstrated by the following procedure:
 *
   (define-module (one) #:export (foo))
   (define foo 1)
   (define-module (two) #:export (foo))
   (define foo 2)

   (define-module (zero))
   (define (get-foo) foo)
   (use-modules (one))
   foo ; => 1
   (get-foo) ; => 1       ; memoize location of foo, from (one)
   (use-modules (two))
   foo ; => 2
   (get-foo) ; => 1       ; returns value of memoized variable

   (define (get-foo) foo) ; new definition, not yet memoized
   (get-foo) ; => 2       ; memoize and look up foo
 * 
 * This behavior is not defined by the scheme standard, inasmuch as r5rs does
 * not define anything module-related. This is an acceptable situation,
 * statically.
 *
 * Some g-wrap modules that choose to share generics modules might experience
 * problems, however. If the set of exports of the generics module changes,
 * perhaps due to imports in unrelated modules, the variables captured by the
 * symbol->variable lookup algorithm can depend on what other modules import.
 *
 * However this problem is a result of our shared-generics-module strategy, not
 * the memoization strategy. So we can conclude that memoization does not affect
 * scm_sym2var.
 *
 * Practically speaking, our strategy is this: Delay generic creation until they
 * are needed. When declaring a method, first check if the generics module
 * already has a definition for generic_name. In that case, we need to make the
 * generic. Otherwise, add the proc and specializers to a hash table for the
 * module binder proc to instantiate as needed.
 *
 * [0] Guile 1.8 does support merging generics from disparate modules; perhaps
 * g-wrap should look into exclusively supporting this mode of operation.
 * However this interacts poorly with our lazy binding.
 */

static SCM
make_class_variable (SCM proc, SCM arg) 
{
  return scm_make_variable (scm_call_1 (proc, arg));
}

static SCM
allocate_generic_variable (SCM module, SCM sym)
{
  SCM uses, var = SCM_BOOL_F, generic, used = SCM_BOOL_F;

  for (uses=SCM_MODULE_USES(module); !scm_is_null (uses); uses=scm_cdr(uses)) {
    used = scm_car (uses);
    var = scm_sym2var (sym, scm_module_lookup_closure (used), SCM_BOOL_F);
    if (!scm_is_false (var))
      break;
  }

  if (scm_is_false (var)) {
    /* Symbol unbound, make a new generic */
    generic = scm_apply_0 (scm_sym_make,
                           scm_list_3 (scm_class_generic, k_name, sym));
    return scm_make_variable (generic);
  } else if (scm_is_true (scm_call_2 (is_a_p_proc, scm_variable_ref (var),
                                      scm_class_generic))) {
    /* I seem to remember theq is_a_p thing is a hack around GOOPS's deficient
       macros, but I don't remember. Anyway the existing binding is a generic,
       let's use it */
    return var;
  } else if (scm_is_true (scm_procedure_p (scm_variable_ref (var)))) {
    /* Make a generic that falls back on the original binding. NB: generics also
       satisfy procedure?. */
    generic = scm_apply_0 (scm_sym_make,
                           scm_list_5 (scm_class_generic,
                                       k_name, sym,
                                       k_default, scm_variable_ref (var)));
    return scm_make_variable (generic);
  } else {
    /* We can't extend the binding, warn and fall through. */
    scm_display (scm_from_locale_string ("WARNING: generic "),
                 scm_current_error_port ());
    scm_display (sym, scm_current_error_port ());
    scm_display (scm_from_locale_string (" incompatibly bound in module "),
                 scm_current_error_port ());
    scm_display (used, scm_current_error_port ());
    scm_newline (scm_current_error_port ());
    return SCM_BOOL_F;
  }
}

static SCM
make_generic_variable (SCM module, SCM sym, SCM procs) 
{
  SCM var = allocate_generic_variable (module, sym);
  
  if (!scm_is_false (var)) {
    SCM generic = scm_variable_ref (var);

    /* hang the methods off the generic */
    for (; !scm_is_null(procs); procs=SCM_CDR(procs)) {
      /* entry := #(proc class_name module n_req_args use_optional_args) */
      SCM entry = SCM_CAR (procs);

      gw_guile_add_subr_method (generic,
                                SCM_SIMPLE_VECTOR_REF (entry, 0),
                                SCM_SIMPLE_VECTOR_REF (entry, 1),
                                SCM_SIMPLE_VECTOR_REF (entry, 2),
                                scm_to_int (SCM_SIMPLE_VECTOR_REF (entry, 3)),
                                scm_is_true (SCM_SIMPLE_VECTOR_REF (entry, 4)));
    }
  }
  
  return var;
}

static SCM
gw_module_binder_proc (SCM module, SCM sym, SCM definep)
{
  SCM latent_variables_hash, pair, var;
  
  latent_variables_hash =
    scm_hashq_ref (SCM_MODULE_OBARRAY (module),
                   sym_sys_gw_latent_variables_hash, SCM_BOOL_F);
  if (scm_is_false (latent_variables_hash))
    abort ();
  else
    latent_variables_hash = scm_variable_ref (latent_variables_hash);
    
  pair = scm_hashq_ref (latent_variables_hash, sym, SCM_BOOL_F);

  if (scm_is_false (pair))
    return SCM_BOOL_F;
  
  if (scm_is_eq (scm_car (pair), sym_class)) {
    var = make_class_variable (scm_cadr (pair), scm_cddr (pair));
  } else if (scm_is_eq (scm_car (pair), sym_generic)) {
    var = make_generic_variable (module, sym, scm_cdr (pair));
  } else {
    scm_error (scm_from_locale_symbol ("wrong-type"),
               "%gw-module-binder",
               "Bad latent binding value for ~S: ~S",
               scm_cons (sym, scm_cons (pair, SCM_EOL)),
               SCM_BOOL_F);
    return SCM_BOOL_F; /* not reached */
  }
    
  if (!scm_is_false (var))
    scm_call_3 (module_add_x, module, sym, var);
  scm_hashq_remove_x (latent_variables_hash, sym);
  return var;
}

/* returns the latent variables hash */
static SCM
gw_guile_ensure_latent_variables_hash_and_binder (SCM module)
{
  SCM ret;
  
  ret = scm_hashq_ref (SCM_MODULE_OBARRAY (module),
                       sym_sys_gw_latent_variables_hash,
                       SCM_BOOL_F);

  if (SCM_FALSEP (ret)) {
    if (SCM_NFALSEP (SCM_MODULE_BINDER (module))) {
      scm_error (scm_from_locale_symbol ("misc-error"),
                 "%gw-guile-ensure-latent-variables-hash-and-binder",
                 "Module ~S already has a binder proc: ~S",
                 scm_cons (module,
                           scm_cons (SCM_MODULE_BINDER (module), SCM_EOL)),
                 SCM_BOOL_F);
      return SCM_BOOL_F; /* won't get here */
    }

    scm_struct_set_x (module, SCM_MAKINUM (scm_module_index_binder),
                      scm_c_make_gsubr ("%gw-module-binder", 3, 0,
                                        0, gw_module_binder_proc));

    ret = scm_make_variable (scm_c_make_hash_table (53));
    scm_hashq_set_x (SCM_MODULE_OBARRAY (module),
                     sym_sys_gw_latent_variables_hash,
                     ret);
  }

  return SCM_VARIABLE_REF (ret);
}

void
gw_guile_make_latent_variable (SCM sym, SCM proc, SCM arg)
{
  SCM latent_variables_hash, h;
  
  latent_variables_hash =
    gw_guile_ensure_latent_variables_hash_and_binder (scm_current_module ());
  
  h = scm_hashq_create_handle_x (latent_variables_hash, sym, SCM_BOOL_F);

  if (scm_is_true (scm_cdr (h))) {
    char *symstr;

    GW_ACCESS_SYMBOL (symstr, SCM_CAR (h));
    gw_raise_error (NULL, "latent var already registered: %s", symstr);
  } else {
    SCM_SETCDR (h, scm_cons (sym_class, scm_cons (proc, arg)));
  }
}

void
gw_guile_set_generics_module_x (SCM module)
{
  SCM current_module = scm_current_module ();

  gw_guile_ensure_latent_variables_hash_and_binder (module);

  scm_hashq_set_x (SCM_MODULE_OBARRAY (current_module),
                   scm_from_locale_symbol ("%generics"),
                   scm_make_variable (module));
}

static SCM
gw_guile_ensure_generics_module (void)
{
  SCM existing_binding;
  SCM current_module = scm_current_module ();
   
  existing_binding =
    scm_hashq_ref (SCM_MODULE_OBARRAY (current_module),
                   scm_from_locale_symbol ("%generics"),
                   SCM_BOOL_F);
  
  if (SCM_FALSEP (existing_binding)) {
    gw_guile_set_generics_module_x (current_module);
    return current_module;
  } else {
    return SCM_VARIABLE_REF (existing_binding);
  }
}

void
gw_guile_procedure_to_method_public (SCM proc, SCM specializers,
                                     SCM generic_name,
                                     SCM n_req_args, SCM use_optional_args)
#define FUNC_NAME "%gw:procedure-to-method-public!"
{
  SCM latent_variables_hash;
  SCM generics;
  SCM pair;
  SCM existing_latents;
  SCM entry;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_LIST (2, specializers);
  SCM_VALIDATE_SYMBOL (3, generic_name);
  SCM_VALIDATE_INUM (4, n_req_args);
  /* the fifth is a bool */
  
  generics = gw_guile_ensure_generics_module ();

  latent_variables_hash =
    gw_guile_ensure_latent_variables_hash_and_binder (generics);
  pair = scm_hashq_ref (latent_variables_hash, generic_name, SCM_BOOL_F);

  if (scm_is_false (pair)) {
    SCM var = scm_hashq_ref (SCM_MODULE_OBARRAY (generics),
                             generic_name, SCM_BOOL_F);
    if (!scm_is_false (var)) {
      /* some other module already forced it, hang the method */
      gw_guile_add_subr_method (SCM_VARIABLE_REF (var),
                                proc,
                                specializers,
                                scm_current_module (),
                                scm_to_int (n_req_args), 
                                scm_is_true (use_optional_args));
      return;
    }

    pair = scm_cons (sym_generic, SCM_EOL);
    scm_hashq_set_x (latent_variables_hash, generic_name, pair);
  } else if (!scm_is_eq (scm_car (pair), sym_generic)) {
    char *symstr;
    GW_ACCESS_SYMBOL (symstr, generic_name);
    gw_raise_error (NULL, "latent non-generic already registered: %s", symstr);
    return;
  }
    
  existing_latents = scm_cdr (pair);
  
  /* entry := #(proc specializers module n_req_args use_optional_args) */
  entry = scm_c_make_vector (5, SCM_BOOL_F);
  
  SCM_SIMPLE_VECTOR_SET (entry, 0, proc);
  SCM_SIMPLE_VECTOR_SET (entry, 1, specializers);
  SCM_SIMPLE_VECTOR_SET (entry, 2, scm_current_module ());
  SCM_SIMPLE_VECTOR_SET (entry, 3, n_req_args);
  SCM_SIMPLE_VECTOR_SET (entry, 4, use_optional_args);

  /* update the hash entry in place */
  SCM_SETCDR (pair, scm_cons (entry, existing_latents));
}
#undef FUNC_NAME

typedef struct {
  GWFunctionInfo *fi;
  void **values;
  void *rvalue;
} ffi_guile_call_info;

typedef void* (*guile_without_func)(void*);

static void*
do_ffi_call (const ffi_guile_call_info *info)
{
  ffi_call (&info->fi->cif, info->fi->proc, info->rvalue, info->values);
  return NULL;
}

static SCM
dynproc_smob_apply (SCM smob, SCM arg_list)
{
  GWFunctionInfo *fi = (GWFunctionInfo *) SCM_SMOB_DATA (smob);
  SCM result;
  void **values;
  void *rvalue;
  int i;
  unsigned offset;
  void *data;
  GWError error;
  ffi_guile_call_info call_info = { fi, NULL, NULL };
  
  /* TODO: Most of this should be factored out into the core; but how
   * to deal with the arg list? */
  
  data = alloca (fi->data_area_size);
  values = (void **) data;
  call_info.values = values;

  error.status = GW_ERR_NONE;

  offset = GW_ALIGN(fi->n_req_args * sizeof (void *),
                    fi->ret_type->type->alignment);
  rvalue = (void *) ((unsigned char *) data + offset);
  call_info.rvalue = rvalue;
  offset += (fi->ret_type->type->size > sizeof(ffi_arg)
             ? fi->ret_type->type->size : sizeof(ffi_arg));
  {
    SCM args = arg_list;

    for (i = 0; i < fi->n_req_args; i++)
      {
	SCM arg;
	offset = GW_ALIGN (offset, fi->arg_types[i]->type->alignment);
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
  }
  
  if (fi->flags & GW_FUNCTION_FLAG_LEAVE_RUNTIME)
      scm_without_guile ((guile_without_func)do_ffi_call,
                         &call_info);
  else
      do_ffi_call (&call_info);

  rvalue = GW_RVALUE_PTR (rvalue, fi->ret_type);
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

  {
    /* Compute the list of dependencies of the returned WCP.  Dependencies
       are all objects that may be eventually referred to by the C object
       underlying WCP, i.e. objects to which WCP has kept a pointer.  Such
       arguments are said to be ``aggregated'' by the return value, hence the
       typespec name.  */
    SCM args, deps = SCM_EOL;

    for (i = 0, args = arg_list;
	 i < fi->n_req_args;
	 i++, args = SCM_CDR (args))
      {
	if (fi->arg_typespecs[i] & GW_TYPESPEC_AGGREGATED)
	  {
	    /* Add this argument to the list of dependencies (aggregated
	       objects) of the return value.  */
	    SCM arg = SCM_CAR (args);

	    if (SCM_NIMP (arg))
	      deps = scm_cons (arg, deps);
	  }
      }

    if (deps != SCM_EOL)
      {
	if (SCM_NIMP (result))
	  gw_wcp_set_dependencies (result, deps);
      }
  }

  return result;
}

static int
dynproc_smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  GWFunctionInfo *fi = (GWFunctionInfo *)SCM_SMOB_DATA (smob);

  scm_display (scm_from_locale_string ("#<gw:dynproc "), port);
  scm_display (scm_from_locale_string (fi->proc_name), port);
  scm_display (scm_from_locale_string (" ("), port);
  scm_display (SCM_I_MAKINUM (fi->n_req_args), port);
  scm_display (scm_from_locale_string (")>"), port);
  
  return 1;
}

static void
gw_guile_handle_wrapper_error(GWLangArena arena,
                              GWError *error,
                              const char *func_name,
                              unsigned int arg_pos)
{
  static SCM out_of_range_key = SCM_BOOL_F;
  static SCM wrong_type_key = SCM_BOOL_F;

  if (scm_is_false (out_of_range_key))
    out_of_range_key = scm_permanent_object (
            scm_c_make_keyword("out-of-range"));
  if (scm_is_false (wrong_type_key))
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
      scm_misc_error(func_name, error->message, *(SCM *)error->data);
      break;
    case GW_ERR_MEMORY:
      scm_memory_error(func_name);
      break;
    case GW_ERR_RANGE:
      scm_error (out_of_range_key,
                 func_name,
                 "Out of range: ~S",
                 scm_cons (*(SCM *)error->data, SCM_EOL),
                 SCM_BOOL_F);
      break;
    case GW_ERR_TYPE:
      scm_error(wrong_type_key,
                func_name,
                "Wrong type: ",
                scm_cons (*(SCM *)error->data, SCM_EOL),
                SCM_BOOL_F);
      break;
    case GW_ERR_ARGC:
      scm_wrong_num_args (scm_from_locale_string (func_name)); break;
    case GW_ERR_ARG_RANGE:
      /* scm_data is the bad arg */
      scm_out_of_range(func_name, *(SCM *)error->data); break;
    case GW_ERR_ARG_TYPE:
      /* scm_data is the bad arg */
      scm_wrong_type_arg(func_name, arg_pos, *(SCM *)error->data); break;
    default:
      scm_misc_error(func_name,
                     "asked to handle nonexistent gw:error type: ~S",
                     scm_cons (scm_from_long (error->status), SCM_EOL));
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
    
    if (fi->generic_name && fi->arg_types)
    {
      int j;
      SCM specializers = SCM_EOL;
      
      for (j = fi->n_req_args - 1; j >= 0; j--)
      {
        GWTypeInfo *arg_type = fi->arg_types[j];
        SCM class_name = ((arg_type->class_name &&
                           !(fi->arg_typespecs[j] & GW_TYPESPEC_UNSPECIALIZED))
                          ? scm_from_locale_symbol (arg_type->class_name)
                          : SCM_BOOL_F);
        specializers = scm_cons (class_name, specializers);
      }
      
      gw_guile_procedure_to_method_public (
              subr, specializers,
              scm_from_locale_symbol (fi->generic_name),
              SCM_I_MAKINUM (fi->n_req_args),
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
    module_add_x = scm_permanent_object (
            SCM_VARIABLE_REF (scm_c_lookup ("module-add!")));
    k_specializers = scm_permanent_object (
            scm_c_make_keyword ("specializers"));
    k_procedure = scm_permanent_object(
            scm_c_make_keyword ("procedure"));
    k_name = scm_permanent_object( scm_c_make_keyword ("name"));
    k_default = scm_permanent_object (scm_c_make_keyword ("default"));
    sym_generic = scm_permanent_object (scm_from_locale_symbol ("generic"));
    sym_class = scm_permanent_object (scm_from_locale_symbol ("class"));
    sym_sys_gw_latent_variables_hash = scm_permanent_object
      (scm_from_locale_symbol ("%gw-latent-variables-hash"));
    
    dynproc_smob_tag = scm_make_smob_type("%gw:dynamic-procedure",
                                          sizeof(GWFunctionInfo *));
    scm_set_smob_free (dynproc_smob_tag, NULL);
    scm_set_smob_apply (dynproc_smob_tag,
                        (SCM (*)())dynproc_smob_apply, 0, 0, 1);
    scm_set_smob_print (dynproc_smob_tag, dynproc_smob_print);

    gw_wct_initialize ();
  }
}

