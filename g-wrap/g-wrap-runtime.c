#include <string.h>
#include <stdio.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>

#include "config.h"

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
