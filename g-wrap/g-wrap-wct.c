/**********************************************************************
Copyright (C) 1996, 1997, 1998 Christopher Lee
Copyright (C) 2000 Rob Browning
 
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

#include <stdio.h>

#include <libguile.h>
#include "g-wrap-wct.h"
#include "g-wrap-compatibility.h"

#ifdef HAVE_CONFIG_H
# include "../config.h"
#endif

#ifndef SCM_SMOB_DATA
#define SCM_SMOB_DATA(x) SCM_CDR((x))
#endif

#ifndef SCM_NEWSMOB
#define GW_NEWSMOB(smob, id, data) \
  SCM_NEWCELL((smob)); \
  SCM_CAR((smob)) = (id); \
  SCM_CDR((smob)) = (data);
#else
#define GW_NEWSMOB SCM_NEWSMOB
#endif

#ifndef SCM_RETURN_NEWSMOB
#define GW_RETURN_NEWSMOB(id, data) \
  do { \
    SCM __SCM_smob_answer; \
    GW_NEWSMOB(__SCM_smob_answer, (id), (data)); \
       return __SCM_smob_answer; \
  } while (0)
#else
#define GW_RETURN_NEWSMOB SCM_RETURN_NEWSMOB
#endif

void
gw_puts(const char* str, SCM port) {
#if HAVE_SCM_PUTS 
  /* (char *) fixes problem with guile 1.3.4 prototype. */
 scm_puts((char *) str,port);
#else
  scm_gen_puts(scm_mb_string,str,port);
#endif
}

/****************************************************************************/
/* Wrapped C type/pointer info */

typedef struct {
  SCM name;
  SCM (*equal_p)(SCM wcp_a, SCM wcp_b);
  int (*print)(SCM wcp, SCM port, char writing_p, int *use_default_printer);
  SCM (*mark)(SCM wcp);
  size_t (*cleanup)(SCM wcp);
} wrapped_c_type_data;

typedef struct {
  SCM type;
  void *pointer;
  SCM scm_data;
} wrapped_c_pointer_data;

static int wct_system_initialized = 0;
static long wct_smob_id = 0;
static long wcp_smob_id = 0;

/* forward defs */

#ifndef SCM_SMOB_PREDICATE
# define SCM_SMOB_PREDICATE(tag, obj) \
  (SCM_NIMP(obj) && SCM_TYP16 (obj) == (tag))
#endif

#define GW_WCT_P(obj) (SCM_SMOB_PREDICATE((wct_smob_id), (obj)))
#define GW_WCP_P(obj) \
  (SCM_FALSEP(obj) || (SCM_SMOB_PREDICATE((wcp_smob_id), (obj))))

int
gw_wct_p(SCM obj) {
  return(GW_WCT_P(obj));
}

int
gw_wcp_p(SCM obj) {
  return(GW_WCP_P(obj));
}

/****************************************************************************/
/* Wrapped C pointer functions */

static size_t
wcp_data_free(SCM wcp) {
  wrapped_c_pointer_data *data;
  wrapped_c_type_data *type_data;
  
  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp);
  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data->type);

  if (type_data->cleanup) {
    type_data->cleanup(wcp);
    /* c pointer may be destrotyed at this point (probably should be) */
  }

  scm_gc_free (data, sizeof(wrapped_c_pointer_data), "gw:wcp");
  
  return 0;
}

static int
wcp_data_print(SCM wcp, SCM port, scm_print_state *pstate) {
  char endstr[512];
  int result = 1;
  int use_default_p = 1;
  int writing_p = SCM_WRITINGP(pstate);
  wrapped_c_pointer_data *data;
  wrapped_c_type_data *type_data;
  
  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp);
  if(!GW_WCT_P(data->type)) {
    scm_misc_error("wcp_data_print", "Unknown type object.", data->type);
  }
  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data->type);

  if(type_data->print) {
    use_default_p = 0;
    result = type_data->print(wcp, port, writing_p, &use_default_p);
  }
  
  if(use_default_p) {
    snprintf(endstr, sizeof(endstr), " %p>", data->pointer);
    gw_puts("#<gw:wcp ", port);
    scm_display(type_data->name, port);
    gw_puts(endstr, port);
    result = 1;
  }
  return result;
}

static SCM
wcp_data_mark(SCM wcp) {
  wrapped_c_pointer_data *data;
  wrapped_c_type_data *type_data;
  
  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp);
  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data->type);

  if(type_data->mark) {
    scm_gc_mark(type_data->mark(wcp));
  }

  scm_gc_mark(data->type);
  return(data->scm_data);
}

static SCM
wcp_data_equal_p (SCM wcp_a, SCM wcp_b)
{
  wrapped_c_pointer_data *data_a;
  wrapped_c_pointer_data *data_b;
  wrapped_c_type_data *type_data;
  
  data_a = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp_a);
  data_b = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp_b);

  if(data_a == data_b) return SCM_BOOL_T;

  if (!SCM_EQ_P(data_a->type, data_b->type)) return SCM_BOOL_F;
 
  if((data_a->pointer == data_b->pointer)) return SCM_BOOL_T;

  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data_a->type);

  if(!type_data->equal_p) return SCM_BOOL_F;

  return type_data->equal_p(wcp_a, wcp_b);
}

SCM
gw_wcp_assimilate_ptr(void *ptr, SCM type) {
  /* create a wrapped C pointer of the given type, wrapping ptr */
  wrapped_c_type_data *type_data;
  wrapped_c_pointer_data *ptr_data; 

  if(!GW_WCT_P(type)) return SCM_BOOL_F;

  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(type);

  ptr_data = (wrapped_c_pointer_data *)
    scm_gc_malloc(sizeof(wrapped_c_pointer_data), "gw:wcp");

  ptr_data->pointer = ptr;
  ptr_data->type = type;
  ptr_data->scm_data = SCM_BOOL_F;

  GW_RETURN_NEWSMOB(wcp_smob_id, ptr_data);
}

void *
gw_wcp_get_ptr(SCM obj) {
  wrapped_c_pointer_data *ptr_data;
  if(!SCM_SMOB_PREDICATE(wcp_smob_id, obj)) return NULL;  
  ptr_data = (wrapped_c_pointer_data *) SCM_SMOB_DATA(obj);
  return(ptr_data->pointer);
}

int
gw_wcp_is_of_type_p(SCM type, SCM obj)
{
  /* return non-zero if wrapped C pointer obj is of the given type. */
  if(SCM_SMOB_PREDICATE(wcp_smob_id, obj))
  {
    wrapped_c_pointer_data *ptr_data =
      (wrapped_c_pointer_data *) SCM_SMOB_DATA (obj);
    return SCM_EQ_P (ptr_data->type, type);
  }
  return 0;
}

SCM
gw_wcp_coerce(SCM obj, SCM new_type) {
  /* return a new wrapped C pointer */

  if(!SCM_SMOB_PREDICATE(wcp_smob_id, obj)) return SCM_BOOL_F;
  if(!GW_WCT_P(new_type)) return SCM_BOOL_F;
  
  return(gw_wcp_assimilate_ptr(gw_wcp_get_ptr(obj),
                               new_type));
}

/****************************************************************************/
/* Wrapped C type functions */

static size_t 
wct_data_free(SCM smob)
{
  scm_gc_free ((void *) SCM_SMOB_DATA (smob), sizeof (wrapped_c_type_data),
               "gw:wct");
  return 0;
}

static SCM
wct_data_mark(SCM smob)
{
  wrapped_c_type_data *data = (wrapped_c_type_data *) SCM_SMOB_DATA(smob);
  return(data->name);
}

static int
wct_data_print(SCM wct, SCM port, scm_print_state *pstate) {
  int writing_p = SCM_WRITINGP(pstate);
  if (writing_p)
  {
    wrapped_c_type_data *data = (wrapped_c_type_data *) SCM_SMOB_DATA(wct);
    
    gw_puts("#<gw:wct ", port);
    scm_display(data->name, port);
    gw_puts(">", port);
  }
  return 1;
}

SCM
gw_wct_create(const char *type_name,
              SCM (*equal_p)(SCM wcp_a, SCM wcp_b),
              int (*print)(SCM wcp, SCM port,
                           char writing_p,
                           int *use_default_printer_p),
              SCM (*mark)(SCM wcp),
              size_t (*cleanup)(SCM wcp))
{
  /* see header for docs */
  wrapped_c_type_data *type_data;

  if(!type_name) {
    scm_misc_error("gw_wct_create_and_register",
                   "null type_name argument",
                   SCM_EOL);
  }

  type_data = (wrapped_c_type_data *)
    scm_gc_malloc(sizeof(wrapped_c_type_data),
                    "gw_wct_create_and_register: type_data");

  /* (char *) fixes problem with guile 1.3.4 prototype. */
  type_data->name = scm_makfrom0str ((char *) type_name);

  type_data->equal_p = equal_p;
  type_data->print = print;
  type_data->mark = mark;
  type_data->cleanup = cleanup;

  GW_RETURN_NEWSMOB(wct_smob_id, type_data);
}


/****************************************************************************/
/* Initialization */


void
gw_wct_initialize()
{
  if(!wct_system_initialized) {

#ifdef GWRAP_OLD_GUILE_SMOB
    {
      scm_smobfuns wct_smob_data = {
        wct_data_mark,
        wct_data_free,
        wct_data_print,
        /* don't need equalp because there should never be more than one
           of these and if we do, then they're *not* equal - only one
           place (module or whatever) can provide a given type. */
        NULL
      };
      scm_smobfuns wcp_smob_data = {
        wcp_data_mark,
        wcp_data_free,
        wcp_data_print,
        wcp_data_equal_p
      };

      wct_smob_id = scm_newsmob(&wct_smob_data);
      wcp_smob_id = scm_newsmob(&wcp_smob_data);
    }
#else 
    wct_smob_id = scm_make_smob_type("gw:wct", sizeof(wrapped_c_type_data));
    scm_set_smob_mark(wct_smob_id, wct_data_mark);
    scm_set_smob_free(wct_smob_id, wct_data_free);
    scm_set_smob_print(wct_smob_id, wct_data_print);
    /* don't need equalp because there should never be more than one
       of these and if we do, then they're *not* equal - only one
       place (module or whatever) can provide a given type. */

    wcp_smob_id = scm_make_smob_type("gw:wcp", sizeof(wrapped_c_type_data));
    scm_set_smob_free(wcp_smob_id, wcp_data_free);
    scm_set_smob_print(wcp_smob_id, wcp_data_print);
    scm_set_smob_mark(wcp_smob_id, wcp_data_mark);
    scm_set_smob_equalp(wcp_smob_id, wcp_data_equal_p);
#endif

    wct_system_initialized = 1;
  }
}
