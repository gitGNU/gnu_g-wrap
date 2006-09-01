/**********************************************************************
Copyright (C) 1996, 1997, 1998 Christopher Lee
Copyright (C) 2000 Rob Browning
Copyright (C) 2004 Andreas Rottmann
Copyright (C) 2005, 2006 Ludovic Court�s

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
#include <string.h>

#include <libguile.h>

#include "g-wrap/guile-runtime.h"
#include "g-wrap/guile-compatibility.h"

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/****************************************************************************/
/* Wrapped C type/pointer info */

typedef struct
{
    SCM name;
    int (*equal_p)(void *wcp_a, void *wcp_b);
    int (*print)(SCM wcp, SCM port, char writing_p, int *use_default_printer);
    SCM (*mark)(SCM wcp);
    size_t (*cleanup)(void *wcp);
} wrapped_c_type_data;

typedef struct wrapped_c_pointer_data
{
    SCM type;
    void *pointer;

    SCM dependencies;
    struct wrapped_c_pointer_data **wcp_dependencies;
    size_t wcp_dependency_count;

    unsigned ref_count;
} wrapped_c_pointer_data;

static int wct_system_initialized = 0;
static scm_t_bits wct_smob_id = 0;
static scm_t_bits wcp_smob_id = 0;


/* forward defs */

#define GW_WCT_P(obj) (SCM_SMOB_PREDICATE((wct_smob_id), (obj)))
#define GW_WCP_P(obj) \
  (scm_is_false(obj) || (SCM_SMOB_PREDICATE((wcp_smob_id), (obj))))

int
gw_wct_p (SCM obj)
{
  return GW_WCT_P(obj);
}

int
gw_wcp_p (SCM obj)
{
  return GW_WCP_P(obj);
}

/****************************************************************************/
/* Wrapped C pointer functions */

/* Actually free the WCP represented by DATA, including the data pointed to
   by DATA.

   Since we want to enforce finalization order for inter-dependent WCPs
   (i.e., to make sure that the `cleanup' function of a dependency can only
   be called after that of its referrer was called), we have to maintain a
   reference counter on each WCP (otherwise, both the referrer and its
   dependencies can become unreachable during the same GC phase and,
   consequently, their `cleanup' function may be called in any order).

   Furthermore, during a given sweep phase, the `SCM' object representing a
   dependency may become invalid, so the `dependencies' field should not be
   traversed from within `wcp_data_free ()'.  Therefore, each WCP has to
   store direct pointers to the `wrapped_c_pointer_data' structure of the
   WCPs it depends on.  */
static void
do_free_wcp (wrapped_c_pointer_data *data)
{
  wrapped_c_type_data *type;

  /* Destroy self first.  */
  type = (wrapped_c_type_data *) SCM_SMOB_DATA (data->type);
  if (type->cleanup)
    type->cleanup (data->pointer);

  data->pointer = NULL;

  if (data->wcp_dependencies)
    {
      /* Decrease the reference count of each WCP depended on, freeing those
	 whose refcount has reached zero.  */
      size_t dep;

      for (dep = 0; dep < data->wcp_dependency_count; dep++)
	{
	  wrapped_c_pointer_data *d;

	  d = data->wcp_dependencies[dep];
	  if (d->ref_count == 0)
	    /* That shouldn't happen.  */
	    abort ();

	  if (--d->ref_count == 0)
	    /* The object depended on is no longer referenced now, which
	       means that it had already been passed to `wcp_data_free ()'
	       but at that time it was still referenced.  */
	    do_free_wcp (d);
	}

      scm_gc_free (data->wcp_dependencies,
		   sizeof (wrapped_c_pointer_data) * data->wcp_dependency_count,
		   "gw:wcp-dependencies");
      data->wcp_dependencies = NULL;
      data->wcp_dependency_count = 0;
    }

  scm_gc_free (data, sizeof (wrapped_c_pointer_data), "gw:wcp");

  /* At this point, DATA is no longer valid!  */
}

static size_t
wcp_data_free (SCM wcp)
{
  wrapped_c_pointer_data *data;

  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA (wcp);

  if (data->ref_count == 0)
    /* That shouldn't happen.  */
    abort ();

  /* If WCP is still referenced, then it will be destroyed when its last
     reference is dropped.  The use of a reference counter enforces the
     destruction order: dependencies can only be freed after the referer.  */
  if (--data->ref_count == 0)
    do_free_wcp (data);

  return 0;
}

static int
wcp_data_print (SCM wcp, SCM port, scm_print_state *pstate)
{
  char endstr[64];
  int result = 1;
  int use_default_p = 1;
  int writing_p = SCM_WRITINGP(pstate);
  wrapped_c_pointer_data *data;
  wrapped_c_type_data *type_data;
  
  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA (wcp);
  if(!GW_WCT_P (data->type))
  {
    scm_misc_error("wcp_data_print", "Unknown type object.", data->type);
  }
  
  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data->type);

  if (type_data->print)
  {
    use_default_p = 0;
    result = type_data->print (wcp, port, writing_p, &use_default_p);
  }
  
  if (use_default_p)
  {
    snprintf (endstr, sizeof (endstr), " %p>", data->pointer);
    scm_puts ("#<gw:wcp ", port);
    scm_display (type_data->name, port);
    scm_puts (endstr, port);
    result = 1;
  }
  
  return result;
}

static SCM
wcp_data_mark (SCM wcp)
{
  wrapped_c_pointer_data *data;
  wrapped_c_type_data *type_data;

  data = (wrapped_c_pointer_data *) SCM_SMOB_DATA(wcp);
  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data->type);

  if (data->dependencies != SCM_EOL)
    /* Mark all the Scheme objects aggregated by WCP.  */
    scm_gc_mark (data->dependencies);

  if (type_data->mark)
    {
      /* Invoke the user-defined mark function.  */
      SCM ret;

      ret = type_data->mark (wcp);
      if (ret != SCM_BOOL_F)
	scm_gc_mark (ret);
    }

  return (data->type);
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

  if (!scm_is_eq (data_a->type, data_b->type)) return SCM_BOOL_F;
 
  if((data_a->pointer == data_b->pointer)) return SCM_BOOL_T;

  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(data_a->type);

  if(!type_data->equal_p) return SCM_BOOL_F;

  return scm_from_bool (type_data->equal_p (data_a->pointer, data_b->pointer));
}

SCM
gw_wcp_assimilate_ptr (void *ptr, SCM type)
{
  /* create a wrapped C pointer of the given type, wrapping ptr */
  wrapped_c_type_data *type_data;
  wrapped_c_pointer_data *ptr_data; 

  if(!GW_WCT_P(type)) return SCM_BOOL_F;

  type_data = (wrapped_c_type_data *) SCM_SMOB_DATA(type);

  ptr_data = (wrapped_c_pointer_data *)
    scm_gc_malloc(sizeof(wrapped_c_pointer_data), "gw:wcp");

  ptr_data->pointer = ptr;
  ptr_data->type = type;
  ptr_data->dependencies = SCM_EOL;
  ptr_data->wcp_dependencies = NULL;
  ptr_data->wcp_dependency_count = 0;
  ptr_data->ref_count = 1;

  SCM_RETURN_NEWSMOB(wcp_smob_id, ptr_data);
}

/* Maximum number of WCPs a WCP can depend on.  */
#define MAX_WCP_DEPS  256

void
gw_wcp_set_dependencies (SCM wcp, SCM deps)
{
  SCM d;
  size_t wcp_count = 0;
  wrapped_c_pointer_data *ptr_data, *wcps[MAX_WCP_DEPS];

  /* WCP is assumed to be a <gw:wcp> SMOB.  DEPS is assumed to be a list of
     Scheme objects.  */
  if ((!GW_WCP_P (wcp)) || (!scm_is_pair (deps)))
    return;

  /* Increase the reference counter of every referred WCP.  */
  for (d = deps; scm_is_pair (d); d = SCM_CDR (d))
    {
      SCM obj = SCM_CAR (d);

      if (GW_WCP_P (obj))
	{
	  ptr_data = (wrapped_c_pointer_data *)SCM_SMOB_DATA (obj);
	  ptr_data->ref_count++;

	  if (wcp_count >= MAX_WCP_DEPS)
	    /* The awful (but reasonable) limitation.  */
	    abort ();

	  wcps[wcp_count++] = ptr_data;
	}
    }

  ptr_data = (wrapped_c_pointer_data *)SCM_SMOB_DATA (wcp);
  ptr_data->dependencies = deps;

  if (ptr_data->wcp_dependencies)
    scm_gc_free (ptr_data->wcp_dependencies,
		 ptr_data->wcp_dependency_count
		 * sizeof (wrapped_c_pointer_data),
		 "gw:wcp-dependencies");

  ptr_data->wcp_dependency_count = wcp_count;
  ptr_data->wcp_dependencies =
    scm_gc_malloc (wcp_count * sizeof (wrapped_c_pointer_data),
		   "gw:wcp-dependencies");
  memcpy (ptr_data->wcp_dependencies, wcps,
	  wcp_count * sizeof (wrapped_c_pointer_data));
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
    return scm_is_eq (ptr_data->type, type);
  }
  return 0;
}

SCM
gw_wcp_coerce(SCM obj, SCM new_type)
{
  /* return a new wrapped C pointer */
  SCM new_obj;
  wrapped_c_pointer_data *ptr_data;

  if(!SCM_SMOB_PREDICATE(wcp_smob_id, obj)) return SCM_BOOL_F;
  if(!GW_WCT_P(new_type)) return SCM_BOOL_F;

  ptr_data = (wrapped_c_pointer_data *)SCM_SMOB_DATA (obj);
  new_obj = gw_wcp_assimilate_ptr (ptr_data->pointer, new_type);

  /* Have the new WCP inherit the dependencies of OBJ.  */
  gw_wcp_set_dependencies (new_obj, ptr_data->dependencies);

  return new_obj;
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

  return data->name;
}

static int
wct_data_print(SCM wct, SCM port, scm_print_state *pstate)
{
  int writing_p = SCM_WRITINGP(pstate);
  if (writing_p)
  {
    wrapped_c_type_data *data = (wrapped_c_type_data *) SCM_SMOB_DATA(wct);
    
    scm_puts("#<gw:wct ", port);
    scm_display(data->name, port);
    scm_puts(">", port);
  }
  return 1;
}

/* create a new wrapped C type.  Returns the new gw:wct on success,
   and #f on other failure.

   equal_p - should return SCM_BOOL_F if the two objects should be
   considered equal, anything else, otherwise.

   If set to NULL, then by default, the objects are equal? only if
   their wcp types and wcp ptrs match.

   If you do specify an equal_p function, it will only be called
   if the two objects' wcp types match, but their pointers don't.

   print - should return non-zero on success.  If the function sets
   use_default_print_p to non-zero value, then the default wcp
   printer will be invoked on return, and this function's return
   value will be ignored.  If print is set to NULL, a default
   representation will be printed.  Note that it is your
   responsibility to make sure you don't try to print a destroyed C
   pointer.  The wcp's DEPENDENCIES is useful for keeping track in
   cases where the scheme side doesn't wholly own the pointer...
  
   mark - should mark any scheme data stored in the c pointer.  As a
   convenience, any scheme object returned by this function will
   also be marked.  You do not need to mark the wcp's DEPENDENCIES,
   that will be handed for you.  If this is set to NULL, only the
   DEPENDENCIES will be marked.

   cleanup - should destroy the c-side pointer as appropriate.  If
   set, will be called at garbage collection time.  You do not
   need to worry about the DEPENDENCIES here.  If possible, this
   function should return the amount of space reclaimed.  Also
   note that you don't need to do anything about SCM data inside
   (say in a struct) your C ptr.  It won't be marked and will be
   collected automagically.  Basically, you just need to worry
   about anything you malloced/gnewed/etc. when you created the
   wcp's data.

*/

/* FIXME FIXME: This is ridiculous. The generated type should be a class, so
   that it can have methods. SMOB types have classes created for them
   automatically by GOOPS. This procedure should create a new SMOB type,
   returning that class, and instances of the wct should be instances of that
   class. */
SCM
gw_wct_create (const char *type_name,
               int (*equal_p)(void *wcp_a, void *wcp_b),
               int (*print)(SCM wcp, SCM port,
                            char writing_p,
                            int *use_default_printer_p),
               SCM (*mark)(SCM wcp),
               size_t (*cleanup)(void *wcp))

{
  wrapped_c_type_data *type_data;

  if (!type_name)
    scm_misc_error("gw_wct_create_and_register",
                   "null type_name argument",
                   SCM_EOL);

  type_data = (wrapped_c_type_data *)
    scm_gc_malloc(sizeof(wrapped_c_type_data),
                    "gw_wct_create_and_register: type_data");

#if (SCM_MAJOR_VERSION <= 1) && (SCM_MINOR_VERSION < 7)
  type_data->name = scm_makfrom0str (type_name);
#else
  type_data->name = scm_from_locale_string (type_name);
#endif

  type_data->equal_p = equal_p;
  type_data->print = print;
  type_data->mark = mark;
  type_data->cleanup = cleanup;

  SCM_RETURN_NEWSMOB (wct_smob_id, type_data);
}


/****************************************************************************/
/* Initialization */


void
gw_wct_initialize()
{
  if(!wct_system_initialized) {

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

    wct_system_initialized = 1;
  }
}
