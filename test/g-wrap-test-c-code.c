
#define _GNU_SOURCE
#include <limits.h>

#include "g-wrap-test-c-code.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/*======================================================================*/
/* General */

long
gw_test_strtol(const char *str, int base)
{
  return strtol (str, NULL, base);
}

long gw_test_out_args(int arg1, int *arg2, char **arg3)
{
  *arg2 = arg1 * arg1;
  *arg3 = "foobar";
  return arg1;
}

long gw_test_out_plus_default_args(int arg1, int arg2, char **arg3)
{
  *arg3 = "foo";
  return arg1 * arg2;
}

int gw_test_retval_exception (int arg)
{
  if (arg < 0)
    return -arg;
  else
    return 0;
}

/*======================================================================*/
/* Generics */

int gw_test_generic__int(int n)
{
  return n * n;
}

char *gw_test_generic__str_int (const char *str, int n)
{
  int i;
  int len = strlen (str);
  char *result;

  result = malloc (n * len + 1);
  if (!result)
    return NULL;
  
  for (i = 0; i < n; i++)
    memcpy (result + (i * len), str, len);
  result[n * len] = '\0';

  return result;
}

const char *gw_test_generic__str_null_ok (const char *str)
{
  return str;
}

void gw_test_generic__double_double_ptr (double d, double *pd)
{
  *pd = d;
}

int gw_test_generic__bool_bool (int b1, int b2)
{
  return b1 && b2;
}

/*======================================================================*/
/* For test-gw-enumeration */

enum GWTestEnum
gw_test_gw_enumeration_echo(enum GWTestEnum arg)
{
  return arg;
}

/*======================================================================*/
/* For test-gw-standard */

void
gw_test_gw_standard_no_op ()
{
}

int
gw_test_gw_standard_echo_bool (int arg)
{
  return arg;
}

char
gw_test_gw_standard_echo_char (char arg)
{
  return arg;
}

float
gw_test_gw_standard_echo_float (float arg)
{
  return arg;
}

double
gw_test_gw_standard_echo_double (double arg)
{
  return arg;
}

int gw_test_gw_standard_get_int_max () { return INT_MAX; }
int gw_test_gw_standard_get_int_min() { return INT_MIN; }
unsigned int gw_test_gw_standard_get_uint_max() { return UINT_MAX; }
long gw_test_gw_standard_get_long_max() { return LONG_MAX; }
long gw_test_gw_standard_get_long_min() { return LONG_MIN; }
unsigned long gw_test_gw_standard_get_ulong_max() { return ULONG_MAX; }
ssize_t gw_test_gw_standard_get_ssize_max() { return SSIZE_MAX; }
ssize_t gw_test_gw_standard_get_ssize_min() { return (-SSIZE_MAX - 1); }

int
gw_test_gw_standard_echo_int (int arg)
{
  return arg;
}

unsigned int
gw_test_gw_standard_echo_unsigned_int (unsigned int arg)
{
  return arg;
}

long
gw_test_gw_standard_echo_long (long arg)
{
  return arg;
}

unsigned long
gw_test_gw_standard_echo_unsigned_long (unsigned long arg)
{
  return arg;
}

ssize_t
gw_test_gw_standard_echo_ssize (ssize_t arg)
{
  return arg;
}

char *
gw_test_gw_standard_echo_mchars_caller_owned (char *arg)
{
  if(arg)
    return strdup(arg);  
  return
    arg;
}

const char *
gw_test_gw_standard_echo_const_mchars_caller_owned (const char *arg)
{
  if(arg)
    return strdup(arg);  
  return
    arg;
}

char *
gw_test_gw_standard_echo_mchars_callee_owned (char *arg)
{
  static char *last_result = NULL;

  if(!arg) return arg;
  if(last_result) free(last_result);
  last_result = strdup(arg);
  return last_result;
}

const char *
gw_test_gw_standard_echo_const_mchars_callee_owned (const char *arg)
{
  static char *last_result = NULL;

  if(!arg) return arg;
  if(last_result) free(last_result);
  last_result = strdup(arg);
  return last_result;
}

/*======================================================================*/
/* For test-gw-wct */

struct _gwTestParentObj {
  char *name;
};

struct _gwTestChildObj {
  char *name;
};

gwTestParentObj *
gw_test_parent_make_obj(const char *name)
{
  gwTestParentObj *f = (gwTestParentObj *) malloc(sizeof(gwTestParentObj));
  f->name = strdup(name);
  return f;
}

gwTestParentObj*
gw_test_parent_same_obj(gwTestParentObj *f)
{
  return f;
}

void gw_test_parent_display_obj(const gwTestParentObj* f)
{
  if(!f) printf("[[ NULL gwTestParentObj* ]]");
  else printf("[[ %s gwTestParentObj* ]]", f->name);
  fflush(stdout);
}

gwTestChildObj *
gw_test_child_make_obj(const char *name)
{
  gwTestChildObj *f = (gwTestChildObj *) malloc(sizeof(gwTestChildObj));
  f->name = strdup(name);
  return f;
}

gwTestChildObj*
gw_test_child_same_obj(gwTestChildObj *f)
{
  return f;
}

void gw_test_child_display_obj(const gwTestChildObj* f)
{
  if(!f) printf("[[ NULL gwTestChildObj* ]]");
  else printf("[[ %s gwTestChildObj* ]]", f->name);
  fflush(stdout);
}

gwTestParentObj*
gw_test_child_pass_back_parent_obj(gwTestParentObj* x)
{
  return x;
}


/* Aggregating object.  */

struct _gwTestAggregatingObj
{
  struct _gwTestAggregatingObj *aggregated;
  size_t ref_count;
};

gwTestAggregatingObj *
gw_test_make_simple_aggregating_obj (void)
{
  gwTestAggregatingObj *obj;

  obj = malloc (sizeof (*obj));
  if (!obj)
    return NULL;

  obj->aggregated = NULL;
  obj->ref_count = 0;

  return obj;
}

gwTestAggregatingObj *
gw_test_make_aggregating_obj (gwTestAggregatingObj *aggregated)
{
  gwTestAggregatingObj *obj;

  obj = gw_test_make_simple_aggregating_obj ();
  obj->aggregated = aggregated;

  /* Increase the reference counter of AGGREGATED.  This will later allow us
     to determine whether a WCP is rightfully being freed (see
     `gw_test_cleanup_aggregating_obj ()' below).  */
  aggregated->ref_count++;

  return obj;
}

void
gw_test_make_aggregating_obj_alt (gwTestAggregatingObj *aggregated,
				  gwTestAggregatingObj **result)
{
  /* The point here is just to make sure that aggregation also works fine
     when the aggregating object is an `out' argument instead of the return
     value.  */
  *result = gw_test_make_aggregating_obj (aggregated);
}

gwTestAggregatingObj *
gw_test_get_aggregated_obj (gwTestAggregatingObj *obj)
{
  /* Hopefully, OBJ->AGGREGATED has not been freed, even if the Scheme code
     no longer holds any reference to it.  Holding (on the Scheme side) a
     SMOB to OBJ should be enough to keep both OBJ and OBJ->AGGREGATED.  */
  return obj->aggregated;
}



/* This function automatically gets called when WCPs wrapping
   `gwTestAggregatingObj' C objects get GC'd.  */
size_t
gw_test_cleanup_aggregating_obj (void *wcp)
{
  gwTestAggregatingObj *obj;

#ifdef DEBUG
  printf ("%s: %p\n", __FUNCTION__, wcp);
#endif

  /* Does this break C99 strict aliasing rules?  I guess no, although the
     spec (6.5, par. 7) doesn't clearly mention this.  (XXX)  */
  obj = (gwTestAggregatingObj *)wcp;

  if (obj->aggregated)
    {
      /* Decrement the reference counter of the object aggregated by OBJ.  */
#ifdef DEBUG
      fprintf (stderr, "object %p aggregates %p\n",
	       obj, obj->aggregated);
#endif

      if (obj->aggregated->ref_count == 0)
	abort ();

      obj->aggregated->ref_count--;
    }

  /* OBJ itself should normally not be GC'd unless all the WCPs that
     aggregate it have already been freed (in which case its REF_COUNT is
     zero).  */
  if (obj->ref_count > 0)
    {
      fprintf (stderr, "trying to clean up aggregating object @ %p which is\n"
	       "still referenced %u times!\n",
	       obj, obj->ref_count);
      abort ();
    }

  obj->aggregated = NULL;
  obj->ref_count = 0;

#ifdef DEBUG
  printf ("freeing object @ %p\n", obj);
#endif
  free (obj);

  return sizeof (gwTestAggregatingObj);
}
