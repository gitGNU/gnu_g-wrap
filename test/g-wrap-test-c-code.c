
#define _GNU_SOURCE
#include <limits.h>

#include "g-wrap-test-c-code.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/*======================================================================*/
/* For test-gw-enumerationx */

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


