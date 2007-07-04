
#ifndef __G_WRAP_TEST_C_CODE_H__
#define __G_WRAP_TEST_C_CODE_H__

#include <unistd.h> /* for ssize_t */
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

/* General */

long gw_test_strtol(const char *str, int base);
long gw_test_out_args(int arg1, int *arg2, char **arg3);
long gw_test_out_plus_default_args(int arg1, int arg2, char **arg3);
int  gw_test_retval_exception (int arg);
int  gw_test_invisible_out_arg (int in, int *out1, int *out2);

int gw_test_generic__int (int n);
char *gw_test_generic__str_int (const char *str, int n);
const char *gw_test_generic__str_null_ok (const char *str);
void gw_test_generic__double_double_ptr (double d, double *pd);
int gw_test_generic__bool_bool (int b1, int b2);

/* For test-enumeration */

enum GWTestEnum
{
  GW_TEST_ENUM_ZERO = 0,
  GW_TEST_ENUM_ONE = 1,
  GW_TEST_ENUM_TWO = 2,
  GW_TEST_ENUM_TWO_TOO = 2,
  GW_TEST_ENUM_TWO_MANY = 2
};

enum GWTestEnum gw_test_gw_enumeration_echo(enum GWTestEnum arg);

/* For test-gw-standard */

#define GW_TEST_GW_STANDARD_FOO_VALUE 42
#define GW_TEST_GW_STANDARD_BAR_VALUE "42"

void gw_test_gw_standard_no_op (void);
int gw_test_gw_standard_echo_bool (int arg);
char gw_test_gw_standard_echo_char (char arg);
float gw_test_gw_standard_echo_float (float arg);
double gw_test_gw_standard_echo_double (double arg);

int gw_test_gw_standard_get_int_max(void);
int gw_test_gw_standard_get_int_min(void);
unsigned int gw_test_gw_standard_get_uint_max(void);

long gw_test_gw_standard_get_long_max(void);
long gw_test_gw_standard_get_long_min(void);
unsigned long gw_test_gw_standard_get_ulong_max(void);

ssize_t gw_test_gw_standard_get_ssize_max(void);
ssize_t gw_test_gw_standard_get_ssize_min(void);

int gw_test_gw_standard_echo_int (int arg);
unsigned int gw_test_gw_standard_echo_unsigned_int (unsigned int arg);
long gw_test_gw_standard_echo_long (long arg);
unsigned long gw_test_gw_standard_echo_unsigned_long (unsigned long arg);
ssize_t gw_test_gw_standard_echo_ssize(ssize_t arg);

char *gw_test_gw_standard_echo_mchars_caller_owned (char *arg);
const char *gw_test_gw_standard_echo_const_mchars_caller_owned (const char *arg);

char *gw_test_gw_standard_echo_mchars_callee_owned (char *arg);
const char *gw_test_gw_standard_echo_const_mchars_callee_owned (const char *arg);

/* For test-wct */

typedef struct _gwTestParentObj gwTestParentObj;
typedef struct _gwTestChildObj gwTestChildObj;
typedef struct _gwTestAggregatingObj gwTestAggregatingObj;

gwTestParentObj *gw_test_parent_make_obj(const char *name);
gwTestParentObj *gw_test_parent_same_obj(gwTestParentObj *f);
void gw_test_parent_display_obj(const gwTestParentObj* f);


gwTestChildObj *gw_test_child_make_obj(const char *name);
gwTestChildObj *gw_test_child_same_obj(gwTestChildObj *f);
void gw_test_child_display_obj(const gwTestChildObj* f);
gwTestParentObj* gw_test_child_pass_back_parent_obj(gwTestParentObj* x);

gwTestAggregatingObj *gw_test_make_simple_aggregating_obj (void);
gwTestAggregatingObj *gw_test_make_aggregating_obj (gwTestAggregatingObj *);
void gw_test_make_aggregating_obj_alt (gwTestAggregatingObj *aggregated,
				       gwTestAggregatingObj **result);
gwTestAggregatingObj *gw_test_get_aggregated_obj (gwTestAggregatingObj *);

size_t gw_test_cleanup_aggregating_obj (void *);

#endif
