#define _GNU_SOURCE
#include <limits.h>

#include "guile-test-c-code.h"

SCM
gw_test_gw_standard_echo_scm (SCM arg)
{
  return arg;
}
