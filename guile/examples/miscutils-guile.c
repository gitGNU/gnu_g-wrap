#include "miscutils-guile.h"

SCM
msu_scm_timespec64_p (SCM obj)
{
  return SCM_BOOL (SCM_NFALSEP (scm_pair_p (obj)) &&
                   SCM_NFALSEP (scm_integer_p (SCM_CAR (obj))) &&
                   SCM_NFALSEP (scm_integer_p (SCM_CDR (obj))));
}

Timespec64
msu_timespec64_to_c (SCM tspec)
#define FUNC_NAME "msu_timespec64_to_c"
{
  Timespec64 result;

  SCM_ASSERT (SCM_CONSP (tspec), tspec, 1, FUNC_NAME);
  
  result.seconds = scm_num2long_long (SCM_CAR (tspec), 1, FUNC_NAME);
  result.nanoseconds = scm_num2long (SCM_CDR (tspec), 1, FUNC_NAME);

  return result;
}
#undef FUNC_NAME

SCM
msu_timespec64_to_scm (Timespec64 tspec)
{
  return scm_cons (scm_long_long2num (tspec.seconds),
                   scm_long2num (tspec.nanoseconds));
}
