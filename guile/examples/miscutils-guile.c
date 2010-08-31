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
  
  result.seconds = scm_to_long_long (SCM_CAR (tspec));
  result.nanoseconds = scm_to_long (SCM_CDR (tspec));

  return result;
}
#undef FUNC_NAME

SCM
msu_timespec64_to_scm (Timespec64 tspec)
{
  return scm_cons (scm_from_long_long (tspec.seconds),
                   scm_from_long (tspec.nanoseconds));
}
