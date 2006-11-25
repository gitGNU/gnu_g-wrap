#ifndef GW_MISCUTILS_GUILE_H
#define GW_MISCUTILS_GUILE_H

#include <libguile.h>

#include "miscutils.h"

SCM msu_scm_timespec64_p(SCM obj);
Timespec64 msu_timespec64_to_c(SCM tspec);
SCM msu_timespec64_to_scm(Timespec64 tspec);

#endif
