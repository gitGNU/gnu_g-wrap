/**********************************************************************
Copyright (C) 2003 Andreas Rottmann
 
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

#ifndef __G_WRAP_RUNTIME_H__
#define __G_WRAP_RUNTIME_H__

#include <libguile.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GWEnumPair GWEnumPair;
typedef enum _GWErrorStatus GWErrorStatus;
typedef struct _GWError GWError;

enum _GWErrorStatus
{
  GW_ERR_NONE,
  GW_ERR_MISC,
  GW_ERR_MEMORY,
  GW_ERR_RANGE,
  GW_ERR_TYPE,
  GW_ERR_ARGC,
  GW_ERR_ARG_RANGE,
  GW_ERR_ARG_TYPE
};

struct _GWError
{
    GWErrorStatus status;
    const char *message;
    SCM data;
};

struct _GWEnumPair
{
    int val;
    const char *sym;
};

void gw_runtime_get_version_info(int *major, int *revision, int *age);

SCM gw_enum_val2sym(GWEnumPair enum_pairs[], SCM scm_val, SCM scm_show_all_p);
SCM gw_enum_val2int(GWEnumPair enum_pairs[], SCM scm_val);

void gw_handle_wrapper_error(GWError *error,
                             const char *func_name,
                             unsigned int arg_pos) __attribute__ ((noreturn));

#ifdef __cplusplus
}
#endif

#endif
