/**********************************************************************
Copyright (C) 2003-2004 Andreas Rottmann
 
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

#ifndef __G_WRAP_GUILE_RUNTIME_H__
#define __G_WRAP_GUILE_RUNTIME_H__

#include <libguile.h>

#include <g-wrap/ffi-support.h>
#include <g-wrap/core-runtime.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _GWEnumPair GWEnumPair;

struct _GWEnumPair
{
    int val;
    const char *sym;
};

void gw_guile_runtime_init (void);
SCM gw_guile_enum_val2sym(GWEnumPair enum_pairs[], SCM scm_val,
                          SCM scm_show_all_p);
SCM gw_guile_enum_val2int(GWEnumPair enum_pairs[], SCM scm_val);

#ifdef __cplusplus
}
#endif

#endif
