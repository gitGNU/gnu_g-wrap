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

/* G-Wrap C pointer object system funcs ****************************/
/*
 * Copyright (C) 1996 Christopher Lee
 */

/** Wrapped C type funcs **/

SCM
gw_wct_create (const char *type_name,
               SCM (*equal_p)(SCM wcp_a, SCM wcp_b),
               int (*print)(SCM wcp, SCM port,
                            char writing_p,
                            int *use_default_printer_p),
               SCM (*mark)(SCM wcp),
               size_t (*cleanup)(SCM wcp));

int gw_wct_p(SCM obj);

/** Wrapped C pointer funcs **/

/* create a wrapped C pointer of the given type, wrapping ptr */
SCM gw_wcp_assimilate_ptr(void *ptr, SCM type);

/* see if the given obj is really a wcp */
int gw_wcp_p(SCM obj);
/* return the C pointer in the given wrapped C pointer object. */
void *gw_wcp_get_ptr(SCM wcp);
/* return non-zero if wrapped C pointer obj is of the given type. */
int   gw_wcp_is_of_type_p(SCM type, SCM wcp);
/* return a new wrapped C pointer */
SCM   gw_wcp_coerce(SCM wcp, SCM new_type);
/* set a finalization routine for the given wcp.  Called at garbage
   collection time with one argument, the wcp. */

/* private -- should only be manipulated by type-related code, not
   accessed directly. */
void gw_wcp_set_scm_data(SCM wcp, SCM user_data);
SCM gw_wcp_get_scm_data(SCM wcp);

/* Misc ************************************************************/
void gw_wct_initialize (void);

#ifdef __cplusplus
}
#endif

#endif
