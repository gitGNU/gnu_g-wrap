/**********************************************************************
Copyright (C) 2002 Rob Browning
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

#ifndef __G_WRAP_COMPATIBILITY_H__
#define __G_WRAP_COMPATIBILITY_H__

#include <libguile.h>
  
#ifdef __cplusplus
extern "C" {
#endif

/* guile 1.3.4 compatibility */
#ifndef SCM_CHAR
#define SCM_CHAR(x) SCM_ICHR(x)
#endif

#ifndef SCM_MAKE_CHAR
#define SCM_MAKE_CHAR(x) SCM_MAKICHR(x)
#endif

/* Define this macro if Guile 1.7.x or better is in use. */
#if defined (SCM_MINOR_VERSION) && (SCM_MINOR_VERSION >= 7) && \
    defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION >= 1)
#define SCM_VERSION_17X 1
#endif

/* Support for coding against Guile 1.7 */
#ifndef SCM_VERSION_17X

void *  scm_malloc(size_t size);
void *  scm_realloc(void *mem, size_t size);

#define scm_gc_malloc(size, what) scm_must_malloc((size), (what))
void    scm_gc_free(void *mem, size_t size, const char *what);

#define SCM_VECTOR_SET(x, idx, val) (SCM_VELTS(x)[(idx)] = (val))

#endif

#ifdef __cplusplus
}
#endif

#endif
