/**********************************************************************
Copyright (C) 2002 Rob Browning
 
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

#ifdef __cplusplus
}
#endif

#endif
