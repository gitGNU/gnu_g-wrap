/**********************************************************************
Copyright (C) 2002 Robert Browning
 
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

#ifndef __G_WRAP_GLIB_H__
#define __G_WRAP_GLIB_H__

#include <libguile.h>
#include <glib.h>
  
#ifdef __cplusplus
extern "C" {
#endif

  SCM gw_glib_gint64_to_scm(const gint64 x);
  gint64 gw_glib_scm_to_gint64(SCM num);
  int gw_glib_gint64_p(SCM num);

#ifdef __cplusplus
}
#endif

#endif
