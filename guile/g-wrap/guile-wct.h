/**********************************************************************
Copyright (C) 1996 Christopher Lee
 
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

#ifndef __G_WRAP_WCT_H__
#define __G_WRAP_WCT_H__

#include <g-wrap/core-runtime.h>
  
#ifdef __cplusplus
extern "C" {
#endif

/* G-Wrap C pointer object system funcs ****************************/

/** Wrapped C type funcs **/

/* create a new wrapped C type.  Returns the new gw:wct on success,
   and #f on other failure.

   equal_p - should return SCM_BOOL_F if the two objects should be
   considered equal, anything else, otherwise.

   If set to NULL, then by default, the objects are equal? only if
   their wcp types and wcp ptrs match.

   If you do specify an equal_p function, it will only be called
   if the two objects' wcp types match, but their pointers don't.

   print - should return non-zero on success.  If the function sets
   use_default_print_p to non-zero value, then the default wcp
   printer will be invoked on return, and this function's return
   value will be ignored.  If print is set to NULL, a default
   representation will be printed.  Note that it is your
   responsibility to make sure you don't try to print a destroyed C
   pointer.  The wcp's scm_data might be useful for keeping track in
   cases where the scheme side doesn't wholly own the pointer...
  
   mark - should mark any scheme data stored in the c pointer.  As a
   convenience, any scheme object returned by this function will
   also be marked.  You do not need to mark the wcp's scm_data,
   that will be handed for you.  If this is set to NULL, only the
   scm_data will be marked.

   cleanup - should destroy the c-side pointer as appropriate.  If
   set, will be called at garbage collection time.  You do not
   need to worry about the scm_data here.  If possible, this
   function should return the amount of space reclaimed.  Also
   note that you don't need to do anything about SCM data inside
   (say in a struct) your C ptr.  It won't be marked and will be
   collected automagically.  Basically, you just need to worry
   about anything you malloced/gnewed/etc. when you created the
   wcp's data.

*/

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
