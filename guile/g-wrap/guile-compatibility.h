/**********************************************************************
Copyright (C) 2002 Rob Browning
Copyright (C) 2003-2004 Andreas Rottmann
Copyright (C) 2005 Ludovic Court�s

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

/* Garbage collection.  */
void *  scm_malloc(size_t size);
void *  scm_realloc(void *mem, size_t size);

#define scm_gc_malloc(size, what) scm_must_malloc((size), (what))
void    scm_gc_free(void *mem, size_t size, const char *what);

/* Strings.  */
#define scm_is_string(_str)                (SCM_STRINGP (_str) != SCM_BOOL_F)
#define scm_c_string_length(_str)          (SCM_STRING_LENGTH (_str))
#define scm_from_locale_string(_str)       (scm_makfrom0str (_str))
char   *scm_to_locale_string (SCM str);
size_t  scm_to_locale_stringbuf (SCM str, char *buf, size_t buf_size);

#define GW_ACCESS_STRING(_c_ptr, _str)		\
  { (_c_ptr) = SCM_STRING_CHARS (_str); }


/* Symbols.  */
#define scm_is_symbol(_sym)                (SCM_SYMBOLP (_sym))
#define scm_from_locale_symbol(_sym)       (scm_str2symbol (_sym))
#define GW_ACCESS_SYMBOL(_c_ptr, _sym)		\
  { (_c_ptr) = SCM_SYMBOL_CHARS (_sym); }


/* Numbers.  */
#ifndef SCM_I_MAKINUM
# define SCM_I_MAKINUM(_num)             (SCM_MAKINUM (_num))
#endif

#define scm_to_double(_num)				\
  (scm_num2double ((_num), 1, "gw:scm->double"))
#define scm_from_double(_num)            (scm_double2num ((double)(_num)))

#define scm_from_ushort(_num)            (scm_ushort2num (_num))
#define scm_to_ushort(_num)              (scm_num2ushort ((_num), 0, NULL))
#define scm_from_short(_num)             (scm_short2num (_num))
#define scm_to_short(_num)               (scm_num2short ((_num), 0, NULL))
#define scm_from_int(_num)               (scm_int2num (_num))
#define scm_to_int(_num)                 (scm_num2int ((_num), 0, NULL))
#define scm_from_uint(_num)              (scm_uint2num (_num))
#define scm_to_uint(_num)                (scm_num2uint ((_num), 0, NULL))
#define scm_from_long(_num)              (scm_long2num (_num))
#define scm_to_long(_num)                (scm_num2long ((_num), 0, NULL))
#define scm_from_ulong(_num)             (scm_ulong2num (_num))
#define scm_to_ulong(_num)               (scm_num2ulong ((_num), 0, NULL))
#define scm_from_long_long(_num)         (scm_long_long2num (_num))
#define scm_to_long_long(_num)           (scm_num2long_long ((_num), 0, NULL))
#define scm_from_ulong_long(_num)        (scm_ulong_long2num (_num))
#define scm_to_ulong_long(_num)          (scm_num2ulong_long ((_num), 0, NULL))

/* Vectors.  */
#define SCM_SIMPLE_VECTOR_SET(v, idx, val) (SCM_VELTS (v)[(idx)] = (val))
#define SCM_SIMPLE_VECTOR_REF(v, idx)      (SCM_VELTS (v)[(idx)])


#else /* SCM_VERSION_17X */


/* Have C_PTR point to a local copy of STR.  */
#define GW_ACCESS_STRING(_c_ptr, _str)			\
  {							\
    size_t _s_str_len = scm_c_string_length (_str);	\
    (_c_ptr) = alloca (_s_str_len + 1);			\
    scm_to_locale_stringbuf (_str, _c_ptr, _s_str_len);	\
    (_c_ptr)[_s_str_len] = '\0';			\
  }

/* Have C_PTR point to a local copy of SYM.  */
#define GW_ACCESS_SYMBOL(_c_ptr, _sym)		\
  {						\
    SCM _s_str = scm_symbol_to_string (_sym);	\
    GW_ACCESS_STRING (_c_ptr, _s_str);		\
  }

#endif

#ifdef __cplusplus
}
#endif

#endif
