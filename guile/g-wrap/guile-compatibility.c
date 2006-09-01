/**********************************************************************
Copyright (C) 2003-2006 Andreas Rottmann
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
#include <string.h>

#include "g-wrap/guile-compatibility.h"

#ifndef SCM_VERSION_17X

void *
scm_malloc(size_t size)
{
  void *result = malloc (size);
  
  if (result)
    return result;

  scm_memory_error("scm_malloc");
}

void *
scm_realloc(void *mem, size_t size)
{
  mem = realloc (mem, size);
  if (mem)
    return mem;

  scm_memory_error("scm_realloc");
}

void
scm_gc_free(void *mem, size_t size, const char *what)
{
  scm_must_free (mem);
  scm_done_free (size);
}


/* Strings.  */

SCM scm_take_locale_string (char *str)
{
  SCM result = scm_makfrom0str (str);
  free (str);
  return result;
}

char *
scm_to_locale_string (SCM str)
{
  char *result;
  size_t len = SCM_STRING_LENGTH (str);

  result = scm_malloc (len + 1);
  memcpy (result, SCM_STRING_CHARS (str), len);
  result[len] = '\0';

  return result;
}

size_t
scm_to_locale_stringbuf (SCM str, char *buf, size_t buf_len)
{
  size_t len = SCM_STRING_LENGTH (str);

  /* Note:  No terminating `\0' will be stored.  */
  len = (len > buf_len) ? buf_len : len;
  memcpy (buf, SCM_STRING_CHARS (str), len);

  return len;
}

#endif
