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

void scm_gc_free(void *mem, size_t size, const char *what)
{
  scm_must_free (mem);
  scm_done_free (size);
}

#endif
