#include "g-wrap-compatibility.h"

#ifndef SCM_VERSION_17X

/* These are basically xmalloc & friends */

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

#endif
