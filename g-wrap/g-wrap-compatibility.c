#include "g-wrap-compatibility.h"

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
