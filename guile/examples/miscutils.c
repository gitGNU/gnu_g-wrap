#include <stdlib.h>
#include <string.h>

#include <time.h>

#include "miscutils.h"

Timespec64
elapsed_time (Timespec64 start, Timespec64 finish)
{
  Timespec64 result;

  /* This expects normalized input times; TODO: fully normalize the
     output */
  result.seconds = finish.seconds - start.seconds;
  result.nanoseconds = finish.nanoseconds - start.nanoseconds;

  if (result.nanoseconds < 0)
    {
      result.seconds -= 1;
      result.nanoseconds += 1000000000;
    }
  
  return result;
}

char *
join_strings (const char *s1, const char *s2)
{
  int len1 = strlen (s1);
  int len2 = strlen (s2);
  char *result = (char *) malloc (len1 + len2 + 1);

  memcpy (result, s1, len1);
  memcpy (result + len1, s2, len2 + 1);

  return result;
}

double
seconds_since_dow (unsigned int dow)
{
  time_t then, now = time (NULL);
  struct tm t;

  localtime_r (&now, &t);

  if (dow > t.tm_wday)
    t.tm_mday -= 7; /* last week */
  t.tm_mday -= (t.tm_wday - dow);

  then = mktime (&t);

  return now - then;
}
