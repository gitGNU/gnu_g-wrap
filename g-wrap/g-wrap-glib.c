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

#include <g-wrap-glib.h>
#include <guile/gh.h>

SCM
gw_glib_gint64_to_scm(const gint64 x)
{
#if GW_GUILE_LONG_LONG_OK 
  return scm_long_long2num(x);
#else
  const gchar negative_p = (x < 0);
  const guint64 magnitude = negative_p ? -x : x;
  const guint32 lower_half = (guint32) (magnitude & 0xFFFFFFFF);
  const guint32 upper_half = (guint32) (magnitude >> 32);
  SCM result;

  result = scm_sum(scm_ash(gh_ulong2scm(upper_half), SCM_MAKINUM(32)),
                   gh_ulong2scm(lower_half));
  
  if (negative_p) {
    return scm_difference(SCM_INUM0, result);
  } else {
    return result;
  }
#endif
}

gint64
gw_glib_scm_to_gint64(SCM num)
{
#if GW_GUILE_LONG_LONG_OK 
  return scm_num2long_long(num, (char *) SCM_ARG1, "gw_glib_scm_to_gint64");
#else
  static SCM bits00to15_mask = SCM_BOOL_F;
  SCM magnitude  = scm_abs(num);
  SCM bits;
  unsigned long c_bits;
  long long     c_result = 0;
  int		i;

  /* This doesn't work -- atm (bit-extract 4000 0 32) proves it */
  /*
  SCM lower = scm_bit_extract(magnitude, SCM_MAKINUM(0), SCM_MAKINUM(32));
  */
  
  if (bits00to15_mask == SCM_BOOL_F) {
    bits00to15_mask = gh_ulong2scm(0xFFFF);
    scm_gc_protect_object (bits00to15_mask);
  }

  /*
   * This isn't very complicated (IMHO).  We work from the "top" of
   * the number downwards.  We assume this is no more than a 64-bit
   * number, otherwise it will fail right away.  Anyways, we keep
   * taking the top 16 bits of the number and move it to c_result.
   * Then we 'remove' those bits from the original number and continue
   * with the next 16 bits down, and so on.  -- warlord@mit.edu
   * 2001/02/13
   */
  for (i = 48; i >=0; i-= 16) {
    bits = scm_ash(magnitude, SCM_MAKINUM(-i));
    c_bits = gh_scm2ulong(scm_logand(bits, bits00to15_mask));
    c_result += ((long long)c_bits << i);
    magnitude = scm_difference(magnitude, scm_ash(bits, SCM_MAKINUM(i)));
  }
  
  if(scm_negative_p(num) != SCM_BOOL_F) {
    return(- c_result);
  } 
  else {
    return(c_result);
  }
#endif
}

int
gw_glib_gint64_p(SCM num)
{
  static int initialized = 0;
  static SCM maxval;
  static SCM minval;

  if (!initialized)
  {
    /* to be super safe, we have to build these manually because
       though we know that we have gint64's here, we *don't* know how
       to portably specify a 64bit constant to the compiler (i.e. like
       0x7FFFFFFFFFFFFFFF). */
    gint64 tmp;
    
    tmp = 0x7FFFFFFF;
    tmp <<= 32;
    tmp |= 0xFFFFFFFF;
    maxval = gw_glib_gint64_to_scm(tmp);

    tmp = 0x80000000;
    tmp <<= 32;
    minval = gw_glib_gint64_to_scm(tmp);

    scm_gc_protect_object(maxval);
    scm_gc_protect_object(minval);
    initialized = 1;
  }

  return (gh_exact_p(num) &&
          (scm_geq_p(num, minval) != SCM_BOOL_F) &&
          (scm_leq_p(num, maxval) != SCM_BOOL_F));
}
