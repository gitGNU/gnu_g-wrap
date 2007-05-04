






AC_DEFUN([ar_WITH_LIB_LIBFFI],
[

AC_ARG_WITH(libffi-prefix, AC_HELP_STRING([--with-libffi-prefix],
	                                [LIBFFI installation prefix]), 
	    [libffi_prefix="$withval"], [libffi_prefix=""])

ac_orig_LIBS="$LIBS" 
ac_orig_CPPFLAGS="$CPPFLAGS"

libffi_lib_prefix=""
libffi_inc_prefix=""

LIBFFI_CFLAGS=""
LIBFFI_LIBS=""

if test x$libffi_prefix != x ; then

  libffi_lib_prefix="$libffi_prefix/lib"
  LIBFFI_LIBS="-L$libffi_lib_prefix"

  libffi_inc_prefix="$libffi_prefix/include"
  LIBFFI_CFLAGS="-I$libffi_inc_prefix"

fi

LIBFFI_LIBS="$LIBFFI_LIBS "

CPPFLAGS="$ac_orig_CPPFLAGS $LIBFFI_CFLAGS"
LIBS="$ac_orig_LIBS $LIBFFI_LIBS"

AC_CHECK_HEADER(ffi.h, [header_found=1], [header_found=0])
if test "$header_found" = 1; then
  for lib in ffi; do
    AC_CHECK_LIB($lib, ffi_prep_cif, [LIBFFI_LIBS="$LIBFFI_LIBS -l$lib"], [LIBFFI_LIBS=""])
    if test -n "$LIBFFI_LIBS"; then break; fi
  done
else
  LIBFFI_LIBS=""
fi
if test "$LIBFFI_LIBS" != ""; then
  ifelse($1,, true, $1)
else
  ifelse($2,, true, $2)
fi

LIBS="$ac_orig_LIBS"
CPPFLAGS="$ac_orig_CPPFLAGS"
])

