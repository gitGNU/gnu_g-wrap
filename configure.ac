### -*-m4-*-

AC_INIT(g-wrap.scm)

dnl Program version

GW_BRANCH=2.0 dnl Branch number, e.g. for pkg-config
AC_SUBST(GW_BRANCH)
GW_VERSION=1.9.0

AM_INIT_AUTOMAKE(g-wrap, $GW_VERSION, no-define)
AM_CONFIG_HEADER(config.h)

AM_MAINTAINER_MODE


dnl Shared library versions
GW_RUNTIME_VINFO(CORE, 0, 0, 0)
GW_RUNTIME_VINFO(GUILE, 0, 0, 0)

dnl
dnl Checks for user options
dnl

AC_ARG_WITH(modules-dir,
[  --with-modules-dir=<dir>  Base directory for contributed Guile modules ],
  GUILEMODDIR=$withval,
  GUILEMODDIR=${datadir}/guile)
AC_SUBST(GUILEMODDIR)

dnl
dnl Checks for programs.
dnl
AC_PROG_CC
AC_PROG_INSTALL
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL


dnl Checks for header files.

dnl Checks for typedefs, structures, and compiler characteristics.
if test "$GCC" = yes; then
  dnl Add the warnings we want
  CFLAGS="$CFLAGS -Wall -Wmissing-prototypes"
  
  AC_MSG_CHECKING([whether $CC accepts -std=gnu99])
  save_CFLAGS="$CFLAGS"
  CFLAGS="-std=gnu99"
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM(), cc_std_gnu99=yes, cc_std_gnu99=no)
  if test "$cc_std_gnu99" = yes; then
    CFLAGS="$save_CFLAGS -std=gnu99"
    AC_MSG_RESULT(yes)
  else
    CFLAGS="$save_CFLAGS"
    AC_MSG_RESULT(no)
  fi
fi

dnl Checks for standard functions
AC_FUNC_ALLOCA


dnl
dnl Checks for required libraries
dnl

dnl libffi
ar_WITH_LIB_LIBFFI
AC_SUBST(LIBFFI_CFLAGS)
AC_SUBST(LIBFFI_LIBS)

dnl Guile
GUILE_FLAGS

AC_OUTPUT([Makefile
           g-wrap/Makefile
	   g-wrap/guile/Makefile
	   test/Makefile
           guile/Makefile
	   guile/g-wrap-2.0-guile.pc
	   guile/g-wrap/Makefile
	   guile/g-wrap/gw/Makefile
	   guile/test/Makefile
])
