### -*-m4-*-

AC_INIT(g-wrap.scm)
AM_INIT_AUTOMAKE(g-wrap, 1.5-tng-pre0, no-define)
AM_CONFIG_HEADER(config.h)

AM_MAINTAINER_MODE

dnl Shared library versions

GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER=0
GW_GUILE_RUNTIME_INTERFACE_REVISION=0
GW_GUILE_RUNTIME_INTERFACE_AGE=0

AC_SUBST(GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER)
AC_SUBST(GW_GUILE_RUNTIME_INTERFACE_REVISION)
AC_SUBST(GW_GUILE_RUNTIME_INTERFACE_AGE)

AC_DEFINE_UNQUOTED(GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER,
                   ${GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER},
                   [The major interface number for libgwrap-guile-runtime.])

AC_DEFINE_UNQUOTED(GW_GUILE_RUNTIME_INTERFACE_REVISION,
                   ${GW_GUILE_RUNTIME_INTERFACE_REVISION},
                   [The revision number for libgwrap-guile-runtime.])

AC_DEFINE_UNQUOTED(GW_GUILE_RUNTIME_INTERFACE_AGE,
                   ${GW_GUILE_RUNTIME_INTERFACE_AGE},
  [Number of previous interfaces supported by the current libgwrap-guile-runtime API.])



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
           g-wrap/Makefile])
