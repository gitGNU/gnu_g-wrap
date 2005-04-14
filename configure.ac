### -*-m4-*-

AC_INIT(g-wrap.scm)

dnl Program version

GW_BRANCH=2.0 dnl Branch number, e.g. for pkg-config
AC_SUBST(GW_BRANCH)
GW_VERSION=1.9.6

AM_INIT_AUTOMAKE(g-wrap, $GW_VERSION, no-define)
AC_CONFIG_HEADERS(config.h g-wrap/ffi-support.h)

AM_MAINTAINER_MODE

AC_SUBST(ACLOCAL_FLAGS,$ACLOCAL_FLAGS)

dnl Shared library versions
GW_RUNTIME_VINFO(CORE, 0, 1, 0)
GW_RUNTIME_VINFO(GUILE, 0, 2, 0)

dnl
dnl Checks for user options
dnl

AC_ARG_WITH(modules-dir,
[  --with-modules-dir=<dir>  Base directory for contributed Guile modules ],
  GUILEMODDIR=$withval,
  GUILEMODDIR=${datadir}/guile/site)
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
STD_CFLAGS=""

dnl Checks for types
AC_CHECK_SIZEOF(long)
AC_C_BIGENDIAN

dnl Checks for standard functions
gl_FUNC_ALLOCA


dnl
dnl Checks for required libraries
dnl

dnl libffi
ar_WITH_LIB_LIBFFI([have_libffi=yes], [have_libffi=no])
LIBFFI_CFLAGS_INSTALLED="$LIBFFI_CFLAGS"
LIBFFI_LIBS_INSTALLED="$LIBFFI_LIBS"
if test "$have_libffi" = no; then
  if ! test -d $srcdir/libffi; then
    AC_MSG_ERROR([G-Wrap needs libffi installed])
  else
    AC_CONFIG_SUBDIRS(libffi)
    LIBFFI_CFLAGS='-I$(top_builddir)/libffi/include'
    LIBFFI_LIBS='$(top_builddir)/libffi/libffi.la'
    LIBFFI_CFLAGS_INSTALLED=""
    LIBFFI_LIBS_INSTALLED="-lffi"
  fi
fi
AM_CONDITIONAL(HAVE_LIBFFI, test "$have_libffi" = yes)
AC_SUBST(LIBFFI_CFLAGS)
AC_SUBST(LIBFFI_LIBS)
AC_SUBST(LIBFFI_CFLAGS_INSTALLED)
AC_SUBST(LIBFFI_LIBS_INSTALLED)

dnl figure out approriate ffi type for size_t
AC_CHECK_SIZEOF(size_t)
AC_CHECK_SIZEOF(ssize_t)
ffi_size_type=uint$(($ac_cv_sizeof_size_t*8))
ffi_ssize_type=sint$(($ac_cv_sizeof_ssize_t*8))
AC_DEFINE_UNQUOTED(ffi_type_size_t, ffi_type_${ffi_size_type}, 
		   [ffi type for size_t])
AC_DEFINE_UNQUOTED(ffi_type_ssize_t, ffi_type_${ffi_ssize_type}, 
		   [ffi type for ssize_t])

dnl Guile
GUILE_FLAGS

GUILE_MODULE_AVAILABLE(HAVE_SRFI_34, [(srfi srfi-34)])
GUILE_MODULE_AVAILABLE(HAVE_SRFI_35, [(srfi srfi-35)])

AM_CONDITIONAL(HAVE_SRFI_34, test "$HAVE_SRFI_34" = yes)
AM_CONDITIONAL(HAVE_SRFI_35, test "$HAVE_SRFI_35" = yes)

dnl Check for optional libraries
PKG_CHECK_MODULES(GLIB, glib, [have_glib=yes], [have_glib=no])
AM_CONDITIONAL(HAVE_GLIB, test "$have_glib" = yes)

dnl Check for compiler flags and warnings. This should be last, since
dnl it can infere with other checks due to changed CFLAGS.
if test "$GCC" = yes; then
  dnl Add the warnings we want
  CFLAGS="$CFLAGS -Wall -Wmissing-prototypes"
  dnl -Werror ?
  AC_ARG_ENABLE([Werror], AC_HELP_STRING([--disable-Werror], 
					 [Don't stop the build on warnings]),
                [], CFLAGS="$CFLAGS -Werror")
  
  AC_MSG_CHECKING([whether $CC accepts -std=gnu99])
  save_CFLAGS="$CFLAGS"
  CFLAGS="-std=gnu99"
  AC_COMPILE_IFELSE(AC_LANG_PROGRAM(), cc_std_gnu99=yes, cc_std_gnu99=no)
  if test "$cc_std_gnu99" = yes; then
    STD_CFLAGS="$STD_CFLAGS -std=gnu99"
    CFLAGS="$save_CFLAGS -std=gnu99"
    AC_MSG_RESULT(yes)
  else
    CFLAGS="$save_CFLAGS"
    AC_MSG_RESULT(no)
  fi
fi

AC_SUBST(STD_CFLAGS)

AC_OUTPUT([Makefile
	   lib/Makefile
           g-wrap/Makefile
	   test/Makefile
           guile/Makefile
	   guile/g-wrap-2.0-guile.pc
	   guile/g-wrap/Makefile
	   guile/g-wrap/gw/Makefile
	   guile/test/Makefile
	   examples/Makefile
	   doc/Makefile
	   g-wrap-config
])
