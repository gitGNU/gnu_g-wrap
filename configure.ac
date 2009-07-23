dnl -*- Autoconf -*-

dnl Program version
AC_INIT(g-wrap, 1.9.13-rc1)
AM_INIT_AUTOMAKE([no-define -Wno-portability])
AC_CONFIG_HEADERS(config.h g-wrap/ffi-support.h)

dnl Branch number, e.g. for pkg-config
GW_BRANCH=2.0
AC_SUBST(GW_BRANCH)

AM_MAINTAINER_MODE

AC_SUBST(ACLOCAL_FLAGS,$ACLOCAL_FLAGS)

dnl Shared library versions
GW_RUNTIME_VINFO(CORE, 2, 0, 0)
GW_RUNTIME_VINFO(GUILE, 2, 0, 0)

dnl
dnl Checks for user options
dnl

AC_ARG_WITH(modules-dir,
[  --with-modules-dir=<dir>  Base directory for contributed Guile modules ],
  GUILEMODDIR=$withval,
  GUILEMODDIR='${prefix}/share/guile/site')
AC_SUBST(GUILEMODDIR)

dnl
dnl Checks for programs.
dnl
AC_PROG_CC
gl_EARLY
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
gl_INIT
gl_FUNC_ALLOCA
gl_FUNC_VASPRINTF

dnl
dnl Checks for required libraries
dnl

dnl libffi
PKG_CHECK_MODULES(LIBFFI, libffi)
AC_SUBST(LIBFFI_CFLAGS)
AC_SUBST(LIBFFI_LIBS)

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
GUILE_PROGS
GUILE_FLAGS
GUILE_VERSION="`guile-config info guileversion | sed -r 's,\\.[[0-9]]+$,,'`"
AC_PATH_PROG(GUILE_VERSIONED, guile-$GUILE_VERSION)
if test "$GUILE_VERSIONED" != ""; then
  GUILE="$GUILE_VERSIONED"
fi
AC_MSG_NOTICE([Using $GUILE])

GUILE_MODULE_AVAILABLE(HAVE_SRFI_34, [(srfi srfi-34)])
GUILE_MODULE_AVAILABLE(HAVE_SRFI_35, [(srfi srfi-35)])

AM_CONDITIONAL(HAVE_SRFI_34, test "$HAVE_SRFI_34" = yes)
AM_CONDITIONAL(HAVE_SRFI_35, test "$HAVE_SRFI_35" = yes)

dnl Allow --with-imp flag
GW_LANGUAGES=guile

AC_DEFUN([GW_ALLOW_LANG],
[AC_ARG_ENABLE([$1],
               AS_HELP_STRING([--enable-$1], [support $1 (default is $2)]),
               [if test "$enableval" = yes; then
                  GW_LANGUAGES="$GW_LANGUAGES $1"
	        else
	          GW_LANGUAGES=$(echo "$GW_LANGUAGES" | sed s/[ ]?$1//)
	        fi])])

GW_ALLOW_LANG(guile, YES)
GW_ALLOW_LANG(scheme48, NO)

AC_SUBST(GW_LANGUAGES)

dnl Check for optional libraries
AC_ARG_WITH([glib], AS_HELP_STRING([--without-glib], [Build without glib-2 (default: test)]))
GLIB=no
if test "x$with_glib" != "xno"; then
  PKG_CHECK_MODULES([GLIB], [glib-2.0],
                    [AC_MSG_NOTICE([building GLib 2.0 support])
		     GLIB=yes],
                    [AC_MSG_NOTICE([Not building GLib 2.0 support])])
fi 
AM_CONDITIONAL(HAVE_GLIB, test "$GLIB" = yes)

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

AC_CONFIG_FILES([Makefile
	   lib/Makefile
           g-wrap/Makefile
	   test/Makefile
           guile/Makefile
	   guile/g-wrap-2.0-guile.pc
	   guile/g-wrap/Makefile
	   guile/g-wrap/gw/Makefile
	   guile/test/Makefile
	   guile/run-guile
	   guile/examples/Makefile
	   scheme48/Makefile
	   scheme48/g-wrap/Makefile
	   scheme48/g-wrap/gw/Makefile
	   scheme48/test/Makefile
	   doc/Makefile
	   g-wrap-config
])

AC_CONFIG_COMMANDS([run-guile-exec], [chmod +x guile/run-guile])

AC_OUTPUT
