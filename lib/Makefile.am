## DO NOT EDIT! GENERATED AUTOMATICALLY!
## Process this file with automake to produce Makefile.in.
# Copyright (C) 2004-2007 Free Software Foundation, Inc.
#
# This file is free software, distributed under the terms of the GNU
# General Public License.  As a special exception to the GNU General
# Public License, this file may be distributed as part of a program
# that contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
# Reproduce by: gnulib-tool --import --dir=. --lib=libgnu --source-base=lib --m4-base=m4 --doc-base=doc --aux-dir=. --libtool --macro-prefix=gl vasprintf

AUTOMAKE_OPTIONS = 1.5 gnits

noinst_HEADERS =
noinst_LIBRARIES =
noinst_LTLIBRARIES =
EXTRA_DIST =
BUILT_SOURCES =
SUFFIXES =
MOSTLYCLEANFILES = core *.stackdump
MOSTLYCLEANDIRS =
CLEANFILES =
DISTCLEANFILES =
MAINTAINERCLEANFILES =

AM_CPPFLAGS =

noinst_LTLIBRARIES += libgnu.la

libgnu_la_SOURCES =
libgnu_la_LIBADD = $(gl_LTLIBOBJS)
libgnu_la_DEPENDENCIES = $(gl_LTLIBOBJS)
EXTRA_libgnu_la_SOURCES =
libgnu_la_LDFLAGS = $(AM_LDFLAGS)

## begin gnulib module alloca-opt

BUILT_SOURCES += $(ALLOCA_H)

# We need the following in order to create <alloca.h> when the system
# doesn't have one that works with the given compiler.
alloca.h: alloca_.h
	{ echo '/* DO NOT EDIT! GENERATED AUTOMATICALLY! */'; \
	  cat $(srcdir)/alloca_.h; \
	} > $@-t
	mv -f $@-t $@
MOSTLYCLEANFILES += alloca.h alloca.h-t

EXTRA_DIST += alloca_.h

## end   gnulib module alloca-opt

## begin gnulib module link-warning

LINK_WARNING_H=$(top_srcdir)/./link-warning.h

## end   gnulib module link-warning

## begin gnulib module size_max

libgnu_la_SOURCES += size_max.h

## end   gnulib module size_max

## begin gnulib module stdint

BUILT_SOURCES += $(STDINT_H)

# We need the following in order to create <stdint.h> when the system
# doesn't have one that works with the given compiler.
stdint.h: stdint_.h
	rm -f $@-t $@
	{ echo '/* DO NOT EDIT! GENERATED AUTOMATICALLY! */'; \
	  sed -e 's/@''HAVE_STDINT_H''@/$(HAVE_STDINT_H)/g' \
	      -e 's|@''ABSOLUTE_STDINT_H''@|$(ABSOLUTE_STDINT_H)|g' \
	      -e 's/@''HAVE_SYS_TYPES_H''@/$(HAVE_SYS_TYPES_H)/g' \
	      -e 's/@''HAVE_INTTYPES_H''@/$(HAVE_INTTYPES_H)/g' \
	      -e 's/@''HAVE_SYS_INTTYPES_H''@/$(HAVE_SYS_INTTYPES_H)/g' \
	      -e 's/@''HAVE_SYS_BITYPES_H''@/$(HAVE_SYS_BITYPES_H)/g' \
	      -e 's/@''HAVE_LONG_LONG_INT''@/$(HAVE_LONG_LONG_INT)/g' \
	      -e 's/@''HAVE_UNSIGNED_LONG_LONG_INT''@/$(HAVE_UNSIGNED_LONG_LONG_INT)/g' \
	      -e 's/@''BITSIZEOF_PTRDIFF_T''@/$(BITSIZEOF_PTRDIFF_T)/g' \
	      -e 's/@''PTRDIFF_T_SUFFIX''@/$(PTRDIFF_T_SUFFIX)/g' \
	      -e 's/@''BITSIZEOF_SIG_ATOMIC_T''@/$(BITSIZEOF_SIG_ATOMIC_T)/g' \
	      -e 's/@''HAVE_SIGNED_SIG_ATOMIC_T''@/$(HAVE_SIGNED_SIG_ATOMIC_T)/g' \
	      -e 's/@''SIG_ATOMIC_T_SUFFIX''@/$(SIG_ATOMIC_T_SUFFIX)/g' \
	      -e 's/@''BITSIZEOF_SIZE_T''@/$(BITSIZEOF_SIZE_T)/g' \
	      -e 's/@''SIZE_T_SUFFIX''@/$(SIZE_T_SUFFIX)/g' \
	      -e 's/@''BITSIZEOF_WCHAR_T''@/$(BITSIZEOF_WCHAR_T)/g' \
	      -e 's/@''HAVE_SIGNED_WCHAR_T''@/$(HAVE_SIGNED_WCHAR_T)/g' \
	      -e 's/@''WCHAR_T_SUFFIX''@/$(WCHAR_T_SUFFIX)/g' \
	      -e 's/@''BITSIZEOF_WINT_T''@/$(BITSIZEOF_WINT_T)/g' \
	      -e 's/@''HAVE_SIGNED_WINT_T''@/$(HAVE_SIGNED_WINT_T)/g' \
	      -e 's/@''WINT_T_SUFFIX''@/$(WINT_T_SUFFIX)/g' \
	      < $(srcdir)/stdint_.h; \
	} > $@-t
	mv $@-t $@
MOSTLYCLEANFILES += stdint.h stdint.h-t

EXTRA_DIST += stdint_.h

## end   gnulib module stdint

## begin gnulib module stdio

BUILT_SOURCES += stdio.h

# We need the following in order to create <stdio.h> when the system
# doesn't have one that works with the given compiler.
stdio.h: stdio_.h
	rm -f $@-t $@
	{ echo '/* DO NOT EDIT! GENERATED AUTOMATICALLY! */' && \
	  sed -e 's|@''ABSOLUTE_STDIO_H''@|$(ABSOLUTE_STDIO_H)|g' \
	      -e 's|@''GNULIB_FPRINTF_POSIX''@|$(GNULIB_FPRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_PRINTF_POSIX''@|$(GNULIB_PRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_SNPRINTF''@|$(GNULIB_SNPRINTF)|g' \
	      -e 's|@''GNULIB_SPRINTF_POSIX''@|$(GNULIB_SPRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_VFPRINTF_POSIX''@|$(GNULIB_VFPRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_VPRINTF_POSIX''@|$(GNULIB_VPRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_VSNPRINTF''@|$(GNULIB_VSNPRINTF)|g' \
	      -e 's|@''GNULIB_VSPRINTF_POSIX''@|$(GNULIB_VSPRINTF_POSIX)|g' \
	      -e 's|@''GNULIB_VASPRINTF''@|$(GNULIB_VASPRINTF)|g' \
	      -e 's|@''REPLACE_FPRINTF''@|$(REPLACE_FPRINTF)|g' \
	      -e 's|@''REPLACE_VFPRINTF''@|$(REPLACE_VFPRINTF)|g' \
	      -e 's|@''REPLACE_PRINTF''@|$(REPLACE_PRINTF)|g' \
	      -e 's|@''REPLACE_VPRINTF''@|$(REPLACE_VPRINTF)|g' \
	      -e 's|@''REPLACE_SNPRINTF''@|$(REPLACE_SNPRINTF)|g' \
	      -e 's|@''HAVE_DECL_SNPRINTF''@|$(HAVE_DECL_SNPRINTF)|g' \
	      -e 's|@''REPLACE_VSNPRINTF''@|$(REPLACE_VSNPRINTF)|g' \
	      -e 's|@''HAVE_DECL_VSNPRINTF''@|$(HAVE_DECL_VSNPRINTF)|g' \
	      -e 's|@''REPLACE_SPRINTF''@|$(REPLACE_SPRINTF)|g' \
	      -e 's|@''REPLACE_VSPRINTF''@|$(REPLACE_VSPRINTF)|g' \
	      -e 's|@''HAVE_VASPRINTF''@|$(HAVE_VASPRINTF)|g' \
	      -e 's|@''REPLACE_VASPRINTF''@|$(REPLACE_VASPRINTF)|g' \
	      -e '/definition of GL_LINK_WARNING/r $(LINK_WARNING_H)' \
	      < $(srcdir)/stdio_.h; \
	} > $@-t
	mv $@-t $@
MOSTLYCLEANFILES += stdio.h stdio.h-t

EXTRA_DIST += stdio_.h

## end   gnulib module stdio

## begin gnulib module vasnprintf


EXTRA_DIST += asnprintf.c float+.h printf-args.c printf-args.h printf-parse.c printf-parse.h vasnprintf.c vasnprintf.h

EXTRA_libgnu_la_SOURCES += asnprintf.c printf-args.c printf-parse.c vasnprintf.c

## end   gnulib module vasnprintf

## begin gnulib module vasprintf


EXTRA_DIST += asprintf.c vasprintf.c

EXTRA_libgnu_la_SOURCES += asprintf.c vasprintf.c

## end   gnulib module vasprintf

## begin gnulib module wchar

BUILT_SOURCES += $(WCHAR_H)

# We need the following in order to create <wchar.h> when the system
# version does not work standalone.
wchar.h: wchar_.h
	rm -f $@-t $@
	{ echo '/* DO NOT EDIT! GENERATED AUTOMATICALLY! */'; \
	  sed -e 's|@''ABSOLUTE_WCHAR_H''@|$(ABSOLUTE_WCHAR_H)|g' \
	    < $(srcdir)/wchar_.h; \
	} > $@-t
	mv $@-t $@
MOSTLYCLEANFILES += wchar.h wchar.h-t

EXTRA_DIST += wchar_.h

## end   gnulib module wchar

## begin gnulib module xsize

libgnu_la_SOURCES += xsize.h

## end   gnulib module xsize

## begin gnulib module dummy

libgnu_la_SOURCES += dummy.c

## end   gnulib module dummy


mostlyclean-local: mostlyclean-generic
	@for dir in '' $(MOSTLYCLEANDIRS); do \
	  if test -n "$$dir" && test -d $$dir; then \
	    echo "rmdir $$dir"; rmdir $$dir; \
	  fi; \
	done
