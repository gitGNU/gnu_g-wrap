SUBDIRS = guile

gwrapmoduledir = @GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

AM_CPPFLAGS = -I$(top_srcdir)

lib_LTLIBRARIES = libgwrap-core-runtime.la

gwrapinclude_HEADERS = core-runtime.h ffi-support.h
libgwrap_core_runtime_la_SOURCES = core-runtime.c
libgwrap_core_runtime_la_LDFLAGS = -rpath $(libdir) \
				    -version-info $(GW_CORE_RUNTIME_VINFO)
libgwrap_core_runtime_la_LIBADD = $(LIBFFI_LIBS) @ALLOCA@

gwrapmodule_DATA = util.scm rti.scm enumeration.scm ws/standard.scm \
		   guile.scm

EXTRA_DIST = $(gwrapmodule_DATA)
 