SUBDIRS = . guile

gwrapmoduledir = @GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

noinst_PROGRAMS =
gwrapinclude_HEADERS = 

AM_CPPFLAGS = -I$(top_srcdir)

gwrapmodule_DATA =

lib_LTLIBRARIES = libgwrap-core-runtime.la \
                  libgwrap-guile-runtime.la

GUILE_FLAGS =

################## libgwrap-core-runtime section

gwrapinclude_HEADERS += core-runtime.h ffi-support.h
libgwrap_core_runtime_la_SOURCES = core-runtime.c
libgwrap_core_runtime_la_LDFLAGS = -rpath $(libdir) \
				    -version-info $(GW_CORE_RUNTIME_VINFO)
libgwrap_core_runtime_la_LIBADD = $(LIBFFI_LIBS) @ALLOCA@

################## libgwrap-guile-runtime section

gwrapinclude_HEADERS += guile-runtime.h guile-compatibility.h guile-wct.h
libgwrap_guile_runtime_la_SOURCES = guile-runtime.c guile-wct.c guile-compatibility.c
libgwrap_guile_runtime_la_LDFLAGS = -rpath $(libdir) \
				    -version-info $(GW_GUILE_RUNTIME_VINFO)
libgwrap_guile_runtime_la_LIBADD = $(GUILE_LDFLAGS) $(LIBFFI_LIBS) @ALLOCA@


EXTRA_DIST = $(gwrapmodule_DATA)
CLEANFILES =
