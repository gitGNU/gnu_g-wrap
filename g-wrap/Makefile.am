
gwrapmoduledir = @GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

CLEANFILES =
noinst_PROGRAMS =
gwrapinclude_HEADERS = 

AM_CFLAGS = $(GLIB_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)

gwrapmodule_DATA =

lib_LTLIBRARIES = \
 libgwrap-guile-runtime.la

GUILE_FLAGS =

################## libgwrap-guile-runtime section

GW_GUILE_RUNTIME_VINFO = \
  ${GW_GUILE_RUNTIME_INTERFACE_MAJOR_VER}:${GW_GUILE_RUNTIME_INTERFACE_REVISION}:${GW_GUILE_RUNTIME_INTERFACE_AGE}
gwrapinclude_HEADERS += guile-runtime.h guile-compatibility.h guile-wct.h
libgwrap_guile_runtime_la_SOURCES = guile-runtime.c guile-wct.c guile-compatibility.c
libgwrap_guile_runtime_la_LDFLAGS = -rpath $(libdir) \
				    -version-info ${GW_GUILE_RUNTIME_VINFO}
libgwrap_guile_runtime_la_LIBADD = $(GUILE_LDFLAGS) $(LIBFFI_LIBS) @ALLOCA@


EXTRA_DIST = $(gwrapmodule_DATA)
