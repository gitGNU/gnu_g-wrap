SUBDIRS = . gw

gwrapmoduledir = @GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

AM_CPPFLAGS = -I$(top_srcdir) -I $(top_srcdir)/guile

lib_LTLIBRARIES = libgwrap-guile-runtime.la

gwrapinclude_HEADERS = guile-runtime.h guile-compatibility.h guile-wct.h
libgwrap_guile_runtime_la_SOURCES = guile-runtime.c guile-wct.c guile-compatibility.c
libgwrap_guile_runtime_la_LDFLAGS = -version-info $(GW_GUILE_RUNTIME_VINFO)
libgwrap_guile_runtime_la_LIBADD = \
	$(top_builddir)/g-wrap/libgwrap-core-runtime.la \
	$(GUILE_LDFLAGS) $(LIBFFI_LIBS)

