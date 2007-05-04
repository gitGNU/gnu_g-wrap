include $(top_srcdir)/common.mk

SUBDIRS = . gw

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)/lib -I$(top_srcdir)/guile
AM_CFLAGS = $(GUILE_CFLAGS) $(LIBFFI_CFLAGS)

lib_LTLIBRARIES = libgwrap-guile-runtime.la

nobase_gwrapmodule_DATA = \
  guile.scm guile/ws/standard.scm \
  compat.scm simple-type.scm gw-standard-spec.scm gw-wct-spec.scm \
  gw-glib-spec.scm gw-wct.scm

gwrapinclude_HEADERS = guile-runtime.h guile-compatibility.h guile-wct.h
libgwrap_guile_runtime_la_SOURCES = \
	guile-runtime.c guile-wct.c guile-compatibility.c
libgwrap_guile_runtime_la_LDFLAGS = -version-info $(GW_GUILE_RUNTIME_VINFO) -no-undefined
libgwrap_guile_runtime_la_LIBADD = \
	$(top_builddir)/g-wrap/libgwrap-core-runtime.la \
	$(GUILE_LDFLAGS) $(LIBFFI_LIBS) $(top_builddir)/lib/libgnu.la

EXTRA_DIST = $(nobase_gwrapmodule_DATA)
