include $(top_srcdir)/common.mk

AM_CPPFLAGS = -I$(top_srcdir) $(LIBFFI_CFLAGS)

lib_LTLIBRARIES = libgwrap-core-runtime.la

gwrapinclude_HEADERS = core-runtime.h ffi-support.h
libgwrap_core_runtime_la_SOURCES = core-runtime.c
libgwrap_core_runtime_la_LDFLAGS =  -version-info $(GW_CORE_RUNTIME_VINFO)
libgwrap_core_runtime_la_LIBADD = $(LIBFFI_LIBS)

nobase_gwrapmodule_DATA = \
  util.scm rti.scm enumeration.scm c-types.scm ws/standard.scm

EXTRA_DIST = $(nobase_gwrapmodule_DATA) ffi-support.h.in
