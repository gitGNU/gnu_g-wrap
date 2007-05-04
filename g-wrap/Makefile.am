include $(top_srcdir)/common.mk

AM_CPPFLAGS = -I$(top_srcdir) $(LIBFFI_CFLAGS)

lib_LTLIBRARIES = libgwrap-core-runtime.la

gwrapinclude_HEADERS = core-runtime.h ffi-support.h
libgwrap_core_runtime_la_SOURCES = core-runtime.c
libgwrap_core_runtime_la_LDFLAGS =  -version-info $(GW_CORE_RUNTIME_VINFO) -no-undefined
libgwrap_core_runtime_la_LIBADD = $(LIBFFI_LIBS) ../lib/libgnu.la

nobase_gwrapmodule_DATA =					\
  util.scm rti.scm enumeration.scm c-types.scm ws/standard.scm	\
  c-codegen.scm scm-codegen.scm

CLEANFILES = 

EXTRA_DIST = config.scm.in $(nobase_gwrapmodule_DATA) ffi-support.h.in

all-local: config.scm

config.scm: Makefile config.scm.in
	sed -e "s|@g_wrap_shlib_dir\@||" \
	    $(srcdir)/config.scm.in > config.scm

install-data-local: Makefile config.scm.in
	$(mkinstalldirs) $(DESTDIR)$(gwrapmoduledir)
	sed -e "s|@g_wrap_shlib_dir@|$(gwrapshlibdir)/|" \
	    $(srcdir)/config.scm.in > $(DESTDIR)$(gwrapmoduledir)/config.scm
	chmod 644 $(DESTDIR)$(gwrapmoduledir)/config.scm

uninstall-local:
	rm -f $(DESTDIR)$(gwrapmoduledir)/config.scm

CLEANFILES += config.scm
