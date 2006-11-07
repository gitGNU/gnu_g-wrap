include $(top_srcdir)/common.mk

SUBDIRS = @GW_LIBFFI_DIR@ lib g-wrap test @GW_LANGUAGES@ examples doc
DIST_SUBDIRS = libffi lib g-wrap test guile scheme48 examples doc

dist-hook:
	if test ! -d libffi; then echo "libffi/ directory needed for make dist">&2; exit 1; fi

nobase_guilemodule_DATA = g-wrap.scm

dist_bin_SCRIPTS = g-wrap-config

aclocaldir = $(datadir)/aclocal

aclocal_DATA = m4/g-wrap.m4

EXTRA_DIST = g-wrap.scm \
             m4/g-wrap.m4 m4/libcheck.m4-in m4/alloca.m4 m4/gw-util.m4

ACLOCAL_AMFLAGS = -I m4 @ACLOCAL_FLAGS@

