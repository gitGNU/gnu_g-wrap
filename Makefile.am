include $(top_srcdir)/common.mk

SUBDIRS = @GW_LIBFFI_DIR@ lib g-wrap test @GW_LANGUAGES@ examples doc

nobase_guilemodule_DATA = g-wrap.scm

dist_bin_SCRIPTS = g-wrap-config

aclocaldir = $(datadir)/aclocal

aclocal_DATA = m4/g-wrap.m4

EXTRA_DIST = g-wrap.scm \
             m4/g-wrap.m4 m4/libcheck.m4-in m4/alloca.m4 m4/gw-util.m4

ACLOCAL_AMFLAGS = -I m4 @ACLOCAL_FLAGS@

