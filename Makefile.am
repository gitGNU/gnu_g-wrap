include $(top_srcdir)/common.mk

SUBDIRS = 
if !HAVE_LIBFFI
SUBDIRS += libffi
endif
SUBDIRS += lib g-wrap test @GW_LANGUAGES@ examples doc

nobase_guilemodule_DATA = g-wrap.scm

dist_bin_SCRIPTS = g-wrap-config

EXTRA_DIST = g-wrap.scm

ACLOCAL_AMFLAGS = -I m4 @ACLOCAL_FLAGS@

