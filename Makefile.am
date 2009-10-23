include $(top_srcdir)/common.mk

SUBDIRS = lib g-wrap test @GW_LANGUAGES@ doc
DIST_SUBDIRS = lib g-wrap test guile scheme48 doc

nobase_guilemodule_DATA = g-wrap.scm

dist_bin_SCRIPTS = g-wrap-config
dist_man_MANS = g-wrap-config.1

aclocaldir = $(datadir)/aclocal

aclocal_DATA = m4/g-wrap.m4

EXTRA_DIST = g-wrap.scm \
	     $(wildcard $(srcdir)/m4/*.m4) \
	     $(wildcard $(srcdir)/m4/*.m4-in)

ACLOCAL_AMFLAGS = -I m4 @ACLOCAL_FLAGS@

if !HAVE_SRFI_34
nobase_guilemodule_DATA += srfi/srfi-34.scm
endif
if !HAVE_SRFI_35
nobase_guilemodule_DATA += srfi/srfi-35.scm
endif

EXTRA_DIST += srfi/srfi-34.scm srfi/srfi-35.scm
