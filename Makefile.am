SUBDIRS = g-wrap test guile doc

guilemoduledir = @GUILEMODDIR@
nobase_guilemodule_DATA = g-wrap.scm

EXTRA_DIST = g-wrap.scm srfi/srfi-34.scm srfi/srfi-35.scm

ACLOCAL_AMFLAGS = -I m4 $(ACLOCAL_FLAGS)

if !HAVE_SRFI_34
nobase_guilemodule_DATA += srfi/srfi-34.scm
endif
if !HAVE_SRFI_35
nobase_guilemodule_DATA += srfi/srfi-35.scm
endif
