SUBDIRS = g-wrap test guile doc

guilemoduledir = @GUILEMODDIR@
nobase_guilemodule_DATA = g-wrap.scm

EXTRA_DIST = $(guilemodule_DATA)

ACLOCAL_AMFLAGS = -I m4 $(ACLOCAL_FLAGS)

if !HAVE_SRFI_34
nobase_guilemodule_DATA += srfi/srfi-34.scm
endif
