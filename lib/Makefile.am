include $(top_srcdir)/common.mk

BUILT_SOURCES =
MOSTLYCLEANFILES =
nobase_guilemodule_DATA = 

if !HAVE_SRFI_34
nobase_guilemodule_DATA += srfi/srfi-34.scm
endif
if !HAVE_SRFI_35
nobase_guilemodule_DATA += srfi/srfi-35.scm
endif

EXTRA_DIST = srfi/srfi-34.scm srfi/srfi-35.scm

# alloca
BUILT_SOURCES += $(ALLOCA_H)
EXTRA_DIST += alloca_.h

# We need the following in order to create an <alloca.h> when the system
# doesn't have one that works with the given compiler.
all-local $(lib_OBJECTS): $(ALLOCA_H)
alloca.h: alloca_.h
	cp $(srcdir)/alloca_.h $@-t
	mv $@-t $@
MOSTLYCLEANFILES += alloca.h alloca.h-t
