# Initializations.

CLEANFILES = $(wildcard gw-test-*.c gw-test-*.scm)
noinst_HEADERS =
noinst_LTLIBRARIES =
EXTRA_DIST = 

TESTS = \
 test-standard \
 test-enumeration \
 test-wct 

check_SCRIPTS = guile-test-env $(TESTS)

EXTRA_DIST += ${check_SCRIPTS}

GUILE_FLAGS = 

LOCALPWD=$(shell pwd)
SETGWPATH = "(set! %load-path (cons \"$(top_builddir)/guile\" (cons \"$(top_srcdir)\" %load-path)))"

EXTRA_DIST += $(wildcard $(srcdir)/*-spec.scm)

std_libs = $(top_builddir)/test/libg-wrap-test-c-code.la \
	   ../g-wrap/libgwrap-guile-runtime.la \
	   $(top_builddir)/g-wrap/libgwrap-core-runtime.la \
	   $(GUILE_LDFLAGS)

# ===========================================================================

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile

noinst_LTLIBRARIES += libg-wrap-guile-test-c-code.la
libg_wrap_guile_test_c_code_la_SOURCES = guile-test-c-code.c guile-test-c-code.h
libg_wrap_guile_test_c_code_la_LDFLAGS = -rpath $(libdir)
libg_wrap_guile_test_c_code_la_LIBADD = $(std_libs)

# ======================================================================
# gw-test-standard

noinst_LTLIBRARIES += libgw-guile-test-standard.la
nodist_libgw_guile_test_standard_la_SOURCES = gw-test-standard.c
libgw_guile_test_standard_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_standard_la_LIBADD = libg-wrap-guile-test-c-code.la $(std_libs)

# ======================================================================
# gw-test-enumeration

noinst_LTLIBRARIES += libgw-guile-test-enumeration.la
nodist_libgw_guile_test_enumeration_la_SOURCES = gw-test-enumeration.c
libgw_guile_test_enumeration_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_enumeration_la_LIBADD = $(std_libs)

# # ======================================================================
# # gw-test-parent

noinst_LTLIBRARIES += libgw-guile-test-parent.la
nodist_libgw_guile_test_parent_la_SOURCES = gw-test-parent.c
libgw_guile_test_parent_la_LDFLAGS = -rpath @libdir@ -module 
libgw_guile_test_parent_la_LIBADD = $(std_libs)

# # ======================================================================
# # gw-test-child

noinst_LTLIBRARIES += libgw-guile-test-child.la
nodist_libgw_guile_test_child_la_SOURCES = gw-test-child.c
libgw_guile_test_child_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_child_la_LIBADD = $(std_libs)


# # ======================================================================
# # Additional rules

gw-test-%.c gw-test-%.scm: guile-test-%-spec.scm $(top_srcdir)/test/test-%-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
	  (use-modules (g-wrap)) \
	  (use-modules (guile test guile-test-$*-spec)) \
	  (generate-wrapset 'guile 'test-$* \"gw-test-$*\")"

.PHONY: ${TESTS}
