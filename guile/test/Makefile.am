include $(top_srcdir)/common.mk

# Initializations.

CLEANFILES = $(wildcard gw-test-*.c gw-test-*.h gw-test-*.scm)
noinst_HEADERS =
check_LTLIBRARIES =
EXTRA_DIST = unit-test.scm

TESTS = \
 test-standard \
 test-enumeration \
 test-wct \
 test-compat
TESTS_ENVIRONMENT = builddir=$(top_builddir)/guile/test

check_SCRIPTS = $(TESTS)

EXTRA_DIST += ${check_SCRIPTS}

GUILE_FLAGS = 

LOCALPWD = $(shell pwd)

EXTRA_DIST += $(wildcard $(srcdir)/*-spec.scm)

std_libs = $(top_builddir)/test/libg-wrap-test-c-code.la \
	   ../g-wrap/libgwrap-guile-runtime.la \
	   $(top_builddir)/g-wrap/libgwrap-core-runtime.la \
	   $(GUILE_LDFLAGS)

# ===========================================================================

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile
AM_CFLAGS = $(GUILE_CFLAGS) $(LIBFFI_CFLAGS)

check_LTLIBRARIES += libg-wrap-guile-test-c-code.la
libg_wrap_guile_test_c_code_la_SOURCES = guile-test-c-code.c guile-test-c-code.h
libg_wrap_guile_test_c_code_la_LDFLAGS = -rpath $(libdir)
libg_wrap_guile_test_c_code_la_LIBADD = $(std_libs)

# ======================================================================
# gw-test-standard

check_LTLIBRARIES += libgw-guile-test-standard.la
nodist_libgw_guile_test_standard_la_SOURCES = gw-test-standard.c
libgw_guile_test_standard_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_standard_la_LIBADD = libg-wrap-guile-test-c-code.la $(std_libs)

# ======================================================================
# gw-test-enumeration

check_LTLIBRARIES += libgw-guile-test-enumeration.la
nodist_libgw_guile_test_enumeration_la_SOURCES = gw-test-enumeration.c
libgw_guile_test_enumeration_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_enumeration_la_LIBADD = $(std_libs)

# # ======================================================================
# # gw-test-parent

check_LTLIBRARIES += libgw-guile-test-parent.la
nodist_libgw_guile_test_parent_la_SOURCES = gw-test-parent.c
libgw_guile_test_parent_la_LDFLAGS = -rpath @libdir@ -module 
libgw_guile_test_parent_la_LIBADD = $(std_libs)

# # ======================================================================
# # gw-test-child

check_LTLIBRARIES += libgw-guile-test-child.la
nodist_libgw_guile_test_child_la_SOURCES = gw-test-child.c
libgw_guile_test_child_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_child_la_LIBADD = $(std_libs)


# # ======================================================================
# # gw-test-aggregating

check_LTLIBRARIES += libgw-guile-test-aggregating.la
nodist_libgw_guile_test_aggregating_la_SOURCES = gw-test-aggregating.c
libgw_guile_test_aggregating_la_LDFLAGS = -rpath @libdir@ -module
libgw_guile_test_aggregating_la_LIBADD = $(std_libs)


# # ======================================================================
# # gw-test-compat

check_LTLIBRARIES += libgw-test-compat.la
nodist_libgw_test_compat_la_SOURCES = gw-test-compat.c
libgw_test_compat_la_LDFLAGS = -rpath @libdir@ -module
libgw_test_compat_la_LIBADD = $(std_libs)

# # ======================================================================
# # Additional rules

gw-test-%.c gw-test-%.scm: guile-test-%-spec.scm $(top_srcdir)/test/test-%-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
	  (use-modules (g-wrap)) \
	  (use-modules (guile test guile-test-$*-spec)) \
	  (generate-wrapset 'guile 'test-$* \"gw-test-$*\")"

gw-test-compat.c gw-test-compat.scm: guile-test-compat-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
	  (use-modules (g-wrap compat)) \
	  (use-modules (guile test guile-test-compat-spec)) \
	  (gw:generate-wrapset \"gw-test-compat\")"

.PHONY: ${TESTS}
