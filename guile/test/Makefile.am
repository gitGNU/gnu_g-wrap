# Initializations.

CLEANFILES =
noinst_HEADERS =
noinst_LTLIBRARIES =
EXTRA_DIST = 

TESTS = \
 test-standard \
 test-enumeration \
 #test-wct 

check_SCRIPTS = guile-test-env $(TESTS)

EXTRA_DIST += ${check_SCRIPTS}

GUILE_FLAGS = 

LOCALPWD=$(shell pwd)
SETGWPATH = "(set! %load-path (cons \"$(top_builddir)/guile\" (cons \"$(top_srcdir)\" %load-path)))"

EXTRA_DIST += $(wildcard $(srcdir)/*-spec.scm)

std_libs = ./libg-wrap-test-c-code.la \
	   ../g-wrap/libgwrap-guile-runtime.la \
	   $(top_builddir)/g-wrap/libgwrap-core-runtime.la \
	   $(GUILE_LDFLAGS)

# ===========================================================================

AM_CFLAGS = -Wall -Wno-uninitialized -Werror 
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile

# ===========================================================================

noinst_LTLIBRARIES += libg-wrap-test-c-code.la
libg_wrap_test_c_code_la_SOURCES = g-wrap-test-c-code.c g-wrap-test-c-code.h
libg_wrap_test_c_code_la_LDFLAGS = -rpath $(libdir) ${GUILE_LIBS}
libg_wrap_test_c_code_la_LIBADD = 

# ======================================================================
# gw-test-standard

noinst_LTLIBRARIES += libgw-guile-test-standard.la
nodist_libgw_guile_test_standard_la_SOURCES = gw-test-standard.c
libgw_guile_test_standard_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_guile_test_standard_la_LIBADD = $(std_libs)

gw-test-standard.scm gw-test-standard.h gw-test-standard.c gw-test-standard.html gw-test-standard-autogen.h: test-standard-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/../..: \
	guile $(GUILE_FLAGS) -c \
	  ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (use-modules (g-wrap guile)) \
           (primitive-load \"$(srcdir)/test-standard-spec.scm\") \
           (generate-wrapset guile test-standard-wrapset \"gw-test-standard\")"
CLEANFILES += gw-test-standard.scm gw-test-standard.h gw-test-standard.c gw-test-standard.html

# ======================================================================
# gw-test-enumeration

noinst_LTLIBRARIES += libgw-guile-test-enumeration.la
nodist_libgw_guile_test_enumeration_la_SOURCES = gw-test-enumeration.c
libgw_guile_test_enumeration_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_guile_test_enumeration_la_LIBADD = $(std_libs)

gw-test-enumeration.scm gw-test-enumeration.h gw-test-enumeration.c gw-test-enumeration.html gw-test-enumeration-autogen.h: test-enumeration-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (primitive-load \"$(srcdir)/test-enumeration-spec.scm\") \
           (generate-wrapset guile test-enumeration-wrapset \"gw-test-enumeration\")"
CLEANFILES += gw-test-enumeration.scm gw-test-enumeration.h gw-test-enumeration.c gw-test-enumeration.html gw-test-enumeration-autogen.h

# # ======================================================================
# # gw-test-parent

# noinst_LTLIBRARIES += libgw-guile-test-parent.la
# nodist_libgw_guile_test_parent_la_SOURCES = gw-test-parent.c
# libgw_guile_test_parent_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
# libgw_guile_test_parent_la_LIBADD = $(std_libs)

# gw-test-parent.scm gw-test-parent.h gw-test-parent.c gw-test-parent.html gw-test-parent-autogen.h: gw-test-parent-spec.scm
# 	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..:${LD_LIBRARY_PATH} \
# 	guile $(GUILE_FLAGS) -c \
# 	  ${SETGWPATH}" \
#            (use-modules (g-wrap)) \
#            (use-modules (g-wrap guile)) \
#            (primitive-load \"$(srcdir)/gw-test-parent-spec.scm\") \
#            (gw:generate-wrapset \"gw-test-parent\")"
# CLEANFILES += gw-test-parent.scm gw-test-parent.h gw-test-parent.c gw-test-parent.html gw-test-parent-autogen.h

# # ======================================================================
# # gw-test-child

# noinst_LTLIBRARIES += libgw-guile-test-child.la
# nodist_libgw_guile_test_child_la_SOURCES = gw-test-child.c
# libgw_guile_test_child_la_LDFLAGS = -rpath @libdir@ -module -export-dynamic
# libgw_guile_test_child_la_LIBADD = $(std_libs)

# gw-test-child.scm gw-test-child.h gw-test-child.c gw-test-child.html gw-test-child-autogen.h: gw-test-child-spec.scm
# 	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..: \
# 	guile $(GUILE_FLAGS) -c \
#           ${SETGWPATH}" \
#            (use-modules (g-wrap)) \
#            (primitive-load \"$(srcdir)/gw-test-parent-spec.scm\") \
#            (primitive-load \"$(srcdir)/gw-test-child-spec.scm\") \
#            (gw:generate-wrapset \"gw-test-child\")"
# CLEANFILES += gw-test-child.scm gw-test-child.h gw-test-child.c gw-test-child.html gw-test-child-autogen.h

.PHONY: ${TESTS}
