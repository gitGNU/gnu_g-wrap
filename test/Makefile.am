# Initializations.

CLEANFILES =
noinst_HEADERS =
noinst_LTLIBRARIES =
EXTRA_DIST = 

check_SCRIPTS = \
 guile-test-env \
 test-gw-standard \
 test-gw-wct \
 test-enumeration

TESTS = \
 test-gw-standard \
 test-gw-wct \
 test-enumeration

EXTRA_DIST += ${check_SCRIPTS}

GUILE_FLAGS = 

LOCALPWD=$(shell pwd)
SETGWPATH = "(set! %load-path (cons \"$(top_builddir)\" (cons \"$(top_srcdir)\" %load-path)))"

EXTRA_DIST += gw-guile
EXTRA_DIST += test-gw-wct test-enumeration
EXTRA_DIST += test-gw-standard guile-test-env
EXTRA_DIST += $(wildcard $(srcdir)/*-spec.scm)

# ===========================================================================

noinst_PROGRAMS = test-link-wct
test_link_wct_SOURCES = dummy-main.c
test_link_wct_LDADD = ../g-wrap/libgw-wct.la

# ===========================================================================

AM_CFLAGS = -Wall -Wno-uninitialized -Werror -I${top_srcdir}/g-wrap

# ===========================================================================

noinst_LTLIBRARIES += libg-wrap-test-c-code.la
libg_wrap_test_c_code_la_SOURCES = g-wrap-test-c-code.c g-wrap-test-c-code.h
libg_wrap_test_c_code_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic \
  ${GUILE_LIBS}
libg_wrap_test_c_code_la_LIBADD = 

# ======================================================================
# gw-test-standard

noinst_LTLIBRARIES += libgw-test-standard.la
nodist_libgw_test_standard_la_SOURCES = gw-test-standard.c
libgw_test_standard_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_test_standard_la_LIBADD = ./libg-wrap-test-c-code.la \
			        ../g-wrap/libgw-standard.la ${GUILE_LIBS}

gw-test-standard.scm gw-test-standard.h gw-test-standard.c gw-test-standard.html gw-test-standard-autogen.h: gw-test-standard-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..: \
	guile $(GUILE_FLAGS) -c \
	  ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (primitive-load \"$(srcdir)/gw-test-standard-spec.scm\") \
           (gw:generate-wrapset \"gw-test-standard\")"
CLEANFILES += gw-test-standard.scm gw-test-standard.h gw-test-standard.c gw-test-standard.html gw-test-standard-autogen.h

# ======================================================================
# gw-test-enumeration

noinst_LTLIBRARIES += libgw-test-enumeration.la
nodist_libgw_test_enumeration_la_SOURCES = gw-test-enumeration.c
libgw_test_enumeration_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_test_enumeration_la_LIBADD = $(top_builddir)/g-wrap/libgw-standard.la \
				   ./libg-wrap-test-c-code.la ${GUILE_LIBS}

gw-test-enumeration.scm gw-test-enumeration.h gw-test-enumeration.c gw-test-enumeration.html gw-test-enumeration-autogen.h: gw-test-enumeration-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (primitive-load \"$(srcdir)/gw-test-enumeration-spec.scm\") \
           (gw:generate-wrapset \"gw-test-enumeration\")"
CLEANFILES += gw-test-enumeration.scm gw-test-enumeration.h gw-test-enumeration.c gw-test-enumeration.html gw-test-enumeration-autogen.h

# ======================================================================
# gw-test-parent

noinst_LTLIBRARIES += libgw-test-parent.la
nodist_libgw_test_parent_la_SOURCES = gw-test-parent.c
libgw_test_parent_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_test_parent_la_LIBADD = ./libg-wrap-test-c-code.la ../g-wrap/libgw-wct.la

#           (read-enable 'positions) \
#

gw-test-parent.scm gw-test-parent.h gw-test-parent.c gw-test-parent.html gw-test-parent-autogen.h: gw-test-parent-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c \
	  ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (primitive-load \"$(srcdir)/gw-test-parent-spec.scm\") \
           (gw:generate-wrapset \"gw-test-parent\")"
CLEANFILES += gw-test-parent.scm gw-test-parent.h gw-test-parent.c gw-test-parent.html gw-test-parent-autogen.h

# ======================================================================
# gw-test-child

noinst_LTLIBRARIES += libgw-test-child.la
nodist_libgw_test_child_la_SOURCES = gw-test-child.c
libgw_test_child_la_LDFLAGS = -rpath @libdir@ -module -export-dynamic
libgw_test_child_la_LIBADD = ./libg-wrap-test-c-code.la \
			     ../g-wrap/libgw-standard.la

gw-test-child.scm gw-test-child.h gw-test-child.c gw-test-child.html gw-test-child-autogen.h: gw-test-child-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LOCALPWD}/..: \
	guile $(GUILE_FLAGS) -c \
          ${SETGWPATH}" \
           (use-modules (g-wrap)) \
           (primitive-load \"$(srcdir)/gw-test-parent-spec.scm\") \
           (primitive-load \"$(srcdir)/gw-test-child-spec.scm\") \
           (gw:generate-wrapset \"gw-test-child\")"
CLEANFILES += gw-test-child.scm gw-test-child.h gw-test-child.c gw-test-child.html gw-test-child-autogen.h

.PHONY: ${TESTS}
