# Initializations.

CLEANFILES =
EXTRA_DIST = 

EXTRA_DIST += $(wildcard $(srcdir)/*-spec.scm)

AM_CPPFLAGS = -I$(top_srcdir)

noinst_LTLIBRARIES = libg-wrap-test-c-code.la
libg_wrap_test_c_code_la_SOURCES = g-wrap-test-c-code.c g-wrap-test-c-code.h
libg_wrap_test_c_code_la_LDFLAGS = -rpath $(libdir)
libg_wrap_test_c_code_la_LIBADD = 
