moduledir = @GUILEMODDIR@/g-wrap/guile
module_DATA = gw-standard-spec.scm

GUILE_FLAGS =
lib_LTLIBRARIES = libgw-guile-standard.la

AM_CPPFLAGS = -I$(top_srcdir)

################## (g-wrap guile gw-standard) section

nodist_libgw_guile_standard_la_SOURCES = gw-standard.c
libgw_guile_standard_la_LDFLAGS = -rpath $(libdir) -module $(GUILE_LDFLAGS)
libgw_guile_standard_la_LIBADD = ../libgwrap-guile-runtime.la

LIBGW_STANDARD_GWGEN = $(addprefix gw-standard, .h .c .scm .html) 

SETGWPPATH= "(set! %load-path (cons \"$(top_builddir)\" (cons \"$(top_srcdir)\" %load-path)))"

${LIBGW_STANDARD_GWGEN}: gw-standard-spec.scm
	guile $(GUILE_FLAGS) -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
	                (use-modules (g-wrap guile)) \
                        (use-modules (g-wrap guile gw-standard-spec)) \
                        (generate-wrapset guile wrapset-gw-standard \"gw-standard\")"

CLEANFILES = ${LIBGW_STANDARD_GWGEN}

EXTRA_DIST = $(module_DATA)

