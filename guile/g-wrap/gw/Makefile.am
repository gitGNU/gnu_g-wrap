gwrapmoduledir = @GUILEMODDIR@/g-wrap/gw

gwrapmodule_DATA = standard.scm

GUILE_FLAGS =

lib_LTLIBRARIES = libgw-guile-standard.la

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile

nodist_libgw_guile_standard_la_SOURCES = standard.c
libgw_guile_standard_la_LDFLAGS = -module $(GUILE_LDFLAGS)
libgw_guile_standard_la_LIBADD = ../libgwrap-guile-runtime.la

LIBGW_STANDARD_GWGEN = $(addprefix standard, .h .c .scm .html) 

SETGWPPATH= "(set! %load-path (cons \"$(top_builddir)/guile\" (cons \"$(top_srcdir)\" %load-path)))"

${LIBGW_STANDARD_GWGEN}: $(top_srcdir)/g-wrap/guile/ws/standard.scm
	guile $(GUILE_FLAGS) -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
	                (use-modules (g-wrap guile)) \
                        (use-modules (g-wrap guile ws standard)) \
                        (generate-wrapset 'guile 'standard \"standard\")"

CLEANFILES = ${LIBGW_STANDARD_GWGEN}

