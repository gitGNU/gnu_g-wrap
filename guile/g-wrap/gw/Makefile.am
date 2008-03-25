include $(top_srcdir)/common.mk

gwmoduledir = $(gwrapmoduledir)/gw
gwmodule_DATA = standard.scm 
gwrapmodule_DATA =

GUILE_FLAGS =
CLEANFILES =
gwrapshlib_LTLIBRARIES = 

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile
AM_CFLAGS = $(LIBFFI_CFLAGS) $(GUILE_CFLAGS)

# libgw-guile-standard
#---------------------
gwrapshlib_LTLIBRARIES += libgw-guile-standard.la
nodist_libgw_guile_standard_la_SOURCES = standard.c
libgw_guile_standard_la_LDFLAGS = -module $(GUILE_LDFLAGS) -no-undefined
libgw_guile_standard_la_LIBADD = ../libgwrap-guile-runtime.la \
				 ../../../g-wrap/libgwrap-core-runtime.la \
				 $(LIBFFI_LIBS)

# Type `long long' is only defined in ISO C99 (or GNU C).  Therefore,
# glibc's <limits.h> will only define the `LLONG_' macros if we are
# compiling to C99.
libgw_guile_standard_la_CFLAGS = -std=c99 $(AM_CFLAGS)

LIBGW_STANDARD_GWGEN = $(addprefix standard, .h .c .scm) 

${LIBGW_STANDARD_GWGEN}: $(top_srcdir)/guile/g-wrap/guile/ws/standard.scm
	guile $(GUILE_FLAGS) -c \
          $(SETGWPATH)"(use-modules (g-wrap)) \
	               (use-modules (g-wrap guile)) \
                       (use-modules (g-wrap guile ws standard)) \
                       (generate-wrapset 'guile 'standard \"standard\")"

CLEANFILES += $(LIBGW_STANDARD_GWGEN)

if HAVE_GLIB

# libgw-guile-gw-glib
#---------------------
gwrapshlib_LTLIBRARIES += libgw-guile-gw-glib.la
nodist_libgw_guile_gw_glib_la_SOURCES = gw-glib.c
libgw_guile_gw_glib_la_LDFLAGS = -module $(GUILE_LDFLAGS) -no-undefined
libgw_guile_gw_glib_la_LIBADD = ../libgwrap-guile-runtime.la \
				../../../g-wrap/libgwrap-core-runtime.la \
				$(GLIB_LIBS)
libgw_guile_gw_glib_la_CFLAGS = $(AM_CFLAGS) $(GLIB_CFLAGS)

LIBGW_GW_GLIB_GWGEN = $(addprefix gw-glib, .h .c .scm) 

${LIBGW_GW_GLIB_GWGEN}: $(top_srcdir)/guile/g-wrap/gw-glib-spec.scm
	guile $(GUILE_FLAGS) -c \
          $(SETGWPATH)"(use-modules (g-wrap)) \
	               (use-modules (g-wrap guile)) \
                       (use-modules (g-wrap gw-glib-spec)) \
                       (generate-wrapset 'guile 'gw-glib \"gw-glib\")"

gwrapmodule_DATA += gw-glib.scm
CLEANFILES += $(LIBGW_GW_GLIB_GWGEN)

endif # HAVE_GLIB
