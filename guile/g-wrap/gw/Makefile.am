include $(top_srcdir)/common.mk

gwmoduledir = $(gwrapmoduledir)/gw
gwmodule_DATA = standard.scm 

GUILE_FLAGS =
CLEANFILES =
lib_LTLIBRARIES = 

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile
AM_CFLAGS = $(LIBFFI_CFLAGS) $(GUILE_CFLAGS)

# libgw-guile-standard
#---------------------
lib_LTLIBRARIES += libgw-guile-standard.la
nodist_libgw_guile_standard_la_SOURCES = standard.c
libgw_guile_standard_la_LDFLAGS = -module $(GUILE_LDFLAGS)
libgw_guile_standard_la_LIBADD = ../libgwrap-guile-runtime.la

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
lib_LTLIBRARIES += libgw-guile-gw-glib.la
nodist_libgw_guile_gw_glib_la_SOURCES = glib.c
libgw_guile_gw_glib_la_LDFLAGS = -module $(GUILE_LDFLAGS)
libgw_guile_gw_glib_la_LIBADD = ../libgwrap-guile-runtime.la $(GLIB_LIBS)
libgw_guile_gw_glib_la_CFLAGS = $(AM_CFLAGS) $(GLIB_CFLAGS)

LIBGW_GW_GLIB_GWGEN = $(addprefix glib, .h .c .scm) 

${LIBGW_GW_GLIB_GWGEN}: $(top_srcdir)/guile/g-wrap/gw-glib-spec.scm
	guile $(GUILE_FLAGS) -c \
          $(SETGWPATH)"(use-modules (g-wrap)) \
	               (use-modules (g-wrap guile)) \
                       (use-modules (g-wrap gw-glib-spec)) \
                       (generate-wrapset 'guile 'gw-glib \"glib\")"

gwmodule_DATA += glib.scm
CLEANFILES += $(LIBGW_GW_GLIB_GWGEN)

endif # HAVE_GLIB
