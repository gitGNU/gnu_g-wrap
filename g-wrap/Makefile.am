
gwrapmoduledir = @GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

CLEANFILES =
noinst_PROGRAMS =
gwrapinclude_HEADERS = 

AM_CFLAGS = $(GLIB_CFLAGS)

EXTRA_DIST =

gwrapmodule_DATA= \
 enumeration.scm \
 dynamic-type.scm \
 simple-type.scm \
 gw-standard-spec.scm \
 gw-standard.scm \
 gw-wct-spec.scm \
 gw-wct.scm \
 \
 runtime.scm \
 \
 g-translate.scm \
 output-file.scm \
 sorting.scm

EXTRA_DIST += $(gwrapmodule_DATA)

lib_LTLIBRARIES = \
 libgwrap-runtime.la \
 libgw-standard.la \
 libgw-wct.la

GUILE_FLAGS =

################## libgwrap-runtime section

GW_RUNTIME_VINFO = \
  ${GW_RUNTIME_INTERFACE_MAJOR_VER}:${GW_RUNTIME_INTERFACE_REVISION}:${GW_RUNTIME_INTERFACE_AGE}
gwrapinclude_HEADERS += g-wrap-runtime.h g-wrap-compatibility.h g-wrap-wct.h
libgwrap_runtime_la_SOURCES = g-wrap-runtime.c g-wrap-wct.c g-wrap-compatibility.c
libgwrap_runtime_la_LDFLAGS = -rpath $(libdir) -version-info ${GW_RUNTIME_VINFO}
libgwrap_runtime_la_LIBADD = @GUILE_LIBS@ $(LIBFFI_LIBS) @ALLOCA@

################## (g-wrap gw-wct) section

nodist_libgw_wct_la_SOURCES = gw-wct.c
libgw_wct_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic
libgw_wct_la_LIBADD =  libgwrap-runtime.la @GUILE_LIBS@ 

gw-wct.scm gw-wct.c gw-wct.h gw-wct.html: gw-wct-spec.scm
	guile $(GUILE_FLAGS) -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-wct-spec)) \
                        (gw:generate-wrapset \"gw-wct\")"

CLEANFILES += gw-wct.scm gw-wct.h gw-wct.c gw-wct.html

noinst_PROGRAMS += test-link-gw-wct
test_link_gw_wct_SOURCES = dummy-main.c
test_link_gw_wct_LDADD = ./libgw-wct.la

################## From ../g-wrapped 

LOCALPWD=$(shell pwd)
SETGWPPATH= "(set! %load-path (cons \"$(top_builddir)\" (cons \"$(top_srcdir)\" %load-path)))"

################## (g-wrap gw-standard) section

nodist_libgw_standard_la_SOURCES = gw-standard.c
libgw_standard_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic @GUILE_LIBS@
libgw_standard_la_LIBADD = libgwrap-runtime.la
LIBGW_STANDARD_GWGEN = $(addprefix gw-standard, .h .c .scm .html) 

${LIBGW_STANDARD_GWGEN}: gw-standard-spec.scm
	guile $(GUILE_FLAGS) -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-standard-spec)) \
                        (gw:generate-wrapset \"gw-standard\")"

CLEANFILES += ${LIBGW_STANDARD_GWGEN}
