
gwrapmoduledir=@GUILEMODDIR@/g-wrap
gwrapincludedir = $(includedir)/g-wrap

CLEANFILES =
noinst_PROGRAMS =
gwrapinclude_HEADERS = g-wrap-compatibility.h

EXTRA_DIST =

if GW_HAVE_GLIB
GW_GLIB_LIB_ADD = libgwrap-glib.la libgw-glib.la
GW_GLIB_DATA_ADD = gw-glib-spec.scm gw-glib.scm
GW_GLIB_CFLAGS_ADD =  $(shell ${GW_GLIB_CONFIG} --cflags glib \
                                  | sed -f ${top_srcdir}/kill-usr-include)
else
GW_GLIB_EXTRAS = gw-glib-spec.scm glib.h glib.c
endif

if GW_HAVE_GUILE_GTK
GW_GUILE_GTK_LIB_ADD = libgw-gtk.la
GW_GUILE_GTK_DATA_ADD = gw-gtk-spec.scm gw-gtk.scm gtkobj.scm
GW_GUILE_GTK_CFLAGS_ADD = $(shell ${GW_BUILD_GUILE_GTK} cflags \
                                  | sed -f ${top_srcdir}/kill-usr-include)
else
GW_GUILE_GTK_EXTRAS = gw-gtk-spec.scm gtkobj.scm
endif

gwrapmodule_DATA= \
 enumeration.scm \
 simple-type.scm \
 \
 ${GW_GLIB_DATA_ADD} \
 ${GW_GUILE_GTK_DATA_ADD} \
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

EXTRA_DIST += $(gwrapmodule_DATA) .cvsignore
EXTRA_DIST += ${GW_GLIB_EXTRAS} ${GW_GUILE_GTK_EXTRAS}

lib_LTLIBRARIES = \
 libgw-standard.la \
 libgwrap-wct.la \
 libgw-wct.la \
 ${GW_GLIB_LIB_ADD} \
 ${GW_GUILE_GTK_LIB_ADD}

#AM_CFLAGS = -O2 -g -Wall -Wno-uninitialized -Werror

AM_CFLAGS = -O2 -g -Wall -Werror \
 ${GW_GLIB_CFLAGS_ADD} \
 ${GW_GUILE_GTK_CFLAGS_ADD}

################## libgwrap-wct section

GW_LIBWCT_VINFO = \
  ${GW_WCT_INTERFACE_MAJOR_VER}:${GW_WCT_INTERFACE_REVISION}:${GW_WCT_INTERFACE_AGE}
gwrapinclude_HEADERS += g-wrap-wct.h
libgwrap_wct_la_SOURCES = g-wrap-wct.c
libgwrap_wct_la_LDFLAGS = -rpath $(libdir) -version-info ${GW_LIBWCT_VINFO}
libgwrap_wct_la_LIBADD = @GUILE_LIBS@

################## (g-wrap gw-wct) section

nodist_libgw_wct_la_SOURCES = gw-wct.c
libgw_wct_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic @GUILE_LIBS@
libgw_wct_la_LIBADD = libgwrap-wct.la

gw-wct.scm gw-wct.c gw-wct.h gw-wct.html: gw-wct-spec.scm
	guile -c \
          ${SETGWPPATH}"(debug-enable 'backtrace) \
                        (debug-enable 'debug) \
                        (read-enable 'positions) \
                        (use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-wct-spec)) \
                        (gw:generate-wrapset \"gw-wct\")"

CLEANFILES+=gw-wct.scm
CLEANFILES+=gw-wct.h
CLEANFILES+=gw-wct.c
CLEANFILES+=gw-wct.html

noinst_PROGRAMS += test-link-gw-wct
test_link_gw_wct_SOURCES = dummy-main.c
test_link_gw_wct_LDADD = ./libgw-wct.la

################## From ../g-wrapped 

LOCALPWD=$(shell pwd)
SETGWPPATH= "(set! %load-path (cons \"${LOCALPWD}/..\" %load-path))"

################## (g-wrap gw-standard) section

nodist_libgw_standard_la_SOURCES = gw-standard.c
libgw_standard_la_LDFLAGS = -rpath $(libdir) -module -export-dynamic @GUILE_LIBS@
libgw_standard_la_LIBADD = 
LIBGW_STANDARD_GWGEN = $(addprefix gw-standard, .h .c .scm .html) 

${LIBGW_STANDARD_GWGEN}: gw-standard-spec.scm
	guile -c \
          ${SETGWPPATH}"(debug-enable 'backtrace) \
                        (debug-enable 'debug) \
                        (read-enable 'positions) \
                        (use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-standard-spec)) \
                        (gw:generate-wrapset \"gw-standard\")"

CLEANFILES += ${LIBGW_STANDARD_GWGEN}

################## libgwrap-glib section

LGWRAP_GLIB_VINFO = \
  ${LGWRAP_GLIB_MAJOR_VER}:${LGWRAP_GLIB_REVISION}:${LGWRAP_GLIB_AGE}
gwrapinclude_HEADERS += g-wrap-glib.h
libgwrap_glib_la_SOURCES = g-wrap-glib.c
libgwrap_glib_la_LDFLAGS = -rpath $(libdir) -version-info ${LGWRAP_GLIB_VINFO}
libgwrap_glib_la_LIBADD = @GUILE_LIBS@

################## (g-wrap gw-glib) section

if GW_HAVE_GLIB

nodist_libgw_glib_la_SOURCES = gw-glib.c
libgw_glib_la_LDFLAGS = -module -export-dynamic \
  $(shell ${GW_GLIB_CONFIG} --libs glib)
libgw_glib_la_LIBADD = @GUILE_LIBS@

endif

gw-glib.scm gw-glib.c gw-glib.h gw-glib.html: gw-glib-spec.scm
	guile -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-glib-spec)) \
                        (gw:generate-wrapset \"gw-glib\")"

CLEANFILES+=gw-glib.scm
CLEANFILES+=gw-glib.h
CLEANFILES+=gw-glib.c
CLEANFILES+=gw-glib.html

################## (g-wrap gw-gtk) section

if GW_HAVE_GUILE_GTK

nodist_libgw_gtk_la_SOURCES = gw-gtk.c
libgw_gtk_la_LDFLAGS = -module -export-dynamic \
  $(shell ${GW_BUILD_GUILE_GTK} libs)
libgw_gtk_la_LIBADD = 

endif

gw-gtk.scm gw-gtk.c gw-gtk.h gw-gtk.html: gw-gtk-spec.scm
	guile -c \
          ${SETGWPPATH}"(use-modules (g-wrap)) \
                        (use-modules (g-wrap gw-gtk-spec)) \
                        (gw:generate-wrapset \"gw-gtk\")"

CLEANFILES+=gw-gtk.scm
CLEANFILES+=gw-gtk.h
CLEANFILES+=gw-gtk.c
CLEANFILES+=gw-gtk.html
