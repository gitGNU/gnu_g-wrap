include $(top_srcdir)/common.mk

lib_LTLIBRARIES = libmiscutils.la
libmiscutils_la_SOURCES = miscutils.c miscutils.h

lib_LTLIBRARIES += libgw-guile-miscutils.la
libgw_guile_miscutils_la_SOURCES = miscutils-guile.c miscutils-guile.h gw-miscutils.c
libgw_guile_miscutils_la_LDFLAGS = -module
libgw_guile_miscutils_la_LIBADD = libmiscutils.la \
				  $(top_builddir)/guile/g-wrap/libgwrap-guile-runtime.la \
	                          $(top_builddir)/g-wrap/libgwrap-core-runtime.la

EXTRA_DIST= README update-format.scm

# Wrapset generation
gw-%.c gw-%.scm: %-spec.scm
	LD_LIBRARY_PATH=${LOCALPWD}/..:${LD_LIBRARY_PATH} \
	guile $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
	  (use-modules (g-wrap)) \
	  (use-modules (examples $*-spec)) \
	  (generate-wrapset 'guile '$* \"gw-$*\")"
