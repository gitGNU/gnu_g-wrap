include $(top_srcdir)/common.mk

GUILE_FLAGS =
CLEANFILES = packages.scm standard.scm

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/guile
AM_CFLAGS = $(LIBFFI_CFLAGS) $(GUILE_CFLAGS)

all-local: $(top_srcdir)/guile/g-wrap/guile/ws/standard.scm
	guile $(GUILE_FLAGS) -c \
          $(SETGWPATH)"(use-modules (g-wrap)) \
	               (use-modules (g-wrap scheme48)) \
                       (use-modules (g-wrap scheme48 ws standard)) \
                       (generate-packages \"packages.scm\" 'standard)"
