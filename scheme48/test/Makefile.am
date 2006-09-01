include $(top_srcdir)/common.mk

WRAPSETS = test-standard

dist_noinst_DATA = s48-test-standard-spec.scm

clean-local:
	rm -f packages.scm test-standard.scm

all-local:
	guile $(GUILE_FLAGS) -c \
	  $(SETGWPATH)" \
	  (use-modules (g-wrap) (g-wrap scheme48)) \
	  $(foreach ws,$(WRAPSETS),(use-modules (scheme48 test s48-$(ws)-spec))) \
	  (apply generate-packages \"packages.scm\" '($(WRAPSETS)))"

.PHONY: $(TESTS)
