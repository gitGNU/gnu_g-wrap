include $(top_srcdir)/common.mk

WRAPSETS = test-standard

all-local:
	guile $(GUILE_FLAGS) -c \
	  $(SETGWPATH)" \
	  (use-modules (g-wrap) (g-wrap scheme48)) \
	  $(foreach ws,$(WRAPSETS),(use-modules (scheme48 test s48-$(ws)-spec))) \
	  (apply generate-packages \"packages.scm\" '($(WRAPSETS)))"

.PHONY: $(TESTS)
