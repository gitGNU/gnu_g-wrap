
# Initializations
bin_SCRIPTS =
CLEANFILES =
EXTRA_DIST =

bin_SCRIPTS += g-wrap-config
CLEANFILES += g-wrap-config
EXTRA_DIST += g-wrap-config.in g-wrap.in

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

# Depend on Makefile so that we pick up changes to ../configure
g-wrap: g-wrap.in Makefile
	rm -f $@.tmp
	sed < $< > $@.tmp \
            -e 's:@-GUILE-@:${GUILE}:' \
            -e 's:@-VERSION-@:${VERSION}:' \
	    -e 's:@-GUILE_MODULE_DIR-@:${GUILEMODDIR}:'
	chmod +x $@.tmp
	mv $@.tmp $@

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*

g-wrap-config: g-wrap-config.in
	rm -f $@.tmp
	sed < $< > $@.tmp \
	    -e 's:@-GUILE-@:${GUILE}:' \
	    -e 's:@-VERSION-@:${VERSION}:' \
	    -e 's:@-libdir-@:${libdir}:' \
	    -e 's:@-includedir-@:${includedir}:' \
	    -e 's:@-GUILE_MODULE_DIR-@:${GUILEMODDIR}:'
	chmod +x $@.tmp
	mv $@.tmp $@
