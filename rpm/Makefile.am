
# Initializations.
CLEANFILES =

GW_MAJ=@GW_RUNTIME_GUILE_INTERFACE_MAJOR_VER@
GW_REV=@GW_RUNTIME_GUILE_INTERFACE_REVISION@
GW_AGE=@GW_RUNTIME_GUILE_INTERFACE_AGE@

EXTRA_DIST = spec.in
EXTRA_DIST += .cvsignore

all-local: spec

## We borrow guile's convention and use @-...-@ as the substitution
## brackets here, instead of the usual @...@.  This prevents autoconf
## from substituting the values directly into the left-hand sides of
## the sed substitutions.  *sigh*
spec: spec.in Makefile
	rm -f $@.tmp
	sed < $@.in > $@.tmp \
            -e 's:@-GW_RTLIB_VER-@:${GW_MAJ}.${GW_REV}.${GW_AGE}:' \
            -e 's:@-GW_RTLIB_MAJ-@:${GW_MAJ}:' \
            -e 's:@-VERSION-@:${VERSION}:'
	mv $@.tmp $@

CLEANFILES += spec
