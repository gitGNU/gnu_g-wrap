#!/bin/sh
# Run this to generate all the initial makefiles, etc.

DIE=0
package=g-wrap
srcfile=g-wrap/core-runtime.c

. ./autogen-support.sh

CONFIGURE_DEF_OPT='--enable-maintainer-mode'
GW_ACLOCAL_FLAGS="-I m4 $ACLOCAL_FLAGS"

autogen_options $@

echo -n "+ check for build tools"
if test ! -z "$NOCHECK"; then echo ": skipped version checks"; else  echo; fi
version_check "autoconf" "$AUTOCONF autoconf autoconf-2.59" \
              "ftp://ftp.gnu.org/pub/gnu/autoconf/" 2 59 || DIE=1
version_check "automake" "$AUTOMAKE automake automake-1.10 automake-1.9 automake-1.8 automake-1.7 automake17 automake-1.6" \
              "ftp://ftp.gnu.org/pub/gnu/automake/" 1 5 || DIE=1
version_check "libtoolize" "libtoolize libtoolize14" \
              "ftp://ftp.gnu.org/pub/gnu/libtool/" 1 4 0 || DIE=1

die_check $DIE

aclocal_check || DIE=1
autoheader_check || DIE=1

die_check $DIE

# if no arguments specified then this will be printed
if test -z "$*"; then
  echo "+ checking for autogen.sh options"
  echo "  This autogen script will automatically run ./configure as:"
  echo "  ./configure $CONFIGURE_DEF_OPT"
  echo "  To pass any additional options, please specify them on the $0"
  echo "  command line."
fi

toplevel_check $srcfile

echo "+ creating m4/libchecks.m4"
( cd m4 && m4 < libchecks.m4-in > libchecks.m4 )

if test -f acinclude.m4; then rm acinclude.m4; fi

tool_run "$libtoolize" "--copy --force"
tool_run "$aclocal" "$GW_ACLOCAL_FLAGS"
tool_run "$autoheader"

tool_run "$autoconf"
debug "automake: $automake"
tool_run "$automake" "-a -c -Wno-portability"

test -n "$NOCONFIGURE" && {
  echo "skipping configure stage for package $package, as requested."
  echo "autogen.sh done."
  exit 0
}

echo "+ running configure ... "
test ! -z "$CONFIGURE_DEF_OPT" && echo "  ./configure default flags: $CONFIGURE_DEF_OPT"
test ! -z "$CONFIGURE_EXT_OPT" && echo "  ./configure external flags: $CONFIGURE_EXT_OPT"
echo

./configure $CONFIGURE_DEF_OPT $CONFIGURE_EXT_OPT || {
        echo "  configure failed"
        exit 1
}

echo "Now type 'make' to compile $package."
