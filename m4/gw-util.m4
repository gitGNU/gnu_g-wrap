AC_DEFUN([GW_RUNTIME_VINFO],
[
GW_$1_RUNTIME_INTERFACE_MAJOR_VER=$2
GW_$1_RUNTIME_INTERFACE_REVISION=$3
GW_$1_RUNTIME_INTERFACE_AGE=$4

GW_$1_RUNTIME_VINFO="${GW_$1_RUNTIME_INTERFACE_MAJOR_VER}:${GW_$1_RUNTIME_INTERFACE_REVISION}:${GW_$1_RUNTIME_INTERFACE_AGE}"

AC_DEFINE_UNQUOTED(GW_$1_RUNTIME_INTERFACE_MAJOR_VER,
                   ${GW_$1_RUNTIME_INTERFACE_MAJOR_VER},
                   [The major number for the $1 runtime shared library ABI.])

AC_DEFINE_UNQUOTED(GW_$1_RUNTIME_INTERFACE_REVISION,
                   ${GW_$1_RUNTIME_INTERFACE_REVISION},
                   [The revision number for $1 runtime shared library ABI.])

AC_DEFINE_UNQUOTED(GW_$1_RUNTIME_INTERFACE_AGE,
                   ${GW_$1_RUNTIME_INTERFACE_AGE},
  [Number of previous interfaces supported by the $1 runtime shared library ABI.])

AC_SUBST(GW_$1_RUNTIME_VINFO)

])
