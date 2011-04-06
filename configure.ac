
# check for one particular file of the sources 
AC_INIT(ocaml-gavl, 0.1.4, savonet-users@lists.sourceforge.net)

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
	CFLAGS="$CFLAGS -I$prefix/include"
	LDFLAGS="$LDFLAGS -L$prefix/lib"
	CPPFLAGS="$CPPFLAGS -I$prefix/include"
fi

AC_CHECK_OCAML_COMPILERS()

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I`pwd`/`dirname $0`"

AC_CHECK_FUNCS([memalign])

AC_PROG_CC()
AC_CHECK_TOOL([AR],[ar],no)
AC_SUBST(AR)
PKG_PROG_PKG_CONFIG()
#PKG_CHECK_MODULES(gavl, gavl, , [AC_MSG_ERROR(gavl not found.)])

# PKG_CHECK_MODULES loses when you need --libs-only-[lL]
AC_MSG_CHECKING([for gavl >= 1.0.1])
if ! $PKG_CONFIG --exists gavl; then
        AC_MSG_ERROR([gavl not found])
else
  if ! $PKG_CONFIG --atleast-version=1.0.1 gavl; then
        AC_MSG_ERROR([gavl version should be at least 1.0.1])
  else
        AC_MSG_RESULT([ok])
  fi
fi
gavl_CFLAGS=`$PKG_CONFIG --cflags gavl`
AC_SUBST([gavl_CFLAGS])
gavl_LIBS=`$PKG_CONFIG --libs-only-l gavl`
AC_SUBST([gavl_LIBS])
gavl_LDFLAGS=`$PKG_CONFIG --libs-only-L gavl`
AC_SUBST([gavl_LDFLAGS])

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)
AC_SUBST(requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_OUTPUT
