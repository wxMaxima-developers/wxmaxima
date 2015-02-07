#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([wxMaxima], [14.12.0])
AC_CONFIG_SRCDIR([src/wxMaxima.cpp])
AM_INIT_AUTOMAKE

AC_CONFIG_HEADER(src/Setup.h)

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

AM_OPTIONS_WXCONFIG

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_CXX
AC_PROG_CXXCPP
AC_PROG_EGREP
AC_SUBST(EGREP)
AC_LANG_CPLUSPLUS

dnl check for host
case "$host" in
        *mingw*)
		AC_CHECK_TOOL(WINDRES, windres)
                win32=true
                ;;
        *cygwin*)
                win32=true
                ;;
        *)
                win32=false
esac


dnl optional support for glyphs drawing, printing and drag'n'drop

AC_ARG_ENABLE(printing,
[  --enable-printing       Enable printing support.],
     	[case "${enableval}" in
          yes) wxm_print=true ;;
          no)  wxm_print=false ;;
	  *) AC_MSG_ERROR(bad value ${enableval} for --enable-printing) ;;
     	 esac],
	[wxm_print=true])

dnl osx compile options


AC_ARG_WITH(macosx-sdk,
[  --with-macosx-sdk=PATH  use an OS X SDK at PATH],
   [MACOSX_SDK=$withval])

if test "x$MACOSX_SDK" != "x"; then 
    eval "CC=\"$CC -isysroot $MACOSX_SDK\""
    eval "CXX=\"$CXX -isysroot $MACOSX_SDK\""
    eval "LD=\"$LD -isysroot $MACOSX_SDK\""
fi

AC_ARG_WITH(macosx-version-min,
[  --with-macosx-version-min=VER   build binaries which require at least this OS X version],
	[MACOSX_VERSION_MIN=$withval])

if test "x$MACOSX_VERSION_MIN" != "x"; then
    eval "CC=\"$CC -mmacosx-version-min=$MACOSX_VERSION_MIN\""
    eval "CXX=\"$CXX -mmacosx-version-min=$MACOSX_VERSION_MIN\""
    eval "LD=\"$LD -mmacosx-version-min=$MACOSX_VERSION_MIN\""
fi

AC_ARG_WITH(macosx-arch,
[  --with-macosx-arch=ARCH build for the specified architecture],
[MACOSX_ARCH=$withval])

if test "x$MACOSX_ARCH" != "x"; then
    eval "CFLAGS=\"$CFLAGS -arch $MACOSX_ARCH\""
    eval "CXXFLAGS=\"$CXXFLAGS -arch $MACOSX_ARCH\""
    eval "CPPFLAGS=\"$CPPFLAGS -arch $MACOSX_ARCH\""
    eval "LDFLAGS=\"$LDFLAGS -arch $MACOSX_ARCH\""
    eval "OBJCFLAGS=\"$OBJFLAGS -arch $MACOSX_ARCH\""
    eval "OBJXXFLAGS=\"$OBJXXFLAGS -arch $MACOSX_ARCH\""
fi

AC_ARG_ENABLE(static-wx,
[  --enable-static-wx      Compile with static wx libraries.],
     	[case "${enableval}" in
          yes) static_wx=true ;;
          no)  static_wx=false ;;
	  *) AC_MSG_ERROR(bad value ${enableval} for --enable-static-wx) ;;
     	 esac],
	[static_wx=false])

AM_PATH_WXCONFIG([3.0.0], [wxWin=1], [wxWin=0], [std,xml,html,adv,aui,core,net,base])

if test "$wxWin" != 1; then
   AC_MSG_ERROR([
          wxWidgets must be installed on your system.

          Please check that wx-config is in path, the directory
          where wxWidgets libraries are installed (returned by
          'wx-config --libs' or 'wx-config --static --libs' command)
          is in LD_LIBRARY_PATH or equivalent variable and
          wxWidgets version is 3.0.0 or above.
          ])
fi

CPPFLAGS="$CPPFLAGS $WX_CPPFLAGS"
CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS_ONLY"
CFLAGS="$CFLAGS $WX_CFLAGS_ONLY"
LIBS="$LIBS $WX_LIBS"

AC_MSG_CHECKING([if we can compile a wxWidgets program])
AC_TRY_LINK([#include <wx/wx.h>],
            [wxString test=wxT("")],
            [AC_MSG_RESULT([yes])],
            [echo ""
             echo ""
             echo "*** The test program failed to compile or link. See the file config.log for the"
             echo "*** exact error that occured. This usually means that wxWidgets library was"
             echo "*** incorrectly installed."
             echo ""
             AC_MSG_ERROR([Failed to compile a test program])])


dnl we have to setup rc compiling under Windows
if test x"${win32}" = x"true" ; then
   RC_OBJ="Resources.o"
   wx_prefix=`$WX_CONFIG_NAME --prefix`
   WX_RC_PATH="${wx_prefix}/include"
else
   RC_OBJ=""
   WX_RC_PATH=""
fi

dnl translations
CATALOGS_TO_INSTALL="install-wxmaxima-catalogs"

if test x"${static_wx}" = x"true" ; then
    CATALOGS_TO_INSTALL="$CATALOGS_TO_INSTALL install-wxstd-catalogs"
fi

dnl Optionally build the windows CHM help files
dnl default to false as requires win32 and Microsoft HTML Help Workshop
AC_ARG_ENABLE(chm,
  [  --enable-chm                 Build Windows CHM help files],
  [case "${enableval}" in
       		yes) chm=true ;;
       		no)  chm=false ;;
		*) AC_MSG_ERROR(bad value ${enableval} for --enable-chm) ;;
   esac],
   [chm=false])
dnl hhc is the HTML Help Compiler for CHM documentation
hhc_default_name=hhc$EXEEXT
AC_ARG_WITH(hhc,
	[  --with-hhc=<prog>            Use HTML Help Compiler executable <prog> (default hhc)],
	[chm=true
	if test "$withval" = "yes"; then
		HHC="${hhc_default_name}"
	else
		HHC="$withval"
	fi],
	[HHC="${hhc_default_name}"])

# Check that hhc exists, using AC_CHECK_PROG
if test x$chm = xtrue; then
  if test -x "${HHC}"; then
    # HHC was a path to the executable, and it existed, which is
    # great! We still say something to the caller, since this is
    # probably less confusing.
    AC_MSG_CHECKING([for hhc])
    AC_MSG_RESULT([yes])
  else
    AC_CHECK_PROG(hhc_found, ${HHC}, yes)
    if test x"${hhc_found}" != x"yes"; then
      AC_MSG_ERROR([HTML Help Compiler executable ${HHC} not found])
    fi
  fi
fi
AM_CONDITIONAL(CHM, test x$chm = xtrue)
if test x"${chm}" = x"true" ; then
   AC_DEFINE([WXM_CHM], [1], ["Using chm help"])
else
   AC_DEFINE([WXM_CHM], [0], ["Not using chm help"])
fi


AC_SUBST(LDFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CATALOGS_TO_INSTALL)
AC_SUBST(WX_LIBS)
AC_SUBST(WX_RC_PATH)
AC_SUBST(RC_OBJ)
AC_SUBST(HHC)

dnl Doxygen creates html code documentation from the comments in the code
AC_CHECK_PROG(HAVE_DOXYGEN,doxygen,yes,no)
AC_SUBST(HAVE_DOXYGEN)
AM_CONDITIONAL(USE_DOXYGEN, test x$HAVE_DOXYGEN = xyes)

dnl GraphViz makes nice diagrams for the doxygen documentation, if installed.
AC_CHECK_PROG(HAVE_GRAPHVIZ,dot,yes,no)
AC_SUBST(HAVE_GRAPHVIZ)
AM_CONDITIONAL(USE_GRAPHVIZ, test x$HAVE_GRAPHVIZ = xyes)

dnl These directory names are needed in order to find wxMaxima's own html documentation.
AC_SUBST(DESTDIR)

AC_CONFIG_FILES([
        Makefile
        src/Makefile
        Doxygen/Makefile
        Doxygen/Doxyfile
        locales/Makefile
        data/Makefile
        info/Makefile
        test/Makefile
        wxmaxima.spec
        data/Info.plist
	data/wxMaxima.desktop
])

AC_OUTPUT
