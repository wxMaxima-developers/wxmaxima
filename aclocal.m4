# generated automatically by aclocal 1.9.2 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
# Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# Configure paths for LIBXML2
# Mike Hommey 2004-06-19
# use CPPFLAGS instead of CFLAGS
# Toshio Kuratomi 2001-04-21
# Adapted from:
# Configure paths for GLIB
# Owen Taylor     97-11-3

dnl AM_PATH_XML2([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for XML, and define XML_CPPFLAGS and XML_LIBS
dnl
AC_DEFUN([AM_PATH_XML2],[ 
AC_ARG_WITH(xml-prefix,
            [  --with-xml-prefix=PFX   Prefix where libxml is installed (optional)],
            xml_config_prefix="$withval", xml_config_prefix="")
AC_ARG_WITH(xml-exec-prefix,
            [  --with-xml-exec-prefix=PFX Exec prefix where libxml is installed (optional)],
            xml_config_exec_prefix="$withval", xml_config_exec_prefix="")
AC_ARG_ENABLE(xmltest,
              [  --disable-xmltest       Do not try to compile and run a test LIBXML program],,
              enable_xmltest=yes)

  if test x$xml_config_exec_prefix != x ; then
     xml_config_args="$xml_config_args"
     if test x${XML2_CONFIG+set} != xset ; then
        XML2_CONFIG=$xml_config_exec_prefix/bin/xml2-config
     fi
  fi
  if test x$xml_config_prefix != x ; then
     xml_config_args="$xml_config_args --prefix=$xml_config_prefix"
     if test x${XML2_CONFIG+set} != xset ; then
        XML2_CONFIG=$xml_config_prefix/bin/xml2-config
     fi
  fi

  AC_PATH_PROG(XML2_CONFIG, xml2-config, no)
  min_xml_version=ifelse([$1], ,2.0.0,[$1])
  AC_MSG_CHECKING(for libxml - version >= $min_xml_version)
  no_xml=""
  if test "$XML2_CONFIG" = "no" ; then
    no_xml=yes
  else
    XML_CPPFLAGS=`$XML2_CONFIG $xml_config_args --cflags`
    XML_LIBS=`$XML2_CONFIG $xml_config_args --libs`
    xml_config_major_version=`$XML2_CONFIG $xml_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    xml_config_minor_version=`$XML2_CONFIG $xml_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    xml_config_micro_version=`$XML2_CONFIG $xml_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_xmltest" = "xyes" ; then
      ac_save_CPPFLAGS="$CPPFLAGS"
      ac_save_LIBS="$LIBS"
      CPPFLAGS="$CPPFLAGS $XML_CPPFLAGS"
      LIBS="$XML_LIBS $LIBS"
dnl
dnl Now check if the installed libxml is sufficiently new.
dnl (Also sanity checks the results of xml2-config to some extent)
dnl
      rm -f conf.xmltest
      AC_TRY_RUN([
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libxml/xmlversion.h>

int 
main()
{
  int xml_major_version, xml_minor_version, xml_micro_version;
  int major, minor, micro;
  char *tmp_version;

  system("touch conf.xmltest");

  /* Capture xml2-config output via autoconf/configure variables */
  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = (char *)strdup("$min_xml_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string from xml2-config\n", "$min_xml_version");
     exit(1);
   }
   free(tmp_version);

   /* Capture the version information from the header files */
   tmp_version = (char *)strdup(LIBXML_DOTTED_VERSION);
   if (sscanf(tmp_version, "%d.%d.%d", &xml_major_version, &xml_minor_version, &xml_micro_version) != 3) {
     printf("%s, bad version string from libxml includes\n", "LIBXML_DOTTED_VERSION");
     exit(1);
   }
   free(tmp_version);

 /* Compare xml2-config output to the libxml headers */
  if ((xml_major_version != $xml_config_major_version) ||
      (xml_minor_version != $xml_config_minor_version) ||
      (xml_micro_version != $xml_config_micro_version))
    {
      printf("*** libxml header files (version %d.%d.%d) do not match\n",
         xml_major_version, xml_minor_version, xml_micro_version);
      printf("*** xml2-config (version %d.%d.%d)\n",
         $xml_config_major_version, $xml_config_minor_version, $xml_config_micro_version);
      return 1;
    } 
/* Compare the headers to the library to make sure we match */
  /* Less than ideal -- doesn't provide us with return value feedback, 
   * only exits if there's a serious mismatch between header and library.
   */
    LIBXML_TEST_VERSION;

    /* Test that the library is greater than our minimum version */
    if ((xml_major_version > major) ||
        ((xml_major_version == major) && (xml_minor_version > minor)) ||
        ((xml_major_version == major) && (xml_minor_version == minor) &&
        (xml_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of libxml (%d.%d.%d) was found.\n",
               xml_major_version, xml_minor_version, xml_micro_version);
        printf("*** You need a version of libxml newer than %d.%d.%d. The latest version of\n",
           major, minor, micro);
        printf("*** libxml is always available from ftp://ftp.xmlsoft.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the xml2-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of LIBXML, but you can also set the XML2_CONFIG environment to point to the\n");
        printf("*** correct copy of xml2-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
    }
  return 1;
}
],, no_xml=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CPPFLAGS="$ac_save_CPPFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi

  if test "x$no_xml" = x ; then
     AC_MSG_RESULT(yes (version $xml_config_major_version.$xml_config_minor_version.$xml_config_micro_version))
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$XML2_CONFIG" = "no" ; then
       echo "*** The xml2-config script installed by LIBXML could not be found"
       echo "*** If libxml was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the XML2_CONFIG environment variable to the"
       echo "*** full path to xml2-config."
     else
       if test -f conf.xmltest ; then
        :
       else
          echo "*** Could not run libxml test program, checking why..."
          CPPFLAGS="$CPPFLAGS $XML_CPPFLAGS"
          LIBS="$LIBS $XML_LIBS"
          AC_TRY_LINK([
#include <libxml/xmlversion.h>
#include <stdio.h>
],      [ LIBXML_TEST_VERSION; return 0;],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding LIBXML or finding the wrong"
          echo "*** version of LIBXML. If it is not finding LIBXML, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
          echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means LIBXML was incorrectly installed"
          echo "*** or that you have moved LIBXML since it was installed. In the latter case, you"
          echo "*** may want to edit the xml2-config script: $XML2_CONFIG" ])
          CPPFLAGS="$ac_save_CPPFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi

     XML_CPPFLAGS=""
     XML_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(XML_CPPFLAGS)
  AC_SUBST(XML_LIBS)
  rm -f conf.xmltest
])

dnl ---------------------------------------------------------------------------
dnl Macros for wxWindows detection. Typically used in configure.in as:
dnl
dnl     AC_ARG_ENABLE(...)
dnl     AC_ARG_WITH(...)
dnl        ...
dnl     AM_OPTIONS_WXCONFIG
dnl        ...
dnl        ...
dnl     AM_PATH_WXCONFIG(2.3.4, wxWin=1)
dnl     if test "$wxWin" != 1; then
dnl        AC_MSG_ERROR([
dnl                wxWindows must be installed on your system
dnl                but wx-config script couldn't be found.
dnl
dnl                Please check that wx-config is in path, the directory
dnl                where wxWindows libraries are installed (returned by
dnl                'wx-config --libs' command) is in LD_LIBRARY_PATH or
dnl                equivalent variable and wxWindows version is 2.3.4 or above.
dnl        ])
dnl     fi
dnl     CPPFLAGS="$CPPFLAGS $WX_CPPFLAGS"
dnl     CXXFLAGS="$CXXFLAGS $WX_CXXFLAGS_ONLY"
dnl     CFLAGS="$CFLAGS $WX_CFLAGS_ONLY"
dnl
dnl     LDFLAGS="$LDFLAGS $WX_LIBS"
dnl ---------------------------------------------------------------------------

dnl ---------------------------------------------------------------------------
dnl AM_OPTIONS_WXCONFIG
dnl
dnl adds support for --wx-prefix, --wx-exec-prefix, --with-wxdir and
dnl --wx-config command line options
dnl ---------------------------------------------------------------------------

AC_DEFUN([AM_OPTIONS_WXCONFIG],
[
    AC_ARG_WITH(wxdir,
                [  --with-wxdir=PATH       Use uninstalled version of wxWindows in PATH],
                [ wx_config_name="$withval/wx-config"
                  wx_config_args="--inplace"])
    AC_ARG_WITH(wx-config,
                [  --with-wx-config=CONFIG wx-config script to use (optional)],
                wx_config_name="$withval" )
    AC_ARG_WITH(wx-prefix,
                [  --with-wx-prefix=PREFIX Prefix where wxWindows is installed (optional)],
                wx_config_prefix="$withval", wx_config_prefix="")
    AC_ARG_WITH(wx-exec-prefix,
                [  --with-wx-exec-prefix=PREFIX
                          Exec prefix where wxWindows is installed (optional)],
                wx_config_exec_prefix="$withval", wx_config_exec_prefix="")
])

dnl ---------------------------------------------------------------------------
dnl AM_PATH_WXCONFIG(VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND
dnl                  [, WX-LIBS [, ADDITIONAL-WX-CONFIG-FLAGS]]]])
dnl
dnl Test for wxWindows, and define WX_C*FLAGS, WX_LIBS and WX_LIBS_STATIC
dnl (the latter is for static linking against wxWindows). Set WX_CONFIG_NAME
dnl environment variable to override the default name of the wx-config script
dnl to use. Set WX_CONFIG_PATH to specify the full path to wx-config - in this
dnl case the macro won't even waste time on tests for its existence.
dnl
dnl Optional WX-LIBS argument contains comma- or space-separated list of
dnl wxWindows libraries to link against (it may include contrib libraries). If
dnl it is not specified then WX_LIBS and WX_LIBS_STATIC will contain flags to
dnl link with all of the core wxWindows libraries.
dnl
dnl Optional ADDITIONAL-WX-CONFIG-FLAGS argument is appended to wx-config
dnl invocation command in present. It can be used to fine-tune lookup of
dnl best wxWidgets build available.
dnl
dnl Example use:
dnl   AM_PATH_WXCONFIG([2.6.0], [wxWin=1], [wxWin=0], [html,core,net]
dnl                    [--unicode --debug])
dnl ---------------------------------------------------------------------------

dnl
dnl Get the cflags and libraries from the wx-config script
dnl
AC_DEFUN([AM_PATH_WXCONFIG],
[
  dnl do we have wx-config name: it can be wx-config or wxd-config or ...
  if test x${WX_CONFIG_NAME+set} != xset ; then
     WX_CONFIG_NAME=wx-config
  fi

  if test "x$wx_config_name" != x ; then
     WX_CONFIG_NAME="$wx_config_name"
  fi

  dnl deal with optional prefixes
  if test x$wx_config_exec_prefix != x ; then
     wx_config_args="$wx_config_args --exec-prefix=$wx_config_exec_prefix"
     WX_LOOKUP_PATH="$wx_config_exec_prefix/bin"
  fi
  if test x$wx_config_prefix != x ; then
     wx_config_args="$wx_config_args --prefix=$wx_config_prefix"
     WX_LOOKUP_PATH="$WX_LOOKUP_PATH:$wx_config_prefix/bin"
  fi
  if test "$cross_compiling" = "yes"; then
     wx_config_args="$wx_config_args --host=$host_alias"
  fi

  dnl don't search the PATH if WX_CONFIG_NAME is absolute filename
  if test -x "$WX_CONFIG_NAME" ; then
     AC_MSG_CHECKING(for wx-config)
     WX_CONFIG_PATH="$WX_CONFIG_NAME"
     AC_MSG_RESULT($WX_CONFIG_PATH)
  else
     AC_PATH_PROG(WX_CONFIG_PATH, $WX_CONFIG_NAME, no, "$WX_LOOKUP_PATH:$PATH")
  fi

  if test "$WX_CONFIG_PATH" != "no" ; then
    WX_VERSION=""
    no_wx=""

    min_wx_version=ifelse([$1], ,2.2.1,$1)
    if test -z "$5" ; then
      AC_MSG_CHECKING([for wxWindows version >= $min_wx_version])
    else
      AC_MSG_CHECKING([for wxWindows version >= $min_wx_version ($5)])
    fi

    WX_CONFIG_WITH_ARGS="$WX_CONFIG_PATH $wx_config_args $5 $4"

    WX_VERSION=`$WX_CONFIG_WITH_ARGS --version 2>/dev/null`
    wx_config_major_version=`echo $WX_VERSION | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    wx_config_minor_version=`echo $WX_VERSION | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    wx_config_micro_version=`echo $WX_VERSION | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

    wx_requested_major_version=`echo $min_wx_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    wx_requested_minor_version=`echo $min_wx_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    wx_requested_micro_version=`echo $min_wx_version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

    wx_ver_ok=""
    if test "x$WX_VERSION" != x ; then
      if test $wx_config_major_version -gt $wx_requested_major_version; then
        wx_ver_ok=yes
      else
        if test $wx_config_major_version -eq $wx_requested_major_version; then
           if test $wx_config_minor_version -gt $wx_requested_minor_version; then
              wx_ver_ok=yes
           else
              if test $wx_config_minor_version -eq $wx_requested_minor_version; then
                 if test $wx_config_micro_version -ge $wx_requested_micro_version; then
                    wx_ver_ok=yes
                 fi
              fi
           fi
        fi
      fi
    fi

    if test "x$wx_ver_ok" = x ; then
      no_wx=yes
    else
      WX_LIBS=`$WX_CONFIG_WITH_ARGS --libs`
      WX_LIBS_STATIC=`$WX_CONFIG_WITH_ARGS --static --libs`

      dnl starting with version 2.2.6 wx-config has --cppflags argument
      wx_has_cppflags=""
      if test $wx_config_major_version -gt 2; then
        wx_has_cppflags=yes
      else
        if test $wx_config_major_version -eq 2; then
           if test $wx_config_minor_version -gt 2; then
              wx_has_cppflags=yes
           else
              if test $wx_config_minor_version -eq 2; then
                 if test $wx_config_micro_version -ge 6; then
                    wx_has_cppflags=yes
                 fi
              fi
           fi
        fi
      fi

      if test "x$wx_has_cppflags" = x ; then
         dnl no choice but to define all flags like CFLAGS
         WX_CFLAGS=`$WX_CONFIG_WITH_ARGS --cflags`
         WX_CPPFLAGS=$WX_CFLAGS
         WX_CXXFLAGS=$WX_CFLAGS

         WX_CFLAGS_ONLY=$WX_CFLAGS
         WX_CXXFLAGS_ONLY=$WX_CFLAGS
      else
         dnl we have CPPFLAGS included in CFLAGS included in CXXFLAGS
         WX_CPPFLAGS=`$WX_CONFIG_WITH_ARGS --cppflags`
         WX_CXXFLAGS=`$WX_CONFIG_WITH_ARGS --cxxflags`
         WX_CFLAGS=`$WX_CONFIG_WITH_ARGS --cflags`

         WX_CFLAGS_ONLY=`echo $WX_CFLAGS | sed "s@^$WX_CPPFLAGS *@@"`
         WX_CXXFLAGS_ONLY=`echo $WX_CXXFLAGS | sed "s@^$WX_CFLAGS *@@"`
      fi
    fi

    if test "x$no_wx" = x ; then
       AC_MSG_RESULT(yes (version $WX_VERSION))
       ifelse([$2], , :, [$2])
    else
       if test "x$WX_VERSION" = x; then
          dnl no wx-config at all
          AC_MSG_RESULT(no)
       else
          AC_MSG_RESULT(no (version $WX_VERSION is not new enough))
       fi

       WX_CFLAGS=""
       WX_CPPFLAGS=""
       WX_CXXFLAGS=""
       WX_LIBS=""
       WX_LIBS_STATIC=""
       ifelse([$3], , :, [$3])
    fi
  fi

  AC_SUBST(WX_CPPFLAGS)
  AC_SUBST(WX_CFLAGS)
  AC_SUBST(WX_CXXFLAGS)
  AC_SUBST(WX_CFLAGS_ONLY)
  AC_SUBST(WX_CXXFLAGS_ONLY)
  AC_SUBST(WX_LIBS)
  AC_SUBST(WX_LIBS_STATIC)
  AC_SUBST(WX_VERSION)
])

#                                                        -*- Autoconf -*-
# Copyright (C) 2002, 2003  Free Software Foundation, Inc.
# Generated from amversion.in; do not edit by hand.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
AC_DEFUN([AM_AUTOMAKE_VERSION], [am__api_version="1.9"])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION so it can be traced.
# This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
	 [AM_AUTOMAKE_VERSION([1.9.2])])

# AM_AUX_DIR_EXPAND

# Copyright (C) 2001, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# For projects using AC_CONFIG_AUX_DIR([foo]), Autoconf sets
# $ac_aux_dir to `$srcdir/foo'.  In other projects, it is set to
# `$srcdir', `$srcdir/..', or `$srcdir/../..'.
#
# Of course, Automake must honor this variable whenever it calls a
# tool from the auxiliary directory.  The problem is that $srcdir (and
# therefore $ac_aux_dir as well) can be either absolute or relative,
# depending on how configure is run.  This is pretty annoying, since
# it makes $ac_aux_dir quite unusable in subdirectories: in the top
# source directory, any form will work fine, but in subdirectories a
# relative path needs to be adjusted first.
#
# $ac_aux_dir/missing
#    fails when called from a subdirectory if $ac_aux_dir is relative
# $top_srcdir/$ac_aux_dir/missing
#    fails if $ac_aux_dir is absolute,
#    fails when called from a subdirectory in a VPATH build with
#          a relative $ac_aux_dir
#
# The reason of the latter failure is that $top_srcdir and $ac_aux_dir
# are both prefixed by $srcdir.  In an in-source build this is usually
# harmless because $srcdir is `.', but things will broke when you
# start a VPATH build or use an absolute $srcdir.
#
# So we could use something similar to $top_srcdir/$ac_aux_dir/missing,
# iff we strip the leading $srcdir from $ac_aux_dir.  That would be:
#   am_aux_dir='\$(top_srcdir)/'`expr "$ac_aux_dir" : "$srcdir//*\(.*\)"`
# and then we would define $MISSING as
#   MISSING="\${SHELL} $am_aux_dir/missing"
# This will work as long as MISSING is not called from configure, because
# unfortunately $(top_srcdir) has no meaning in configure.
# However there are other variables, like CC, which are often used in
# configure, and could therefore not use this "fixed" $ac_aux_dir.
#
# Another solution, used here, is to always expand $ac_aux_dir to an
# absolute PATH.  The drawback is that using absolute paths prevent a
# configured tree to be moved without reconfiguration.

AC_DEFUN([AM_AUX_DIR_EXPAND],
[dnl Rely on autoconf to set up CDPATH properly.
AC_PREREQ([2.50])dnl
# expand $ac_aux_dir to an absolute path
am_aux_dir=`cd $ac_aux_dir && pwd`
])

# AM_CONDITIONAL                                              -*- Autoconf -*-

# Copyright (C) 1997, 2000, 2001, 2003, 2004 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 6

# AM_CONDITIONAL(NAME, SHELL-CONDITION)
# -------------------------------------
# Define a conditional.
AC_DEFUN([AM_CONDITIONAL],
[AC_PREREQ(2.52)dnl
 ifelse([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
	[$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
AC_SUBST([$1_TRUE])
AC_SUBST([$1_FALSE])
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi
AC_CONFIG_COMMANDS_PRE(
[if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
  AC_MSG_ERROR([[conditional "$1" was never defined.
Usually this means the macro was only invoked conditionally.]])
fi])])

# serial 7						-*- Autoconf -*-

# Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
# Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.


# There are a few dirty hacks below to avoid letting `AC_PROG_CC' be
# written in clear, in which case automake, when reading aclocal.m4,
# will think it sees a *use*, and therefore will trigger all it's
# C support machinery.  Also note that it means that autoscan, seeing
# CC etc. in the Makefile, will ask for an AC_PROG_CC use...



# _AM_DEPENDENCIES(NAME)
# ----------------------
# See how the compiler implements dependency checking.
# NAME is "CC", "CXX", "GCJ", or "OBJC".
# We try a few techniques and use that to set a single cache variable.
#
# We don't AC_REQUIRE the corresponding AC_PROG_CC since the latter was
# modified to invoke _AM_DEPENDENCIES(CC); we would have a circular
# dependency, and given that the user is not expected to run this macro,
# just rely on AC_PROG_CC.
AC_DEFUN([_AM_DEPENDENCIES],
[AC_REQUIRE([AM_SET_DEPDIR])dnl
AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])dnl
AC_REQUIRE([AM_MAKE_INCLUDE])dnl
AC_REQUIRE([AM_DEP_TRACK])dnl

ifelse([$1], CC,   [depcc="$CC"   am_compiler_list=],
       [$1], CXX,  [depcc="$CXX"  am_compiler_list=],
       [$1], OBJC, [depcc="$OBJC" am_compiler_list='gcc3 gcc'],
       [$1], GCJ,  [depcc="$GCJ"  am_compiler_list='gcc3 gcc'],
                   [depcc="$$1"   am_compiler_list=])

AC_CACHE_CHECK([dependency style of $depcc],
               [am_cv_$1_dependencies_compiler_type],
[if test -z "$AMDEP_TRUE" && test -f "$am_depcomp"; then
  # We make a subdir and do the tests there.  Otherwise we can end up
  # making bogus files that we don't know about and never remove.  For
  # instance it was reported that on HP-UX the gcc test will end up
  # making a dummy file named `D' -- because `-MD' means `put the output
  # in D'.
  mkdir conftest.dir
  # Copy depcomp to subdir because otherwise we won't find it if we're
  # using a relative directory.
  cp "$am_depcomp" conftest.dir
  cd conftest.dir
  # We will build objects and dependencies in a subdirectory because
  # it helps to detect inapplicable dependency modes.  For instance
  # both Tru64's cc and ICC support -MD to output dependencies as a
  # side effect of compilation, but ICC will put the dependencies in
  # the current directory while Tru64 will put them in the object
  # directory.
  mkdir sub

  am_cv_$1_dependencies_compiler_type=none
  if test "$am_compiler_list" = ""; then
     am_compiler_list=`sed -n ['s/^#*\([a-zA-Z0-9]*\))$/\1/p'] < ./depcomp`
  fi
  for depmode in $am_compiler_list; do
    # Setup a source with many dependencies, because some compilers
    # like to wrap large dependency lists on column 80 (with \), and
    # we should not choose a depcomp mode which is confused by this.
    #
    # We need to recreate these files for each test, as the compiler may
    # overwrite some of them when testing with obscure command lines.
    # This happens at least with the AIX C compiler.
    : > sub/conftest.c
    for i in 1 2 3 4 5 6; do
      echo '#include "conftst'$i'.h"' >> sub/conftest.c
      # Using `: > sub/conftst$i.h' creates only sub/conftst1.h with
      # Solaris 8's {/usr,}/bin/sh.
      touch sub/conftst$i.h
    done
    echo "${am__include} ${am__quote}sub/conftest.Po${am__quote}" > confmf

    case $depmode in
    nosideeffect)
      # after this tag, mechanisms are not by side-effect, so they'll
      # only be used when explicitly requested
      if test "x$enable_dependency_tracking" = xyes; then
	continue
      else
	break
      fi
      ;;
    none) break ;;
    esac
    # We check with `-c' and `-o' for the sake of the "dashmstdout"
    # mode.  It turns out that the SunPro C++ compiler does not properly
    # handle `-M -o', and we need to detect this.
    if depmode=$depmode \
       source=sub/conftest.c object=sub/conftest.${OBJEXT-o} \
       depfile=sub/conftest.Po tmpdepfile=sub/conftest.TPo \
       $SHELL ./depcomp $depcc -c -o sub/conftest.${OBJEXT-o} sub/conftest.c \
         >/dev/null 2>conftest.err &&
       grep sub/conftst6.h sub/conftest.Po > /dev/null 2>&1 &&
       grep sub/conftest.${OBJEXT-o} sub/conftest.Po > /dev/null 2>&1 &&
       ${MAKE-make} -s -f confmf > /dev/null 2>&1; then
      # icc doesn't choke on unknown options, it will just issue warnings
      # or remarks (even with -Werror).  So we grep stderr for any message
      # that says an option was ignored or not supported.
      # When given -MP, icc 7.0 and 7.1 complain thusly:
      #   icc: Command line warning: ignoring option '-M'; no argument required
      # The diagnosis changed in icc 8.0:
      #   icc: Command line remark: option '-MP' not supported
      if (grep 'ignoring option' conftest.err ||
          grep 'not supported' conftest.err) >/dev/null 2>&1; then :; else
        am_cv_$1_dependencies_compiler_type=$depmode
        break
      fi
    fi
  done

  cd ..
  rm -rf conftest.dir
else
  am_cv_$1_dependencies_compiler_type=none
fi
])
AC_SUBST([$1DEPMODE], [depmode=$am_cv_$1_dependencies_compiler_type])
AM_CONDITIONAL([am__fastdep$1], [
  test "x$enable_dependency_tracking" != xno \
  && test "$am_cv_$1_dependencies_compiler_type" = gcc3])
])


# AM_SET_DEPDIR
# -------------
# Choose a directory name for dependency files.
# This macro is AC_REQUIREd in _AM_DEPENDENCIES
AC_DEFUN([AM_SET_DEPDIR],
[AC_REQUIRE([AM_SET_LEADING_DOT])dnl
AC_SUBST([DEPDIR], ["${am__leading_dot}deps"])dnl
])


# AM_DEP_TRACK
# ------------
AC_DEFUN([AM_DEP_TRACK],
[AC_ARG_ENABLE(dependency-tracking,
[  --disable-dependency-tracking  speeds up one-time build
  --enable-dependency-tracking   do not reject slow dependency extractors])
if test "x$enable_dependency_tracking" != xno; then
  am_depcomp="$ac_aux_dir/depcomp"
  AMDEPBACKSLASH='\'
fi
AM_CONDITIONAL([AMDEP], [test "x$enable_dependency_tracking" != xno])
AC_SUBST([AMDEPBACKSLASH])
])

# Generate code to set up dependency tracking.   -*- Autoconf -*-

# Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
#   Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

#serial 2

# _AM_OUTPUT_DEPENDENCY_COMMANDS
# ------------------------------
AC_DEFUN([_AM_OUTPUT_DEPENDENCY_COMMANDS],
[for mf in $CONFIG_FILES; do
  # Strip MF so we end up with the name of the file.
  mf=`echo "$mf" | sed -e 's/:.*$//'`
  # Check whether this is an Automake generated Makefile or not.
  # We used to match only the files named `Makefile.in', but
  # some people rename them; so instead we look at the file content.
  # Grep'ing the first line is not enough: some people post-process
  # each Makefile.in and add a new line on top of each file to say so.
  # So let's grep whole file.
  if grep '^#.*generated by automake' $mf > /dev/null 2>&1; then
    dirpart=`AS_DIRNAME("$mf")`
  else
    continue
  fi
  # Extract the definition of DEPDIR, am__include, and am__quote
  # from the Makefile without running `make'.
  DEPDIR=`sed -n 's/^DEPDIR = //p' < "$mf"`
  test -z "$DEPDIR" && continue
  am__include=`sed -n 's/^am__include = //p' < "$mf"`
  test -z "am__include" && continue
  am__quote=`sed -n 's/^am__quote = //p' < "$mf"`
  # When using ansi2knr, U may be empty or an underscore; expand it
  U=`sed -n 's/^U = //p' < "$mf"`
  # Find all dependency output files, they are included files with
  # $(DEPDIR) in their names.  We invoke sed twice because it is the
  # simplest approach to changing $(DEPDIR) to its actual value in the
  # expansion.
  for file in `sed -n "
    s/^$am__include $am__quote\(.*(DEPDIR).*\)$am__quote"'$/\1/p' <"$mf" | \
       sed -e 's/\$(DEPDIR)/'"$DEPDIR"'/g' -e 's/\$U/'"$U"'/g'`; do
    # Make sure the directory exists.
    test -f "$dirpart/$file" && continue
    fdir=`AS_DIRNAME(["$file"])`
    AS_MKDIR_P([$dirpart/$fdir])
    # echo "creating $dirpart/$file"
    echo '# dummy' > "$dirpart/$file"
  done
done
])# _AM_OUTPUT_DEPENDENCY_COMMANDS


# AM_OUTPUT_DEPENDENCY_COMMANDS
# -----------------------------
# This macro should only be invoked once -- use via AC_REQUIRE.
#
# This code is only required when automatic dependency tracking
# is enabled.  FIXME.  This creates each `.P' file that we will
# need in order to bootstrap the dependency handling code.
AC_DEFUN([AM_OUTPUT_DEPENDENCY_COMMANDS],
[AC_CONFIG_COMMANDS([depfiles],
     [test x"$AMDEP_TRUE" != x"" || _AM_OUTPUT_DEPENDENCY_COMMANDS],
     [AMDEP_TRUE="$AMDEP_TRUE" ac_aux_dir="$ac_aux_dir"])
])

# Do all the work for Automake.                            -*- Autoconf -*-

# This macro actually does too much some checks are only needed if
# your package does certain things.  But this isn't really a big deal.

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
# Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 11

# AM_INIT_AUTOMAKE(PACKAGE, VERSION, [NO-DEFINE])
# AM_INIT_AUTOMAKE([OPTIONS])
# -----------------------------------------------
# The call with PACKAGE and VERSION arguments is the old style
# call (pre autoconf-2.50), which is being phased out.  PACKAGE
# and VERSION should now be passed to AC_INIT and removed from
# the call to AM_INIT_AUTOMAKE.
# We support both call styles for the transition.  After
# the next Automake release, Autoconf can make the AC_INIT
# arguments mandatory, and then we can depend on a new Autoconf
# release and drop the old call support.
AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_PREREQ([2.58])dnl
dnl Autoconf wants to disallow AM_ names.  We explicitly allow
dnl the ones we care about.
m4_pattern_allow([^AM_[A-Z]+FLAGS$])dnl
AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
AC_REQUIRE([AC_PROG_INSTALL])dnl
# test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" &&
   test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi

# test whether we have cygpath
if test -z "$CYGPATH_W"; then
  if (cygpath --version) >/dev/null 2>/dev/null; then
    CYGPATH_W='cygpath -w'
  else
    CYGPATH_W=echo
  fi
fi
AC_SUBST([CYGPATH_W])

# Define the identity of the package.
dnl Distinguish between old-style and new-style calls.
m4_ifval([$2],
[m4_ifval([$3], [_AM_SET_OPTION([no-define])])dnl
 AC_SUBST([PACKAGE], [$1])dnl
 AC_SUBST([VERSION], [$2])],
[_AM_SET_OPTIONS([$1])dnl
 AC_SUBST([PACKAGE], ['AC_PACKAGE_TARNAME'])dnl
 AC_SUBST([VERSION], ['AC_PACKAGE_VERSION'])])dnl

_AM_IF_OPTION([no-define],,
[AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
 AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package])])dnl

# Some tools Automake needs.
AC_REQUIRE([AM_SANITY_CHECK])dnl
AC_REQUIRE([AC_ARG_PROGRAM])dnl
AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version})
AM_MISSING_PROG(AUTOCONF, autoconf)
AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version})
AM_MISSING_PROG(AUTOHEADER, autoheader)
AM_MISSING_PROG(MAKEINFO, makeinfo)
AM_PROG_INSTALL_SH
AM_PROG_INSTALL_STRIP
AC_REQUIRE([AM_PROG_MKDIR_P])dnl
# We need awk for the "check" target.  The system "awk" is bad on
# some platforms.
AC_REQUIRE([AC_PROG_AWK])dnl
AC_REQUIRE([AC_PROG_MAKE_SET])dnl
AC_REQUIRE([AM_SET_LEADING_DOT])dnl
_AM_IF_OPTION([tar-ustar], [_AM_PROG_TAR([ustar])],
              [_AM_IF_OPTION([tar-pax], [_AM_PROG_TAR([pax])],
	      		     [_AM_PROG_TAR([v7])])])
_AM_IF_OPTION([no-dependencies],,
[AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [_AM_DEPENDENCIES(CC)],
                  [define([AC_PROG_CC],
                          defn([AC_PROG_CC])[_AM_DEPENDENCIES(CC)])])dnl
AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [_AM_DEPENDENCIES(CXX)],
                  [define([AC_PROG_CXX],
                          defn([AC_PROG_CXX])[_AM_DEPENDENCIES(CXX)])])dnl
])
])


# When config.status generates a header, we must update the stamp-h file.
# This file resides in the same directory as the config header
# that is generated.  The stamp files are numbered to have different names.

# Autoconf calls _AC_AM_CONFIG_HEADER_HOOK (when defined) in the
# loop where config.status creates the headers, so we can generate
# our stamp files there.
AC_DEFUN([_AC_AM_CONFIG_HEADER_HOOK],
[# Compute $1's index in $config_headers.
_am_stamp_count=1
for _am_header in $config_headers :; do
  case $_am_header in
    $1 | $1:* )
      break ;;
    * )
      _am_stamp_count=`expr $_am_stamp_count + 1` ;;
  esac
done
echo "timestamp for $1" >`AS_DIRNAME([$1])`/stamp-h[]$_am_stamp_count])

# AM_PROG_INSTALL_SH
# ------------------
# Define $install_sh.

# Copyright (C) 2001, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_DEFUN([AM_PROG_INSTALL_SH],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
install_sh=${install_sh-"$am_aux_dir/install-sh"}
AC_SUBST(install_sh)])

#                                                          -*- Autoconf -*-
# Copyright (C) 2003  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 1

# Check whether the underlying file-system supports filenames
# with a leading dot.  For instance MS-DOS doesn't.
AC_DEFUN([AM_SET_LEADING_DOT],
[rm -rf .tst 2>/dev/null
mkdir .tst 2>/dev/null
if test -d .tst; then
  am__leading_dot=.
else
  am__leading_dot=_
fi
rmdir .tst 2>/dev/null
AC_SUBST([am__leading_dot])])

# Check to see how 'make' treats includes.	-*- Autoconf -*-

# Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 2

# AM_MAKE_INCLUDE()
# -----------------
# Check to see how make treats includes.
AC_DEFUN([AM_MAKE_INCLUDE],
[am_make=${MAKE-make}
cat > confinc << 'END'
am__doit:
	@echo done
.PHONY: am__doit
END
# If we don't find an include directive, just comment out the code.
AC_MSG_CHECKING([for style of include used by $am_make])
am__include="#"
am__quote=
_am_result=none
# First try GNU make style include.
echo "include confinc" > confmf
# We grep out `Entering directory' and `Leaving directory'
# messages which can occur if `w' ends up in MAKEFLAGS.
# In particular we don't look at `^make:' because GNU make might
# be invoked under some other name (usually "gmake"), in which
# case it prints its new name instead of `make'.
if test "`$am_make -s -f confmf 2> /dev/null | grep -v 'ing directory'`" = "done"; then
   am__include=include
   am__quote=
   _am_result=GNU
fi
# Now try BSD make style include.
if test "$am__include" = "#"; then
   echo '.include "confinc"' > confmf
   if test "`$am_make -s -f confmf 2> /dev/null`" = "done"; then
      am__include=.include
      am__quote="\""
      _am_result=BSD
   fi
fi
AC_SUBST([am__include])
AC_SUBST([am__quote])
AC_MSG_RESULT([$_am_result])
rm -f confinc confmf
])

#  -*- Autoconf -*-


# Copyright (C) 1997, 1999, 2000, 2001, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

# AM_MISSING_PROG(NAME, PROGRAM)
# ------------------------------
AC_DEFUN([AM_MISSING_PROG],
[AC_REQUIRE([AM_MISSING_HAS_RUN])
$1=${$1-"${am_missing_run}$2"}
AC_SUBST($1)])


# AM_MISSING_HAS_RUN
# ------------------
# Define MISSING if not defined so far and test if it supports --run.
# If it does, set am_missing_run to use it, otherwise, to nothing.
AC_DEFUN([AM_MISSING_HAS_RUN],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
test x"${MISSING+set}" = xset || MISSING="\${SHELL} $am_aux_dir/missing"
# Use eval to expand $SHELL
if eval "$MISSING --run true"; then
  am_missing_run="$MISSING --run "
else
  am_missing_run=
  AC_MSG_WARN([`missing' script is too old or missing])
fi
])

# AM_PROG_MKDIR_P
# ---------------
# Check whether `mkdir -p' is supported, fallback to mkinstalldirs otherwise.

# Copyright (C) 2003, 2004 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# Automake 1.8 used `mkdir -m 0755 -p --' to ensure that directories
# created by `make install' are always world readable, even if the
# installer happens to have an overly restrictive umask (e.g. 077).
# This was a mistake.  There are at least two reasons why we must not
# use `-m 0755':
#   - it causes special bits like SGID to be ignored,
#   - it may be too restrictive (some setups expect 775 directories).
#
# Do not use -m 0755 and let people choose whatever they expect by
# setting umask.
#
# We cannot accept any implementation of `mkdir' that recognizes `-p'.
# Some implementations (such as Solaris 8's) are not thread-safe: if a
# parallel make tries to run `mkdir -p a/b' and `mkdir -p a/c'
# concurrently, both version can detect that a/ is missing, but only
# one can create it and the other will error out.  Consequently we
# restrict ourselves to GNU make (using the --version option ensures
# this.)
AC_DEFUN([AM_PROG_MKDIR_P],
[if mkdir -p --version . >/dev/null 2>&1 && test ! -d ./--version; then
  # We used to keeping the `.' as first argument, in order to
  # allow $(mkdir_p) to be used without argument.  As in
  #   $(mkdir_p) $(somedir)
  # where $(somedir) is conditionally defined.  However this is wrong
  # for two reasons:
  #  1. if the package is installed by a user who cannot write `.'
  #     make install will fail,
  #  2. the above comment should most certainly read
  #     $(mkdir_p) $(DESTDIR)$(somedir)
  #     so it does not work when $(somedir) is undefined and
  #     $(DESTDIR) is not.
  #  To support the latter case, we have to write
  #     test -z "$(somedir)" || $(mkdir_p) $(DESTDIR)$(somedir),
  #  so the `.' trick is pointless.
  mkdir_p='mkdir -p --'
else
  # On NextStep and OpenStep, the `mkdir' command does not
  # recognize any option.  It will interpret all options as
  # directories to create, and then abort because `.' already
  # exists.
  for d in ./-p ./--version;
  do
    test -d $d && rmdir $d
  done
  # $(mkinstalldirs) is defined by Automake if mkinstalldirs exists.
  if test -f "$ac_aux_dir/mkinstalldirs"; then
    mkdir_p='$(mkinstalldirs)'
  else
    mkdir_p='$(install_sh) -d'
  fi
fi
AC_SUBST([mkdir_p])])

# Helper functions for option handling.                    -*- Autoconf -*-

# Copyright (C) 2001, 2002, 2003  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 2

# _AM_MANGLE_OPTION(NAME)
# -----------------------
AC_DEFUN([_AM_MANGLE_OPTION],
[[_AM_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])

# _AM_SET_OPTION(NAME)
# ------------------------------
# Set option NAME.  Presently that only means defining a flag for this option.
AC_DEFUN([_AM_SET_OPTION],
[m4_define(_AM_MANGLE_OPTION([$1]), 1)])

# _AM_SET_OPTIONS(OPTIONS)
# ----------------------------------
# OPTIONS is a space-separated list of Automake options.
AC_DEFUN([_AM_SET_OPTIONS],
[AC_FOREACH([_AM_Option], [$1], [_AM_SET_OPTION(_AM_Option)])])

# _AM_IF_OPTION(OPTION, IF-SET, [IF-NOT-SET])
# -------------------------------------------
# Execute IF-SET if OPTION is set, IF-NOT-SET otherwise.
AC_DEFUN([_AM_IF_OPTION],
[m4_ifset(_AM_MANGLE_OPTION([$1]), [$2], [$3])])

#
# Check to make sure that the build environment is sane.
#

# Copyright (C) 1996, 1997, 2000, 2001, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

# AM_SANITY_CHECK
# ---------------
AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftest.file
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftest.file 2> /dev/null`
   if test "$[*]" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftest.file`
   fi
   rm -f conftest.file
   if test "$[*]" != "X $srcdir/configure conftest.file" \
      && test "$[*]" != "X conftest.file $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "$[2]" = conftest.file
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
AC_MSG_RESULT(yes)])

# AM_PROG_INSTALL_STRIP

# Copyright (C) 2001, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# One issue with vendor `install' (even GNU) is that you can't
# specify the program used to strip binaries.  This is especially
# annoying in cross-compiling environments, where the build's strip
# is unlikely to handle the host's binaries.
# Fortunately install-sh will honor a STRIPPROG variable, so we
# always use install-sh in `make install-strip', and initialize
# STRIPPROG with the value of the STRIP variable (set by the user).
AC_DEFUN([AM_PROG_INSTALL_STRIP],
[AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
# Installed binaries are usually stripped using `strip' when the user
# run `make install-strip'.  However `strip' might not be the right
# tool to use in cross-compilation environments, therefore Automake
# will honor the `STRIP' environment variable to overrule this program.
dnl Don't test for $cross_compiling = yes, because it might be `maybe'.
if test "$cross_compiling" != no; then
  AC_CHECK_TOOL([STRIP], [strip], :)
fi
INSTALL_STRIP_PROGRAM="\${SHELL} \$(install_sh) -c -s"
AC_SUBST([INSTALL_STRIP_PROGRAM])])

# Check how to create a tarball.                            -*- Autoconf -*-

# Copyright (C) 2004  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 1


# _AM_PROG_TAR(FORMAT)
# --------------------
# Check how to create a tarball in format FORMAT.
# FORMAT should be one of `v7', `ustar', or `pax'.
#
# Substitute a variable $(am__tar) that is a command
# writing to stdout a FORMAT-tarball containing the directory
# $tardir.
#     tardir=directory && $(am__tar) > result.tar
#
# Substitute a variable $(am__untar) that extract such
# a tarball read from stdin.
#     $(am__untar) < result.tar
AC_DEFUN([_AM_PROG_TAR],
[# Always define AMTAR for backward compatibility.
AM_MISSING_PROG([AMTAR], [tar])
m4_if([$1], [v7],
     [am__tar='${AMTAR} chof - "$$tardir"'; am__untar='${AMTAR} xf -'],
     [m4_case([$1], [ustar],, [pax],,
              [m4_fatal([Unknown tar format])])
AC_MSG_CHECKING([how to create a $1 tar archive])
# Loop over all known methods to create a tar archive until one works.
_am_tools='gnutar m4_if([$1], [ustar], [plaintar]) pax cpio none'
_am_tools=${am_cv_prog_tar_$1-$_am_tools}
# Do not fold the above two line into one, because Tru64 sh and
# Solaris sh will not grok spaces in the rhs of `-'.
for _am_tool in $_am_tools
do
  case $_am_tool in
  gnutar)
    for _am_tar in tar gnutar gtar;
    do
      AM_RUN_LOG([$_am_tar --version]) && break
    done
    am__tar="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$$tardir"'
    am__tar_="$_am_tar --format=m4_if([$1], [pax], [posix], [$1]) -chf - "'"$tardir"'
    am__untar="$_am_tar -xf -"
    ;;
  plaintar)
    # Must skip GNU tar: if it does not support --format= it doesn't create
    # ustar tarball either.
    (tar --version) >/dev/null 2>&1 && continue
    am__tar='tar chf - "$$tardir"'
    am__tar_='tar chf - "$tardir"'
    am__untar='tar xf -'
    ;;
  pax)
    am__tar='pax -L -x $1 -w "$$tardir"'
    am__tar_='pax -L -x $1 -w "$tardir"'
    am__untar='pax -r'
    ;;
  cpio)
    am__tar='find "$$tardir" -print | cpio -o -H $1 -L'
    am__tar_='find "$tardir" -print | cpio -o -H $1 -L'
    am__untar='cpio -i -H $1 -d'
    ;;
  none)
    am__tar=false
    am__tar_=false
    am__untar=false
    ;;
  esac

  # If the value was cached, stop now.  We just wanted to have am__tar
  # and am__untar set.
  test -n "${am_cv_prog_tar_$1}" && break

  # tar/untar a dummy directory, and stop if the command works
  rm -rf conftest.dir
  mkdir conftest.dir
  echo GrepMe > conftest.dir/file
  AM_RUN_LOG([tardir=conftest.dir && eval $am__tar_ >conftest.tar])
  rm -rf conftest.dir
  if test -s conftest.tar; then
    AM_RUN_LOG([$am__untar <conftest.tar])
    grep GrepMe conftest.dir/file >/dev/null 2>&1 && break
  fi
done
rm -rf conftest.dir

AC_CACHE_VAL([am_cv_prog_tar_$1], [am_cv_prog_tar_$1=$_am_tool])
AC_MSG_RESULT([$am_cv_prog_tar_$1])])
AC_SUBST([am__tar])
AC_SUBST([am__untar])
]) # _AM_PROG_TAR

