/*
  Stand-in for wxWidgets' own wx/msw/gccpriv.h, used only on Cygwin when the
  installed wxWidgets lacks it (see the CYGWIN block in CMakeLists.txt).

  A GTK build of wxWidgets on Cygwin defines __GNUWIN32__ in its setup.h,
  which makes wx/platform.h include wx/msw/gccpriv.h. But wxWidgets only
  installs that header for a native Windows build, so Cygwin's wx 3.2
  packages don't ship it and every file including a wx header fails to
  compile.

  The real header detects MinGW's version; on Cygwin it defines nothing more
  than what wx/platform.h defines when it doesn't include it at all, and that
  is all this one does.
*/

/* THIS IS A C FILE, DON'T USE C++ FEATURES (IN PARTICULAR COMMENTS) IN IT */

#ifndef _WX_MSW_GCCPRIV_H_
#define _WX_MSW_GCCPRIV_H_

#define wxCHECK_W32API_VERSION(maj, min) (0)
#define wxCHECK_MINGW32_VERSION(major, minor) (0)
#define wxDECL_FOR_MINGW32_ALWAYS(rettype, func, params)
#define wxDECL_FOR_STRICT_MINGW32(rettype, func, params)

#endif /* _WX_MSW_GCCPRIV_H_ */
