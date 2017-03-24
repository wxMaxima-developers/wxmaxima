// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//


/*! \file
  This file defines the class DirStructure

  DirStructure informs about what directory is to be found where on the current system.
*/


#include "Dirstructure.h"
#include <wx/filename.h>

wxString Dirstructure::ResourcesDir()
{
#if defined __WXMSW__
  wxString exe = wxStandardPaths::Get().GetExecutablePath();
  int lastSlash = exe.rfind(wxT('/'));
  int lastBackslash = exe.rfind(wxT('\\'));
  if (lastSlash < lastBackslash)
    exe = exe.Left(lastBackslash + 1);
  else
    exe = exe.Left(lastSlash + 1);
  return exe;
#elif defined __WXMAC__
  wxString exe = wxStandardPaths::Get().GetExecutablePath();
  exe.Replace(wxT("MacOS/wxmaxima"), wxT("Resources/"));
  return exe;
#else
  return Prefix()+wxT("/share/wxMaxima/");
#endif
}

wxString Dirstructure::Prefix()
{
#ifndef PREFIX
#define PREFIX "/usr"
#endif
  return wxT(PREFIX);
}

wxString Dirstructure::GetwxMaximaLocation()
{
  #if defined __WXMAC__
  wxString applicationPath = wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPathWithSep();
  
  if(applicationPath.EndsWith(wxT("/Contents/MacOS/")))
  {
    wxString bundle_nonAbsolute;
    wxFilename bundle = wxFileName(applicationPath+wxT("../../"););
    bundle.MakeAbsolute();
    if(bundle.GetFullPath().EndsWith(wxT(".app")))
      return bundle.GetFullPath();
  }

  if(wxFileExists(applicationPath))
    return wxFileName(applicationPath).MakeAbsolute().GetFullPath();

  if (wxFileExists("/Applications/wxMaxima.app"))
    return wxT("/Applications/wxMaxima.app");
  if (wxFileExists("/Applications/wxmaxima.app"))
    return wxT("/Applications/wxmaxima.app");
  return(wxT("wxmaxima"));
  #else
  
  return wxStandardPaths::Get().GetExecutablePath();
  #endif

}

wxString Dirstructure::UserConfDir()
{
  return wxGetHomeDir()+wxT("/");
}

wxString Dirstructure::MaximaDefaultLocation()
{
#if defined __WXMSW__
  wxString maxima = wxGetCwd();
  if (maxima.Right(8) == wxT("wxMaxima"))
    maxima.Replace(wxT("wxMaxima"), wxT("bin\\maxima.bat"));
  else
    maxima.Append("\\maxima.bat");
  if (!wxFileExists(maxima))
  {
    wxFileName exe = wxStandardPaths::Get().GetExecutablePath();
    exe.MakeAbsolute();
    wxString exeDir = exe.GetPathWithSep();
    wxString maximapath = exeDir + wxT("..") + exe.GetPathSeparator() +
      wxT("bin")  + exe.GetPathSeparator() + wxT("maxima.bat");
  }
  wxFileName maximapath(maxima);
  maximapath.MakeAbsolute();
  return maximapath.GetFullPath();
#elif defined __WXMAC__
  wxString command;
  if (wxFileExists("/Applications/Maxima.app"))
    command = wxT("/Applications/Maxima.app");
  if (wxFileExists("/Applications/maxima.app"))
    command = wxT("/Applications/maxima.app");
  else if (wxFileExists("/usr/local/bin/maxima"))
    command = wxT("/usr/local/bin/maxima");
  else if (wxFileExists("/usr/bin/maxima"))
    command = wxT("/usr/bin/maxima");
    else
      command = wxT("maxima");
  return command;
#else
  return wxT("maxima");
#endif
}
