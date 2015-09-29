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

#include "Dirstructure.h"

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
  return wxT(PREFIX);
}

wxString Dirstructure::UserConfDir()
{
  return wxGetHomeDir()+wxT("/");
}

wxString Dirstructure::MaximaDefaultLocation()
{
#if defined __WXMSW__
  wxString exe = wxStandardPaths::Get().GetExecutablePath();
  exe.Replace(wxT("wxMaxima/wxmaxima.exe"), wxT("bin/maxima.bat"));
  return exe;
#elif defined __WXMAC__
  return wxT("/Applications/Maxima.app");
#else
  return wxT("maxima");
#endif
}
