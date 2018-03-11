// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  SPDX-License-Identifier: GPL-2.0+


/*! \file
  This file defines the class DirStructure

  DirStructure informs about what directory is to be found where on the current system.
*/


#include "Dirstructure.h"
#include <wx/filename.h>
#include <wx/dir.h>

Dirstructure::Dirstructure()
{
  m_helpDir = ResourcesDir();
  if(wxDirExists(m_helpDir + wxT("/doc/wxmaxima")))
    m_helpDir += wxT("/doc/wxmaxima");
  
  if(wxDirExists(m_helpDir + wxT("/help")))
    m_helpDir += wxT("/help");
  
  if(wxDirExists(m_helpDir + wxT("/info")))
    m_helpDir += wxT("/info");
  
  if(!wxGetEnv(wxT("MAXIMA_USERDIR"),&m_userConfDir))
    m_userConfDir = wxGetHomeDir();
  m_userConfDir += "/";
  
  
#ifndef __WXMSW__
  m_userConfDir += wxT(".");
#endif
  
  m_userConfDir += wxT("maxima");
  
  if(!wxDirExists(m_userConfDir))
    wxMkDir(m_userConfDir, wxS_DIR_DEFAULT);
  
  m_userConfDir += "/";

}

wxString Dirstructure::ResourcesDir()
{
  // Our ressources dir is somewhere near to the dir the binary can be found.
  wxFileName exe(wxStandardPaths::Get().GetExecutablePath());
  
  // We only need the drive and the directory part of the path to the binary
  exe.ClearExt();
  exe.SetName(wxEmptyString);
  
  // If the binary is in a source or bin folder the resources dir is one level above
  wxArrayString dirs = exe.GetDirs();
  if((dirs.Last().Upper() == wxT("SRC")) || (dirs.Last().Upper() == wxT("BIN")))
  {
    exe.RemoveLastDir();
    dirs = exe.GetDirs();
  }
  
  // If the binary is in the wxMaxima folder the resources dir is two levels above as we
  // are in MacOS/wxmaxima
  if((dirs.Last().Upper() == wxT("MACOS")))
  {
    exe.RemoveLastDir();
    dirs = exe.GetDirs();
  }
  
  // If there is a Resources folder the ressources are there
  if(wxDirExists(exe.GetPath() + wxT("/Resources")))
  {
    exe.AppendDir("Resources");
    dirs = exe.GetDirs();
  }
  
  // If there is a share folder the ressources are there
  if(wxDirExists(exe.GetPath() + wxT("/share")))
  {
    exe.AppendDir("share");
    dirs = exe.GetDirs();
  }
  
  return exe.GetPath();
}

wxString Dirstructure::DataDir()
{
  wxString dir = ResourcesDir();
  if(wxDirExists(dir + wxT("/data")))
    dir += wxT("/data");
  if(wxDirExists(dir + wxT("/wxMaxima")))
    dir += wxT("/wxMaxima");

  return dir;
}

wxString Dirstructure::ArtDir()
{
  wxString dir = ResourcesDir();
  if(wxDirExists(dir + wxT("/wxMaxima")))
    dir += wxT("/wxMaxima");

  if(wxDirExists(dir + wxT("/art")))
    dir += wxT("/art");

  return dir;
}

wxString Dirstructure::AppIconDir()
{
  wxString dir = ResourcesDir();

  if(wxFileExists(ResourcesDir() + wxT("/wxmaxima.png")))
    return ResourcesDir();
  else
  {
    if(wxDirExists(dir + wxT("/wxMaxima")))
      dir += wxT("/wxMaxima");
    
    if(wxDirExists(dir + wxT("/data")))
      dir += wxT("/data");
    
    return dir;
  }
}

wxString Dirstructure::ConfigArtDir()
{
  wxString dir = ArtDir();
  if(wxDirExists(dir + wxT("/config")))
    dir += wxT("/config");

  return dir;
}

wxString Dirstructure::ConfigToolbarDir()
{
  wxString dir = ArtDir();
  if(wxDirExists(dir + wxT("/toolbar")))
    dir += wxT("/toolbar");

  return dir;
}

wxString Dirstructure::ConfigStatusbarDir()
{
  wxString dir = ArtDir();
  if(wxDirExists(dir + wxT("/statusbar")))
    dir += wxT("/statusbar");

  return dir;
}

wxString Dirstructure::MaximaLispLocation()
{
  wxString result;
  wxString basedir = "/usr/local/share/maxima/";
  wxDir dir (basedir);
  if(!dir.IsOpened())
  {
    basedir = "/usr/share/maxima/";
    dir.Open(basedir);
  }

  if(!dir.IsOpened())
  {
    wxFileName maximaLocation(MaximaDefaultLocation());
    basedir = maximaLocation.GetPath();
    if(wxDirExists(basedir + "/../maxima/share/maxima"))
    {
      basedir = basedir + "/../maxima/share/maxima";
      dir.Open(basedir);
    }
  }

  if(!dir.IsOpened())
  {
    wxFileName maximaLocation(MaximaDefaultLocation());
    basedir = maximaLocation.GetPath();
    if(wxDirExists(basedir + "/../share/maxima"))
    {
      basedir = basedir + "/../share/maxima";
      dir.Open(basedir);
    }
  }

  if(!dir.IsOpened())
  {
    wxFileName maximaLocation(MaximaDefaultLocation());
    basedir = maximaLocation.GetPath();
    if(wxDirExists(basedir + "/../maxima"))
    {
      basedir = basedir + "/../maxima";
      dir.Open(basedir);
    }
  }
    
  if(dir.IsOpened())
  {
    bool more = dir.GetFirst(&result);
    while(more)
      more = dir.GetNext(&result);
  }

  if(result != wxEmptyString)
  {
    result = basedir + wxT("/") + result;
    return result;
  }

  result = MaximaDefaultLocation();
  if(result.EndsWith(".app"))
  {
    result += "/";
    return result;
  }
  else
  {
    wxFileName maximaName(result);
    maximaName.RemoveLastDir();
    return maximaName.GetPath();
  }
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
