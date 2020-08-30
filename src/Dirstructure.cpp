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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+


/*! \file
  This file defines the class DirStructure

  DirStructure informs about what directory is to be found where on the current system.
*/

#include "Dirstructure.h"
#include <wx/filename.h>
#include <wx/dir.h>
#include "Version.h"

// This macro is used to mark the macports prefix, so that it can be easily patched
// The MacPorts port file for wxMaxima uses this, so don't remove this!
// Note: this is not done with a define passed through cmake because I didn't find a way
// to pass paths which need quoting through cmake.
#define OSX_MACPORTS_PREFIX "/opt/local"
// If set to 1, the macports path is put first into the search path (after app package paths).
// This could be done with a define but then it doesn't make sense to use a different method.
#define OSX_MACPORTS_PREFER 0

Dirstructure::Dirstructure()
{
  m_dirStructure = this;
  m_helpDir = ResourcesDir();

  // The path Gentoo hides the manual at
  if(wxDirExists(m_helpDir + wxString::Format("/doc/wxmaxima-%s",GITVERSION)))
    m_helpDir += wxString::Format("/doc/wxmaxima-%s",GITVERSION); 
  if(wxDirExists(m_helpDir + wxT("/doc/wxmaxima")))
    m_helpDir += wxT("/doc/wxmaxima");

  if(wxDirExists(m_helpDir + wxT("/html")))
    m_helpDir += wxT("/html");

  if(wxDirExists(m_helpDir + wxT("/info")))
    m_helpDir += wxT("/info");

  if(wxDirExists(m_helpDir + wxT("/help")))
    m_helpDir += wxT("/help");
  
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

wxString Dirstructure::ResourcesDir() const
{
  wxString exepath = wxStandardPaths::Get().GetExecutablePath();
  if(!exepath.IsEmpty())
  {
    // Our resources dir is somewhere near to the dir the binary can be found.
    wxFileName exe(exepath);
    
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
      exe.RemoveLastDir();
    
    // If there is a Resources folder the resources are there
    if(wxDirExists(exe.GetPath() + wxT("/Resources")))
      exe.AppendDir("Resources");
    
    // If there is a share folder the resources are there
    if(wxDirExists(exe.GetPath() + wxT("/share")))
      exe.AppendDir("share");

    
    exepath = exe.GetPath();
  }
  else
  {
    exepath = CMAKE_INSTALL_PREFIX;
  }
  return exepath;
}

wxString Dirstructure::DataDir() const
{
  wxString dir = ResourcesDir();
  if(wxDirExists(dir + wxT("/data")))
    dir += wxT("/data");
  if(wxDirExists(dir + wxT("/wxMaxima")))
    dir += wxT("/wxMaxima");

  return dir;
}

wxString Dirstructure::MaximaDefaultLocation()
{
  wxString maximaLocation;
  wxFileName exe = wxStandardPaths::Get().GetExecutablePath();
  exe.MakeAbsolute();
  wxFileName maxima;  
#if defined __WXMSW__ || defined __WXOSX__
  wxString notFound = _("Maxima not found as %s");
#endif
#if defined __WXMSW__
  wxString exeDir = exe.GetPathWithSep();
  maxima = exeDir + "../bin/maxima.bat";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "maxima.bat";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "bin/maxima.bat";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "../maxima.bat";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "maxima";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "bin/maxima";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "../maxima";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "../bin/maxima";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
#elif defined __WXOSX__
  wxString exeDir = exe.GetPathWithSep();
  maximaLocation =  "/Applications/Maxima.app";
  if (wxFileExists(maximaLocation))
    return maximaLocation;
  
  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maximaLocation = "/Applications/maxima.app";
  if (wxFileExists(maximaLocation))
    return maximaLocation;

  // The Macports path (if it is preferred over homebrew)
#if OSX_MACPORTS_PREFER
  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maximaLocation = OSX_MACPORTS_PREFIX "/bin/maxima";
  if (wxFileExists(maximaLocation))
    return maximaLocation;
#endif

  // The homebrew path
  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maximaLocation = "/usr/local/bin/maxima";
  if (wxFileExists(maximaLocation))
    return maximaLocation;

  // The Macports path (if it is not preferred over homebrew)
#if !OSX_MACPORTS_PREFER
  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maximaLocation = OSX_MACPORTS_PREFIX "/bin/maxima";
  if (wxFileExists(maximaLocation))
    return maximaLocation;
#endif

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maximaLocation = "/usr/bin/maxima";
  if (wxFileExists(maximaLocation))
    return maximaLocation;
  
  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
  maxima = exeDir + "maxima";
  maxima.MakeAbsolute();
  if(wxFileExists(maximaLocation = maxima.GetFullPath()))
    return maximaLocation;

  wxLogMessage(wxString::Format(notFound,maximaLocation.utf8_str()));
#endif
  return wxT("maxima");
}

wxString Dirstructure::GnuplotDefaultLocation(wxString pathguess)
{
  wxString gnuplot_binary;

  if(wxFileName(pathguess).IsAbsolute())
    gnuplot_binary = pathguess;
  else
  {
    wxPathList pathlist;

    // Add paths relative to the path of the wxMaxima executable
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath());
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot");
    pathlist.Add(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath()+"/../gnuplot/bin");

    // Add paths from the PATH environment variable
    pathlist.AddEnvList(wxT("PATH"));

    // Add OSX specific paths
#ifdef __WXOSX__
    // MacPorts:
    // The MacPorts default binary path /opt/local/bin/ is not in the PATH for applications.
    // It is added to .profile, but this is only used by shells.
    // => Add the default MacPorts binary path /opt/local/bin/ to our search path list.
    //
    // Homebrew:
    // Homebrew installs binaries in /usr/local/bin, which is in the PATH by default.
    //
    // Application packages including gnuplot:
    // The above wxMaxima executable relative logic should work
    //
    // If gnuplot is somewhere else (e.g. non default MacPort or Homebrew path), the command
    //   gnuplot_command:"/opt/local/bin/gnuplot"$
    // must be added manually to ~/.maxima/wxmaxima-init.mac
    // This should be documented for the installer packages, e.g. as MacPorts "notes" field.
    pathlist.Add(OSX_MACPORTS_PREFIX "/bin");
#endif

    // Find executable "gnuplot" in our list of paths
    gnuplot_binary = pathlist.FindAbsoluteValidPath(pathguess);
    // If not successful, Find executable "gnuplot.exe" in our list of paths
    if(gnuplot_binary == wxEmptyString)
      gnuplot_binary = pathlist.FindAbsoluteValidPath(pathguess + wxT(".exe"));
    // If not successful, use the original command (better than empty for error messages)
    if(gnuplot_binary == wxEmptyString)
    {
      wxLogMessage(_("Gnuplot not found, using the default: ") + pathguess);
      gnuplot_binary = pathguess;
    }
    else
    {
      wxLogMessage(_("Gnuplot found at: ") + gnuplot_binary);
    }    
  }

  return gnuplot_binary;
}

wxString Dirstructure::UserAutocompleteFile()
{
  wxString newFileName = UserConfDir() + wxT("wxmaxima.ac");
  if(!wxFileExists(newFileName))
  {
    wxString potentialOldFile = UserConfDir() + wxT(".wxmaxima.ac");
    if(wxFileExists(potentialOldFile))
      wxRenameFile(potentialOldFile, newFileName);
    potentialOldFile = UserConfDir() + wxT(".wxmax.ac");
    if(wxFileExists(potentialOldFile))
      wxRenameFile(potentialOldFile, newFileName);
  }
  return newFileName;
}

Dirstructure *Dirstructure::m_dirStructure;
wxString Dirstructure::m_userConfDir;
