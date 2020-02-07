// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2013 Doug Ilijev <doug.ilijev@gmail.com>
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
  This file declares the class DirStructure

  DirStructure informs about what directory is to be found where on the current system.
*/

#ifndef DIRSTRUCTURE_H
#define DIRSTRUCTURE_H

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/stdpaths.h>
#include <wx/utils.h>

/*! An object that represents the directory structure wxMaxima is installed in

  wxMaxima finds its data in different places on different operating systems:
 - wxStandardPaths::GetExecutablePath() on windows
 - PREFIX+"/share/wxMaxima/" on Linux
 - wxStandardPaths::GetExecutablePath()+"/wxMaxima.app/Contents/Resources" on mac.

   - on linux in 
*/
class Dirstructure
{
public:
  //! The constructor
  Dirstructure();
  
private:
  //! The directory all data is stored relative to.
  wxString ResourcesDir() const;

public:
  //! The directory the user stores its data in.
  wxString UserConfDir() const {return m_userConfDir;}
  //! Set the directory the user stores its data in.
  void UserConfDir(wxString userConfDir) {m_userConfDir = userConfDir + wxT("/");}

  //! The directory general data is stored in
  wxString DataDir() const;

    //! The directory our private fonts are stored in
  wxString FontDir() const {return DataDir()+wxT("/../fonts");}

  //! The directory the help file is stored in
  wxString HelpDir() const {return m_helpDir;}
  //! Set the directory the help file is stored in
  void HelpDir(wxString helpDir){m_helpDir = helpDir;}

  /*! The file private accellerator key information is stored in

    \todo Document this file in the texinfo manual
   */
#if defined __WXMSW__
  wxString UserAutocompleteFile() const {return UserConfDir()+wxT("wxmax.ac");}
#else

  wxString UserAutocompleteFile() const
  { return UserConfDir() + wxT(".wxmaxima.ac"); }

#endif

  //! The path to wxMaxima's own AutoComplete file
  wxString AutocompleteFile() const
  { return DataDir() + wxT("/autocomplete.txt"); }

  //! The directory art is stored relative to
  wxString ArtDir() const;

  /*! The directory the locale data is to be found in

    Is only used on MSW and MAC
   */
  wxString LocaleDir() const
  { return ResourcesDir() + wxT("/locale"); }

  //! The path we pass to the operating system if we want it to locate maxima instead
  static wxString MaximaDefaultLocation();

  static Dirstructure *Get()
    {
      return m_dirStructure;
    }
private:
  wxString m_helpDir;
  wxString m_userConfDir;
  static Dirstructure *m_dirStructure;
};

#endif // DIRSTRUCTURE_H
