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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares the class DirStructure

  DirStructure informs about what directory is to be found where on the current system.
*/

#ifndef DIRSTRUCTURE_H
#define DIRSTRUCTURE_H


// This macro is used to mark the macports prefix, so that it can be easily patched
// The MacPorts port file for wxMaxima uses this, so don't remove this!
// Note: this is not done with a define passed through cmake because I didn't find a way
// to pass paths which need quoting through cmake.
#define OSX_MACPORTS_PREFIX "/opt/local"
// If set to 1, the macports path is put first into the search path (after app package paths).
// This could be done with a define but then it doesn't make sense to use a different method.
#define OSX_MACPORTS_PREFER 0

#include "precomp.h"
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
    static wxString UserConfDir() {return m_userConfDir;}
    //! Set the directory the user stores its data in.
    static void UserConfDir(wxString userConfDir);

    //! The directory general data is stored in
    wxString DataDir() const;

    //! The directory our private fonts are stored in
    wxString FontDir() const {return DataDir()+wxS("/../fonts");}

    //! The directory the help file is stored in
    wxString HelpDir() const {return m_helpDir;}
    //! Set the directory the help file is stored in
    void HelpDir(wxString helpDir){m_helpDir = helpDir;}

    /*! The file private accelerator key information is stored in

     */
    static wxString UserAutocompleteFile();

    //! The path to wxMaxima's own AutoComplete file
    wxString AutocompleteFile() const
        { return DataDir() + wxS("/autocomplete.txt"); }

    /*! The directory the locale data is to be found in

      Is only used on MSW and MAC
    */
    wxString LocaleDir() const
        { return ResourcesDir() + wxS("/locale"); }

    //! The executable file path to the maxima executable (or .bat on Windows)
    static wxString MaximaDefaultLocation();

    static wxString
    AnchorsCacheFile()
        {
            return UserConfDir() + "/manual_anchors.xml";
        }

    static Dirstructure *Get()
        {
            return m_dirStructure;
        }
private:
    wxString m_helpDir;
    static wxString m_userConfDir;
    static Dirstructure *m_dirStructure;
};

#endif // DIRSTRUCTURE_H
