// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Declares SaveAsDefaults(), what the "Save As" dialog starts out with.
*/

#ifndef SAVEASDEFAULTS_H
#define SAVEASDEFAULTS_H

#include <wx/string.h>

//! The file name and file type the "Save As" dialog is opened with
struct SaveAsDefault
{
  //! The file name, without directory but with its extension
  wxString name;
  //! The extension that goes with filterIndex: wxmx or wxm
  wxString extension;
  //! 0 for "Whole document (*.wxmx)", 1 for "The input (*.wxm)"
  int filterIndex = 0;
};

/*! What the "Save As" dialog proposes for a document

  The proposed name always carries its extension, which has to agree with the
  file type the dialog shows (GH #2440). If it had none, wxGTK's
  wxFileDialog would append the extension of the *first* file type to it on
  construction, and choosing a different type with SetFilterIndex() later
  doesn't change the name again. A .wxm document was therefore proposed as
  name.wxmx while the dialog said *.wxm, and saved as a .wxmx. Neither
  does picking another file type in GTK's dialog rename the file: there the
  file type only filters what the dialog lists.

  \param currentFile The document's file name, empty for an untitled one.
  \param defaultExt  The extension of the last document saved, for an untitled
                     document.
  \param untitled    The (translated) name of an untitled document.
*/
SaveAsDefault SaveAsDefaults(const wxString &currentFile,
                             const wxString &defaultExt,
                             const wxString &untitled);

#endif // SAVEASDEFAULTS_H
