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
  Loads and saves worksheet files (.wxmx, .wxm, .mac/.out, .xml) for wxMaxima.

  Reading the various worksheet file formats into the worksheet, and writing it
  back out, used to be part of the wxMaxima god class; that file I/O is being
  peeled off into this class to shrink wxMaxima.cpp. The loaders/savers still
  drive the frame (its worksheet, status bar, title) through the m_wxMaxima
  reference, so MaximaFileIO is a friend of wxMaxima; the wxMaxima frame owns
  this object by value.
*/

#ifndef MAXIMAFILEIO_H
#define MAXIMAFILEIO_H

#include <wx/string.h>

class wxMaxima;
class Worksheet;
class wxStringTokenizer;

/*! The worksheet file loaders/savers extracted from the wxMaxima god class.

  Owned by value by the wxMaxima frame. Holds a reference back to that frame for
  the services the loaders/savers need (the worksheet, the status bar, the
  title, the Maxima process).
*/
class MaximaFileIO
{
public:
  explicit MaximaFileIO(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Loads a Maxima/xMaxima batch file (.mac/.out) into the worksheet.
  bool OpenMACFile(const wxString &file, Worksheet *document,
                   bool clearDocument = true);

  //! Loads a plain-text wxMaxima worksheet (.wxm) into the worksheet.
  bool OpenWXMFile(const wxString &file, Worksheet *document,
                   bool clearDocument = true);

  //! Loads a zipped wxMaxima worksheet (.wxmx) into the worksheet, with several
  //! fallback strategies for recovering a damaged archive.
  bool OpenWXMXFile(const wxString &file, Worksheet *document,
                    bool clearDocument = true);

  //! Loads a bare content.xml (extracted from a .wxmx) into the worksheet.
  bool OpenXML(const wxString &file, Worksheet *document);

  //! Warns if a document was saved by a newer wxMaxima; returns false if it is
  //! too new to open at all.
  bool CheckWXMXVersion(const wxString &docversion);

  //! Reassembles a cell whose closing XML tag went missing in a damaged .wxmx,
  //! used by OpenWXMXFile's recovery path (recursive).
  wxString ReadPotentiallyUnclosedTag(wxStringTokenizer &lines,
                                      wxString firstLine);

  //! Opens a file: picks the loader by extension (or hands the file to Maxima
  //! via load()/batch()/demo() for non-worksheet files) and updates the UI.
  bool OpenFile(const wxString &file, const wxString &command = {});

  //! Saves the worksheet, prompting for a name (Save As) if needed or forced.
  bool SaveFile(bool forceSave = false);

  //! Saves the project to a temp file (autosave), or to its real file.
  bool AutoSave();

private:
  //! The wxMaxima frame whose services the loaders/savers use. Not owned; the
  //! frame owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMAFILEIO_H
