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
  Decides whether wxMaxima can open a file it was handed, and how.

  A file given on the command line, by the desktop ("Open with") or by the
  macOS Finder used to be opened whatever it was: every one got a window and
  a Maxima process of its own, and anything that wasn't a worksheet was sent
  to Maxima as load("file"). Selecting wxmaxima.exe and its DLLs and opening
  them with wxMaxima therefore started dozens of windows and Maxima
  processes at once, each asking its Lisp to load an executable -- which
  GCL, for one, really tries to link in as native code. The machine ran out
  of memory long before anything useful happened.

  Opening a file is now refused unless its type is one wxMaxima knows, and a
  file whose format is text is refused if it looks binary. Loading a package
  stays possible for the package types Maxima itself recognises; the "Load
  package" menu, which the user picks a file for explicitly, is unaffected.

  Deliberately GUI-free, so test_FileOpenKind can check it directly.
*/

#ifndef WXM_FILEOPENKIND_H
#define WXM_FILEOPENKIND_H

#include <wx/string.h>

namespace wxm {

//! What wxMaxima does with a file it is asked to open
enum class FileOpenKind {
  WxmWorksheet,  //!< .wxm (and its backup .wxm~): read into the worksheet
  WxmxWorksheet, //!< .wxmx (and .wxmx~, .zip): read into the worksheet
  MacFile,       //!< .mac or xmaxima's .out: read into the worksheet
  XmlWorksheet,  //!< .xml, the content.xml from a broken .wxmx
  Demo,          //!< .dem: run by Maxima's demo()
  Package,       //!< a Lisp or Maxima source package: run by Maxima's load()
  Unsupported    //!< anything else: refused
};

/*! Which kind of file this is, judged by its name alone

  A "#uuid" suffix, by which a cell in a worksheet can be addressed, is
  ignored.
*/
FileOpenKind ClassifyFileForOpening(const wxString &file);

/*! True if the start of the file looks like binary data rather than text

  Looks at the first few kilobytes: a NUL byte, or the signature of a
  Windows, Linux or macOS executable, counts as binary. A file that can't be
  read counts as not binary; opening it will report that properly.
*/
bool FileLooksBinary(const wxString &file);

/*! Why wxMaxima won't open this file, or an empty string if it will

  Combines ClassifyFileForOpening() with FileLooksBinary() for the kinds that
  are text. The file has to exist for the content check; whether it exists
  is not checked here.
*/
wxString WhyFileCannotBeOpened(const wxString &file);

/*! A file name as a Maxima string literal, quotes included

  For building commands like load("file"). A file name on Linux and macOS
  may contain a double quote, which used to end the string early: importing
  a CSV file named `data.csv"),system("...` ran a shell command. A backslash
  is a Maxima escape character too, so an unescaped Windows path lost all of
  its backslashes.
*/
wxString MaximaStringLiteral(const wxString &str);

} // namespace wxm

#endif // WXM_FILEOPENKIND_H
