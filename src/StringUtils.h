// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Everyone is permitted to copy and distribute verbatim copies
//  of this licence document, but changing it is not allowed.
//
//                       WXWINDOWS LIBRARY LICENCE
//     TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public Licence as published by
//  the Free Software Foundation; either version 2 of the Licence, or (at your
//  option) any later version.
//
//  This library is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//  Licence for more details.
//
//  You should have received a copy of the GNU Library General Public Licence
//  along with this software, usually in a file named COPYING.LIB.  If not,
//  write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
//  Floor, Boston, MA 02110-1301 USA.
//
//  EXCEPTION NOTICE
//
//  1. As a special exception, the copyright holders of this library give
//  permission for additional uses of the text contained in this release of the
//  library as licenced under the wxWindows Library Licence, applying either
//  version 3.1 of the Licence, or (at your option) any later version of the
//  Licence as published by the copyright holders of version 3.1 of the Licence
//  document.
//
//  2. The exception is that you may use, copy, link, modify and distribute
//  under your own terms, binary object code versions of works based on the
//  Library.
//
//  3. If you copy code from files distributed under the terms of the GNU
//  General Public Licence or the GNU Library General Public Licence into a
//  copy of this library, as this licence permits, the exception does not apply
//  to the code that you add in this way.  To avoid misleading anyone as to the
//  status of such modified files, you must delete this exception notice from
//  such code and/or adjust the licensing conditions notice accordingly.
//
//  4. If you write modifications of your own for this library, it is your
//  choice whether to permit this exception to apply to your modifications.  If
//  you do not wish that, you must delete the exception notice from such code
//  and/or adjust the licensing conditions notice accordingly.
//
//  SPDX-License-Identifier: wxWindows

#ifndef WXMAXIMA_STRINGUTILS_H
#define WXMAXIMA_STRINGUTILS_H

#include <wx/string.h>
#include <wx/translation.h>

namespace wxm {

/*! An empty instance of the wxString object.
 *
 * This instance can be returned by const reference where an empty string is
 * desired.
 * This is different from wxEmptyString, the latter being of a different type.
 * The code `const wxString &fun() { return wxEmptyString; }` is undefined
 * behavior, as a temporary wxString instance is returned.
 */
  extern const wxString emptyString;

/*! Provides a static instance of a string - it will only be constructed once.
 *
 * Usagee: S_("foo") - in place of wxT("foo")
 */
#define S_(string) ([]()->const wxString &{ static const wxString str(wxT(string)); return str; }())

/*! Provides a static instance of a translated string - it will only be constructed once.
 *
 * Usagee: T_("foo") - in place of _(wxT("foo"))
 */
#define T_(string) ([]()->const wxString &{ static const wxString &str = _(wxT(string)); return str; }())

// String Comparisons

//! Whether a string begins with a given character
  bool StartsWithChar(const wxString &str, wxUniChar ch);
//! Whether a string begins with a given character
  bool StartsWithChar(const wxString &str, wxStringCharType ch);
//! Whether a string begins with a given character
  bool StartsWithChar(const wxString &str, char ch);

//! Whether a string ends with a given character
  bool EndsWithChar(const wxString &str, wxUniChar ch);
//! Whether a string begins with a given character
  bool EndsWithChar(const wxString &str, wxStringCharType ch);
//! Whether a string begins with a given character
  bool EndsWithChar(const wxString &str, char ch);

// String normalization

//! Removes all NULs from the string, converts "\r\n" to "\n", and lone "\r" to "\n".
  void NormalizeEOLsRemoveNULs(wxString &str);

} // namespace wxm

#endif
