// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include <iterator>
#include <utility>
#include "StringUtils.h"


namespace wxm {

  const wxString emptyString;

  // String Comparisons

  bool StartsWithChar(const wxString &str, wxUniChar ch) {
    return !str.empty() && *str.begin() == ch;
  }

  bool StartsWithChar(const wxString &str, wxStringCharType ch) {
    return !str.empty() && *str.begin() == ch;
  }

  bool StartsWithChar(const wxString &str, char ch) {
    return !str.empty() && *str.begin() == ch;
  }

  bool EndsWithChar(const wxString &str, wxUniChar ch) {
    return !str.empty() && *std::next(str.end(), -1) == ch;
  }

  bool EndsWithChar(const wxString &str, wxStringCharType ch) {
    return !str.empty() && *std::next(str.end(), -1) == ch;
  }

  bool EndsWithChar(const wxString &str, char ch) {
    return !str.empty() && *std::next(str.end(), -1) == ch;
  }

  // String normalization

  void NormalizeEOLsRemoveNULs(wxString *str) {
    using std::swap;
    wxString normalized;
    normalized.reserve(str->size());

    // Clean up the output from zeroes, and normalize the line endings
    wxStringCharType prevCh = {};
    for (auto const ch : const_cast<const wxString &>(*str)) {
      if (ch == '\0') {                          /* "\0" -> "" */
      } else if (prevCh == '\r' && ch != '\n') { /* "\r[^\n]" -> "\n" */
        normalized += '\n';
      } else if (ch != '\r') {
        normalized += ch;
      }
      prevCh = ch;
    }
    if (prevCh == '\r') { /* "\r$" -> "\n" */
      normalized += '\n';
    }

    swap(*str, normalized);
  }

} // namespace wxm
