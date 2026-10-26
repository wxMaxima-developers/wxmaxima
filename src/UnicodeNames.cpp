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
  Unicode code points and their names, see UnicodeNames.h.
*/

#include "UnicodeNames.h"
#include "../data/UnicodeData.h"
#include <mutex>
#include <wx/mstream.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/zstream.h>

namespace wxm {
std::optional<long> HexCodePoint(const wxString &text) {
  wxString digits = text;
  if (digits.StartsWith(wxS("u+")) || digits.StartsWith(wxS("U+")))
    digits = digits.Mid(2);
  // ToLong() would accept a sign, leading blanks and a "0x" prefix, none of
  // which are part of a code point typed after ESC.
  if (digits.IsEmpty() || (digits.Length() > 8))
    return std::nullopt;
  for (const auto &ch : digits)
    if (!wxIsxdigit(ch))
      return std::nullopt;
  long value = 0;
  if (!digits.ToLong(&value, 16))
    return std::nullopt;
  if ((value > 0x10FFFF) || ((value >= 0xD800) && (value <= 0xDFFF)))
    return std::nullopt;
  return value;
}

void ForEachUnicodeChar(const std::function<bool(long, const wxString &)> &func) {
  wxMemoryInputStream istream(UnicodeData_txt_gz, UnicodeData_txt_gz_len);
  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  // The IsOk() term keeps a decompression error from spinning this loop
  // forever: Eof() only reports wxSTREAM_EOF, not read-error states.
  while (zstream.IsOk() && !zstream.Eof()) {
    wxString line = textIn.ReadLine();
    wxStringTokenizer items(line, wxS(";"), wxTOKEN_RET_EMPTY_ALL);
    wxString number = items.GetNextToken();
    wxString name = items.GetNextToken();
    if (name.IsEmpty() || (name == wxS("<control>")) ||
        name.StartsWith(wxS("<Plane")))
      continue;
    long codePoint;
    if (!number.ToLong(&codePoint, 16))
      continue;
    if (!func(codePoint, name))
      return;
  }
}

wxString UnicodeCharName(long codePoint) {
  static std::mutex cacheLock;
  static long cachedCodePoint = -1;
  static wxString cachedName;
  {
    const std::lock_guard<std::mutex> lock(cacheLock);
    if (codePoint == cachedCodePoint)
      return cachedName;
  }
  wxString name;
  ForEachUnicodeChar([codePoint, &name](long code, const wxString &charName) {
    // "<CJK Ideograph, First>" and the like mark where a range of
    // characters without names of their own starts.
    if ((code == codePoint) && !charName.StartsWith(wxS("<")))
      name = charName;
    // The table is sorted: once past the code point it isn't in it.
    return code < codePoint;
  });
  const std::lock_guard<std::mutex> lock(cacheLock);
  cachedCodePoint = codePoint;
  cachedName = name;
  return name;
}
} // namespace wxm
