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
*/

#include "FileOpenKind.h"

#include <wx/file.h>
#include <wx/intl.h>
#include <wx/log.h>

#include <algorithm>
#include <array>
#include <cstring>
#include <string_view>

namespace wxm {

namespace {
//! The file name without a "#uuid" cell address, in lower case
wxString LowerFileName(const wxString &file) {
  wxString name = file;
  if (name.Contains(wxS("#")))
    name = name.BeforeFirst(wxS('#'));
  return name.Lower();
}

//! True if wxMaxima expects this kind of file to be text
bool IsTextKind(FileOpenKind kind) {
  switch (kind) {
  case FileOpenKind::WxmWorksheet:
  case FileOpenKind::MacFile:
  case FileOpenKind::XmlWorksheet:
  case FileOpenKind::Demo:
  case FileOpenKind::Package:
    return true;
  case FileOpenKind::WxmxWorksheet:
  case FileOpenKind::Unsupported:
    return false;
  }
  return false;
}
} // namespace

FileOpenKind ClassifyFileForOpening(const wxString &file) {
  const wxString name = LowerFileName(file);

  if (name.EndsWith(wxS(".wxm")) || name.EndsWith(wxS(".wxm~")))
    return FileOpenKind::WxmWorksheet;
  if (name.EndsWith(wxS(".wxmx")) || name.EndsWith(wxS(".wxmx~")) ||
      name.EndsWith(wxS(".zip")))
    return FileOpenKind::WxmxWorksheet;
  if (name.EndsWith(wxS(".mac")) || name.EndsWith(wxS(".out")))
    return FileOpenKind::MacFile;
  if (name.EndsWith(wxS(".xml")))
    return FileOpenKind::XmlWorksheet;
  if (name.EndsWith(wxS(".dem")))
    return FileOpenKind::Demo;
  // The source package types Maxima's load() itself recognises
  // (file_type_lisp, file_type_maxima), minus the ones handled above. Not
  // compiled Lisp (.fasl, .fas, GCL's .o): that is native code -- GCL links
  // whatever .o it is given in, C object files included -- and nobody opens
  // it by double-clicking. "Load package" still loads it.
  if (name.EndsWith(wxS(".lisp")) || name.EndsWith(wxS(".lsp")) ||
      name.EndsWith(wxS(".mc")))
    return FileOpenKind::Package;
  return FileOpenKind::Unsupported;
}

bool FileLooksBinary(const wxString &file) {
  wxString name = file;
  if (name.Contains(wxS("#")))
    name = name.BeforeFirst(wxS('#'));

  wxLogNull suppressOpenErrors;
  wxFile in;
  if (!in.Open(name, wxFile::read))
    return false;

  std::array<unsigned char, 8192> buf{};
  const ssize_t got = in.Read(buf.data(), buf.size());
  if (got <= 0)
    return false;
  const auto len = static_cast<size_t>(got);

  auto startsWith = [&](std::string_view magic) {
    return len >= magic.size() &&
      std::memcmp(buf.data(), magic.data(), magic.size()) == 0;
  };
  // Linux (ELF) and macOS (Mach-O, both byte orders, 32 and 64 bit, and
  // universal binaries) executables and libraries. Not "MZ", the start of a
  // Windows executable: a .mac may well begin with a variable of that name,
  // and the DOS header that follows it is full of NUL bytes anyway.
  static constexpr std::array<std::string_view, 6> executableMagic = {
    "\x7f" "ELF",
    "\xfe\xed\xfa\xce", "\xce\xfa\xed\xfe",
    "\xfe\xed\xfa\xcf", "\xcf\xfa\xed\xfe",
    "\xca\xfe\xba\xbe"};
  if (std::ranges::any_of(executableMagic, startsWith))
    return true;

  // UTF-16 text is full of NUL bytes, but has a byte order mark.
  static constexpr std::array<std::string_view, 2> utf16ByteOrderMarks = {
    "\xff\xfe", "\xfe\xff"};
  if (std::ranges::any_of(utf16ByteOrderMarks, startsWith))
    return false;
  return std::memchr(buf.data(), 0, len) != nullptr;
}

wxString WhyFileCannotBeOpened(const wxString &file) {
  const FileOpenKind kind = ClassifyFileForOpening(file);
  if (kind == FileOpenKind::Unsupported)
    return _("wxMaxima cannot open files of this type.");
  if (IsTextKind(kind) && FileLooksBinary(file))
    return _("The file should contain text, but contains binary data.");
  return {};
}

wxString MaximaStringLiteral(const wxString &str) {
  wxString escaped = str;
  escaped.Replace(wxS("\\"), wxS("\\\\"));
  escaped.Replace(wxS("\""), wxS("\\\""));
  return wxS("\"") + escaped + wxS("\"");
}

} // namespace wxm
