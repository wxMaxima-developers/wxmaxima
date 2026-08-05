// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares all the wizards the draw sidepane needs.
*/

#ifndef WXMATHML_H
#define WXMATHML_H

#include <cstddef>
#include "precomp.h"
#include "Configuration.h"
#include <wx/string.h>
#include <wx/tokenzr.h>

class wxMathML
{
public:
  explicit wxMathML(Configuration *config);
  wxString GetCmd();
  //! Read the wxMathML.lisp from the file filename instead from the builtin data.
  static void Set_MathML_Filename(const wxString &filename) {m_wxMathML_file = filename;}
  //! The name to read wxMathML.lisp from. If empty we use the builtin file.
  static const wxString& Get_MathML_Filename() {return m_wxMathML_file;}
private:
  //! If we read wxMathml.lisp from a file this variable is not-empty and contains its name
  static wxString m_wxMathML_file;
  wxString m_wxMathML;
#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ < 12
  // GCC < 12 doesn't support [[maybe_unused]] on non-static data members and
  // warns "attribute ignored" about it (fixed in GCC 12); Clang needs the
  // attribute here to silence -Wunused-private-field, so it can't just be
  // removed.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wattributes"
#endif
  [[maybe_unused]] Configuration *m_configuration = NULL;
#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ < 12
#pragma GCC diagnostic pop
#endif
  static wxString m_maximaCMD;
};

#endif
