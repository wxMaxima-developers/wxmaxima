// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file contains the code for MarkDownParser.

  MarkDownParser is the class that handles the markdown syntax
*/

#ifndef MARKDOWN_H
#define MARKDOWN_H

#include <wx/string.h>
#include "Configuration.h"

/*! A generic markdown Parser.

 */
class MarkDownParser
{
  class IndentManager;
public:
  virtual ~MarkDownParser();  
  wxString MarkDown(wxString str);

protected:
  struct ElementPack;
  Configuration *m_configuration;
  const ElementPack &m_e;

  explicit MarkDownParser(Configuration *cfg, const ElementPack &elts);
  virtual void DoReplacementsOn(wxString &str) = 0;
};

//! A markdown parser for TeX
class MarkDownTeX : public MarkDownParser
{
  struct ElementPack;
  static const ElementPack elementPack;
public:
  explicit MarkDownTeX(Configuration *cfg);
  void DoReplacementsOn(wxString &str) override;
};

//! A markdown parser for HTML
class MarkDownHTML : public MarkDownParser
{
  struct ElementPack;
  static const ElementPack elementPack;
public:
  explicit MarkDownHTML(Configuration *cfg);
  void DoReplacementsOn(wxString &str) override;
};

#endif // MARKDOWN_H
