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
  This file defines the class Styles.
 */

#include "Styles.h"
#include <wx/config.h>

void Styles::Read(wxConfigBase *config) {
  for (const auto &[style, prefix] : ConfigKeys())
    m_styles[style].Read(config, prefix);
}

void Styles::Write(wxConfigBase *config) const {
  for (const auto &[style, prefix] : ConfigKeys())
    m_styles[style].Write(config, prefix);
}

const std::vector<std::pair<TextStyle, wxString>> &Styles::ConfigKeys() {
  // The single source of truth for the config-key prefix of every persisted text
  // style. Read() and Write() both iterate this, so read and write can never
  // disagree on a key. (TS_CURSOR intentionally shares the cell-bracket key.)
  static const std::vector<std::pair<TextStyle, wxString>> keys = {
    {TS_MATH, wxS("Style/Math/")},
    {TS_TEXT, wxS("Style/Text/")},
    {TS_CODE_VARIABLE, wxS("Style/CodeHighlighting/Variable/")},
    {TS_CODE_FUNCTION, wxS("Style/CodeHighlighting/Function/")},
    {TS_CODE_COMMENT, wxS("Style/CodeHighlighting/Comment/")},
    {TS_CODE_NUMBER, wxS("Style/CodeHighlighting/Number/")},
    {TS_CODE_STRING, wxS("Style/CodeHighlighting/String/")},
    {TS_CODE_OPERATOR, wxS("Style/CodeHighlighting/Operator/")},
    {TS_CODE_LISP, wxS("Style/CodeHighlighting/Lisp/")},
    {TS_CODE_ENDOFLINE, wxS("Style/CodeHighlighting/EndOfLine/")},
    {TS_HEADING6, wxS("Style/Heading6/")},
    {TS_HEADING5, wxS("Style/Heading5/")},
    {TS_SUBSUBSECTION, wxS("Style/Subsubsection/")},
    {TS_SUBSECTION, wxS("Style/Subsection/")},
    {TS_SECTION, wxS("Style/Section/")},
    {TS_TITLE, wxS("Style/Title/")},
    {TS_WARNING, wxS("Style/Warning/")},
    {TS_MAIN_PROMPT, wxS("Style/MainPrompt/")},
    {TS_OTHER_PROMPT, wxS("Style/OtherPrompt/")},
    {TS_LABEL, wxS("Style/Label/")},
    {TS_USERLABEL, wxS("Style/UserDefinedLabel/")},
    {TS_SPECIAL_CONSTANT, wxS("Style/Special/")},
    {TS_GREEK_CONSTANT, wxS("Style/Greek/")},
    {TS_CODE_DEFAULT, wxS("Style/Default/")},
    {TS_NUMBER, wxS("Style/Number/")},
    {TS_STRING, wxS("Style/String/")},
    {TS_ASCIIMATHS, wxS("Style/ASCIImaths/")},
    {TS_VARIABLE, wxS("Style/Variable/")},
    {TS_OPERATOR, wxS("Style/Operator/")},
    {TS_FUNCTION, wxS("Style/Function/")},
    {TS_HIGHLIGHT, wxS("Style/Highlight/")},
    {TS_TEXT_BACKGROUND, wxS("Style/Background/")},
    {TS_DOCUMENT_BACKGROUND, wxS("Style/DocumentBackground/")},
    {TS_ERROR, wxS("Style/Error/")},
    {TS_CELL_BRACKET, wxS("Style/CellBracket/")},
    {TS_ACTIVE_CELL_BRACKET, wxS("Style/ActiveCellBracket/")},
    {TS_CURSOR, wxS("Style/ActiveCellBracket/")},
    {TS_SELECTION, wxS("Style/Selection/")},
    {TS_EQUALSSELECTION, wxS("Style/EqualsSelection/")},
    {TS_OUTDATED, wxS("Style/Outdated/")},
  };
  return keys;
}
