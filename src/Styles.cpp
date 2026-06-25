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
#include "StringUtils.h"
#include <wx/config.h>
#include <unordered_map>

Styles::Styles() {
  m_codeStyles = {TS_CODE_VARIABLE, TS_CODE_FUNCTION, TS_CODE_COMMENT,
                  TS_CODE_NUMBER,   TS_CODE_STRING,   TS_CODE_OPERATOR,
                  TS_CODE_LISP,     TS_CODE_ENDOFLINE, TS_EQUALSSELECTION};
  m_2dMathStyles = {TS_VARIABLE,        TS_OPERATOR, TS_NUMBER,
                    TS_FUNCTION,        TS_SPECIAL_CONSTANT, TS_GREEK_CONSTANT,
                    TS_STRING,          TS_MAIN_PROMPT, TS_OTHER_PROMPT,
                    TS_LABEL,           TS_USERLABEL, TS_HIGHLIGHT,
                    TS_WARNING,         TS_ERROR, TS_ASCIIMATHS, TS_TEXT};
  m_colorOnlyStyles = {TS_TEXT_BACKGROUND, TS_DOCUMENT_BACKGROUND, TS_CELL_BRACKET,
                       TS_ACTIVE_CELL_BRACKET, TS_CURSOR, TS_SELECTION,
                       TS_EQUALSSELECTION, TS_OUTDATED};
}

bool Styles::AffectsCode(TextStyle style) const {
  for (const auto &i : m_codeStyles)
    if (style == i)
      return true;
  return false;
}

bool Styles::AffectsMathOut(TextStyle style) const {
  for (const auto &i : m_2dMathStyles)
    if (style == i)
      return true;
  return false;
}

bool Styles::AffectsColorOnly(TextStyle style) const {
  for (const auto &i : m_colorOnlyStyles)
    if (style == i)
      return true;
  return false;
}

void Styles::MakeConsistent() {
  for (const auto &style : m_codeStyles) {
    m_styles[style].SetFamily(m_styles[TS_CODE_DEFAULT].GetFamily());
    m_styles[style].SetEncoding(m_styles[TS_CODE_DEFAULT].GetEncoding());
    m_styles[style].SetFontSize(m_styles[TS_CODE_DEFAULT].GetFontSize());
    m_styles[style].SetFontName(m_styles[TS_CODE_DEFAULT].GetFontName());
    m_styles[style].SetBold(m_styles[TS_CODE_DEFAULT].IsBold());
    m_styles[style].SetItalic(m_styles[TS_CODE_DEFAULT].IsItalic());
    m_styles[style].SetSlant(m_styles[TS_CODE_DEFAULT].IsSlant());
    m_styles[style].SetStrikethrough(m_styles[TS_CODE_DEFAULT].IsStrikethrough());
    m_styles[style].SetUnderlined(m_styles[TS_CODE_DEFAULT].IsUnderlined());
    m_styles[style].CantChangeFontName(true);
    m_styles[style].CantChangeFontVariant(true);
  }

  for (const auto &style : m_2dMathStyles) {
    if ((style != TS_ASCIIMATHS) && (style != TS_TEXT)) {
      m_styles[style].SetFontSize(m_styles[TS_MATH].GetFontSize());
      m_styles[style].SetFamily(m_styles[TS_MATH].GetFamily());
      m_styles[style].SetEncoding(m_styles[TS_MATH].GetEncoding());
      m_styles[style].SetFontName(m_styles[TS_MATH].GetFontName());
      m_styles[style].CantChangeFontName(true);
    }
  }

  for (const auto &style : m_colorOnlyStyles) {
    m_styles[style].SetFamily(m_styles[TS_CODE_DEFAULT].GetFamily());
    m_styles[style].SetEncoding(m_styles[TS_CODE_DEFAULT].GetEncoding());
    m_styles[style].SetFontSize(m_styles[TS_CODE_DEFAULT].GetFontSize());
    m_styles[style].SetFontName(m_styles[TS_CODE_DEFAULT].GetFontName());
    m_styles[style].SetBold(m_styles[TS_CODE_DEFAULT].IsBold());
    m_styles[style].SetItalic(m_styles[TS_CODE_DEFAULT].IsItalic());
    m_styles[style].SetUnderlined(m_styles[TS_CODE_DEFAULT].IsUnderlined());
    m_styles[style].SetSlant(m_styles[TS_CODE_DEFAULT].IsSlant());
    m_styles[style].SetStrikethrough(m_styles[TS_CODE_DEFAULT].IsStrikethrough());
    m_styles[style].CantChangeFontName(true);
    m_styles[style].CantChangeFontVariant(true);
  }
}

const wxString &Styles::Name(TextStyle textStyle) {
  // Built once on first use (the translations must be loaded by then -- this is
  // only ever called from the configuration dialog, long after startup).
  static const std::unordered_map<TextStyle, wxString> names = {
    {TS_VARIABLE, _("Output: Variable names")},
    {TS_OPERATOR, _("Output: Operators")},
    {TS_NUMBER, _("Output: Numbers and Digits")},
    {TS_FUNCTION, _("Output: Function names")},
    {TS_SPECIAL_CONSTANT, _("Output: Special constants (%e,%i)")},
    {TS_GREEK_CONSTANT, _("Output: Latin names as greek symbols")},
    {TS_STRING, _("Output: Strings")},
    {TS_CODE_DEFAULT, _("Code Default (Maxima input)")},
    {TS_MAIN_PROMPT, _("Input labels")},
    {TS_OTHER_PROMPT, _("Maxima question labels")},
    {TS_LABEL, _("Output labels")},
    {TS_USERLABEL, _("User-defined labels")},
    {TS_HIGHLIGHT, _("Highlight (box)")},
    {TS_WARNING, _("Maxima warnings")},
    {TS_ERROR, _("Maxima errors")},
    {TS_ASCIIMATHS, _("ASCII maths")},
    {TS_TEXT, _("Standard Text")},
    {TS_HEADING6, _("Heading 6")},
    {TS_HEADING5, _("Heading 5")},
    {TS_SUBSUBSECTION, _("Subsubsection cell (Heading 4)")},
    {TS_SUBSECTION, _("Subsection cell (Heading 3)")},
    {TS_SECTION, _("Section cell (Heading 2)")},
    {TS_TITLE, _("Title cell (Heading 1)")},
    {TS_TEXT_BACKGROUND, _("Text cell background")},
    {TS_DOCUMENT_BACKGROUND, _("Document background")},
    {TS_CELL_BRACKET, _("Cell bracket fill, Inactive")},
    {TS_ACTIVE_CELL_BRACKET, _("Cell bracket fill, Active")},
    {TS_CURSOR, _("Cursor color")},
    {TS_SELECTION, _("Selection color")},
    {TS_EQUALSSELECTION, _("Color of text equal to selection")},
    {TS_OUTDATED, _("Color of Outdated cells")},
    {TS_CODE_VARIABLE, _("Code highlighting: Variables")},
    {TS_CODE_FUNCTION, _("Code highlighting: Functions")},
    {TS_CODE_COMMENT, _("Code highlighting: Comments")},
    {TS_CODE_NUMBER, _("Code highlighting: Numbers")},
    {TS_CODE_STRING, _("Code highlighting: Strings")},
    {TS_CODE_OPERATOR, _("Code highlighting: Operators")},
    {TS_CODE_LISP, _("Code highlighting: Lisp")},
    {TS_CODE_ENDOFLINE, _("Code highlighting: End of line markers")},
    {TS_MATH, _("Math Default")},
  };
  const auto it = names.find(textStyle);
  return it != names.end() ? it->second : wxm::emptyString;
}

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
