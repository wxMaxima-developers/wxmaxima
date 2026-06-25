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
#include <wx/font.h>
#include <wx/settings.h>
#include <algorithm>
#include <unordered_map>

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

void Styles::SetDefaults() {
  std::fill(std::begin(m_styles), std::end(m_styles), Style{});

  // TODO It's a fat chance that this font actually will be monospace.
  wxFont monospace(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL,
                   wxFONTWEIGHT_NORMAL);
  m_styles[TS_ASCIIMATHS].SetFontName(monospace.GetFaceName());
  m_styles[TS_ASCIIMATHS].FontSize(12.0);
  m_styles[TS_TEXT].SetFontName(monospace.GetFaceName());
  m_styles[TS_TEXT].FontSize(12.0);
  m_styles[TS_MATH].FontSize(12.0);

  m_styles[TS_TEXT].FontSize(12);
  m_styles[TS_CODE_VARIABLE].Color(0, 128, 0).Slant();
  m_styles[TS_CODE_FUNCTION].Color(128, 0, 0).Slant();
  m_styles[TS_CODE_COMMENT].Color(64, 64, 64).Slant();
  m_styles[TS_CODE_NUMBER].Color(128, 64, 0).Slant();
  m_styles[TS_CODE_STRING].Color(0, 0, 128).Slant();
  m_styles[TS_CODE_OPERATOR].Slant();
  m_styles[TS_CODE_LISP].Color(255, 0, 128).Slant();
  m_styles[TS_CODE_ENDOFLINE].Color(128, 128, 128).Slant();
  m_styles[TS_GREEK_CONSTANT].Slant();
  m_styles[TS_HEADING6].Bold().FontSize(14);
  m_styles[TS_HEADING5].Bold().FontSize(15);
  m_styles[TS_SUBSUBSECTION].Bold().FontSize(16);
  m_styles[TS_SUBSECTION].Bold().FontSize(16);
  m_styles[TS_SECTION].Bold().Slant().FontSize(18);
  m_styles[TS_TITLE].Bold().Underlined().FontSize(24);
  m_styles[TS_WARNING].Color(wxS("orange")).Bold().FontSize(12);
  m_styles[TS_ERROR].Color(*wxRED).FontSize(12);
  m_styles[TS_MAIN_PROMPT].Color(255, 128, 128);
  m_styles[TS_OTHER_PROMPT].Color(*wxRED).Slant();
  m_styles[TS_LABEL].Color(255, 192, 128);
  m_styles[TS_USERLABEL].Color(255, 64, 0);
  m_styles[TS_CODE_DEFAULT].Bold().Slant().FontSize(12);
  m_styles[TS_STRING].Slant();
  m_styles[TS_VARIABLE].Slant();
  m_styles[TS_HIGHLIGHT].Color(*wxRED);
  m_styles[TS_TEXT_BACKGROUND].Color(*wxWHITE);
  m_styles[TS_DOCUMENT_BACKGROUND].Color(*wxWHITE);
  m_styles[TS_ACTIVE_CELL_BRACKET].Color(*wxRED);
  m_styles[TS_SELECTION].Color(wxSYS_COLOUR_HIGHLIGHT);
  m_styles[TS_EQUALSSELECTION]
    .Color(wxSYS_COLOUR_HIGHLIGHT)
    .ChangeLightness(150);
  m_styles[TS_OUTDATED].Color(153, 153, 153);
}

void Styles::SetDarkDefaults() {
  // The dark set reuses the light set's fonts, sizes and bold/slant structure,
  // then overrides the colors with a curated dark palette. Foreground styles
  // that inherit the system text color in light mode would be invisible on a
  // dark background, so they get an explicit light color here. Assumes the light
  // set has already been built (SetDefaults()).
  std::copy(std::begin(m_styles), std::end(m_styles), std::begin(m_stylesDark));

  const wxColour fg(220, 220, 220);   // default light-on-dark foreground
  const wxColour bg(30, 30, 30);      // dark background

  m_stylesDark[TS_DOCUMENT_BACKGROUND].Color(bg);
  m_stylesDark[TS_TEXT_BACKGROUND].Color(bg);

  // Foreground text / math / structure styles -> light.
  for (auto s : {TS_TEXT, TS_MATH, TS_ASCIIMATHS, TS_CODE_DEFAULT, TS_NUMBER,
                 TS_FUNCTION, TS_SPECIAL_CONSTANT, TS_GREEK_CONSTANT, TS_STRING,
                 TS_VARIABLE, TS_OPERATOR, TS_HEADING6, TS_HEADING5,
                 TS_SUBSUBSECTION, TS_SUBSECTION, TS_SECTION, TS_TITLE})
    m_stylesDark[s].Color(fg);

  // Code highlighting -> brighter variants that read on a dark background.
  m_stylesDark[TS_CODE_VARIABLE].Color(120, 200, 120);
  m_stylesDark[TS_CODE_FUNCTION].Color(230, 130, 130);
  m_stylesDark[TS_CODE_COMMENT].Color(150, 150, 150).Slant();
  m_stylesDark[TS_CODE_NUMBER].Color(220, 160, 100);
  m_stylesDark[TS_CODE_STRING].Color(130, 170, 230);
  m_stylesDark[TS_CODE_OPERATOR].Color(fg);
  m_stylesDark[TS_CODE_LISP].Color(255, 130, 190);
  m_stylesDark[TS_CODE_ENDOFLINE].Color(170, 170, 170);

  // Cursor / cell bracket inherit the (dark) system text color in light mode, so
  // they would be invisible on a dark background -- give them light colors.
  m_stylesDark[TS_CURSOR].Color(fg);
  m_stylesDark[TS_CELL_BRACKET].Color(150, 150, 150);

  // Outdated cells dim against the dark background.
  m_stylesDark[TS_OUTDATED].Color(120, 120, 120);
}

void Styles::ClearCaches() {
  for (auto &s : m_styles)
    s.ClearCache();
  for (auto &s : m_stylesDark)
    s.ClearCache();
}

// The dark set is stored under a parallel "StyleDark/..." key namespace, so the
// light set keeps the historical "Style/..." keys (existing configs migrate into
// the light set untouched, and a fresh config simply has no dark keys yet -> the
// curated dark defaults stay in place).
static wxString DarkKey(const wxString &lightPrefix) {
  return wxS("StyleDark") + lightPrefix.Mid(5); // "Style/X/" -> "StyleDark/X/"
}

void Styles::Read(wxConfigBase *config) {
  for (const auto &[style, prefix] : ConfigKeys()) {
    m_styles[style].Read(config, prefix);
    m_stylesDark[style].Read(config, DarkKey(prefix));
  }
}

void Styles::Write(wxConfigBase *config) const {
  for (const auto &[style, prefix] : ConfigKeys()) {
    m_styles[style].Write(config, prefix);
    m_stylesDark[style].Write(config, DarkKey(prefix));
  }
}

const std::vector<std::pair<TextStyle, wxString>> &Styles::ConfigKeys() {
  // The single source of truth for the config-key prefix of every persisted text
  // style. Read() and Write() both iterate this, so read and write can never
  // disagree on a key. Every style needs its OWN key: TS_CURSOR used to share the
  // active-cell-bracket key, which broke dark mode (the cursor must be light while
  // the active bracket stays red -- one slot can't hold both).
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
    {TS_CURSOR, wxS("Style/Cursor/")},
    {TS_SELECTION, wxS("Style/Selection/")},
    {TS_EQUALSSELECTION, wxS("Style/EqualsSelection/")},
    {TS_OUTDATED, wxS("Style/Outdated/")},
  };
  return keys;
}
