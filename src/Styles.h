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
  This file declares the class Styles.
 */

#ifndef WXMAXIMA_STYLES_H
#define WXMAXIMA_STYLES_H

#include "cells/TextStyle.h"
#include <utility>
#include <vector>

class wxConfigBase;

/*! Storage for the worksheet's per-TextStyle text styles.

  Extracted from Configuration so the (large) style subsystem -- the array of
  styles plus its load/save logic -- lives in one cohesive place. Configuration
  holds a Styles instance and forwards its public style accessors to it.
 */
class Styles
{
public:
  //! The style used for a given TextStyle. Indexed like the old raw array, so
  //! both TextStyle enumerators and the plain integer indices some callers use
  //! work unchanged.
  Style &operator[](int textStyle) { return m_styles[textStyle]; }
  const Style &operator[](int textStyle) const { return m_styles[textStyle]; }

  //! Iteration over all styles (so range-for and std::fill/std::begin work).
  Style *begin() { return m_styles; }
  Style *end() { return m_styles + NUMBEROFSTYLES; }
  const Style *begin() const { return m_styles; }
  const Style *end() const { return m_styles + NUMBEROFSTYLES; }

  //! Read every persisted style from config.
  void Read(wxConfigBase *config);
  //! Write every persisted style to config.
  void Write(wxConfigBase *config) const;

  /*! The single source of truth mapping each persisted text style to its
    config-key prefix.

    Read() and Write() both iterate this list, so a style can never be read and
    written under mismatched keys (which used to cause settings to silently fail
    to round-trip). Adding a persisted style is a one-line edit here.
  */
  static const std::vector<std::pair<TextStyle, wxString>> &ConfigKeys();

  //! The human-readable, translated name of a style (used by the config dialog).
  //! Returns an empty string for out-of-range / unnamed styles.
  static const wxString &Name(TextStyle textStyle);

  // Defined inline so the lightweight unit tests (which include this header but do
  // not link Styles.cpp) can still construct a Styles / Configuration.
  Styles()
    : m_codeStyles{TS_CODE_VARIABLE, TS_CODE_FUNCTION, TS_CODE_COMMENT,
                   TS_CODE_NUMBER,   TS_CODE_STRING,   TS_CODE_OPERATOR,
                   TS_CODE_LISP,     TS_CODE_ENDOFLINE, TS_EQUALSSELECTION},
      m_2dMathStyles{TS_VARIABLE,    TS_OPERATOR,  TS_NUMBER,
                     TS_FUNCTION,    TS_SPECIAL_CONSTANT, TS_GREEK_CONSTANT,
                     TS_STRING,      TS_MAIN_PROMPT, TS_OTHER_PROMPT,
                     TS_LABEL,       TS_USERLABEL, TS_HIGHLIGHT,
                     TS_WARNING,     TS_ERROR, TS_ASCIIMATHS, TS_TEXT},
      m_colorOnlyStyles{TS_TEXT_BACKGROUND, TS_DOCUMENT_BACKGROUND, TS_CELL_BRACKET,
                        TS_ACTIVE_CELL_BRACKET, TS_CURSOR, TS_SELECTION,
                        TS_EQUALSSELECTION, TS_OUTDATED} {}

  //! The styles that share the code-default font/attributes.
  const std::vector<TextStyle> &CodeStylesList() const { return m_codeStyles; }
  //! The styles that share the 2d-math font.
  const std::vector<TextStyle> &MathStylesList() const { return m_2dMathStyles; }
  //! The styles where only the color is configurable.
  const std::vector<TextStyle> &ColorOnlyStylesList() const { return m_colorOnlyStyles; }
  bool AffectsCode(TextStyle style) const;
  bool AffectsMathOut(TextStyle style) const;
  bool AffectsColorOnly(TextStyle style) const;
  //! Propagate the code-default / math fonts to the styles that must share them.
  void MakeConsistent();

private:
  Style m_styles[NUMBEROFSTYLES];
  //! Styles that follow the code-default font/attributes.
  std::vector<TextStyle> m_codeStyles;
  //! Styles that follow the 2d-math font.
  std::vector<TextStyle> m_2dMathStyles;
  //! Styles where only the color is user-configurable.
  std::vector<TextStyle> m_colorOnlyStyles;
};

#endif // WXMAXIMA_STYLES_H
