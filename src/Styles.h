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
  //! Indexing and iteration return the *active* set (light or dark), so all the
  //! display code reads the right palette without knowing which is in use.
  Style &operator[](int textStyle) { return active()[textStyle]; }
  const Style &operator[](int textStyle) const { return active()[textStyle]; }
  Style *begin() { return active(); }
  Style *end() { return active() + NUMBEROFSTYLES; }
  const Style *begin() const { return active(); }
  const Style *end() const { return active() + NUMBEROFSTYLES; }

  //! Select which set indexing/iteration return.
  void SetUseDark(bool dark) { m_useDark = dark; }
  bool UsingDark() const { return m_useDark; }
  //! Invalidate the font caches of every style in BOTH sets.
  void ClearCaches();

  //! Reset every style to wxMaxima's built-in (light) defaults.
  void SetDefaults();

  //! Reset every style to wxMaxima's built-in *dark-mode* defaults (a curated
  //! palette for a dark background; same fonts/structure as SetDefaults()).
  void SetDarkDefaults();

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
  Style *active() { return m_useDark ? m_stylesDark : m_styles; }
  const Style *active() const { return m_useDark ? m_stylesDark : m_styles; }
  //! Fill the given array with the built-in light defaults (shared by
  //! SetDefaults() and, as a base, SetDarkDefaults()).
  void SetLightDefaultsInto(Style *styles);

  //! The light style set (also the persisted set / config-dialog target for now).
  Style m_styles[NUMBEROFSTYLES];
  //! The dark style set.
  Style m_stylesDark[NUMBEROFSTYLES];
  //! Which set operator[]/begin()/end() return.
  bool m_useDark = false;
  //! Styles that follow the code-default font/attributes.
  std::vector<TextStyle> m_codeStyles;
  //! Styles that follow the 2d-math font.
  std::vector<TextStyle> m_2dMathStyles;
  //! Styles where only the color is user-configurable.
  std::vector<TextStyle> m_colorOnlyStyles;
};

#endif // WXMAXIMA_STYLES_H
