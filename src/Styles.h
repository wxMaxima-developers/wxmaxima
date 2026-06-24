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

private:
  Style m_styles[NUMBEROFSTYLES];
};

#endif // WXMAXIMA_STYLES_H
