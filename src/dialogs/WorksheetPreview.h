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
  Declares the sample worksheet the configuration dialog's "Worksheet" tab
  shows to preview what its display settings do.
*/

#ifndef WORKSHEETPREVIEW_H
#define WORKSHEETPREVIEW_H

#include <array>
#include <memory>
#include <wx/string.h>

class Configuration;
class GroupCell;

namespace wxm {

/*! The sample worksheet of the configuration dialog's "Worksheet" tab

  A few cells, each with the output Maxima would send for it, so no Maxima
  is needed: labels (one user-defined, one automatic) and multiplication
  signs, special symbols and a Greek letter, a long number, a narrow and a
  wide fraction for the layout strategy and a matrix too large for the
  window. Each Topic starts with a text cell naming it; when
  a setting changes, the dialog scrolls that heading to the top, so the
  cells the setting affects are in view.
*/
struct WorksheetPreview {
  //! What part of the preview a setting affects
  enum class Topic {
    //! Labels, the label width, indenting maths and multiplication signs
    labels,
    //! %e, %i and %pi with or without their percent sign, and a Greek letter
    specialSymbols,
    //! 200!, as long a number as the digit settings need
    longNumbers,
    /*! A narrow and a wide fraction

      The wide one is too wide to stay 2D unless "2D, whenever possible" is
      chosen; the narrow one is linear only if "Prefer 1D" is.
    */
    layoutStrategy,
    //! A matrix too large for the window
    oversizedMatrix,
    count
  };

  //! The preview's cells, to hand to Worksheet::InsertGroupCells()
  std::unique_ptr<GroupCell> tree;
  //! The text cell each topic starts with; owned by tree
  std::array<GroupCell *, static_cast<size_t>(Topic::count)> headings{};
  //! The code cell whose output is the oversized matrix; owned by tree
  GroupCell *matrixGroup = nullptr;

  GroupCell *HeadingFor(Topic topic) const {
    return headings[static_cast<size_t>(topic)];
  }

  //! The oversized matrix has this many rows and as many columns
  static constexpr int MatrixSize = 40;

  //! Builds the preview's cells with this configuration
  static WorksheetPreview Make(Configuration *config);

  /*! What Maxima sends for genmatrix(lambda([i,j],100*i+j),n,n), n = MatrixSize

    The entries tell where in the matrix one is, which makes it obvious
    which rows and columns were left out.
  */
  static wxString MatrixXml();

  /*! What Maxima sends for expand((x+1)^n), n = WideFractionPower

    The numerator of the layout strategy's wide fraction: about twice as
    wide as the preview has room for.
  */
  static wxString ExpandedBinomialXml();
  //! The power of x+1 ExpandedBinomialXml() expands
  static constexpr int WideFractionPower = 12;

  //! The decimal digits of 200!, a number long enough to be shortened by default
  static wxString Factorial200();
};

} // namespace wxm

#endif // WORKSHEETPREVIEW_H
