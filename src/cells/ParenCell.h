// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class ParenCell

  ParenCell is the Cell type that represents a math element that is kept
  between parenthesis.
*/

#ifndef PARENCELL_H
#define PARENCELL_H

#include "Cell.h"

/*! The class that represents parenthesis that are wrapped around text

  Once IsBrokenIntoLines(), the draw list (see GetBrokenCellCount()/
  GetBrokenCell()) expands this cell into the following individual cells
  instead of drawing it as a single 2D object:

  - The opening "("
  - The contents
  - The closing ")".
*/
class ParenCell final : public Cell
{
public:
  /*! \image html ParenCellGeometry.svg
      \image html ParenCellLinearGeometry.svg */
  ParenCell(GroupCell *group, Configuration *config,
 std::unique_ptr<Cell> &&inner);
  ParenCell(GroupCell *group, const ParenCell &cell);
  const CellTypeInfo &GetInfo() override;
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;

  //! ORDER MATTERS: also used, via the default GetBrokenCellCount()/
  //! GetBrokenCell(), as this cell's broken/linear draw sequence -- "(",
  //! then the (possibly multi-cell) contents, then ")", unconditionally.
  size_t GetInnerCellCount() const override { return 3; }
  Cell *GetInnerCell(size_t index) const override {
    switch (index) {
    case 0:
      return m_open.get();
    case 1:
      return m_innerCell.get();
    case 2:
      return m_close.get();
    default:
      return nullptr;
    }
  }

  Cell *GetInner() const { return m_innerCell.get(); }
  void SetInner(std::unique_ptr<Cell> inner, CellType type = MC_TYPE_DEFAULT);

  void SetPrint(bool print) { m_print = print; }

  //! \todo m_open and m_close are recalculated in handdrawn mode, too.
  void Recalculate(const AFontSize fontsize) const override;

  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;

  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  bool BreakUp() const override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

private:
  // The pointers below point to inner cells and must be kept contiguous.
  // ** This is the draw list order. All pointers must be the same:
  // ** either Cell * or std::unique_ptr<Cell>. NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_innerCell;
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.

  //! How to create a big parenthesis sign?
  mutable Configuration::drawMode m_bigParenType = Configuration::ascii;
  mutable int m_charWidth1 = 12, m_charHeight1 = 12;
  mutable int m_signWidth = 12;
  mutable CachedInteger<int> m_signHeight;

//** Bitfield objects (1 bytes)
//**
  bool m_print : 1 = true;
};

#endif // PARENCELL_H
