// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef EXPTCELL_H
#define EXPTCELL_H

#include "Cell.h"
#include "TextCell.h"
#include <memory>
#include <utility>

/*!\file

  This file declares the class ExptCell which represents a exp() or %e^x-construct.
*/

/*! This cell represents a exp() or %e^x-construct.

  Once IsBrokenIntoLines(), the draw list (see GetBrokenCellCount()/
  GetBrokenCell(), which here is simply GetInnerCellCount()/GetInnerCell())
  expands this cell into the following individual cells instead of drawing
  it as a single 2D object:

  - The base
  - "^"
  - The opening "("
  - The exponent
  - The closing ")".
*/
class ExptCell final : public Cell
{
public:
  /*! \image html ExptCellGeometry.svg */
  ExptCell(GroupCell *group, Configuration *config, std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&expt);
  ExptCell(GroupCell *group, const ExptCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  //! ORDER MATTERS: also used, via the default GetBrokenCellCount()/
  //! GetBrokenCell(), as this cell's broken/linear draw sequence -- the
  //! base, "^", "(", the exponent, ")", unconditionally.
  size_t GetInnerCellCount() const override { return 5; }
  Cell *GetInnerCell(size_t index) const override {
    switch (index) {
    case 0:
      return m_baseCell.get();
    case 1:
      return m_exp.get();
    case 2:
      return m_open.get();
    case 3:
      return m_exptCell.get();
    case 4:
      return m_close.get();
    default:
      return nullptr;
    }
  }

  //! By how much do we want to rise the power?
  double PowRise() const {return .3 * m_fontSize_Scaled;}

  void Recalculate(const AFontSize fontsize) const override;

  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;

  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  wxString GetDiffPart() const override;

  void IsMatrix(bool isMatrix) { m_isMatrix = isMatrix; }

  bool BreakUp() const override;

  void SetAltCopyText(const wxString &text) override { m_altCopyText = text; }
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

private:
  void MakeBreakupCells();

  //! Text that should end up on the clipboard if this cell is copied as text.
  wxString m_altCopyText;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** This is the draw list order. All pointers must be the same:
  // ** either Cell * or std::unique_ptr<Cell>. NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_baseCell;
  std::unique_ptr<Cell> m_exp;
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_exptCell;
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.

  mutable int m_expt_yoffset = 0;

//** Bitfield objects (1 bytes)
//**
  bool m_isMatrix : 1 = false;
};


#endif // EXPTCELL_H
