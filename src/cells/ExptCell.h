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

  A matrix power A^^n (Maxima's mncexpt, see IsMatrix()) draws its exponent
  between angle brackets, A^⟨n⟩, like Maxima's own 2D display and tex() do:
  in Maxima A^n of a matrix is the elementwise power, so an unmarked A^^n
  would look exactly like a different result (GH #1005). Broken into lines
  it reads A^^n instead.
*/
class ExptCell final : public Cell
{
public:
  /*! \image html ExptCellGeometry.svg */
  ExptCell(GroupCell *group, Configuration *config, std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&expt);
  ExptCell(GroupCell *group, const ExptCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  //! ORDER MATTERS: the first five are also this cell's broken/linear draw
  //! sequence (see GetBrokenCellCount()) -- the base, "^", "(", the exponent,
  //! ")", unconditionally. The last two are the angle brackets of a matrix
  //! power, which only the 2D form draws.
  size_t GetInnerCellCount() const override { return 7; }
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
    case 5:
      return m_matrixOpen.get();
    case 6:
      return m_matrixClose.get();
    default:
      return nullptr;
    }
  }

  //! The linear form never shows the angle brackets of a matrix power.
  size_t GetBrokenCellCount() const override { return 5; }

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

  /*! Is this a matrix power (A^^n) rather than an ordinary one (A^n)?

    Draws angle brackets around the exponent and turns the linear form's
    "^" into "^^".
  */
  void IsMatrix(bool isMatrix);
  bool IsMatrix() const { return m_isMatrix; }
  //! The tooltip explaining a matrix power; SubSupCell uses it, too.
  static wxString MatrixPowerToolTip();

  bool BreakUp() const override;

  void SetAltCopyText(const wxString &text) override { m_altCopyText = text; }
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

private:
  void MakeBreakupCells();
  //! The width the exponent takes in the 2D form, angle brackets included.
  wxCoord ExponentWidth() const;
  //! The exponent as MathML, angle brackets included.
  wxString ExponentToMathML() const;

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
  std::unique_ptr<Cell> m_matrixOpen;
  std::unique_ptr<Cell> m_matrixClose;
  // The pointers above point to inner cells and must be kept contiguous.

  mutable int m_expt_yoffset = 0;

//** Bitfield objects (1 bytes)
//**
  bool m_isMatrix : 1 = false;
};


#endif // EXPTCELL_H
