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

/*!\file

  This file declares the class ExptCell which represents a exp() or %e^x-construct.
 */

/*! This cell represents a exp() or %e^x-construct.

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The ExptCell itself
   - The opening "exp("
   - The contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell. 
 */
class ExptCell final : public Cell
{
public:
  ExptCell(GroupCell *parent, Configuration **config, std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&expt);
  ExptCell(const ExptCell &cell);
  std::unique_ptr<Cell> Copy() const override;
  const CellTypeInfo &GetInfo() override;

  int GetInnerCellCount() const override { return 5; }
  Cell *GetInnerCell(int index) const override { return (&m_baseCell)[index].get(); }

  //! By how much do we want to rise the power?
  double PowRise() const {return .3 * m_fontSize_Scaled;}
  
  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  wxString GetDiffPart() const override;

  void IsMatrix(bool isMatrix) { m_isMatrix = isMatrix; }

  bool BreakUp() override;

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

  int m_expt_yoffset = 0;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
    m_isMatrix = false;
  }
  bool m_isMatrix : 1 /* InitBitFields */;
};


#endif // EXPTCELL_H
