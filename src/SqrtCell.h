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

#ifndef SQRTCELL_H
#define SQRTCELL_H

#include "Cell.h"
#include "TextCell.h"

/*! \file

  This file defines the class for the cell type that represents a square root.
 */

/*! This class represents a square root
  
  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The SqrtCell itself
   - The opening "sqrt("
   - The contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell.
 */
class SqrtCell final : public Cell
{
public:
  SqrtCell(GroupCell *parent, Configuration **config);
  SqrtCell(const SqrtCell &cell);
  Cell *Copy() const override { return new SqrtCell(*this); }

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_innerCell); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_close); }

  void SetInner(Cell *inner);

  void RecalculateHeight(AFontSize fontsize) override;
  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  bool BreakUp() override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  CellPtr<Cell> m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_innerCell;
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.

  double m_signFontScale = 0;
  int m_signWidth = 18;
  int m_signSize = 50;
  int m_signTop = m_signSize / 2;
  int m_signType = 0;
};

#endif // SQRTCELL_H
