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

#ifndef CONJUGATECELL_H
#define CONJUGATECELL_H

#include "Cell.h"
#include "TextCell.h"

/*! \file

  This file defines the class for the cell type that represents a conjugate(x) block.
 */

/*! A cell that represents a conjugate(x) block

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The ConjugateCell itself
   - The opening "conjugate("
   - The contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell.
 */
class ConjugateCell final : public Cell
{
public:
  ConjugateCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  ConjugateCell(const ConjugateCell &cell);
  Cell *Copy() override { return new ConjugateCell(*this); }
  ~ConjugateCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_innerCell); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_close); }

  void SetInner(Cell *inner);

  bool BreakUp() override;

  void SetNextToDraw(Cell *next) override;
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  Cell *m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_innerCell;
  std::shared_ptr<Cell> m_open;
  std::shared_ptr<Cell> m_close;
  Cell *m_last;

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToXML() override;
};

#endif // CONJUGATECELL_H
