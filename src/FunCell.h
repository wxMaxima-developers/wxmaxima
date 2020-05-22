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

#ifndef FUNCELL_H
#define FUNCELL_H

#include "Cell.h"
/*! \file

  This file declares the class FunCell() that represents a maxima function.
 */

/*! FunCell represents a maxiam function no special visual representation exists for 

  Examples of functions with special visual representation would be:
   - SqrtCell
   - ExptCell
   - AbsCell
   - ConjugateCell

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The FunCell itself
   - The function name"
   - The ParenCell containing its contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell. 
*/
class FunCell final : public Cell
{
public:
  FunCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  FunCell(const FunCell &cell);
  Cell *Copy() override {return new FunCell(*this);}
  ~FunCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_nameCell); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_argCell); }

  void SetName(Cell *name);

  void SetArg(Cell *arg);

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToXML() override;

  wxString ToOMML() override;

  bool BreakUp() override;

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override {return m_nextToDraw;}

private:
  Cell *m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_nameCell;
  std::shared_ptr<Cell> m_argCell;
  Cell *m_nameCell_Last;
  Cell *m_argCell_Last;
};


#endif // FUNCELL_H
