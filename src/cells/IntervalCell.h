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
  This file declares the class IntervalCell

  IntervalCell is the Cell type that represents a list
 */

#ifndef INTERVALCELL_H
#define INTERVALCELL_H

#include "Cell.h"
#include "TextCell.h"

/*! The class that represents parenthesis that are wrapped around text

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The IntervalCell itself
   - The opening "["
   - The contents
   - The closing "]".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell.
 */
class IntervalCell : public Cell
{
public:
  IntervalCell(GroupCell *group, Configuration *config, std::unique_ptr<Cell> &&start,
               std::unique_ptr<Cell> &&end);
  IntervalCell(GroupCell *group, const IntervalCell &cell);
  virtual std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  virtual const CellTypeInfo &GetInfo() override;

  int GetInnerCellCount() const override { return 8; }
  // cppcheck-suppress objectIndex
  Cell *GetInnerCell(int index) const override { return (&m_open)[index].get(); }

  void Recalculate(AFontSize fontsize) override;

  virtual void Draw(wxPoint point) override;

  bool BreakUp() override;

  wxString ToMathML() const override;
  virtual wxString ToMatlab() const override;
  wxString ToOMML() const override;
  virtual wxString ToString() const override;
  virtual wxString ToTeX() const override;
  virtual wxString ToXML() const override;

  void SetNextToDraw(Cell *next) override;

protected:
  void DrawBigLeftOpenBracket(wxDC *dc, wxPoint point);
  void DrawBigRightOpenBracket(wxDC *dc, wxPoint point);
  bool m_leftBracketOpensLeft;
  bool m_rightBracketOpensRight;
  // The pointers below point to inner cells and must be kept contiguous.
  // ** This is the draw list order. All pointers must be the same:
  // ** either Cell * or std::unique_ptr<Cell>. NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_openBracket;
  std::unique_ptr<Cell> m_start;
  std::unique_ptr<TextCell> m_comma;
  std::unique_ptr<TextCell> m_ellipsis;
  std::unique_ptr<Cell> m_stop;
  std::unique_ptr<Cell> m_closeBracket;
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.

  int m_signWidth = 12, m_signHeight = -1;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initialization order below same as the order
    // of bit fields in this class!
    m_drawAsAscii = true;
  }
  //! How to create a big parenthesis sign?
  bool m_drawAsAscii : 1 /* InitBitFields */;
};

#endif // INTERVALCELL_H
