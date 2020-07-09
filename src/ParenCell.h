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
#include "TextCell.h"

/*! The class that represents parenthesis that are wrapped around text

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The ParenCell itself
   - The opening "("
   - The contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell.
 */
class ParenCell final : public Cell
{
public:
  ParenCell(GroupCell *parent, Configuration **config);
  ParenCell(const ParenCell &cell);
  Cell *Copy() const override { return new ParenCell(*this); }

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_innerCell); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_close); }

  Cell *GetInner() const { return m_innerCell.get(); }
  void SetInner(Cell *inner, CellType type = MC_TYPE_DEFAULT);
  void SetInner(std::unique_ptr<Cell> inner, CellType type = MC_TYPE_DEFAULT);

  void SetPrint(bool print) { m_print = print; }

  void RecalculateHeight(AFontSize fontsize) override;
  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  bool BreakUp() override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToXML() override;

  void SetNextToDraw(Cell *next) override;
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  void SetFont(AFontSize fontsize);

  CellPtr<Cell> m_nextToDraw;
  // The pointers below point to inner cells and must be kept contiguous.
  std::unique_ptr<Cell> m_innerCell;
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_close;

  //! How to create a big parenthesis sign?
  Configuration::drawMode m_bigParenType = Configuration::ascii;
  int m_numberOfExtensions = 0;
  int m_charWidth = 12, m_charHeight = 12;
  int m_charWidth1 = 12, m_charHeight1 = 12;
  int m_signWidth = 12, m_signHeight = 50;
  int  m_signTopHeight = 12, m_signBotHeight = 12, m_extendHeight = 12;
  bool m_print = true;
};

#endif // PARENCELL_H
