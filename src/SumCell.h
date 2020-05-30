// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class SumCell

  SumCell is the Cell type that represents maxima's <code>sum()</code>, 
  <code>lsum</code> and <code>product()</code> 
  commands.
*/

#ifndef SUMCELL_H
#define SUMCELL_H

#include "Cell.h"
#include "ParenCell.h"

enum
{
  SM_SUM,
  SM_PROD
};

class SumCell final : public Cell
{
public:
  SumCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  SumCell(const SumCell &cell);
  Cell *Copy() override { return new SumCell(*this); }
  ~SumCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_under); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_paren); }
  
  void RecalculateHeight(int fontsize) override;
  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  void SetBase(Cell *base);

  void SetUnder(Cell *under);

  void SetOver(Cell *over);

  void SetSumStyle(int style) { m_sumStyle = style; }

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToXML() override;

  wxString ToOMML() override;

  void SetNextToDraw(Cell *next) override { m_nextToDraw = next; }
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  Cell *m_nextToDraw = {};

  std::shared_ptr<Cell> m_base;
  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_under;
  std::shared_ptr<Cell> m_over;
  std::shared_ptr<Cell> m_paren;
  // The pointers above point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_displayedBase;
  int m_signHeight;
  double m_signWidth;
  int m_sumStyle;
  int m_signWCenter;
  int m_signTop;
};

#endif // SUMCELL_H
