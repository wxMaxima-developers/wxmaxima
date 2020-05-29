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

/*! \file
  This file declares the class LimitCell

  LimitCell is the Cell type that represents maxima's <code>limit()</code> command.
*/

#ifndef LIMITCELL_H
#define LIMITCELL_H

#include "Cell.h"
#include "TextCell.h"

class LimitCell final : public Cell
{
public:
  LimitCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  LimitCell(const LimitCell &cell);
  Cell *Copy() override { return new LimitCell(*this); }
  ~LimitCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_base); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_close); }

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  void SetBase(Cell *base);

  void SetUnder(Cell *under);

  void SetName(Cell *name);

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToXML() override;

  wxString ToOMML() override;

  wxString ToMathML() override;

  bool BreakUp() override;

  void SetNextToDraw(Cell *next) override;
  Cell *GetNextToDraw() const override {return m_nextToDraw;}

private:
  Cell *m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_base;
  std::shared_ptr<Cell> m_under;
  std::shared_ptr<Cell> m_name;
  std::shared_ptr<Cell> m_open;
  std::shared_ptr<Cell> m_comma;
  std::shared_ptr<Cell> m_close;
  Cell *m_name_last;
  Cell *m_base_last;
  Cell *m_under_last;
};

#endif // LIMITCELL_H
