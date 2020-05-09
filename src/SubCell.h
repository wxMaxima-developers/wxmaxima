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

#ifndef SUBCELL_H
#define SUBCELL_H

#include "Cell.h"

class SubCell : public Cell
{
public:
  SubCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  SubCell(const SubCell &cell);
  Cell *Copy() override {return new SubCell(*this);}

  ~SubCell();

  SubCell operator=(const SubCell&) = delete;

  InnerCellIterator InnerBegin() const override { return &m_baseCell; }
  InnerCellIterator InnerEnd() const override { return &m_indexCell+1; }

  void SetBase(Cell *base);

  void SetIndex(Cell *index);

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  virtual void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToXML() override;

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override {return m_nextToDraw;}
  
private:
    Cell *m_nextToDraw;
protected:
  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_baseCell;
  std::shared_ptr<Cell> m_indexCell;
};

#endif // SUBCELL_H
