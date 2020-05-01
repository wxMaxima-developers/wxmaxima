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

#ifndef ATCELL_H
#define ATCELL_H

#include "Cell.h"

class AtCell : public Cell
{
public:
  AtCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  AtCell(const AtCell &cell);
  Cell *Copy() override {return new AtCell(*this);}
  ~AtCell();

  //! This class can be derived from wxAccessible which has no copy constructor
  AtCell &operator=(const AtCell&) = delete;

  InnerCells GetInnerCells() const override;
  
  void SetBase(Cell *base);
  void SetIndex(Cell *index);

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  virtual void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToXML() override;

  wxString ToOMML() override;

  wxString ToMathML() override;

protected:
  std::shared_ptr<Cell> m_baseCell;
  std::shared_ptr<Cell> m_indexCell;
};

#endif // ATCELL_H
