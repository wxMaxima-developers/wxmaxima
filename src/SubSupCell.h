// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef SUBSUPCELL_H
#define SUBSUPCELL_H

#include <memory>
#include "Cell.h"

class SubSupCell final : public Cell
{
public:
  SubSupCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  SubSupCell(const SubSupCell &cell);
  Cell *Copy() override { return new SubSupCell(*this); }
  ~SubSupCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_baseCell); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_preSupCell); }

  void SetBase(Cell *base);

  void SetIndex(Cell *index);

  void SetExponent(Cell *expt);

  void SetPreSub(Cell *index);

  void SetPreSup(Cell *index);

  void SetPostSub(Cell *index);

  void SetPostSup(Cell *index);
  
  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToXML() override;

  wxString ToOMML() override;

  wxString ToMathML() override;

  void SetNextToDraw(Cell *next) override { m_nextToDraw = next; }
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  Cell *m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  std::shared_ptr<Cell> m_baseCell;
  std::shared_ptr<Cell> m_postSubCell;
  std::shared_ptr<Cell> m_postSupCell;
  std::shared_ptr<Cell> m_preSubCell;
  std::shared_ptr<Cell> m_preSupCell;
  //! The inner cells set via SetPre* or SetPost*, but not SetBase nor SetIndex
  //! nor SetExponent.
  std::vector<std::shared_ptr<Cell>> m_scriptCells;
};

#endif // SUBSUPCELL_H
