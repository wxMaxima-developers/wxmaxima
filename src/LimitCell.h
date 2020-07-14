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

/* ! The cell type that represents a <code>limit</code> command

   \todo If the limit text is very short we should add more space
   between the "lim" and the following text.
 */
class LimitCell final : public Cell
{
public:
  LimitCell(GroupCell *parent, Configuration **config);
  LimitCell(const LimitCell &cell);
  Cell *Copy() const override { return new LimitCell(*this); }

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_base); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_close); }

  void RecalculateHeight(AFontSize fontsize) override;
  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  void SetBase(Cell *base);

  void SetUnder(Cell *under);

  void SetName(Cell *name);

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  bool BreakUp() override;

  void SetNextToDraw(Cell *next) override;
  Cell *GetNextToDraw() const override {return m_nextToDraw;}

private:
  CellPtr<Cell> m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_base;
  std::unique_ptr<Cell> m_under;
  std::unique_ptr<Cell> m_name;
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_comma;
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.
};

#endif // LIMITCELL_H
