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

#ifndef LONGNUMBERCELL_H
#define LONGNUMBERCELL_H

#include "TextCell.h"

/*! A cell containing a long number

  A specialised TextCell, that can display a long number, or shorten it using an ellipsis.
*/
class LongNumberCell final : public TextCell
{
public:
  //! The constructor for cell that, if displayed, means that something is amiss
  LongNumberCell(GroupCell *group, Configuration *config, const wxString &number);
  LongNumberCell(GroupCell *group, const LongNumberCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  void Recalculate(const AFontSize fontsize) const override;
  void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override;
  bool BreakUp() override;
  void SetNextToDraw(Cell *next) override;
  size_t GetInnerCellCount() const override { if(m_innerCell) return 1; else return 0; }
  // cppcheck-suppress objectIndex
  Cell *GetInnerCell(size_t index) const override;

protected:
  virtual void UpdateDisplayedText() const override;
private:
  //** Large objects (144 bytes)
  //**
  //! The first few digits
  mutable wxString m_numStart;
  //! The inividual digits, if this cell is broken into lines
  std::unique_ptr<Cell> m_innerCell;
  //! The "not all digits displayed" message.
  mutable wxString m_ellipsis;

  //** 4-byte objects (12 bytes)
  //**
  mutable int m_numStartWidth = 0;
  mutable int m_ellipsisWidth = 0;

  //** Bitfield objects (0 bytes)
  //**
  static void InitBitFields_LongNumberCell()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
    }
};

#endif // LONGNUMBERCELL_H
