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

  A specialised TextCell that can display a long number in multiple modes:
  1. **Short Form**: Displays only the first few digits followed by an ellipsis 
     (e.g., "123..."). This is the default to keep the worksheet clean.
  2. **Long Form**: Displays the full number. If the number fits in the 
     horizontal space, it is drawn as a single string.
  3. **Broken Up**: If a long number exceeds the available width, it is 
     decomposed into a list of individual DigitCell objects. Each DigitCell 
     represents a single digit, acting as a potential line-break point. This 
     allows the layout engine to wrap the number precisely at the right margin 
     across multiple lines.
*/
class LongNumberCell final : public TextCell
{
public:
  /*! \image html LongNumberCellGeometry.svg
      \image html LongNumberLayout.svg */
  //! The constructor for cell that, if displayed, means that something is amiss
  LongNumberCell(GroupCell *group, Configuration *config, const wxString &number);
  LongNumberCell(GroupCell *group, const LongNumberCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  void Recalculate(const AFontSize fontsize) const override;
  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;
  void Draw(wxDC *dc, wxDC *antialiassingDC) override;
  bool BreakUp() const override;
  void SetNextToDraw(Cell *next) const override;
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
  mutable std::unique_ptr<Cell> m_innerCell;
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
