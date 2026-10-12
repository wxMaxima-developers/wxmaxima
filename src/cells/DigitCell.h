// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef DIGITCELL_H
#define DIGITCELL_H

#include <wx/regex.h>
#include "TextCell.h"
#include <memory>

/*! A Text cell that display a digit of a more-than-one-line-wide number
 */
class DigitCell : public TextCell
{
public:
  DigitCell(GroupCell *group, Configuration *config, const wxString &text = {}, TextStyle style = TS_NUMBER);
  DigitCell(GroupCell *group, const DigitCell &cell);
  virtual ~DigitCell(){}
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  void Recalculate(const AFontSize fontsize) const override;
  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;
  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  /*! How many digits the whole number this group belongs to has (GH #192).

    0 if that number isn't one DigitGroupGaps() would group at all. Decides,
    together with the configuration, whether this group is followed by the
    small gap that separates digit groups; the configuration can change after
    LongNumberCell::BreakUp() has made this cell, so this can't be decided
    up front.
  */
  void SetNumberDigits(size_t digits) { m_numberDigits = digits; }

private:
  //! Does a digit-group gap follow this group? See SetNumberDigits().
  bool GapFollows() const;
  //! See SetNumberDigits()
  size_t m_numberDigits = 0;
};

#endif // DIGITCELL_H
