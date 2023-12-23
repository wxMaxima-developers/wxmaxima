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

#ifndef VISIBLYINVALIDCELL_H
#define VISIBLYINVALIDCELL_H

#include "TextCell.h"

/*! A visibly invalid cell

  A placeholder that, if everything works as it should, is overridden, before it is displayed.
*/
class VisiblyInvalidCell final : public TextCell
{
public:
  //! The constructor for cell that, if displayed, means that something is amiss
  VisiblyInvalidCell(GroupCell *group, Configuration *config);
  //! Constructor for a cell that shows that something is amiss using a custom tooltip
  VisiblyInvalidCell(GroupCell *group, Configuration *config, wxString &&toolTip);
  VisiblyInvalidCell(GroupCell *group, Configuration *config, const wxString *toolTip);
  VisiblyInvalidCell(GroupCell *group, const VisiblyInvalidCell &cell);
//  std::unique_ptr<Cell> Copy(GroupCell *cell) const override;
  const CellTypeInfo &GetInfo() override;

private:
//** Bitfield objects (0 bytes)
//**
  static void InitBitFields_VisiblyInvalidCell()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
    }
};

#endif // VISIBLYINVALIDCELL_H
