// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class SetCell

  SetCell is the Cell type that represents a set
*/

#ifndef SETCELL_H
#define SETCELL_H

#include "Cell.h"
#include "ListCell.h"
#include "TextCell.h"

/*! The class that represents parenthesis that are wrapped around text

  Once IsBrokenIntoLines(), the draw list (see GetBrokenCellCount()/
  GetBrokenCell(), inherited unchanged from ListCell) expands this cell into
  the following individual cells instead of drawing it as a single 2D object:

  - The opening "["
  - The contents
  - The closing "]".
*/
class SetCell final : public ListCell
{
public:
  /*! \image html ListCellGeometry.svg */
  SetCell(GroupCell *group, Configuration *config,
 std::unique_ptr<Cell> &&inner);
  SetCell(GroupCell *group, const SetCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  wxString ToMatlab() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;
};

#endif // SETCELL_H
