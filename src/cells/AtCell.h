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

class AtCell final : public Cell
{
public:
  AtCell(GroupCell *group, Configuration *config,
    std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&index);
  AtCell(GroupCell *group, const AtCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  int GetInnerCellCount() const override { return 2; }
  // cppcheck-suppress objectIndex
  Cell *GetInnerCell(int index) const override { return (&m_baseCell)[index].get(); }

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

private:
  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_baseCell;
  std::unique_ptr<Cell> m_indexCell;
  // The pointers above point to inner cells and must be kept contiguous.

//** Bitfield objects (0 bytes)
//**
  void InitBitFields()
  { // Keep the initialization order below same as the order
    // of bit fields in this class!
  }
};

#endif // ATCELL_H
