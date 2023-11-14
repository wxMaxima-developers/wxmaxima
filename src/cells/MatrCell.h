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

#ifndef MATRCELL_H
#define MATRCELL_H

#include "Cell.h"

#include <vector>

class MatrCell final : public Cell
{
public:
  MatrCell(GroupCell *group, Configuration *config);
  MatrCell(GroupCell *group, const MatrCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  size_t GetInnerCellCount() const override { return m_cells.size(); }
  Cell *GetInnerCell(size_t index) const override { return m_cells[index].get(); }
  Cell *GetInnerCell(long x, long y) const {
    return m_cells[static_cast<size_t>(x) * m_matWidth + y].get(); }

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override;

  void AddNewCell(std::unique_ptr<Cell> &&cell);

  void NewRow() { m_matHeight++; m_dropCenters.emplace_back(-1, -1);}
  void NewColumn() { m_matWidth++; m_widths.emplace_back(-1);}

  void SetDimension();

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  void SetSpecialFlag(bool special) { m_specialMatrix = special; }

  void SetInferenceFlag(bool inference) { m_inferenceMatrix = inference; }

  void RowNames(bool rn) { m_rowNames = rn; }

  void ColNames(bool cn) { m_colNames = cn; }

  void RoundedParens()  { m_parenType = paren_rounded;}
  void BracketParens()  { m_parenType = paren_brackets;}
  void StraightParens() { m_parenType = paren_straight;}
  void AngledParens()   { m_parenType = paren_angled;}

private:
  struct DropCenter
  {
    int drop = {}, center = {};
    constexpr int Sum() const { return drop + center; }
    constexpr DropCenter() = default;
    constexpr DropCenter(int drop, int center) : drop(drop), center(center) {}
  };

  //! Collection of pointers to inner cells.
  std::vector<std::unique_ptr<Cell>> m_cells;

  std::vector<wxCoord> m_widths;
  std::vector<DropCenter> m_dropCenters;

  size_t m_matWidth = 0;
  size_t m_matHeight = 0;

  enum parenType : int8_t
  {
    paren_rounded = 0,
    paren_brackets = 1,
    paren_angled = 2,
    paren_straight = 3
  };
//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
      m_parenType = paren_rounded;
      m_specialMatrix = false;
      m_inferenceMatrix = false;
      m_rowNames = false;
      m_colNames = false;
    }
  uint8_t m_parenType : 2 /* InitBitFields */;
  bool m_specialMatrix : 1 /* InitBitFields */;
  bool m_inferenceMatrix : 1 /* InitBitFields */;
  bool m_rowNames : 1 /* InitBitFields */;
  bool m_colNames : 1 /* InitBitFields */;
};

#endif // MATRCELL_H
