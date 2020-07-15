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
  MatrCell(GroupCell *parent, Configuration **config);
  MatrCell(const MatrCell &cell);
  std::unique_ptr<Cell> Copy() const override;

  InnerCellIterator InnerBegin() const override
  { return m_cells.empty() ? InnerCellIterator{} : InnerCellIterator(&m_cells.front()); }
  InnerCellIterator InnerEnd() const override
  { return m_cells.empty() ? InnerCellIterator{} : ++InnerCellIterator(&m_cells.back()); }

  void RecalculateHeight(AFontSize fontsize) override;
  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  void AddNewCell(Cell *cell) { m_cells.emplace_back(cell); }

  void NewRow() { m_matHeight++; }
  void NewColumn() { m_matWidth++; }

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

  void RoundedParens(bool rounded) { m_roundedParens = rounded;}

  void SetNextToDraw(Cell *next) override { m_nextToDraw = next; }
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

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

  std::vector<int> m_widths;
  std::vector<DropCenter> m_dropCenters;
  CellPtr<Cell> m_nextToDraw;

  unsigned int m_matWidth = 0;
  unsigned int m_matHeight = 0;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
    m_roundedParens = false;
    m_specialMatrix = false;
    m_inferenceMatrix = false;
    m_rowNames = false;
    m_colNames = false;
  }
  bool m_roundedParens : 1 /* InitBitFields */;
  bool m_specialMatrix : 1 /* InitBitFields */;
  bool m_inferenceMatrix : 1 /* InitBitFields */;
  bool m_rowNames : 1 /* InitBitFields */;
  bool m_colNames : 1 /* InitBitFields */;
};

#endif // MATRCELL_H
