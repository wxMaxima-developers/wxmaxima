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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef MATRCELL_H
#define MATRCELL_H

#include "Cell.h"

#include <vector>

using namespace std;

class MatrCell : public Cell
{
public:
  MatrCell(Cell *parent, Configuration **config, CellPointers *cellPointers);

  ~MatrCell();

  std::list<Cell *> GetInnerCells();

  Cell *Copy();

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  virtual void Draw(wxPoint point);

  void AddNewCell(Cell *cell)
  {
    m_cells.push_back(cell);
  }

  void NewRow()
  {
    m_matHeight++;
  }

  void NewColumn()
  {
    m_matWidth++;
  }

  void SetDimension();

  wxString ToString();

  wxString ToMatlab();

  wxString ToTeX();

  wxString ToMathML();

  wxString ToOMML();

  wxString ToXML();

  void SetSpecialFlag(bool special)
  { m_specialMatrix = special; }

  void SetInferenceFlag(bool inference)
  { m_inferenceMatrix = inference; }

  void RowNames(bool rn)
  { m_rowNames = rn; }

  void ColNames(bool cn)
  { m_colNames = cn; }

  void RoundedParens(bool rounded)
  { m_roundedParens = rounded;}
protected:
  unsigned int m_matWidth;
  bool m_roundedParens;
  unsigned int m_matHeight;
  bool m_specialMatrix, m_inferenceMatrix, m_rowNames, m_colNames;
  vector<Cell *> m_cells;
  vector<int> m_widths;
  vector<int> m_drops;
  vector<int> m_centers;
};

#endif // MATRCELL_H
