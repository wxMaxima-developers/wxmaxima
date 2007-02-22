///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef _MATRCELL_H_
#define _MATRCELL_H_

#include "MathCell.h"

#include <vector>

using namespace std;

class MatrCell : public MathCell
{
public:
  MatrCell();
  ~MatrCell();
  void Destroy();
  MathCell* Copy(bool all);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  void AddNewCell(MathCell* cell)
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
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
  void SetSpecialFlag(bool special) { m_specialMatrix = special; }
protected:
  int m_matWidth;
  int m_matHeight;
  bool m_specialMatrix;
  vector<MathCell*> m_cells;
  vector<int> m_widths;
  vector<int> m_drops;
  vector<int> m_centers;
};

#endif //_MATRCELL_H_
