///
///  Copyright (C) 2008 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef GROUPCELL_H_
#define GROUPCELL_H_

#include "MathCell.h"

class GroupCell: public MathCell
{
public:
  GroupCell();
  ~GroupCell();
  void Destroy();
  void SetInput(MathCell *input);
  void SetOutput(MathCell *output);
  void AppendInput(MathCell *cell);
  void AppendOutput(MathCell *cell);
  MathCell* Copy(bool all);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void BreakLines(int fullWidth);
protected:
  MathCell *m_input, *m_output;
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
};

#endif /* GROUPCELL_H_ */
