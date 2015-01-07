//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef ABSCELL_H
#define ABSCELL_H

#include "MathCell.h"

/*! \file

  This file defines the class for the cell type that represents an abs(x) block.
 */

/*! A cell that represents an abs(x) block

 */
class AbsCell : public MathCell
{
public:
  AbsCell();
  ~AbsCell();
  void Destroy();
  void SetInner(MathCell *inner);
  MathCell* Copy();
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  bool BreakUp();
  void Unbreak(bool all);
  void SetParent(MathCell *parent);
protected:
  MathCell *m_innerCell;
  MathCell *m_open, *m_close, *m_last;
  void RecalculateSize(CellParser& parser, int fontsize);
  void RecalculateWidths(CellParser& parser, int fontsize);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString();
  wxString ToTeX();
  wxString ToXML();
};

#endif // ABSCELL_H
