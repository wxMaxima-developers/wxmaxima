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

#ifndef FUNCELL_H
#define FUNCELL_H

#include "MathCell.h"

class FunCell : public MathCell
{
public:
  FunCell();
  ~FunCell();
  MathCell* Copy(bool all);
  void Destroy();
  void SetName(MathCell *base);
  void SetArg(MathCell *index);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString();
  wxString ToTeX();
  wxString ToXML();
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  bool BreakUp();
  void Unbreak(bool all);
  void SetParent(MathCell *parent, bool all);
protected:
  MathCell *m_nameCell;
  MathCell *m_argCell;
};


#endif // FUNCELL_H
