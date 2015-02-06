//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef SQRTCELL_H
#define SQRTCELL_H

#include "MathCell.h"

class SqrtCell : public MathCell
{
public:
  SqrtCell();
  ~SqrtCell();
  MathCell* Copy();
  void Destroy();
  void SetInner(MathCell *inner);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void RecalculateSize(CellParser& parser, int fontsize);
  void RecalculateWidths(CellParser& parser, int fontsize);
  void Draw(CellParser& parser, wxPoint point, int fontsize);
  bool BreakUp();
  void Unbreak();
  wxString ToString();
  wxString ToTeX();
  wxString ToXML();
  void SetParent(MathCell *parent);
protected:
  MathCell *m_innerCell;
  MathCell *m_open, *m_close, *m_last;
  int m_signWidth, m_signSize, m_signTop;
  int m_signType;
  double m_signFontScale;
};

#endif // SQRTCELL_H
