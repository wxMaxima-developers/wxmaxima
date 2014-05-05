///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef SQRTCELL_H
#define SQRTCELL_H

#include "MathCell.h"

class SqrtCell : public MathCell
{
public:
  SqrtCell();
  ~SqrtCell();
  MathCell* Copy(bool all);
  void Destroy();
  void SetInner(MathCell *inner);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  bool BreakUp();
  void Unbreak(bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
	wxString ToXML(bool all);	//new!!
  void SetParent(MathCell *parent, bool all);
protected:
  MathCell *m_innerCell;
  MathCell *m_open, *m_close, *m_last;
  int m_signWidth, m_signSize, m_signTop;
  int m_signType;
  double m_signFontScale;
};

#endif // SQRTCELL_H
