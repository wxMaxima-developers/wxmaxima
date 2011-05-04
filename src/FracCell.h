///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _FRACCELL_H_
#define _FRACCELL_H_

#include "MathCell.h"

enum {
  FC_NORMAL,
  FC_CHOOSE,
  FC_DIFF
};

class FracCell : public MathCell
{
public:
  FracCell();
  ~FracCell();
  MathCell* Copy(bool all);
  void Destroy();
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  void SetFracStyle(int style)
  {
    m_fracStyle = style;
  }
  void SetNum(MathCell* num);
  void SetDenom(MathCell* denom);
  bool IsOperator()
  {
    return true;
  }
  void SelectInner(wxRect& rect, MathCell **first, MathCell **last);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
	wxString ToXML(bool all);	//new!
  void SetExponentFlag();
  bool BreakUp();
  void SetupBreakUps();
  void Unbreak(bool all);
  void SetParent(MathCell *parent, bool all);
protected:
  MathCell *m_num;
  MathCell *m_denom;
  MathCell *m_open1, *m_open2, *m_close1, *m_close2, *m_divide;
  MathCell *m_last1, *m_last2;
  bool m_exponent;
  int m_fracStyle;
  int m_expDivideWidth;
};

#endif //_FRACCELL_H_
