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

#ifndef _INTCELL_H_
#define _INTCELL_H_

#include "MathCell.h"
#include "Setup.h"

enum {
  INT_DEF,
  INT_IDEF
};

class IntCell : public MathCell
{
public:
  IntCell();
  ~IntCell();
  MathCell* Copy(bool all);
  void Destroy();
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  void SetBase(MathCell* base);
  void SetUnder(MathCell* under);
  void SetOver(MathCell* name);
  void SetVar(MathCell* var);
  void SetIntStyle(int style)
  {
    m_intStyle = style;
  }
  wxString ToString(bool all);
  wxString ToTeX(bool all);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void SetParent(MathCell *parent, bool all);
protected:
  MathCell *m_base;
  MathCell *m_under;
  MathCell *m_over;
  MathCell *m_var;
  int m_signSize;
  int m_signWidth;
  int m_signMiddle;
  int m_intStyle;
#if defined __WXMSW__ || (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  int m_charHeight, m_charWidth;
#endif
};

#endif  //_UNDERCELL_H_
