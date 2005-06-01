/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef _ABSCELL_H_
#define _ABSCELL_H_

#include "MathCell.h"

class AbsCell : public MathCell
{
  public:
    AbsCell();
    ~AbsCell();
    void Destroy();
    void SetInner(MathCell *inner);
    MathCell* Copy(bool all);
    void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
    bool BreakUp();
    void Unbreak(bool all);
	protected:
    MathCell *m_innerCell;
    MathCell *m_open, *m_close, *m_last;
    void RecalculateSize(CellParser& parser, int fontsize, bool all);
    void RecalculateWidths(CellParser& parser, int fontsize, bool all);
    void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
    wxString ToString(bool all);
};

#endif	//_ABSCELL_H_
