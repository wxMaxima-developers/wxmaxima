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

#ifndef _TEXTCELL_H_
#define _TEXTCELL_H_

#include "MathCell.h"

class TextCell : public MathCell
{
  public:
    TextCell();
    TextCell(wxString text) : m_text(text) { m_symbol = false; m_greek = false; }
    ~TextCell();
    MathCell* Copy(bool all);
    void Destroy();
    void SetValue(wxString text) { m_text = text; }
    void RecalculateSize(CellParser& parser, int fontsize, bool all);
    void RecalculateWidths(CellParser& parser, int fontsize, bool all);
    void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
    void SetFont(CellParser& parser, int fontsize);
    void SetForeground(CellParser& parser);
    wxString ToString(bool all);
    wxString GetDiffPart();
    bool IsOperator();
    void SetSymbol(bool symbol) { m_symbol = symbol; }
    void SetGreek(bool greek) { m_greek = greek; }
    wxString GetGreekString(CellParser& parser);
#if defined __WXGTK20__
    wchar_t* GetGreekStringUnicode();
#else
    wxString GetGreekStringIso();
#endif
    bool IsShortNum();
    void Hide(bool hide);
  protected:
    wxString m_text;
    bool m_symbol;
    bool m_greek;
};


#endif	//_TEXTCELL_H_
