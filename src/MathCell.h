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

#ifndef _MATHCELL_H_
#define _MATHCELL_H_

#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define MIN(a,b) ((a)>(b) ? (b) : (a))
#define ABS(a) ((a)>=0 ? (a) : -(a))
#define SCALE_PX(px, scale) ((int)(((double)(px))*(scale) + 0.5))

#include <wx/wx.h>
#include "CellParser.h"
#include "TextStyle.h"

enum {
  TC_VARIABLE,
  TC_OPERATOR,
  TC_NUMBER,
  TC_PROMPT,
  TC_MAIN_PROMPT,
  TC_INPUT,
  TC_ERROR,
  TC_LABEL
};

class MathCell
{
  public:
    MathCell();
    virtual ~MathCell();
    virtual void Destroy() = 0;
    virtual MathCell* Copy(bool all) = 0;
    int GetHeight() { return m_height; }
    int GetWidth() { return m_width; }
    int GetCenter() { return m_center; }
    int GetDrop() { return m_height - m_center; }
    MathCell* GetNext() { return m_next; }
    MathCell* GetPrevious() { return m_previous; }
    int GetType() { return m_style; }
    void AppendCell(MathCell *p_next);
    virtual void RecalculateSize(CellParser& parser, int fontsize, bool all);
    virtual void RecalculateWidths(CellParser& parser, int fontsize, bool all);
    virtual void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
    virtual wxString GetDiffPart();
    virtual bool IsOperator();
    virtual wxString ToString(bool all);
    void SelectRect(wxRect& rect, MathCell** first, MathCell** last);
    void SelectFirst(wxRect& rect, MathCell** first);
    void SelectLast(wxRect& rect, MathCell** last);
    virtual void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
    bool IsCompound();
    bool DrawThisCell(CellParser& parser, wxPoint point);
    int GetMaxDrop();
    int GetMaxCenter();
    int GetMaxHeight();
    int GetFullWidth(double scale);
    int GetLineWidth(double scale);
    void ResetData();
    void ForceBreakLine(bool force) { m_forceBreakLine = m_breakLine = force; }
    void BreakLine(bool breakLine) { m_breakLine = breakLine; }
    void BreakPage(bool breakPage) { m_breakPage = breakPage; }
    bool ContainsPoint(wxPoint& point) { return GetRect().Inside(point); }
    virtual wxRect GetRect(bool all = false);
    void SetSkip(bool skip) { m_bigSkip = skip; }
    void SetStyle(int style) { m_style = style; }
    void DrawBoundingBox(wxDC& dc, bool all = false);
    bool BreakLineHere() { return m_breakLine || m_forceBreakLine; }
    bool BreakPageHere() { return m_breakPage; }
    int GetCurrentX() { return m_currentPoint.x; }
    int GetCurrentY() { return m_currentPoint.y; }
    bool ContainsRect(wxRect& big, bool all = true);
    void SetPen(CellParser& parser);
    void UnsetPen(CellParser& parser);
    MathCell* m_next;
    MathCell* m_previous;
    MathCell* m_nextToDraw;
    wxPoint m_currentPoint;
    bool m_bigSkip;
    bool m_nextToDrawIsNext;
  protected:
    int m_height;
    int m_maxHeight;
    int m_width;
    int m_fullWidth;
    int m_lineWidth;
    int m_center;
    int m_maxCenter;
    int m_maxDrop;
    int m_style;
    bool m_breakPage;
    bool m_breakLine;
    bool m_forceBreakLine;
};

#endif	//_MATHCELL_H_
