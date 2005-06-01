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

#ifndef _MATHCTRL_H_
#define _MATHCTRL_H_

#include <wx/wx.h>

#include "MathCell.h"

class MathCtrl:public wxScrolledWindow
{
  public:
    MathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
   ~MathCtrl();
    void DestroyTree();
    void DestroyTree(MathCell* tree);
    MathCell* CopyTree();
    void AddLine(MathCell *newLine, bool forceNewLine = false);
    void Recalculate(bool scroll = true);
    void RecalculateForce();
    void Recalculate(MathCell *cell, bool scroll = true);
    void RecalculateWidths();
    void RecalculateWidths(MathCell *cell);
    void RecalculateSize();
    void RecalculateSize(MathCell *cell);
    void ClearWindow();
    bool CanCopy() { return m_selectionStart != NULL; }
    bool CanDeleteSelection();
    void DeleteSelection();
    bool Copy(bool lb = false);
    bool CopyBitmap();
    bool CopyToFile(wxString file);
    bool CopyToFile(wxString file, MathCell* start, MathCell* end, bool asData = false);
    bool ExportToHTML(wxString file);
    wxString GetString();
    MathCell* GetTree() { return m_tree; }
    void BreakUpCells();
    void BreakUpCells(MathCell *cell);
    void UnBreakUpCells();
  protected:
    MathCell* CopySelection();
    MathCell* CopySelection(MathCell* start, MathCell* end, bool asData = false);
    void GetMaxPoint(int* width, int* height);
    void BreakLines(MathCell* cell);
    void OnTimer(wxTimerEvent& event);
    void OnMouseExit(wxMouseEvent& event);
    void OnMouseEnter(wxMouseEvent& event);
    void OnPaint(wxPaintEvent& event);
    void OnSize(wxSizeEvent& event);
    void OnMouseLeftUp(wxMouseEvent& event);
    void OnMouseLeftDown(wxMouseEvent& event);
    void OnMouseMotion(wxMouseEvent& event);
    void OnKeyUp(wxKeyEvent& event);
    void SelectPoint(wxPoint& point);
    void SelectRect(wxPoint one, wxPoint two);
    void AdjustSize(bool scroll);
    wxPoint m_down;
    wxPoint m_up;
    wxPoint m_mousePoint;
    bool m_leftDown;
    bool m_mouseDrag;
    bool m_selectWholeLine;
    bool m_mouseOutside;
    bool m_forceUpdate;
    MathCell *m_tree;
    MathCell *m_last;
    MathCell *m_firstVisible;
    MathCell *m_lastVisible;
    MathCell *m_selectionStart;
    MathCell *m_selectionEnd;
    wxTimer m_timer;
    wxBitmap *m_memory;
    DECLARE_EVENT_TABLE()
};

#endif //_MATHCTRL_H_
