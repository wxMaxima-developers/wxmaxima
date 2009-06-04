///
///  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
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

#ifndef _MINIMATHCTRL_H_
#define _MINIMATHCTRL_H_

#include <wx/wx.h>

#include "MathCell.h"

class MiniMathCtrl: public wxScrolledWindow
{
public:
  MiniMathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
  ~MiniMathCtrl();
  void DestroyTree();
  void DestroyTree(MathCell* tree);
  MathCell* CopyTree();
  void Recalculate(bool force = false);
  void RecalculateForce();
  void ClearWindow();
  bool CanCopy()
  {
    return m_selectionStart != NULL;
  }
  void SelectAll();
  bool Copy();
  bool CopyCells();
  bool CopyTeX();
  bool CopyBitmap();
  wxString GetString(bool lb = false);
  MathCell* GetTree()
  {
    return m_tree;
  }
  MathCell* GetSelectionStart()
  {
    return m_selectionStart;
  }
  void SetSelection(MathCell* sel)
  {
    m_selectionStart = m_selectionEnd = sel;
  }
  void ScrollToSelectionStart();
  void ScrollToCell(MathCell *cell);
  void ShowPoint(wxPoint point);
  void OnSetFocus(wxFocusEvent& event);
  void OnKillFocus(wxFocusEvent& event);
  bool IsSelected(int type);
protected:
  MathCell* CopySelection();
  MathCell* CopySelection(MathCell* start, MathCell* end, bool asData = false);
  void GetMaxPoint(int* width, int* height);
  void OnTimer(wxTimerEvent& event);
  void OnMouseExit(wxMouseEvent& event);
  void OnMouseEnter(wxMouseEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnSize(wxSizeEvent& event);
  void OnMouseRightDown(wxMouseEvent& event);
  void OnMouseLeftUp(wxMouseEvent& event);
  void OnMouseLeftDown(wxMouseEvent& event);
  void OnMouseMotion(wxMouseEvent& event);
  void OnDoubleClick(wxMouseEvent& event);
  void OnKeyDown(wxKeyEvent& event);
  void OnChar(wxKeyEvent& event);
  void ClickNDrag(wxPoint down, wxPoint up);
  void AdjustSize();
  void OnEraseBackground(wxEraseEvent& event)
  { }
  void CheckUnixCopy();
  wxPoint m_down;
  wxPoint m_up;
  wxPoint m_mousePoint;
  bool m_leftDown;
  bool m_mouseDrag;
  bool m_mouseOutside;
  MathCell *m_tree;
  MathCell *m_last;
  MathCell *m_selectionStart;
  MathCell *m_selectionEnd;
  CellParser *m_selectionParser;
  wxBitmap *m_memory;
  DECLARE_EVENT_TABLE()
};

#endif //_MINIMATHCTRL_H_
