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

#ifndef _MATHCTRL_H_
#define _MATHCTRL_H_

#include <wx/wx.h>

#include "MathCell.h"
#include "GroupCell.h"

enum {
  popid_copy,
  popid_cut,
  popid_paste,
  popid_select_all,
  popid_copy_image,
  popid_delete,
  popid_simplify,
  popid_expand,
  popid_factor,
  popid_solve,
  popid_solve_num,
  popid_integrate,
  popid_diff,
  popid_subst,
  popid_plot2d,
  popid_plot3d,
  popid_float,
  popid_edit,
  popid_reeval,
  popid_add_comment,
  popid_insert_input,
  popid_copy_tex,
#if defined __WXMSW__ || defined __WXMAC__
  popid_image_copy,
#endif
  popid_image,
};

enum {
  SELECTION_TYPE_NONE,
  SELECTION_TYPE_GROUP,
  SELECTION_TYPE_INPUT,
  SELECTION_TYPE_OUTPUT
};

class MathCtrl: public wxScrolledWindow
{
public:
  MathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
  ~MathCtrl();
  void DestroyTree();
  void DestroyTree(MathCell* tree);
  MathCell* CopyTree();
  void InsertLine(MathCell *newLine, bool forceNewLine = false);
  void Recalculate(bool scroll = true);
  void RecalculateForce();
  void RecalculateWidths();
  void RecalculateSize();
  void ClearWindow();
  bool CanCopy(bool fromActive = false)
  {
    return m_selectionStart != NULL ||
           (fromActive && m_activeCell != NULL && m_activeCell->CanCopy());
  }
  bool CanPaste()
  {
    return (m_activeCell != NULL) || (m_hCaretActive);
  }
  bool CanCut()
  {
    return m_activeCell != NULL && m_activeCell->CanCopy();
  }
  void SelectAll();
  bool CanDeleteSelection();
  bool CanAnimate() {
    return m_selectionStart != NULL && m_selectionStart == m_selectionEnd &&
      m_selectionStart->GetType() == MC_TYPE_SLIDE;
  }
  void Animate(bool run);
  void DeleteSelection(bool deletePrompt = true);
  bool CutToClipboard();
  void PasteFromClipboard();
  bool Copy();
  bool CopyInput();
  bool CopyTeX();
  bool CopyBitmap();
  bool CopyToFile(wxString file);
  bool CopyToFile(wxString file, MathCell* start, MathCell* end, bool asData = false);
  bool ExportToHTML(wxString file);
  bool ExportToMAC(wxString file);
  bool ExportToTeX(wxString file);
  wxString GetString(bool lb = false);
  MathCell* GetTree()
  {
    return m_tree;
  }
  void BreakUpCells();
  void BreakUpCells(MathCell *cell);
  void UnBreakUpCells();
  MathCell* GetLastCell();
  MathCell* GetLastPrompt();
  void SetInsertPoint(MathCell* insert)
  {
    if (insert == NULL)
      m_insertPoint = NULL;
    else
      m_insertPoint = (GroupCell *)insert;
  }
  MathCell* GetInsertPoint()
  {
    return m_insertPoint;
  }
  void ClearInsertPoint()
  {
    m_insertPoint = NULL;
  }
  MathCell* GetSelectionStart()
  {
    return m_selectionStart;
  }
  void SetSelection(MathCell* sel)
  {
    m_selectionStart = m_selectionEnd = sel;
  }
  void SetScrollTo(int to)
  {
    m_scrollTo = to;
  }
  void ScrollToBottom();
  bool CanEdit();
  void EnableEdit(bool enable = true)
  {
    m_editingEnabled = enable;
  }
  bool SelectPrevInput();
  bool SelectNextInput(bool input = false);
  bool SelectPrompt();
  void ScrollToSelectionStart(bool top = true);
  void ScrollToCell(MathCell *cell, bool top = true);
  bool SelectLastInput();
  bool SelectFirstInput();
  void SetActiveCell(MathCell *cell);
  MathCell* GetActiveCell()
  {
    return m_activeCell;
  }
  void ShowPoint(wxPoint point);
  void OnSetFocus(wxFocusEvent& event);
  void OnKillFocus(wxFocusEvent& event);
  bool IsSelected(int type);
  bool AnimationRunning() { return m_animate; }
  void PrependGroup(int id, wxString value, bool refresh, bool prepend = true);
  bool IsActiveInLast() {
    return m_activeCell != NULL && m_activeCell->GetParent() == m_last;
  }
  void SetWorkingGroup(GroupCell *group);
  bool IsSelectionInWorking();
  void SetHCaret(MathCell *where, bool active = true);
protected:
  MathCell* CopySelection();
  MathCell* CopySelection(MathCell* start, MathCell* end, bool asData = false);
  void OpenHCaret(wxString txt = wxEmptyString);
  void GetMaxPoint(int* width, int* height);
  void BreakLines();
  void OnTimer(wxTimerEvent& event);
  void OnMouseExit(wxMouseEvent& event);
  void OnMouseEnter(wxMouseEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnSize(wxSizeEvent& event);
  void OnMouseRightUp(wxMouseEvent& event);
  void OnMouseLeftUp(wxMouseEvent& event);
  void OnMouseLeftDown(wxMouseEvent& event);
  void OnMouseMotion(wxMouseEvent& event);
  void OnDoubleClick(wxMouseEvent& event);
  void OnKeyDown(wxKeyEvent& event);
  void OnChar(wxKeyEvent& event);
  void SelectPoint(wxPoint& point);
  void SelectRect();
  void AdjustSize(bool scroll = false);
  void OnEraseBackground(wxEraseEvent& event)
  { }
  void CheckUnixCopy();
  wxPoint m_down;
  wxPoint m_up;
  wxPoint m_mousePoint;
  bool m_hCaretActive; // horizontal caret
  GroupCell *m_hCaretPosition;
  GroupCell *m_hCaretPositionStart, *m_hCaretPositionEnd; // selection with caret
  bool m_leftDown;
  bool m_mouseDrag;
  bool m_selectWholeLine;
  bool m_mouseOutside;
  bool m_forceUpdate;
  GroupCell *m_tree;
  GroupCell *m_last;
  GroupCell *m_workingGroup;
  MathCell *m_selectionStart;
  MathCell *m_selectionEnd;
  int m_selectionType;
  GroupCell *m_selectionInGC;
  GroupCell *m_insertPoint;
  MathCell *m_activeCell;
  CellParser *m_selectionParser;
  bool m_switchDisplayCaret;
  bool m_editingEnabled;
  int m_scrollTo;
  wxTimer m_timer, m_caretTimer, m_animationTimer;
  bool m_animate;
  wxBitmap *m_memory;
  DECLARE_EVENT_TABLE()
};

#endif //_MATHCTRL_H_
