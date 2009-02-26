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
  popid_add_comment,
  popid_insert_input,
  popid_copy_tex,
#if defined __WXMSW__ || defined __WXMAC__
  popid_image_copy,
#endif
  popid_image,
  popid_evaluate
};

enum {
  CLICK_TYPE_NONE,
  CLICK_TYPE_GROUP_SELECTION,
  CLICK_TYPE_INPUT_SELECTION,
  CLICK_TYPE_OUTPUT_SELECTION
};

class EvaluationQueueElement {
  public:
    EvaluationQueueElement(GroupCell* gr) {
      group = gr;
      next = NULL;
    }
    ~EvaluationQueueElement() {
    }
    GroupCell* group;
    EvaluationQueueElement* next;
};

// A simple FIFO queue with manual removal of elements
class EvaluationQueue
{
  public:
    EvaluationQueue() {
      m_queue = NULL;
      m_last = NULL;
    }
    ~EvaluationQueue() {
    }

    bool IsInQueue(GroupCell* gr) {
      EvaluationQueueElement* tmp = m_queue;
      while (tmp != NULL) {
        if (tmp->group == gr)
          return true;
        tmp = tmp->next;
      }
      return false;
    }

    void AddToQueue(GroupCell* gr) {
      if (gr->GetEditable()->GetType() != MC_TYPE_INPUT) // dont add cells which can't be evaluated
        return;
      EvaluationQueueElement* newelement = new EvaluationQueueElement(gr);
      if (m_last == NULL)
        m_queue = m_last = newelement;
      else {
        m_last->next = newelement;
        m_last = newelement;
      }
    }

    void RemoveFirst() {
      if (m_queue == NULL)
        return; // shouldn't happen
      EvaluationQueueElement* tmp = m_queue;
      if (m_queue == m_last) {
        m_queue = m_last = NULL;
      }
      else
      m_queue = m_queue->next;

      delete tmp;
    }

    GroupCell* GetFirst() {
    if (m_queue != NULL)
      return m_queue->group;
    else
      return NULL; // queu is empty
    }
  protected:
    EvaluationQueueElement* m_queue;
    EvaluationQueueElement* m_last;
};

class MathCtrl: public wxScrolledWindow
{
public:
  MathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
  ~MathCtrl();
  void DestroyTree();
  void DestroyTree(MathCell* tree);
  MathCell* CopyTree();
  void InsertLine(MathCell *newLine, bool forceNewLine = false, bool hide = false);
  void Recalculate(bool force = false);
  void RecalculateForce();
  void ClearWindow();
  void ResetInputPrompts();
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
  bool CopyCells();
  bool CopyTeX();
  bool CopyBitmap();
  bool CopyToFile(wxString file);
  bool CopyToFile(wxString file, MathCell* start, MathCell* end, bool asData = false);
  bool ExportToHTML(wxString file);
  bool ExportToMAC(wxString file);
	bool ExportToWDR(wxString file);	//export to xml compatible file
  bool ExportToTeX(wxString file);
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
  bool CanEdit();
  void EnableEdit(bool enable = true)
  {
    m_editingEnabled = enable;
  }
  bool ActivatePrevInput();
  bool ActivateNextInput(bool input = false);
  void ScrollToSelectionStart();
  void ScrollToCell(MathCell *cell);
  bool ActivateLastInput();
  bool ActivateFirstInput();
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
  GroupCell *PrependGroup(int id, wxString value, bool refresh, bool prepend = true);
  bool IsActiveInLast() {
    return m_activeCell != NULL && m_activeCell->GetParent() == m_last;
  }
  void SetWorkingGroup(GroupCell *group);
  bool IsSelectionInWorking();
  void SetHCaret(MathCell *where);
  void OpenHCaret(wxString txt = wxEmptyString, int type = MC_TYPE_INPUT);
  void ShowHCaret();
  bool CanUndo();
  void Undo();
  bool IsSaved() { return m_saved; }
  void SetSaved(bool saved) { m_saved = saved; }
  // methods related to evaluation queue
  void AddDocumentToEvaluationQueue();
  void AddSelectionToEvaluationQueue();
  void AddCellToEvaluationQueue(GroupCell* gc);
  void ClearEvaluationQueue();
  EvaluationQueue* m_evaluationQueue;
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
  bool m_hCaretActive; // horizontal caret
  GroupCell *m_hCaretPosition; // group above hcaret, NULL for the top
  GroupCell *m_hCaretPositionStart, *m_hCaretPositionEnd; // selection with caret
  bool m_leftDown;
  bool m_mouseDrag;
  bool m_mouseOutside;
  GroupCell *m_tree;
  GroupCell *m_last;
  GroupCell *m_workingGroup;
  MathCell *m_selectionStart;
  MathCell *m_selectionEnd;
  int m_clickType;
  GroupCell *m_clickInGC;
  MathCell *m_activeCell;
  CellParser *m_selectionParser;
  bool m_switchDisplayCaret;
  bool m_editingEnabled;
  wxTimer m_timer, m_caretTimer, m_animationTimer;
  bool m_animate;
  wxBitmap *m_memory;
  bool m_saved;
  DECLARE_EVENT_TABLE()
};

#endif //_MATHCTRL_H_
