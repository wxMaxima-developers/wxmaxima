///
///  Copyright (C) 2004-2008 Andrej Vodopivec <andrejv@users.sourceforge.net>
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


#include "wxMaxima.h"
#include "MathCtrl.h"
#include "Bitmap.h"
#include "Setup.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "SlideShowCell.h"

#include <wx/clipbrd.h>
#include <wx/config.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>

#define SCROLL_UNIT 10
#define CARET_TIMER_TIMEOUT 500
#define ANIMATION_TIMER_TIMEOUT 300
#define LEFT_BORDER 13

void AddLineToFile(wxTextFile& output, wxString s, bool unicode = true);

enum
{
  TIMER_ID,
  CARET_TIMER_ID,
  ANIMATION_TIMER_ID
};

MathCtrl::MathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size) :
  wxScrolledWindow(parent, id, position, size,
  wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
  | wxSUNKEN_BORDER
#endif
  ) {
  m_scrollTo = -1;
  m_tree = NULL;
  m_memory = NULL;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_selectionType = SELECTION_TYPE_NONE;
  m_selectionInGC = NULL;
  m_last = NULL;
  m_insertPoint = NULL;
  m_hCaretActive = false;
  m_hCaretPosition = NULL; // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_activeCell = NULL;
  m_leftDown = false;
  m_mouseDrag = false;
  m_mouseOutside = false;
  m_forceUpdate = false;
  m_editingEnabled = true;
  m_switchDisplayCaret = true;
  m_timer.SetOwner(this, TIMER_ID);
  m_caretTimer.SetOwner(this, CARET_TIMER_ID);
  m_animationTimer.SetOwner(this, ANIMATION_TIMER_ID);
  m_animate = false;
  m_workingGroup = NULL;
  AdjustSize(false);
}

MathCtrl::~MathCtrl() {
  if (m_tree != NULL)
    DestroyTree();
  if (m_memory != NULL)
    delete m_memory;
}

/***
 * Redraw the control
 */
void MathCtrl::OnPaint(wxPaintEvent& event) {
  wxPaintDC dc(this);

  wxMemoryDC dcm;

  // Get the font size
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  // Prepare data
  wxRect rect = GetUpdateRegion().GetBox();
  //printf("Updating rect [%d, %d] -> [%d, %d]\n", rect.x, rect.y, rect.width, rect.height);
  wxSize sz = GetSize();
  int tmp, top, bottom, drop;
  CalcUnscrolledPosition(0, rect.GetTop(), &tmp, &top);
  CalcUnscrolledPosition(0, rect.GetBottom(), &tmp, &bottom);

  // Thest if m_memory is NULL (resize event)
  if (m_memory == NULL)
    m_memory = new wxBitmap(sz.x, sz.y);

  // Prepare memory DC
  wxString bgColStr= wxT("white");
  config->Read(wxT("Style/Background/color"), &bgColStr);
  SetBackgroundColour(wxTheColourDatabase->Find(bgColStr));

  dcm.SelectObject(*m_memory);
  dcm.SetBackground(*(wxTheBrushList->FindOrCreateBrush(GetBackgroundColour(), wxSOLID)));
  dcm.Clear();
  PrepareDC(dcm);
  dcm.SetMapMode(wxMM_TEXT);
  dcm.SetBackgroundMode(wxTRANSPARENT);

  // Draw content
  if (m_tree != NULL) {
    wxPoint point;
    point.x = MC_BASE_INDENT;
    point.y = MC_BASE_INDENT + m_tree->GetMaxCenter();
    // Draw tree
    MathCell* tmp = m_tree;
    drop = tmp->GetMaxDrop();
    CellParser parser(dcm);
    parser.SetBouns(top, bottom);

    while (tmp != NULL) {
      tmp->m_currentPoint.x = point.x;
      tmp->m_currentPoint.y = point.y;
      if (tmp->DrawThisCell(parser, point))
        tmp->Draw(parser, point, MAX(fontsize, MC_MIN_SIZE), false);
      if (tmp->m_next != NULL) {
        if (tmp->m_next->BreakLineHere()) {
          point.x = MC_BASE_INDENT;
          point.y += drop + tmp->m_next->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_GROUP_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        } else
          point.x += tmp->GetWidth() + MC_CELL_SKIP;
      }
      tmp = tmp->m_next;
    }

    // Draw selection
    if (m_selectionStart != NULL)
    {
      MathCell* tmp = m_selectionStart;

      dcm.SetLogicalFunction(wxXOR);
      dcm.SetBrush(*wxWHITE_BRUSH);
      dcm.SetPen(wxNullPen);

      if (m_selectionStart->GetType() == MC_TYPE_GROUP) // selection of groups
      {
        while (tmp != NULL)
        {

          wxRect rect = tmp->GetRect();
          // TODO globally define x coordinates of the left GC brackets
          dcm.DrawRectangle( 1, rect.GetTop() - 2, 13, rect.GetHeight() + 4);

          if (tmp == m_selectionEnd)
              break;
          tmp = tmp->m_next;
        }

      }
      else {

      // We have a selection with click
      if (m_selectWholeLine) {
        if (m_selectionStart == m_selectionEnd) {
          if (m_selectionStart->GetType() != MC_TYPE_SLIDE && m_activeCell != tmp)
            m_selectionStart->DrawBoundingBox(dcm);
        } else {
          while (tmp != NULL && tmp->m_isBroken)
            tmp = tmp->m_nextToDraw;
          if (tmp != NULL)
            tmp->DrawBoundingBox(dcm, true);
          while (tmp != NULL) {
            tmp = tmp->m_nextToDraw;
            if (tmp == NULL)
              break;
            if (tmp->BreakLineHere() && !tmp->m_isBroken)
              tmp->DrawBoundingBox(dcm, true);
            if (tmp == m_selectionEnd)
              break;
          }
        }
      }
      // We have a selection by dragging
      else {
        while (1) {
          if (!tmp->m_isBroken && !tmp->m_isHidden && tmp->GetType() != MC_TYPE_SLIDE &&
              m_activeCell != tmp)
            tmp->DrawBoundingBox(dcm, false);
          if (tmp == m_selectionEnd)
            break;
          tmp = tmp->m_nextToDraw;
          if (tmp == NULL)
            break;
        }
      }
    }
    }
  }
  //
  // Draw horizontal caret
  //
  if (m_hCaretActive && m_hCaretPositionStart == NULL)
  {
    dcm.SetLogicalFunction(wxXOR);
    dcm.SetPen(*wxWHITE_PEN);

    if (m_hCaretPosition == NULL)
      dcm.DrawLine( 0, 5, 3000, 5);
    else {
      wxRect currentGCRect = m_hCaretPosition->GetRect();
      int caretY = ((int) MC_GROUP_SKIP) / 2 + currentGCRect.GetBottom() + 1;
      dcm.DrawLine( 0, caretY, 3000,  caretY);
    }

  }

  // Blit the memory image to the window
  dcm.SetDeviceOrigin(0, 0);
  dc.Blit(0, rect.GetTop(), sz.x, rect.GetBottom() - rect.GetTop() + 1, &dcm,
      0, rect.GetTop());
}


/***
 * Add a new line
 */
void MathCtrl::InsertLine(MathCell *newCell, bool forceNewLine) {

  SetActiveCell(NULL);

  GroupCell *tmp = m_insertPoint;
  if (m_hCaretActive && newCell->GetType() != MC_TYPE_ERROR)
    tmp = m_hCaretPosition;

  if (tmp == NULL)
    tmp = m_last;

  if (tmp == NULL && newCell->GetType() != MC_TYPE_MAIN_PROMPT) {
    GroupCell *newGroup = new GroupCell;
    TextCell *prompt = new TextCell;
    prompt->SetValue(wxT("!!"));
    prompt->SetType(MC_TYPE_MAIN_PROMPT);
    newGroup->SetInput(prompt);
    newGroup->SetSpecial(true);
    tmp = m_tree = m_last = newGroup;
  }

  newCell->ForceBreakLine(forceNewLine);

  if (newCell->GetType() == MC_TYPE_MAIN_PROMPT) {
    SetWorkingGroup(NULL);
    GroupCell *newGroup = new GroupCell;
    newGroup->SetInput(newCell);
    if (newCell->GetValue() != wxT("/*")) {
      EditorCell *input = new EditorCell();
      input->SetType(MC_TYPE_INPUT);
      newGroup->AppendInput(input);
    }
    if (m_last == NULL) {
      m_last = m_tree = newGroup;
    }
    else {
      m_last->AppendCell(newGroup);
      m_last = newGroup;
    }
    tmp = newGroup;
  }

  else if (newCell->GetType() == MC_TYPE_INPUT) {
    tmp->AppendInput(newCell);
    SetWorkingGroup(tmp);
  }

  else {
    if (newCell->GetType() == MC_TYPE_TITLE ||
        newCell->GetType() == MC_TYPE_SECTION ||
        newCell->GetType() == MC_TYPE_COMMENT)
      tmp->SetSpecial(true);
    tmp->AppendOutput(newCell);
    if (newCell->GetType() == MC_TYPE_PROMPT) {
      m_workingGroup = tmp;
      OpenHCaret();
    }
  }

  while (newCell != NULL) {
    newCell->SetParent(tmp, false);
    newCell = newCell->m_next;
  }

  m_selectionStart = NULL;
  m_selectionEnd = NULL;

  Recalculate(true);

  Refresh();
}

/***
 * Prepend a new cell
 */
void MathCtrl::PrependGroup(int type, wxString value, bool refresh, bool prepend) {
  GroupCell *where;

  if (m_selectionStart != NULL) {
    where = (GroupCell *)m_selectionStart->GetParent();
    if (!prepend && where->m_next != NULL)
      where = (GroupCell *)where->m_next;
  }

  else if (m_hCaretActive) {
    if (m_hCaretPosition == NULL)
      where = m_tree;
    else
      where = m_hCaretPosition;
  }

  else
    where = m_last;

  GroupCell *newGroup = new GroupCell;

  TextCell *prompt = new TextCell;
  if (type == MC_TYPE_INPUT)
    prompt->SetValue(wxT(">> "));
  else {
    newGroup->SetSpecial(true);
    prompt->SetValue(wxT("/*"));
  }
  prompt->SetType(MC_TYPE_MAIN_PROMPT);

  newGroup->SetInput(prompt);

  EditorCell *newCell = new EditorCell;
  newCell->SetType(type);
  newCell->SetValue(value);

  if (type != MC_TYPE_INPUT)
    newGroup->AppendOutput(newCell);
  else
    newGroup->AppendInput(newCell);

  newGroup->SetParent(newGroup, false);

  if (where == m_tree) {
    newGroup->m_next = m_tree;
    newGroup->m_nextToDraw = m_tree;
    m_tree->m_previous = newGroup;
    m_tree->m_previousToDraw = newGroup;
    m_tree = newGroup;

    Recalculate(false);
  }

  else {
    where->m_previous->m_next = newGroup;
    where->m_previous->m_nextToDraw = newGroup;
    newGroup->m_previous = where->m_previous;
    newGroup->m_previousToDraw = where->m_previous;

    newGroup->m_next = where;
    newGroup->m_nextToDraw = where;
    where->m_previous = newGroup;
    where->m_previousToDraw = newGroup;

    Recalculate(false);
  }

  SetActiveCell(NULL);
  ScrollToSelectionStart(false);

  if (refresh)
    Refresh();
}

/***
 * Recalculate dimensions of cells
 */
void MathCtrl::RecalculateForce() {
  if (m_tree != NULL) {
    m_forceUpdate = true;
    Recalculate(false);
    m_forceUpdate = false;
  }
}

/***
 * Recalculate size of this line
 */
void MathCtrl::Recalculate(bool scroll) {
  UnBreakUpCells();
  RecalculateWidths();
  BreakUpCells();
  BreakLines();
  RecalculateSize();
  AdjustSize(scroll);
}

/***
 * Recalculate widths of cells
 */
void MathCtrl::RecalculateWidths() {
  MathCell *tmp = m_tree;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(m_forceUpdate);

  while (tmp != NULL) {
    tmp->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
    tmp = tmp->m_next;
  }
}

/***
 * Recalculate sizes of cells
 */
void MathCtrl::RecalculateSize() {
  MathCell *tmp = m_tree;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(m_forceUpdate);

  wxPoint point;
  point.x = MC_BASE_INDENT;
  point.y = MC_BASE_INDENT + m_tree->GetMaxCenter();

  while (tmp != NULL) {
    tmp->m_currentPoint.x = point.x;
    tmp->m_currentPoint.y = point.y;
    tmp->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);
    point.y += tmp->GetMaxDrop();
    tmp = tmp->m_next;
    if (tmp != NULL)
      point.y += tmp->GetMaxCenter();
    point.y += MC_GROUP_SKIP;
  }
}

/***
 * Resize the controll
 */
void MathCtrl::OnSize(wxSizeEvent& event) {
  wxDELETE(m_memory);

  if (m_tree != NULL) {
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    Recalculate(false);
  }

  Refresh();
  wxScrolledWindow::OnSize(event);
}

/***
 * Clear the window
 */
void MathCtrl::ClearWindow() {
  if (m_tree != NULL) {
    SetActiveCell(NULL);
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    m_last = NULL;
    DestroyTree();
  }
  Refresh();
  Scroll(0, 0);
}

/***
 * Right mouse - popup-menu
 */
void MathCtrl::OnMouseRightUp(wxMouseEvent& event) {
  wxMenu* popupMenu = new wxMenu();

  if (m_activeCell == NULL) {
    /* If we have no selection or we are not in editing mode don't popup a menu!*/
    if (m_editingEnabled == false)
      return;

    if (IsSelected(MC_TYPE_IMAGE))
    {
#if defined __WXMSW__
      popupMenu->Append(popid_image_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
#endif
      popupMenu->Append(popid_image, _("Save image"), wxEmptyString, wxITEM_NORMAL);
    }

    else {
      if (CanCopy()) {
        popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_copy_tex, _("Copy TeX"), wxEmptyString, wxITEM_NORMAL);
#if defined __WXMSW__
        popupMenu->Append(popid_copy_image, _("Copy as image"),
            wxEmptyString, wxITEM_NORMAL);
#endif
        if (CanDeleteSelection())
          popupMenu->Append(popid_delete, _("Delete selection"), wxEmptyString, wxITEM_NORMAL);

        popupMenu->AppendSeparator();
      }

      if (CanEdit()) {
        if (m_selectionStart->GetType() == MC_TYPE_INPUT) {
          popupMenu->Append(popid_edit, _("Edit input"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_reeval, _("Re-evaluate input"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_insert_input, _("Insert input"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_add_comment, _("Insert text"), wxEmptyString, wxITEM_NORMAL);
        } else {
          popupMenu->Append(popid_edit, _("Edit text"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_reeval, _("Re-evaluate input"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_insert_input, _("Insert input"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_add_comment, _("Insert text"), wxEmptyString, wxITEM_NORMAL);
        }
      }

      else {
        popupMenu->Append(popid_float, _("To float"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_solve, _("Solve ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_solve_num, _("Find root ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_simplify, _("Simplify expression"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_factor, _("Factor expression"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_expand, _("Expand expression"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_subst, _("Substitute ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_integrate, _("Integrate ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_diff, _("Differentiate ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_plot2d, _("Plot 2d ..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_plot3d, _("Plot 3d ..."), wxEmptyString, wxITEM_NORMAL);
      }
    }
  }

  else {
    popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_cut, _("Cut"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_paste, _("Paste"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->AppendSeparator();
    popupMenu->Append(popid_select_all, _("Select all"), wxEmptyString, wxITEM_NORMAL);

    popupMenu->Enable(popid_copy, m_activeCell->CanCopy());
    popupMenu->Enable(popid_cut, m_activeCell->CanCopy());
  }

  PopupMenu(popupMenu);

  delete popupMenu;
}

/***
 * Left mouse button down - selection handling
 *
 * Sets m_selectionType and m_selectionInGC according to what we clicked,
 * and selects appropriately.
 * m_selectionType is used in SelectRect when click-draging to determine what kind of selection
 * behaviour we want.
 *
 * - check in which GroupCell it falls
 * - if it falls between groupCells activate caret and SELECTION_TYPE_GROUP
 * - if it falls within a groupcell investigate where did it fall (input or output)
 */
void MathCtrl::OnMouseLeftDown(wxMouseEvent& event) {
  m_animate = false;
  m_leftDown = true;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

  if (m_tree == NULL)
    return ;

  // default when clicking
  m_selectionType = SELECTION_TYPE_NONE;
  m_selectionStart = m_selectionEnd = NULL;
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_hCaretPosition = NULL;
  m_hCaretActive = false;
  SetActiveCell(NULL);

  GroupCell * tmp = (GroupCell *) m_tree;
  wxRect rect;
  GroupCell * clickedBeforeGC = NULL;
  GroupCell * clickedInGC = NULL;
  while (tmp != NULL) { // go through all groupcells
    rect = tmp->GetRect();
    if (m_down.y < rect.GetTop() )
    {
      clickedBeforeGC = (GroupCell *)tmp;
      break;
    }
    else if (m_down.y <= rect.GetBottom() )
    {
      clickedInGC = (GroupCell *)tmp;
      break;
    }
    tmp = (GroupCell *)tmp->m_next;
  }

  if (clickedBeforeGC != NULL) { // we clicked between groupcells, set hCaret
    m_hCaretPosition = (GroupCell *)tmp->m_previous; // can also be NULL
    m_hCaretActive = true;
    m_selectionType = SELECTION_TYPE_GROUP;
  } // end if (clickedBeforeGC != NULL) // we clicked between groupcells, set hCaret

  else if (clickedInGC != NULL) { // we clicked in a groupcell, find out where

    if (m_down.x <= LEFT_BORDER) { // we clicked in left bracket area
      if ((clickedInGC->HideRect()).Contains(m_down)) // did we hit the hide rectancle
      {
        clickedInGC->SwitchHide(); // todo if there's nothin to hide, select as normal
        clickedInGC->ResetData();
        Recalculate(false);
        m_selectionType = SELECTION_TYPE_NONE; // ignore drag-select
      }
      else {
        m_selectionType = SELECTION_TYPE_GROUP;
        m_selectionStart = m_selectionEnd = (MathCell *)clickedInGC;
      }
    } // end we clicked in left bracket area

    else { // we didn't click in left bracket space
      MathCell * editor = clickedInGC->GetEditable();
      if (editor != NULL) {
        rect = editor->GetRect();
        if ((m_down.y >= rect.GetTop()) && (m_down.y <= rect.GetBottom())) {
          SetActiveCell(editor);
          wxClientDC dc(this);
          m_activeCell->SelectPointText(dc, m_down);
          m_switchDisplayCaret = false;
          m_selectionType = SELECTION_TYPE_INPUT;
          Refresh();
          return;
        }
      }
      // what if we tried to select something in output, select it (or if editor, activate it)
      if ((clickedInGC->GetOutputRect()).Contains(m_down)) {
        wxRect rect2(m_down.x, m_down.y, 1,1);
        wxPoint mmm(m_down.x + 1, m_down.y +1);
        clickedInGC->SelectRectInOutput(rect2, m_down, mmm,
                                        &m_selectionStart, &m_selectionEnd);
        if (m_selectionStart != NULL) {
          if ((m_selectionStart == m_selectionEnd) && (m_selectionStart->GetType() == MC_TYPE_INPUT)
               && (clickedInGC == m_workingGroup)) // if we clicked an editor in output - activate it if working!
          {
            SetActiveCell(m_selectionStart);
            wxClientDC dc(this);
            m_activeCell->SelectPointText(dc, m_down);
            m_switchDisplayCaret = false;
            m_selectionType = SELECTION_TYPE_INPUT;
            Refresh();
            return;
          }
          else {
            m_selectionType = SELECTION_TYPE_OUTPUT;
            m_selectionInGC = clickedInGC;
          }
        }
      }

    } // end we didn't click in left bracket space

  } // end if (clickedInGC != NULL) // we clicked in a groupcell, find out where

  else { // we clicked below last groupcell (both clickedInGC and clickedBeforeGC == NULL)
    // set hCaret (or activate last cell?)
    /*m_hCaretPosition = (GroupCell *)m_last; // can also be NULL
    m_hCaretActive = true;
    m_selectionType = SELECTION_TYPE_GROUP;
    */
    SetActiveCell( m_last->GetEditable()  );
    wxClientDC dc(this);
    ((EditorCell *) m_activeCell)->CaretToEnd();
    m_switchDisplayCaret = false;
    m_selectionType = SELECTION_TYPE_INPUT;
  }
  Refresh();
}

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  m_animate = false;
  m_leftDown = false;
  m_mouseDrag = false;
  m_selectionInGC = NULL; // pointer to NULL to prevent crashes if the cell is deleted
  CheckUnixCopy();
  SetFocus();
}

void MathCtrl::OnMouseMotion(wxMouseEvent& event) {
  if (m_tree == NULL || !m_leftDown)
    return;
  m_mouseDrag = true;
  m_selectWholeLine = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_up.x, &m_up.y);
  if (m_mouseOutside) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
  SelectRect();
}

/***
 * Select the rectangle surounded by m_down and m_up. Called from OnMouseMotion.
 *
 * The method decides what to do, based on the value of m_selectionType which
 * was set previously in OnMouseLeftDown. This enables different selection behaviours
 * depending on where we first clicked. If m_selectionType equals
 * SELECTION_TYPE_NONE - click-draging does not result in a selection (we clicked in hideRect for instance)
 * SELECTION_TYPE_GROUP - we are selecting full groupcells only. Only y-coordinate matters.
 * SELECTION_TYPE_INPUT - we clicked in an editor (GroupCell::GetEditable()) and draging
 *   results in selecting text in EditorCell
 * SELECTION_TYPE_OUTPUT - we clicked in an output, we want selection to be confined to that
 *   GroupCell's output. GC we first clicked in was stored in OnMouseMotion method
 *   into m_selectionInGC pointer.
 */
void MathCtrl::SelectRect() {

  if (m_selectionType == SELECTION_TYPE_NONE)
    return;

  // If we have an active cell handle it
  if (m_selectionType == SELECTION_TYPE_INPUT)
  if (m_activeCell != NULL) {
      wxClientDC dc(this);
      m_activeCell->SelectRectText(dc, m_down, m_up);
      m_switchDisplayCaret = false;
      wxRect rect = m_activeCell->GetRect();
      CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
      RefreshRect(rect);
      return;
  }

  MathCell *st = m_selectionStart, *en = m_selectionEnd;
  wxRect rect;

  if (m_selectionType == SELECTION_TYPE_GROUP) {
    m_selectionStart = m_selectionEnd = NULL;
    int ytop    = MIN( m_down.y, m_up.y );
    int ybottom = MAX( m_down.y, m_up.y );
    // find out group cells between ytop and ybottom (including these two points)
    GroupCell * tmp = (GroupCell *) m_tree;
    while (tmp != NULL) {
      rect = tmp->GetRect();
      if (ytop <= rect.GetBottom()) {
        m_selectionStart = tmp;
        break;
      }
      tmp = (GroupCell *)tmp->m_next;
    }

    if (tmp == NULL) { // below last cell, handle with care
      m_hCaretActive = true;
      m_hCaretPosition = (GroupCell *)m_last;
      m_selectionStart = m_selectionEnd = NULL;
      if (st != m_selectionStart || en != m_selectionEnd)
        Refresh();
      return;
    }


    tmp = (GroupCell *) m_tree;
    while (tmp != NULL) {
      rect = tmp->GetRect();
      if (ybottom < rect.GetTop()) {
        m_selectionEnd = tmp->m_previous;
        break;
      }
      tmp = (GroupCell *)tmp->m_next;
    }
    if (tmp == NULL)
      m_selectionEnd = m_last;
    if (m_selectionEnd == (m_selectionStart->m_previous)) {
    m_hCaretActive = true;
    m_hCaretPosition = (GroupCell *)m_selectionEnd;
    m_selectionStart = m_selectionEnd = NULL;
    }
    else {
      m_hCaretActive = false;
      m_hCaretPosition = NULL;
    }

  } // end  SELECTION_TYPE_GROUP

  if (m_selectionType == SELECTION_TYPE_OUTPUT) {
    m_selectionStart = m_selectionEnd = NULL;
    rect.x = MIN(m_down.x, m_up.x);
    rect.y = MIN(m_down.y, m_up.y);
    rect.width = MAX(ABS(m_down.x - m_up.x), 1);
    rect.height = MAX(ABS(m_down.y - m_up.y), 1);

    if (m_selectionInGC != NULL)
      m_selectionInGC->SelectRectInOutput(rect, m_down, m_up, &m_selectionStart, &m_selectionEnd);

  } // end  SELECTION_TYPE_OUTPUT

  // Refresh only if the selection has changed
  if (st != m_selectionStart || en != m_selectionEnd)
    Refresh();
}

/***
 * Do selection when selecting by click
 */
void MathCtrl::SelectPoint(wxPoint& point) {
  if (m_tree == NULL)
    return;

  // If we have active cell handle it special.
  if (m_activeCell != NULL) {
    if (m_activeCell->ContainsPoint(m_down)) {
      wxClientDC dc(this);
      m_activeCell->SelectPointText(dc, m_down);
      m_switchDisplayCaret = false;
      Refresh();
      return;
    } else {
      SetActiveCell(NULL);
    }
  }

  GroupCell* tmp = NULL;
  m_selectWholeLine = true;

  //
  // Which cell did we select.
  //
  tmp = m_tree;
  wxRect rect;
  GroupCell * clickedBeforeGC = NULL;
  while (tmp != NULL) { // go through all groupcells

    if (tmp->ContainsPoint(point) || (tmp->HideRect()).Contains(point))
      break;

    rect = tmp->GetRect();
    if (point.y < rect.GetTop() )
    {
      clickedBeforeGC = (GroupCell *)tmp;
      break;
    }

    tmp = (GroupCell *)tmp->m_next;
  }

  m_hCaretActive = false;
  m_hCaretPosition = NULL;

  if (clickedBeforeGC != NULL) // clicked between groupcells
  {
    if (m_selectionStart != NULL) {
      m_selectionStart = NULL;
      m_selectionEnd = NULL;
    }
    m_hCaretPosition = (GroupCell *) clickedBeforeGC->m_previous;
    if (m_hCaretPosition != m_last)
      m_hCaretActive = true;
    else
      m_hCaretPosition = NULL;
    Refresh();
    return;
  }

  // clicked below last groupcell
  else if (point.y > (m_last->GetRect()).GetBottom())
  {
    if (m_workingGroup == NULL) {
      MathCell *input = m_last->GetInput();
      if (input != NULL) {
        SetActiveCell(input);
        return;
      }
    }
  }

  // We did no select anything.
  if (tmp == NULL) {
    if (m_selectionStart != NULL) {
      m_selectionStart = NULL;
      m_selectionEnd = NULL;
      Refresh();
    }
    return;
  }

  m_selectionStart = m_selectionEnd = NULL;
  MathCell *tr;
  tr = tmp->GetPrompt();

  // Check if we clicked on prompt.
  if (tr != NULL && tr->ContainsPoint(point)) {
    m_selectionStart = m_selectionEnd = tr;
  }

  // Check if we cliked on input.
  if (m_selectionStart == NULL) {
    tr = tmp->GetInput();
    if (tr != NULL && tr->ContainsPoint(point)) {
      m_selectionStart = m_selectionEnd = tr;
      if (m_workingGroup == NULL) {
        SetActiveCell(tr);
        wxClientDC dc(this);
        m_activeCell->SelectPointText(dc, m_down);
      }
    }
  }

  // Check if we clicked on label.
  if (m_selectionStart == NULL) {
    tr = tmp->GetLabel();
    if (tr != NULL && tr->ContainsPoint(point)) {
      m_selectionStart = m_selectionEnd = tr;
      if (tr->IsEditable() && m_workingGroup == NULL) {
        SetActiveCell(tr);
        wxClientDC dc(this);
        m_activeCell->SelectPointText(dc, m_down);
      }
    }
  }

  // Check if we clicked on the hide box
  if (tmp->HideRect().Contains(m_down)) {
    tmp->SwitchHide();
    tmp->ResetData();
    Recalculate(false);
  }

  // Check if we clicked on output.
  if (m_selectionStart == NULL) {
    if ((tmp->GetOutputRect()).Contains(point)) {
      tmp->SelectOutput(&m_selectionStart, &m_selectionEnd);
    }
  }

  Refresh();
}

/***
 * Get the string representation of the selection
 */
wxString MathCtrl::GetString(bool lb) {

  if (m_selectionStart == NULL) {
    if (m_activeCell == NULL)
      return wxEmptyString;
    else
      return m_activeCell->ToString(false);
  }

  wxString s;
  MathCell* tmp = m_selectionStart;
  while (tmp != NULL) {
    if (lb && tmp->BreakLineHere() && s.Length() > 0)
      s += wxT("\n");
    s += tmp->ToString(false);
    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_nextToDraw;
  }
  return s;
}

/***
 * Copy selection to clipboard.
 */
bool MathCtrl::Copy() {
  if (m_activeCell != NULL) {
    return m_activeCell->CopyToClipboard();
  }

  if (m_selectionStart == NULL)
    return false;
  wxString s = GetString(true);

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

bool MathCtrl::CopyTeX() {
  if (m_activeCell != NULL)
    return false;

  if (m_selectionStart == NULL)
    return false;

  wxString s;
  MathCell* tmp = m_selectionStart;

  bool inMath = false;
  bool inVerbatim = false;
  wxString label;

  if (tmp->GetType() != MC_TYPE_GROUP) {
    inMath = true;
    s = wxT("$$");
  }
  while (tmp != NULL) {
    s += tmp->ToTeX(false);
    tmp = tmp->m_next;
  }
  if (inMath == true)
    s += wxT("$$");

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool MathCtrl::CopyInput() {
  if (m_selectionStart == NULL)
      return false;

  wxString s;
  GroupCell *tmp = (GroupCell *)m_selectionStart->GetParent();
  GroupCell *end = (GroupCell *)m_selectionEnd->GetParent();
  MathCell *input;

  while (tmp != NULL) {
    input = tmp->GetInput();
    if (input != NULL)
      s += wxT("<wxmaxima-input>\n") + input->ToString(false) + wxT("\n");
    if (tmp == end)
      break;
    tmp = (GroupCell *)tmp->m_next;
  }
  s += wxT("<wxmaxima-input>");

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

/***
 * Can delete selection - we can't delete the last prompt!
 */
bool MathCtrl::CanDeleteSelection() {
  if (m_selectionStart == NULL || m_selectionEnd == NULL ||
      m_insertPoint != NULL || m_workingGroup != NULL)
    return false;

  GroupCell *start = (GroupCell *)m_selectionStart->GetParent();
  GroupCell *end = (GroupCell *)m_selectionEnd->GetParent();

  if (start == NULL || end == NULL)
    return false;

  if (end->m_next == NULL)
    return false;

  return true;
}

/***
 * Delete the selection
 */
void MathCtrl::DeleteSelection(bool deletePrompt) {
  if (m_selectionStart == NULL || m_selectionEnd == NULL ||
      m_insertPoint != NULL)
    return;

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  GroupCell *start = (GroupCell *)m_selectionStart->GetParent();
  GroupCell *end = (GroupCell *)m_selectionEnd->GetParent();

  if (start == NULL || end == NULL)
    return;

  SetActiveCell(NULL);
  m_hCaretActive = false;
  m_hCaretPosition = NULL;

  GroupCell *newSelection = (GroupCell *)end->m_next;

  if (end->m_next == NULL)
    return;

  if (start == m_tree) {
    if (end->m_previous != NULL) {
      end->m_previous->m_nextToDraw = NULL;
      end->m_previous->m_next = NULL;
    }
    end->m_next->m_previous = NULL;
    end->m_next->m_previousToDraw = NULL;

    m_tree = (GroupCell *)end->m_next;
    end->m_next = NULL;
    DestroyTree(start);
  }

  else {
    start->m_previous->m_next = end->m_next;
    start->m_previous->m_nextToDraw = end->m_next;
    end->m_next->m_previous = start->m_previous;
    end->m_next->m_previousToDraw = start->m_previous;
    end->m_next = NULL;
    DestroyTree(start);
  }


  if (newSelection != NULL) {
    m_selectionStart = m_selectionEnd = NULL;
    m_hCaretPosition = (GroupCell *)newSelection->m_previous;
    m_hCaretActive = true;
  }

  Recalculate(false);
  AdjustSize(false);
  Refresh();
}

void MathCtrl::OpenHCaret(wxString txt)
{
  if (m_workingGroup != NULL) {
    EditorCell *newInput = new EditorCell;
    newInput->SetType(MC_TYPE_INPUT);
    newInput->SetValue(txt);
    newInput->CaretToEnd();

    m_workingGroup->AppendOutput(newInput);
    newInput->SetParent(m_workingGroup, false);
    SetActiveCell(newInput);

    Recalculate();
    Refresh();

    return;
  }

  if (!m_hCaretActive)
    return;

  if (m_hCaretPosition != NULL) {
      SetSelection(m_hCaretPosition);
      PrependGroup(MC_TYPE_INPUT, txt, false, false);
      SelectNextInput();
      ((EditorCell *)m_activeCell)->CaretToEnd();
    }
    else {
      SetSelection(m_tree);
      PrependGroup(MC_TYPE_INPUT, txt, false, true);
      SelectPrevInput();
      if (m_activeCell != NULL)
        ((EditorCell *)m_activeCell)->CaretToEnd();
    }
}

/***
 * Support for copying and deleting with keyboard
 */
void MathCtrl::OnKeyDown(wxKeyEvent& event) {
  switch (event.GetKeyCode()) {

    case 'c':
    case 'C':
      if (!event.CmdDown() || event.AltDown()) {
        event.Skip();
        break;
      }
      if (m_activeCell != NULL)
        m_activeCell->CopyToClipboard();
      else if (CanCopy())
        Copy();
      else
        event.Skip();
      break;

    case 'x':
    case 'X':
      if (!event.CmdDown() || event.AltDown()) {
        event.Skip();
        break;
      }
      if (m_activeCell != NULL) {
        m_activeCell->CutToClipboard();
        RecalculateForce();
        Refresh();
      }
      break;

    case 'v':
    case 'V':
      if (!event.CmdDown() || event.AltDown()) {
        event.Skip();
        break;
      }
      if (m_activeCell != NULL) {
        m_activeCell->PasteFromClipboard();
        RecalculateForce();
        Refresh();
      }
      else if (m_hCaretActive) {
        if (wxTheClipboard->Open())
        {
          if (wxTheClipboard->IsSupported(wxDF_TEXT))
          {
            wxTextDataObject obj;
            wxTheClipboard->GetData(obj);
            wxString txt = obj.GetText();
            OpenHCaret(txt);
          }
          wxTheClipboard->Close();
        }
      }
      break;

    case 'a':
    case 'A':
      if (!event.CmdDown() || event.AltDown()) {
        event.Skip();
        break;
      }
      if (m_activeCell != NULL) {
        m_activeCell->SelectAll();
        Refresh();
      }
      break;

    case WXK_DELETE:
    case WXK_BACK:
      if (CanDeleteSelection()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
        (wxGetApp().GetTopWindow())->ProcessEvent(ev);
      } else
        event.Skip();
      break;

    case WXK_RETURN:

      if (CanEdit()) {
        if (event.ControlDown() || event.ShiftDown()) {
          if (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_INPUT)
            m_selectionStart->AddEnding();
          wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_reeval);
          (wxGetApp().GetTopWindow())->ProcessEvent(ev);
        } else
          SetActiveCell(m_selectionStart);
      }

      else if (m_activeCell != NULL) {
        if (event.ControlDown() || event.ShiftDown()) {
          if (m_activeCell->GetType() == MC_TYPE_INPUT)
            m_activeCell->AddEnding();
          wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, deactivate_cell_ok);
          (wxGetApp().GetTopWindow())->ProcessEvent(ev);
        } else
          event.Skip();
      }

      else if (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_TEXT) {
        PrependGroup(MC_TYPE_INPUT, GetString(), true, false);
        SelectNextInput();
      }

      else
        event.Skip();
      break;

    case WXK_ESCAPE:
      if (m_activeCell == NULL) {
        SetSelection(NULL);
        Refresh();
      }

      else {/*
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, deactivate_cell_cancel);
        (wxGetApp().GetTopWindow())->ProcessEvent(ev);
        */
        MathCell *parent = m_activeCell->GetParent();
        if (parent != m_last) {
          m_hCaretPosition = (GroupCell *)parent;
          SetActiveCell(NULL);
          m_hCaretActive = true;
        }
      }
      break;
    default:
      event.Skip();
  }
}

void MathCtrl::OnChar(wxKeyEvent& event) {

  if (m_activeCell != NULL) { // we are in an active cell
    bool hasHeightChanged = false;

    if (event.GetKeyCode() == WXK_UP &&
        ((EditorCell *)m_activeCell)->CaretAtStart() &&
        !event.ShiftDown()) { // don't exit the cell if we are making a selection
      m_hCaretPosition = (GroupCell *)(m_activeCell->GetParent())->m_previous;
      m_hCaretActive = true;
      SetActiveCell(NULL);
      return;
    }

    if (event.GetKeyCode() == WXK_DOWN &&
        ((EditorCell *)m_activeCell)->CaretAtEnd() &&
        !event.ShiftDown()) {
      MathCell *tmp = m_activeCell->GetParent();
      if (tmp != m_last) {
        m_hCaretPosition = (GroupCell *)tmp;
        m_hCaretActive = true;
        SetActiveCell(NULL);
      }
      else
        event.Skip();
      return;
    }

    m_activeCell->ProcessEvent(event);

    // CTRL+"s deactivates on MAC
    if (m_activeCell == NULL)
      return;

    m_switchDisplayCaret = false;

    wxClientDC dc(this);
    CellParser parser(dc);

    wxPoint point = m_activeCell->PositionToPoint(parser);

    if (m_activeCell->IsDirty()) {
      int height = m_activeCell->GetHeight();

      int fontsize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontsize);

      m_activeCell->ResetData();
      m_activeCell->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
      m_activeCell->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);

      GroupCell *group = (GroupCell *)m_activeCell->GetParent();
      group->ResetData();
      group->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
      group->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);

      if (height != m_activeCell->GetHeight())
        hasHeightChanged = true;
    }

    if (hasHeightChanged) {
      Recalculate(false);
      Refresh();
    }
    else {
      wxRect rect = m_activeCell->GetRect();
      CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
      rect.width = GetSize().x;
      RefreshRect(rect);
    }

    ShowPoint(point);
  }

  else { // m_activeCell == NULL
    wxClientDC dc(this);
    CellParser parser(dc);

    if (!event.ShiftDown())
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

    switch (event.GetKeyCode()) {

      case WXK_UP:
        if (m_hCaretActive) {

          if (event.ShiftDown()) {
            if (m_hCaretPositionStart == NULL || m_hCaretPositionEnd == NULL) {
              if (m_hCaretPosition != NULL)
                m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPosition;
            }
            else {
              if (m_hCaretPositionEnd->m_previous != NULL) {
                if (m_hCaretPosition != NULL && m_hCaretPosition->m_next == m_hCaretPositionEnd)
                  m_hCaretPositionStart = (GroupCell *)m_hCaretPositionStart->m_previous;
                m_hCaretPositionEnd = (GroupCell *)m_hCaretPositionEnd->m_previous;
              }
            }
            if (m_hCaretPositionEnd != NULL)
              ScrollToCell(m_hCaretPositionEnd, true);
          }

          else {
            if (m_hCaretPosition != NULL)
            {
              MathCell * editor = m_hCaretPosition->GetEditable();
              if(editor != NULL && m_workingGroup == NULL)
              {
                SetActiveCell(editor);
                m_hCaretActive = false;
                ((EditorCell *)m_activeCell)->CaretToEnd();
                ShowPoint(m_activeCell->PositionToPoint(parser));
              }
              else { // can't get editor... jump over cell..
                m_hCaretPosition = (GroupCell *) m_hCaretPosition->m_previous;
                Refresh();
              }
            }
            else
              event.Skip();
          }
        }

        else {
          if (!SelectPrevInput())
            event.Skip();
          else
            Refresh();
        }

        break;

      case WXK_DOWN:
        //
        if (m_hCaretActive)
        {

          if (event.ShiftDown()) {
            if (m_hCaretPositionStart == NULL || m_hCaretPositionEnd == NULL) {
              if (m_hCaretPosition == NULL)
                m_hCaretPositionStart = m_hCaretPositionEnd = m_tree;
              else if (m_hCaretPosition->m_next != m_last)
                m_hCaretPositionStart = m_hCaretPositionEnd = (GroupCell *)m_hCaretPosition->m_next;
            }
            else {
              if (m_hCaretPositionEnd->m_next != NULL) {
                if (m_hCaretPosition == m_hCaretPositionEnd)
                  m_hCaretPositionStart = (GroupCell *)m_hCaretPositionStart->m_next;
                m_hCaretPositionEnd = (GroupCell *)m_hCaretPositionEnd->m_next;
              }
            }
            if (m_hCaretPositionEnd != NULL)
              ScrollToCell(m_hCaretPositionEnd, false);
          }

          else {
            if (m_hCaretPosition == NULL)
            {
              MathCell *editor = ((GroupCell *)m_tree)->GetEditable();
              if (editor != NULL && m_workingGroup == NULL) // try to edit the first cell
              {
                SetActiveCell(editor);
                m_hCaretActive = false;
                ((EditorCell *)m_activeCell)->CaretToStart();
                ShowPoint(m_activeCell->PositionToPoint(parser));
              }
              else { // else jump over
                m_hCaretPosition = (GroupCell *)m_tree;
                Refresh();
              }
            }

            else if (m_hCaretPosition->m_next != NULL)
            {
              MathCell *editor = ((GroupCell *)m_hCaretPosition->m_next)->GetEditable();
              if( editor != NULL && m_workingGroup == NULL)
              {
                SetActiveCell(editor);
                m_hCaretActive = false;
                ((EditorCell *)m_activeCell)->CaretToStart();
                ShowPoint(m_activeCell->PositionToPoint(parser));
              }
              else { // can't get editor.. jump over cell..
                m_hCaretPosition = (GroupCell *) m_hCaretPosition->m_next;
                Refresh();
              }
            }
          }
        }

        else {
          if (!SelectNextInput())
            event.Skip();
          else
            Refresh();
        }

        break;

      case WXK_LEFT:
        m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

        if (m_selectionStart != NULL) {
          GroupCell *group = (GroupCell *)m_selectionStart->GetParent();
          if (m_selectionStart->GetType() == MC_TYPE_INPUT && group->m_previous != NULL) {
            group = (GroupCell *)group->m_previous;
            MathCell *start, *end;
            group->SelectOutput(&start, &end);
            if (start != NULL && end != NULL) {
              m_selectionStart = start;
              m_selectionEnd = end;
            }
          }
          else {
            MathCell *input = group->GetInput();
            if (input != NULL)
              m_selectionStart = m_selectionEnd = input;
          }
          ScrollToSelectionStart(true);
          Refresh();
        }
        else
          event.Skip();
        break;

      case WXK_RIGHT:
        m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

        if (m_selectionStart != NULL) {
          GroupCell *group = (GroupCell *)m_selectionStart->GetParent();
          if (m_selectionStart->GetType() == MC_TYPE_TEXT && group->m_next != NULL) {
            group = (GroupCell *)group->m_next;
            MathCell *input = group->GetInput();
            if (input != NULL)
              m_selectionStart = m_selectionEnd = input;
          } else {
            group->SelectOutput(&m_selectionStart, &m_selectionEnd);
          }
          ScrollToSelectionStart(false);
          Refresh();
        }
        else
          event.Skip();
        break;

      default:
        m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

        if (m_hCaretActive || m_workingGroup != NULL)
        {
#if wxUSE_UNICODE
          wxString txt(event.GetUnicodeKey());
#else
          wxString txt = wxString::Format(wxT("%c"), event.GetKeyCode());
#endif
          OpenHCaret(txt);
        }
        event.Skip();
    }

    if (m_hCaretPositionStart != NULL && m_hCaretPositionEnd != NULL) {
      if (m_hCaretPositionStart->GetCurrentY() < m_hCaretPositionEnd->GetCurrentY()) {
        m_selectionStart = m_hCaretPositionStart;
        m_selectionEnd = m_hCaretPositionEnd;
      }
      else {
        m_selectionStart = m_hCaretPositionEnd;
        m_selectionEnd = m_hCaretPositionStart;
      }

      Refresh();
    }
  }
}

/***
 * Get maximum x and y in the tree.
 */
void MathCtrl::GetMaxPoint(int* width, int* height) {
  MathCell* tmp = m_tree;
  int currentHeight= MC_BASE_INDENT;
  int currentWidth= MC_BASE_INDENT;
  *width = MC_BASE_INDENT;
  *height = MC_BASE_INDENT;
  bool bigSkip = false;

  while (tmp != NULL) {
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere()) {
        currentHeight += tmp->GetMaxHeight();
        if (bigSkip)
          currentHeight += MC_GROUP_SKIP;
        *height = currentHeight;
        currentWidth = MC_BASE_INDENT + tmp->GetWidth();
        *width = MAX(currentWidth + MC_BASE_INDENT, *width);
      }
      else {
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
        *width = MAX(currentWidth - MC_CELL_SKIP, *width);
      }
      bigSkip = tmp->m_bigSkip;
    }
    tmp = tmp->m_next;
  }

}

/***
 * Break lines.
 */
void MathCtrl::BreakLines() {
  GroupCell *tmp = m_tree;
  int fullWidth = GetClientSize().GetWidth() - 9;

  while (tmp != NULL) {
    tmp->ResetData();
    tmp->BreakLines(fullWidth);
    tmp = (GroupCell *)tmp->m_next;
  }
}

/***
 * Adjust the virtual size and scrollbars.
 */
void MathCtrl::AdjustSize(bool scroll) {
  int width= MC_BASE_INDENT, height= MC_BASE_INDENT;
  int clientWidth, clientHeight, virtualHeight;

  GetClientSize(&clientWidth, &clientHeight);
  if (m_tree != NULL)
    GetMaxPoint(&width, &height);
  virtualHeight = MAX(clientHeight, height) + 10;

  SetVirtualSize(width + 9, virtualHeight + 9);
  SetScrollRate(SCROLL_UNIT, SCROLL_UNIT);
  if (scroll && height > clientHeight) {
    if (m_scrollTo > -1)
      Scroll(0, m_scrollTo);
    else
      Scroll(0, height);
  }
}

/***
 * Support for selecting cells outside display
 */
void MathCtrl::OnMouseExit(wxMouseEvent& event) {
  m_mouseOutside = true;
  if (m_leftDown) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
    m_timer.Start(200, true);
  }
}

void MathCtrl::OnMouseEnter(wxMouseEvent& event) {
  m_mouseOutside = false;
}

void MathCtrl::OnTimer(wxTimerEvent& event) {
  switch (event.GetId()) {
    case TIMER_ID:
      {
        if (!m_leftDown || !m_mouseOutside)
          return;
        int dx = 0, dy = 0;
        int currX, currY;

        wxSize size = GetClientSize();
        CalcUnscrolledPosition(0, 0, &currX, &currY);

        if (m_mousePoint.x <= 0)
          dx = -10;
        else if (m_mousePoint.x >= size.GetWidth())
          dx = 10;
        if (m_mousePoint.y <= 0)
          dy = -10;
        else if (m_mousePoint.y >= size.GetHeight())
          dy = 10;

        Scroll((currX + dx) / 10, (currY + dy) / 10);
        m_timer.Start(50, true);
      }
      break;
    case ANIMATION_TIMER_ID:
      {
        if (m_selectionStart != NULL && m_selectionStart == m_selectionEnd &&
            m_selectionStart->GetType() == MC_TYPE_SLIDE && m_animate) {

          SlideShow *tmp = (SlideShow *)m_selectionStart;
          tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());

          wxRect rect = m_selectionStart->GetRect();
          CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
          RefreshRect(rect);

          m_animationTimer.Start(ANIMATION_TIMER_TIMEOUT);
        }
        else
          m_animate = false;
      }
      break;
    case CARET_TIMER_ID:
      {
        if (m_activeCell != NULL) {
          if (m_switchDisplayCaret) {
            m_activeCell->SwitchCaretDisplay();

            wxRect rect = m_activeCell->GetRect();
            CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
            RefreshRect(rect);
          }
          m_switchDisplayCaret = true;
          m_caretTimer.Start(CARET_TIMER_TIMEOUT, true);
        }
      }
      break;
   }
}

/***
 * Destroy the tree
 */
void MathCtrl::DestroyTree() {
  m_hCaretActive = false;
  m_hCaretPosition = NULL;
  DestroyTree(m_tree);
  m_tree = m_last = NULL;
}

void MathCtrl::DestroyTree(MathCell* tmp) {
  MathCell* tmp1;
  while (tmp != NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
}

/***
 * Copy tree
 */
MathCell* MathCtrl::CopyTree() {
  if (m_tree == NULL)
    return (MathCell*)NULL;

  MathCell* tmp1 = m_tree;
  MathCell* tmp;
  MathCell* copy;
  tmp = tmp1->Copy(false);
  copy = tmp;

  if (tmp1->m_isFolded)
    tmp1 = tmp1->m_next;
  else
    tmp1 = tmp1->m_next;
  while (tmp1 != NULL) {
    tmp->AppendCell(tmp1->Copy(false));
    tmp = tmp->m_next;
    if (tmp1->m_isFolded)
      tmp1 = tmp1->m_next;
    else
      tmp1 = tmp1->m_next;
  }
  return copy;
}

/***
 * Copy selection as bitmap
 */
bool MathCtrl::CopyBitmap() {
  MathCell* tmp = CopySelection();

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToClipboard();
}

bool MathCtrl::CopyToFile(wxString file) {
  MathCell* tmp = CopySelection();

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToFile(file);
}

bool MathCtrl::CopyToFile(wxString file, MathCell* start, MathCell* end,
    bool asData) {
  MathCell* tmp = CopySelection(start, end, asData);

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToFile(file);
}

/***
 * Copy selection
 */
MathCell* MathCtrl::CopySelection() {
  return CopySelection(m_selectionStart, m_selectionEnd);
}

MathCell* MathCtrl::CopySelection(MathCell* start, MathCell* end, bool asData) {
  MathCell *tmp, *tmp1= NULL, *tmp2= NULL;
  tmp = start;

  while (tmp != NULL) {
    if (tmp1 == NULL) {
      tmp1 = tmp->Copy(false);
      tmp2 = tmp1;
    } else {
      tmp2->AppendCell(tmp->Copy(false));
      tmp2 = tmp2->m_next;
    }
    if (tmp == end)
      break;
    if (asData)
      tmp = tmp->m_next;
    else
      tmp = tmp->m_next;
  }

  return tmp1;
}

/***
 * Export content to a HTML file.
 */

void AddLineToFile(wxTextFile& output, wxString s, bool unicode) {
  if (s == wxT("\n") || s == wxEmptyString)
    output.AddLine(wxEmptyString);
  else {
    wxStringTokenizer lines(s, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
    wxString line;

    while (lines.HasMoreTokens()) {
      line = lines.GetNextToken();
      if (unicode) {
#if wxUNICODE
        output.AddLine(line);
#else
        wxString t(line.wc_str(wxConvLocal), wxConvUTF8);
        output.AddLine(t);
#endif
      } else
        output.AddLine(line);
    }
  }
}

wxString PrependNBSP(wxString input)
{
  wxString line = wxEmptyString;
  for (unsigned int i = 0; i < input.Length(); i++) {
    while (input.GetChar(i) == '\n') {
      line += wxT("<BR>\n");
      i++;
      while (i < input.Length() && input.GetChar(i) == ' ') {
        line += wxT("&nbsp;");
        i++;
      }
    }
    line += input.GetChar(i);
  }
  return line;
}

bool MathCtrl::ExportToHTML(wxString file) {
  wxString imgDir;
  wxString path, filename, ext;
  int count = 0;
  GroupCell *tmp = (GroupCell *)m_tree;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxT("/") + filename + wxT("_img");

  if (!wxDirExists(imgDir))
    if (!wxMkdir(imgDir))
      return false;

  wxTextFile output(file);
  if (output.Exists()) {
    if (!output.Open(file))
      return false;
    output.Clear();
  } else if (!output.Create(file))
    return false;

  AddLineToFile(
      output,
      wxT("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"));
  AddLineToFile(output, wxT("<HTML>"));
  AddLineToFile(output, wxT(" <HEAD>"));
  AddLineToFile(output, wxT("  <TITLE>wxMaxima HTML export</TITLE>"));
  AddLineToFile(output, wxT("  <META NAME=\"generator\" CONTENT=\"wxMaxima\">"));
  AddLineToFile(
      output,
      wxT("  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">"));

//////////////////////////////////////////////
// Write styles
//////////////////////////////////////////////

  wxString font;
  wxString colorInput(wxT("blue"));
  wxString colorPrompt(wxT("red"));
  wxString colorMain(wxT("black"));
  wxString colorTextBg(wxT("white"));
  bool italicInput = false;
  bool boldInput = false;
  bool italicPrompt = false;
  bool boldPrompt = false;
  bool italicHidden = false;
  bool boldHidden = false;
  bool underlinedHidden = false;
  bool boldString = false;
  bool italicString = false;
  int fontSize = 12;
  wxConfigBase* config= wxConfig::Get();

  config->Read(wxT("Style/fontname"), &font);
  config->Read(wxT("Style/Input/color"), &colorInput);
  config->Read(wxT("Style/MainPrompt/color"), &colorPrompt);
  config->Read(wxT("Style/NormalText/color"), &colorMain);
  config->Read(wxT("fontSize"), &fontSize);
  config->Read(wxT("Style/Input/bold"), &boldInput);
  config->Read(wxT("Style/String/bold"), &boldString);
  config->Read(wxT("Style/Input/italic"), &italicInput);
  config->Read(wxT("Style/String/italic"), &italicString);
  config->Read(wxT("Style/MainPrompt/bold"), &boldPrompt);
  config->Read(wxT("Style/MainPrompt/italic"), &italicPrompt);
  config->Read(wxT("Style/HiddenText/bold"), &boldHidden);
  config->Read(wxT("Style/HiddenText/italic"), &italicHidden);
  config->Read(wxT("Style/HiddenText/underlined"), &underlinedHidden);
  config->Read(wxT("Style/TextBackground/color"), &colorTextBg);

  AddLineToFile(output, wxT("  <STYLE TYPE=\"text/css\">"));

  // BODY STYLE
  AddLineToFile(output, wxT("body {"));
  if (font.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    font +
    wxT(";"));
  }
  if (colorMain.Length()) {
    wxColour color(colorMain);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  AddLineToFile(output, wxT("}"));

  // INPUT STYLE
  AddLineToFile(output, wxT(".input {"));
  if (colorInput.Length()) {
    wxColour color(colorInput);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (boldInput)
    AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicInput)
    AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("}"));

  // COMMENT STYLE
  AddLineToFile(output, wxT(".comment {"));
  if (colorMain.Length()) {
    wxColour color(colorMain);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (colorTextBg.Length()) {
    wxColour color(colorTextBg);
    AddLineToFile(output, wxT("  background-color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));
  AddLineToFile(output, wxT(".section {"));
  if (colorMain.Length()) {
    wxColour color(colorMain);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (colorTextBg.Length()) {
    wxColour color(colorTextBg);
    AddLineToFile(output, wxT("  background-color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  AddLineToFile(output, wxT("  font-weight: bold;"));
  AddLineToFile(output, wxT("  text-decoration: underline;"));
  AddLineToFile(output, wxT("  font-size: 1.5em;"));
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));
  AddLineToFile(output, wxT(".title {"));
  if (colorMain.Length()) {
    wxColour color(colorMain);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (colorTextBg.Length()) {
    wxColour color(colorTextBg);
    AddLineToFile(output, wxT("  background-color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  AddLineToFile(output, wxT("  font-weight: bold;"));
  AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("  text-decoration: underline;"));
  AddLineToFile(output, wxT("  font-size: 2em;"));
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));

  // PROMPT STYLE
  AddLineToFile(output, wxT(".prompt {"));
  if (colorPrompt.Length()) {
    wxColour color(colorPrompt);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (boldPrompt)
    AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicPrompt)
    AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("}"));

  // HIDDEN STYLE
  AddLineToFile(output, wxT(".hidden {"));
  if (colorPrompt.Length()) {
    wxColour color(colorPrompt);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if (boldHidden)
    AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicHidden)
    AddLineToFile(output, wxT("  font-style: italic;"));
  if (underlinedHidden)
    AddLineToFile(output, wxT("  text-decoration: underline;"));
  AddLineToFile(output, wxT("}"));

  AddLineToFile(output, wxT("  </STYLE>"));
  AddLineToFile(output, wxT(" </HEAD>"));
  AddLineToFile(output, wxT(" <BODY>"));

  wxString version(wxT(VERSION));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("<!---------------------------------------------------------->"));
  AddLineToFile(output, wxT("<!--          Created with wxMaxima version ") + version + wxT("         -->"));
  AddLineToFile(output, wxT("<!---------------------------------------------------------->"));

//////////////////////////////////////////////
// Write contents
//////////////////////////////////////////////

  while (tmp != NULL) {
    AddLineToFile(output, wxT("\n\n<!-- Input/Output group -->\n\n"));

    MathCell *prompt = tmp->GetPrompt();
    if (prompt != NULL && !tmp->IsSpecial()) {
      AddLineToFile(output, wxT("<P>"));
      AddLineToFile(output, wxT("  <SPAN CLASS=\"prompt\">"));
      AddLineToFile(output, prompt->ToString(false));
      AddLineToFile(output, wxT("  </SPAN>"));
    }

    MathCell *input = tmp->GetInput();
    if (input != NULL) {
      AddLineToFile(output, wxT("  <SPAN CLASS=\"input\">"));
      AddLineToFile(output, PrependNBSP(input->ToString(false)));
      AddLineToFile(output, wxT("  </SPAN>"));
    }

    MathCell *out = tmp->GetLabel();
    if (out == NULL) {
      AddLineToFile(output, wxEmptyString);
    }
    else {
      if (tmp->IsSpecial()) {
        switch(out->GetType()) {
          case MC_TYPE_COMMENT:
            AddLineToFile(output, wxT("<P CLASS=\"comment\">"));
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("<P CLASS=\"section\">"));
            break;
          case MC_TYPE_TITLE:
            AddLineToFile(output, wxT("<P CLASS=\"title\">"));
            break;
        }
        AddLineToFile(output, PrependNBSP(out->ToString(false)));
      }
      else {
        CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count), out, NULL, true);
        AddLineToFile(output, wxT("  <BR>"));
        AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_img/") +
            filename +
            wxString::Format(wxT("_%d.png\">"), count));
        count++;
      }
    }

    AddLineToFile(output, wxT("</P>"));

    tmp = (GroupCell *)tmp->m_next;
  }

//////////////////////////////////////////////
// Footer
//////////////////////////////////////////////

  AddLineToFile(output, wxT(" <HR>"));
  AddLineToFile(output, wxT(" <SMALL> Created with")
  wxT(" <A HREF=\"http://wxmaxima.sourceforge.net/\">")
  wxT("wxMaxima</A>")
  wxT(".</SMALL>"));
  AddLineToFile(output, wxEmptyString);

  //
  // Close document
  //
  AddLineToFile(output, wxT(" </BODY>"));
  AddLineToFile(output, wxT("</HTML>"));

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

bool MathCtrl::ExportToTeX(wxString file) {
  wxString imgDir;
  wxString path, filename, ext;
  MathCell *tmp = m_tree, *start= NULL, *end= NULL;
  int count = 0;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxT("/") + filename + wxT("_img");

  wxTextFile output(file);
  if (output.Exists()) {
    if (!output.Open(file))
      return false;
    output.Clear();
  } else if (!output.Create(file))
    return false;

  AddLineToFile(output, wxT("\\documentclass{article}"));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("%% Created with wxMaxima"));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\usepackage{graphicx}"));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\begin{document}"));

  //
  // Write contents
  //

  while (tmp != NULL) {
    wxString s = tmp->ToTeX(false);
    AddLineToFile(output, s);
    tmp = tmp->m_next;
  }

  //
  // Close document
  //
  AddLineToFile(output, wxT("\\end{document}"));

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

bool MathCtrl::ExportToMAC(wxString file) {

  bool wxm = false;
  if (file.Right(4) == wxT(".wxm"))
    wxm = true;

  wxTextFile output(file);
  if (output.Exists()) {
    if (!output.Open(file))
      return false;
    output.Clear();
  } else if (!output.Create(file))
    return false;

  if (wxm) {
    AddLineToFile(output, wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"), false);
    wxString version(wxT(VERSION));
    AddLineToFile(output, wxT("/* [ Created with wxMaxima version ") + version + wxT(" ] */"), false);
  }

  GroupCell* tmp = (GroupCell *)m_tree;

  //
  // Write contents
  //
  while (tmp != NULL) {

    // Write input
    if (!tmp->IsSpecial()) {
      MathCell *txt = tmp->GetInput();
      if (txt != NULL) {
        wxString input = txt->ToString(false);
        if (input.Length()>0) {
          AddLineToFile(output, wxEmptyString, false);
          if (wxm)
            AddLineToFile(output, wxT("/* [wxMaxima: input   start ] */"), false);
          AddLineToFile(output, input, false);
          if (wxm)
            AddLineToFile(output, wxT("/* [wxMaxima: input   end   ] */"), false);
        }
      }
    }

    // Write text
    else {
      AddLineToFile(output, wxEmptyString, false);
      MathCell *txt = tmp->GetLabel();

      if (wxm) {
        switch (txt->GetType()) {
          case MC_TYPE_COMMENT:
            AddLineToFile(output, wxT("/* [wxMaxima: comment start ]"), false);
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("/* [wxMaxima: section start ]"), false);
            break;
          case MC_TYPE_TITLE:
            AddLineToFile(output, wxT("/* [wxMaxima: title   start ]"), false);
            break;
        }
      }
      else
        AddLineToFile(output, wxT("/*"), false);

      wxString comment = txt->ToString(false);
      AddLineToFile(output, comment, false);

      if (wxm) {
        switch (txt->GetType()) {
          case MC_TYPE_COMMENT:
            AddLineToFile(output, wxT("   [wxMaxima: comment end   ] */"), false);
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("   [wxMaxima: section end   ] */"), false);
            break;
          case MC_TYPE_TITLE:
            AddLineToFile(output, wxT("   [wxMaxima: title   end   ] */"), false);
            break;
        }
      }
      else
        AddLineToFile(output, wxT("*/"), false);
    }

    tmp = (GroupCell *)tmp->m_next;
  }

  AddLineToFile(output, wxEmptyString, false);
  if (wxm) {
    AddLineToFile(output, wxT("/* Maxima can't load/batch files which end with a comment! */"), false);
    AddLineToFile(output, wxT("\"Created with wxMaxima\"$"), false);
  }

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

void MathCtrl::BreakUpCells() {
  if (m_tree != NULL)
    BreakUpCells(m_tree);
}

void MathCtrl::BreakUpCells(MathCell *cell) {
  wxClientDC dc(this);
  CellParser parser(dc);
  int fontsize = 12;
  GroupCell *tmp = (GroupCell *)cell;

  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int clientWidth = GetClientSize().GetWidth() - 9;
  fontsize = MAX(fontsize, MC_MIN_SIZE);

  while (tmp != NULL) {
    tmp->BreakUpCells(dc, parser, fontsize, clientWidth);
    tmp = (GroupCell *)tmp->m_next;
  }
}

void MathCtrl::UnBreakUpCells() {
  GroupCell *tmp = m_tree;
  while (tmp != NULL) {
    tmp->UnBreakUpCells();
    tmp = (GroupCell *)tmp->m_next;
  }
}

/**
 * CanEdit: we can edit the input if the we have the whole input in selection!
 */
bool MathCtrl::CanEdit() {
  if (m_selectionStart == NULL || m_selectionEnd != m_selectionStart
      || m_insertPoint != NULL || m_editingEnabled == false)
    return false;

  if (!m_selectionStart->IsEditable())
    return false;

  if (m_selectionStart->m_previous == NULL)
    return false;

  if (m_selectionStart->m_previous->GetType() != MC_TYPE_MAIN_PROMPT)
    return false;

  return true;
}

MathCell* MathCtrl::GetLastCell() {
  if (m_last == NULL)
    m_last = m_tree;
  if (m_last == NULL)
    return NULL;
  while (m_last->m_next)
    m_last = (GroupCell *)m_last->m_next;
  return m_last;
}

MathCell* MathCtrl::GetLastPrompt() {
  GroupCell *tmp = m_last;

  if (tmp == NULL)
    return NULL;

  return tmp->GetPrompt();
}

void MathCtrl::OnDoubleClick(wxMouseEvent &event) {
  if (m_activeCell != NULL) {
  ((EditorCell *) m_activeCell)->SelectWordUnderCaret();
  Refresh();
  }
}

bool MathCtrl::SelectPrevInput() {
  if (m_selectionStart == NULL && m_activeCell == NULL)
    return false;

  GroupCell *tmp;
  if (m_selectionStart != NULL)
    tmp = (GroupCell *)m_selectionStart->GetParent();
  else {
    tmp = (GroupCell *)m_activeCell->GetParent();
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = (GroupCell *)tmp->m_previous;
  if (tmp == NULL)
    return false;

  MathCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL) {
    inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = (GroupCell *)tmp->m_previous;
  }

  if (inpt == NULL)
    return false;

  m_selectionStart = m_selectionEnd = inpt;
  ScrollToSelectionStart(true);

  SetActiveCell(m_selectionStart);
  ((EditorCell *)m_activeCell)->CaretToEnd();

  Refresh();

  return true;
}

bool MathCtrl::SelectNextInput(bool input) {
  if (m_selectionStart == NULL && m_activeCell == NULL)
    return false;

  GroupCell *tmp;
  if (m_selectionStart != NULL)
    tmp = (GroupCell *)m_selectionStart->GetParent();
  else {
    tmp = (GroupCell *)m_activeCell->GetParent();
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = (GroupCell *)tmp->m_next;
  if (tmp == NULL)
    return false;

  MathCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL) {
    if (input)
      inpt = tmp->GetInput();
    else
      inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = (GroupCell *)tmp->m_next;
  }

  if (inpt == NULL)
    return false;

  m_selectionStart = m_selectionEnd = inpt;
  ScrollToSelectionStart(false);

  if (FindFocus() == this) {
    SetActiveCell(m_selectionStart);
    ((EditorCell *)m_activeCell)->CaretToStart();
  }

  Refresh();

  return true;
}

bool MathCtrl::SelectLastInput() {
  if (m_last == NULL)
    return false;

  GroupCell *tmp = (GroupCell *)m_last;

  MathCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL) {
    inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = (GroupCell *)tmp->m_previous;
  }

  if (inpt == NULL)
    return false;

  m_selectionStart = m_selectionEnd = inpt;

  ScrollToSelectionStart();

  SetActiveCell(m_selectionStart);
  ((EditorCell *)m_activeCell)->CaretToStart();

  return true;
}

bool MathCtrl::SelectFirstInput() {
  GroupCell *tmp = m_tree;
  if (tmp == NULL)
      return false;

  MathCell *input = tmp->GetInput();
  while (tmp != NULL && input == NULL) {
    tmp = (GroupCell *)tmp->m_next;
    input = tmp->GetInput();
  }

  if (input == NULL)
    return false;

  m_selectionStart = m_selectionEnd = input;
  Refresh();

  return true;
}

bool MathCtrl::SelectPrompt() {
  GroupCell *tmp = m_last;

  if (m_selectionStart != NULL)
    tmp = (GroupCell *)m_selectionStart->GetParent();
  else if (m_activeCell != NULL)
    tmp = (GroupCell *)m_activeCell->GetParent();
  else if (m_hCaretActive) {
    if (m_hCaretPosition == NULL)
      tmp = (GroupCell *)m_tree;
    else
      tmp = (GroupCell *)m_hCaretPosition->GetParent()->m_next;
  }

  if (tmp == NULL)
    return false;

  m_selectionStart = m_selectionEnd = tmp;
  Refresh();

  return true;
}

void MathCtrl::ScrollToSelectionStart(bool top) {
  ScrollToCell(m_selectionStart, top);
}

void MathCtrl::ScrollToCell(MathCell *cell, bool top)
{
  if (cell == NULL)
    return;

  MathCell *tmp = cell->GetParent();
  if (tmp == NULL)
    return;

  int cellY = tmp->GetCurrentY();

  if (cellY == -1)
    return;

  int cellDrop = tmp->GetDrop();
  int cellCenter = tmp->GetCenter();

  int view_x, view_y;
  int height, width;

  GetViewStart(&view_x, &view_y);
  GetSize(&width, &height);

  view_y *= SCROLL_UNIT;

  if ((cellY - cellCenter - SCROLL_UNIT < view_y) || (cellY + cellDrop
      + SCROLL_UNIT > view_y + height)) {
    if (top)
      Scroll(-1, MAX(cellY/SCROLL_UNIT - 2, 0));
    else
      Scroll(-1, MAX((cellY - height + cellDrop)/SCROLL_UNIT + 4, 0));
  }
  Refresh();
}

void MathCtrl::SetActiveCell(MathCell *cell) {
  if (m_activeCell != NULL)
    m_activeCell->ActivateCell();

  m_activeCell = cell;

  if (m_activeCell != NULL) {
    SetSelection(NULL);
    bool match = false;
    if (m_activeCell->GetType() == MC_TYPE_INPUT)
      wxConfig::Get()->Read(wxT("matchParens"), &match);
    m_activeCell->ActivateCell();
    m_activeCell->SetMatchParens(match);
    m_switchDisplayCaret = false;
    m_caretTimer.Start(CARET_TIMER_TIMEOUT, true);
  }

  if (cell != NULL)
    m_hCaretActive = false; // we have activeted a cell .. disable caret

  Refresh();
}

void MathCtrl::ShowPoint(wxPoint point) {
  if (point.x == -1 || point.y == -1)
    return;

  int view_x, view_y;
  int height, width;
  bool sc = false;

  int scrollToX = -1, scrollToY = -1;

  GetViewStart(&view_x, &view_y);
  GetSize(&width, &height);

  view_x *= SCROLL_UNIT;
  view_y *= SCROLL_UNIT;

  if ((point.y < view_y) || (point.y > view_y + height
      - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20)) {
    sc = true;
    scrollToY = point.y - height / 2;
  } else
    scrollToY = view_y;

  if ((point.x < view_x) || (point.x > view_x + width
      - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20)) {
    sc = true;
    scrollToX = point.x - width / 2;
  } else
    scrollToX = view_x;

  if (sc)
    Scroll(scrollToX / SCROLL_UNIT, scrollToY / SCROLL_UNIT);
}

bool MathCtrl::CutToClipboard() {
  if (m_activeCell == NULL)
    return false;

  m_activeCell->CutToClipboard();
  Recalculate(false);
  Refresh();
  return true;
}

/****
 * PasteFromClipboard
 * Pastes text into activeCell or opens a new cell
 * if hCaretActive == true
 */
void MathCtrl::PasteFromClipboard() {
  if (m_activeCell != NULL) {
    m_activeCell->PasteFromClipboard();
    Recalculate(false);
    Refresh();
  }
  else if ((m_hCaretActive == true) && (wxTheClipboard->Open())) {
    if (wxTheClipboard->IsSupported(wxDF_TEXT)) {
      wxTextDataObject obj;
      wxTheClipboard->GetData(obj);
      wxString txt = obj.GetText();

      OpenHCaret(txt);
      Refresh();
    }
    wxTheClipboard->Close();
  }
}

void MathCtrl::SelectAll() {
  if (m_activeCell == NULL)
    return;

  m_activeCell->SelectAll();
  Refresh();
}

void MathCtrl::OnSetFocus(wxFocusEvent& event) {

}

void MathCtrl::OnKillFocus(wxFocusEvent& event) {
  m_hCaretActive = false;
  m_hCaretPosition = NULL;
  SetActiveCell(NULL);
  Refresh();
}

void MathCtrl::CheckUnixCopy() {
  bool copy = false;
  wxConfig::Get()->Read(wxT("unixCopy"), &copy);

  if (copy) {
#if defined __WXGTK__
    wxTheClipboard->UsePrimarySelection(true);
#endif
    if (CanCopy() && wxTheClipboard->Open()) {
      wxTheClipboard->SetData(new wxTextDataObject(GetString()));
      wxTheClipboard->Close();
    }
#if defined __WXGTK__
    wxTheClipboard->UsePrimarySelection(false);
#endif
  }
}

bool MathCtrl::IsSelected(int type) {
  return m_selectionStart != NULL && m_selectionStart == m_selectionEnd
      && m_selectionStart->GetType() == type;
}

void MathCtrl::ScrollToBottom() {
  int width = -1, height = -1;
  GetVirtualSize(&width, &height);
  Scroll(-1, height/SCROLL_UNIT);
}

void MathCtrl::Animate(bool run) {
  if (CanAnimate()) {
    if (run) {
      SlideShow *tmp = (SlideShow *)m_selectionStart;
      tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());
      Refresh();

      m_animate = true;
      m_animationTimer.Start(ANIMATION_TIMER_TIMEOUT, true);
    }
    else
      m_animate = false;
  }
}

void MathCtrl::SetWorkingGroup(GroupCell *group) {
  if (m_workingGroup != NULL)
    m_workingGroup->SetWorking(false);
  m_workingGroup = group;
  if (m_workingGroup != NULL)
    m_workingGroup->SetWorking(group);
}

bool MathCtrl::IsSelectionInWorking() {
  if (m_selectionStart == NULL)
    return false;

  if (m_workingGroup == NULL)
    return false;

  if (m_selectionStart->GetParent() != m_workingGroup)
    return false;

  return true;
}

void MathCtrl::SetHCaret(MathCell *where, bool active) {
  if (active) {
    SetActiveCell(NULL);
    m_hCaretPosition = (GroupCell *)where;
    m_hCaretActive = true;
  }
  else {
    m_hCaretPosition = NULL;
    m_hCaretActive = false;
  }
}

BEGIN_EVENT_TABLE(MathCtrl, wxScrolledWindow)
  EVT_SIZE(MathCtrl::OnSize)
  EVT_PAINT(MathCtrl::OnPaint)
  EVT_LEFT_UP(MathCtrl::OnMouseLeftUp)
  EVT_RIGHT_UP(MathCtrl::OnMouseRightUp)
  EVT_LEFT_DOWN(MathCtrl::OnMouseLeftDown)
  EVT_LEFT_DCLICK(MathCtrl::OnDoubleClick)
  EVT_MOTION(MathCtrl::OnMouseMotion)
  EVT_ENTER_WINDOW(MathCtrl::OnMouseEnter)
  EVT_LEAVE_WINDOW(MathCtrl::OnMouseExit)
  EVT_TIMER(TIMER_ID, MathCtrl::OnTimer)
  EVT_TIMER(CARET_TIMER_ID, MathCtrl::OnTimer)
  EVT_TIMER(ANIMATION_TIMER_ID, MathCtrl::OnTimer)
  EVT_KEY_DOWN(MathCtrl::OnKeyDown)
  EVT_CHAR(MathCtrl::OnChar)
  EVT_ERASE_BACKGROUND(MathCtrl::OnEraseBackground)
  EVT_KILL_FOCUS(MathCtrl::OnKillFocus)
  EVT_SET_FOCUS(MathCtrl::OnSetFocus)
END_EVENT_TABLE()
