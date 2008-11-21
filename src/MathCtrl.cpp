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

#if wxUSE_DRAG_AND_DROP && WXM_DND
#include <wx/dnd.h>
#endif

#define SCROLL_UNIT 10
#define CARET_TIMER_TIMEOUT 500
#define ANIMATION_TIMER_TIMEOUT 300

void AddLineToFile(wxTextFile& output, wxString s, bool unicode = true);

enum
{
  TIMER_ID,
  CARET_TIMER_ID,
  ANIMATION_TIMER_ID
};

MathCtrl::MathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size) :
  wxScrolledWindow(parent, id, position, size,
  wxVSCROLL | wxHSCROLL | wxSUNKEN_BORDER | wxWANTS_CHARS) {
  m_scrollTo = -1;
  m_tree = NULL;
  m_memory = NULL;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_last = NULL;
  m_insertPoint = NULL;
  m_hCaretActive = true;
  m_hCaretPosition = NULL; // horizontal caret at the top of document
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
  dcm.SetBackground(*(wxTheBrushList->FindOrCreateBrush(GetBackgroundColour(),
      wxSOLID)));
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
  //
  // Draw horizontal caret
  //
  if (m_hCaretActive)
  {
    dcm.SetLogicalFunction(wxXOR);
    dcm.SetPen(*wxLIGHT_GREY_PEN);
    if (m_hCaretPosition == NULL)
    {
      dcm.DrawLine( 0, 5, 1000, 5);
    } else {
      wxRect currentGCRect = m_hCaretPosition->GetRect();
      int pad = ((int) MC_GROUP_SKIP) / 2;
      dcm.DrawLine( 0, currentGCRect.GetBottom() + pad, 3000,  currentGCRect.GetBottom() + pad);
    }
    
  }
  
  

  // Blit the memory image to the window
  dcm.SetDeviceOrigin(0, 0);
  dc.Blit(0, rect.GetTop(), sz.x, rect.GetBottom() - rect.GetTop() + 1, &dcm,
      0, rect.GetTop());
}

GroupCell* MathCtrl::CreateGCAfter(GroupCell *afterThisGC, wxString contents)
{
  GroupCell *newCell = new GroupCell;
  GroupCell *tmp;
  
  if (afterThisGC == NULL) // create at the top of the document
  {
    tmp = (GroupCell *)m_tree;
    m_tree = newCell;
    newCell->m_next = tmp;
  } else {
    tmp = (GroupCell *) afterThisGC->m_next;
    afterThisGC->m_next = newCell;
    newCell->m_next = tmp;
  }
   // TODO implement different types ?? if needed
  EditorCell * editor = new EditorCell;
  editor->SetValue( contents );
  newCell->AppendInput((MathCell *) editor  );
  
  SetActiveCell(newCell->GetEditable());
  
  
  return newCell;
}


/***
 * Add a new line
 */
void MathCtrl::InsertLine(MathCell *newCell, bool forceNewLine) {

  GroupCell *tmp = m_insertPoint;
  if (m_hCaretActive)
    tmp = m_hCaretPosition;
  
  if (tmp == NULL)
    tmp = m_last;

  newCell->ForceBreakLine(forceNewLine);

  if (newCell->GetType() == MC_TYPE_MAIN_PROMPT) {
    GroupCell *newGroup = new GroupCell;
    newGroup->SetInput(newCell);
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
  }

  else {
    if (newCell->GetType() == MC_TYPE_TITLE ||
        newCell->GetType() == MC_TYPE_SECTION ||
        newCell->GetType() == MC_TYPE_COMMENT)
      tmp->SetSpecial(true);
    tmp->AppendOutput(newCell);
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
void MathCtrl::PrependCell(int type, wxString value, bool refresh, bool prepend) {
  GroupCell *where;

  if (m_selectionStart == NULL)
    where = m_last;
  else {
    where = (GroupCell *)m_selectionStart->GetParent();
    if (!prepend && where->m_next != NULL)
      where = (GroupCell *)where->m_next;
  }

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

    if (IsSelected(MC_TYPE_IMAGE)) {
#if defined __WXMSW__
      popupMenu->Append(popid_image_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
#endif
      popupMenu->Append(popid_image, _("Save image"), wxEmptyString, wxITEM_NORMAL);
    } else {
      if (CanCopy()) {
        popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_copy_tex, _("Copy TeX"), wxEmptyString, wxITEM_NORMAL);
#if defined __WXMSW__
        popupMenu->Append(popid_copy_image, _("Copy as image"),
            wxEmptyString, wxITEM_NORMAL);
#endif
	popupMenu->Append(popid_copy_to_input, _("Copy to input"), wxEmptyString, wxITEM_NORMAL);

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
      } else {
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
  } else {
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
 * Left mouse - selection handling
 */
void MathCtrl::OnMouseLeftDown(wxMouseEvent& event) {
  m_animate = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

#if wxUSE_DRAG_AND_DROP && WXM_DND
  if (m_selectionStart != NULL)
  {
    MathCell *tmp = NULL;
    for (tmp = m_selectionStart; tmp != NULL && tmp != m_selectionEnd; tmp = tmp->m_next)
    if (tmp->ContainsPoint(m_down))
    break;
    if (tmp != NULL && (tmp != m_selectionEnd ||
            (tmp == m_selectionEnd && tmp->ContainsPoint(m_down))))
    {
      wxDropSource dragSource(this);
      wxTextDataObject my_data(GetString());
      dragSource.SetData( my_data );
      dragSource.DoDragDrop(true);
    }
    else
    m_leftDown = true;
  }
  else
  m_leftDown = true;
#else
  m_leftDown = true;
#endif
}

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  m_animate = false;
  if (!m_mouseDrag)
    SelectPoint(m_down);
  m_leftDown = false;
  m_mouseDrag = false;
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
 * Select the rectangle sorounded by point one and two.
 */
void MathCtrl::SelectRect() {
  if (m_tree == NULL)
    return;

  if (m_activeCell != NULL) {
    if (!m_activeCell->GetParent()->ContainsPoint(m_down))
      SetActiveCell(NULL);
  }

  // If we have an acrive cell handle it
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
  m_selectionStart = m_selectionEnd = NULL;
  wxRect rect;

  rect.x = MIN(m_down.x, m_up.x);
  rect.y = MIN(m_down.y, m_up.y);
  rect.width = MAX(ABS(m_down.x - m_up.x), 1);
  rect.height = MAX(ABS(m_down.y - m_up.y), 1);

  // find the first group in selection
  GroupCell *tmp = (GroupCell *)m_tree;
  while (tmp != NULL && !rect.Intersects(tmp->GetRect()) &&
      !rect.Intersects(tmp->HideRect()))
    tmp = (GroupCell *)tmp->m_next;

  // find the last group in selection
  m_selectionStart = tmp;
  m_selectionEnd = tmp;
  while (tmp != NULL) {
    if (rect.Intersects(tmp->GetRect()) || rect.Intersects(tmp->HideRect()))
      m_selectionEnd = tmp;
    tmp = (GroupCell *)tmp->m_next;
  }

  if (m_selectionStart != NULL && m_selectionEnd != NULL) {
    if (m_selectionStart == m_selectionEnd) {
      GroupCell *group = (GroupCell *)m_selectionStart;
      group->SelectRectGroup(rect, m_down, m_up, &m_selectionStart, &m_selectionEnd);
    }
  }

  if (m_selectionStart != NULL && m_selectionStart == m_selectionEnd
      && m_selectionStart->IsEditable()) {
    bool activate = true;
    wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);
    if (activate) {
      SetActiveCell(m_selectionStart);
      wxClientDC dc(this);
      m_activeCell->SelectRectText(dc, m_down, m_up);
    }
  }

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
  
  if (clickedBeforeGC != NULL) // clicked between groupcells
  {
    if (m_selectionStart != NULL) {
        m_selectionStart = NULL;
        m_selectionEnd = NULL;
      }
    m_hCaretActive = true;
    m_hCaretPosition = (GroupCell *) clickedBeforeGC->m_previous;
    Refresh();
    return;
  } else if (point.y > (m_last->GetRect()).GetBottom()) // clicked below last groupcell
  {
    if (m_selectionStart != NULL) {
        m_selectionStart = NULL;
        m_selectionEnd = NULL;
      }
    m_hCaretActive = true;
    m_hCaretPosition = (GroupCell *) m_last;
    Refresh();
    return;
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
      bool activate = true;
      wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);
      if (activate) {
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
      if (tr->IsEditable()) {
        bool activate = true;
        wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);
        if (activate) {
          SetActiveCell(tr);
          wxClientDC dc(this);
          m_activeCell->SelectPointText(dc, m_down);
        }
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
  if (m_selectionStart == NULL || m_selectionEnd == NULL || m_insertPoint
      != NULL)
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

  GroupCell *start = (GroupCell *)m_selectionStart->GetParent();
  GroupCell *end = (GroupCell *)m_selectionEnd->GetParent();

  if (start == NULL || end == NULL)
    return;

  SetActiveCell(NULL);

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


  if (newSelection != NULL)
    m_selectionStart =  m_selectionEnd = newSelection->GetEditable();

  Recalculate(false);
  AdjustSize(false);
  Refresh();
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
        } else {
          SetActiveCell(m_selectionStart);
        }
      } else if (m_activeCell != NULL) {
        if (event.ControlDown() || event.ShiftDown()) {
          if (m_activeCell->GetType() == MC_TYPE_INPUT)
            m_activeCell->AddEnding();
          wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, deactivate_cell_ok);
          (wxGetApp().GetTopWindow())->ProcessEvent(ev);
        } else
          event.Skip();
      } else if (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_TEXT) {
        PrependCell(MC_TYPE_INPUT, GetString(), true, false);
        SelectNextInput();
        SetActiveCell(m_selectionStart);
      } else
        event.Skip();
      break;

    case WXK_ESCAPE:
      if (m_activeCell == NULL) {
        SetSelection(NULL);
        Refresh();
      } else {
      /*
        if (m_activeCell->GetType() == MC_TYPE_INPUT) {
          if (m_activeCell->AddEnding()) {
            m_activeCell->ResetData();
            GroupCell *group = (GroupCell *)m_activeCell->GetParent();
            Recalculate(false);
          }
        }
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, deactivate_cell_cancel);
        (wxGetApp().GetTopWindow())->ProcessEvent(ev);
        */
        m_hCaretPosition = (GroupCell *)m_activeCell->GetParent();
        SetActiveCell(NULL);
        m_hCaretActive = true;
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
        ((EditorCell *)m_activeCell)->CaretAtStart()) {
        
        m_hCaretPosition = (GroupCell *)(m_activeCell->GetParent())->m_previous;
        m_hCaretActive = true;
        SetActiveCell(NULL);
    //  if (SelectPrevInput())
    //    ((EditorCell *)m_activeCell)->CaretToEnd();
      return;
    }

    if (event.GetKeyCode() == WXK_DOWN &&
        ((EditorCell *)m_activeCell)->CaretAtEnd()) {
        m_hCaretPosition = (GroupCell *)(m_activeCell->GetParent());
        m_hCaretActive = true;
        SetActiveCell(NULL);
     // if (SelectNextInput())
     //   ((EditorCell *)m_activeCell)->CaretToStart();
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
    } else {
      wxRect rect = m_activeCell->GetRect();
      CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
      rect.width = GetSize().x;
      RefreshRect(rect);
    }

    ShowPoint(point);

  }
  else { // m_activeCell == NULL
    switch (event.GetKeyCode()) { 

      case WXK_UP:
        //if (!SelectPrevInput())
        //  event.Skip();
        //else
        //  Refresh();
        if (m_hCaretActive)
          if (m_hCaretPosition != NULL)
          {
            MathCell * editor = m_hCaretPosition->GetEditable();
            if( editor != NULL) 
            {
              SetActiveCell(editor);
              m_hCaretActive = false;
              ((EditorCell *)m_activeCell)->CaretToEnd();
            } else { // can't get editor.. jump over cell..
            
              m_hCaretPosition = (GroupCell *) m_hCaretPosition->m_previous;
              Refresh();
            }
          }
        
        break;

      case WXK_DOWN:
        //if (!SelectNextInput())
        //  event.Skip();
        //else
        //  Refresh();
        if (m_hCaretActive)
        {
          if (m_hCaretPosition == NULL)
          {
            if ( ((GroupCell *)m_tree)->GetEditable() != NULL ) // try to edit the first cell
            {
              MathCell * editor = ((GroupCell *)m_tree)->GetEditable();
              SetActiveCell(editor);
              m_hCaretActive = false;
              ((EditorCell *)m_activeCell)->CaretToStart();
              
            } else { // else jump over
            m_hCaretPosition = (GroupCell *)m_tree;  
            Refresh();
            }
            
          } else if (m_hCaretPosition->m_next != NULL)
          {
            MathCell * editor = ((GroupCell *)m_hCaretPosition->m_next)->GetEditable();
            if( editor != NULL) 
            {
              SetActiveCell(editor);
              m_hCaretActive = false;
              ((EditorCell *)m_activeCell)->CaretToStart();
            } else { // can't get editor.. jump over cell..
              
              m_hCaretPosition = (GroupCell *) m_hCaretPosition->m_next;
             Refresh();
            }
          
          }
        
        
        }
        
        break;

      case WXK_LEFT:
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
        //////
        if (m_hCaretActive)
        {
          CreateGCAfter(m_hCaretPosition,  event.GetUnicodeKey()); // ZIGA bug bug
        
        }
        //////
        event.Skip();
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
      } else {
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
        AddLineToFile(output, wxEmptyString, false);
        if (wxm)
          AddLineToFile(output, wxT("/* [wxMaxima: input   start ] */"), false);
        wxString input = txt->ToString(false);
        AddLineToFile(output, input, false);
        if (wxm)
          AddLineToFile(output, wxT("/* [wxMaxima: input   end   ] */"), false);
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
  if (CanEdit()) {
    if (event.ControlDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_reeval);
      (wxGetApp().GetTopWindow())->ProcessEvent(ev);
    } else {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_edit);
      (wxGetApp().GetTopWindow())->ProcessEvent(ev);
    }
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

  bool activate = true;
  wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);
  if (activate) {
    SetActiveCell(m_selectionStart);
    ((EditorCell *)m_activeCell)->CaretToEnd();
  }

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
    inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = (GroupCell *)tmp->m_next;
  }

  if (inpt == NULL)
    return false;

  m_selectionStart = m_selectionEnd = inpt;
  ScrollToSelectionStart(false);

  bool activate = true;
  wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);
  if (activate) {
    SetActiveCell(m_selectionStart);
    ((EditorCell *)m_activeCell)->CaretToStart();
  }

  Refresh();

  return true;
}

bool MathCtrl::SelectLastInput() {
  if (m_last == NULL)
    return false;

  GroupCell *tmp = (GroupCell *)m_last->m_previous;
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

  ScrollToSelectionStart();

  bool activate = true;
  wxConfig::Get()->Read(wxT("activateOnSelect"), &activate);

  if (activate) {
    SetActiveCell(m_selectionStart);
    ((EditorCell *)m_activeCell)->CaretToStart();
  }

  return true;
}

bool MathCtrl::SelectFirstInput() {
  if (m_tree == NULL)
      return false;

  MathCell *input = m_tree->GetInput();
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

  if (tmp == NULL)
    return false;

  m_selectionStart = m_selectionEnd = tmp;
  Refresh();

  return true;
}

void MathCtrl::ScrollToSelectionStart(bool top) {
  if (m_selectionStart == NULL)
    return;

  MathCell *tmp = m_selectionStart->GetParent();
  if (tmp == NULL)
    return;

  int cellY = tmp->GetCurrentY();

  if (cellY == -1)
    return;

  int cellDrop = m_selectionStart->GetDrop();
  int cellCenter = m_selectionStart->GetCenter();

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
  if (m_activeCell != NULL) {
    m_activeCell->ActivateCell();
  }

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

void MathCtrl::PasteFromClipboard() {
  if (m_activeCell == NULL)
    return;

  m_activeCell->PasteFromClipboard();
  Recalculate(false);
  Refresh();
}

void MathCtrl::SelectAll() {
  if (m_activeCell == NULL)
    return;

  m_activeCell->SelectAll();
  Refresh();
}

void MathCtrl::OnSetFocus(wxFocusEvent& event) {
  if (m_activeCell != NULL)
    m_activeCell->SetFocus(true);
}

void MathCtrl::OnKillFocus(wxFocusEvent& event) {
  if (m_activeCell != NULL)
    m_activeCell->SetFocus(false);
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
