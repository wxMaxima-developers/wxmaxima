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

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>

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
  wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
  | wxSUNKEN_BORDER
#endif
  )
{
  m_tree = NULL;
  m_memory = NULL;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_clickType = CLICK_TYPE_NONE;
  m_clickInGC = NULL;
  m_last = NULL;
  m_insertPoint = NULL;
  m_hCaretActive = false;
  m_hCaretPosition = NULL; // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_activeCell = NULL;
  m_leftDown = false;
  m_mouseDrag = false;
  m_mouseOutside = false;
  m_editingEnabled = true;
  m_switchDisplayCaret = true;
  m_timer.SetOwner(this, TIMER_ID);
  m_caretTimer.SetOwner(this, CARET_TIMER_ID);
  m_animationTimer.SetOwner(this, ANIMATION_TIMER_ID);
  m_animate = false;
  m_workingGroup = NULL;
  m_saved = true;
  AdjustSize();
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
  SetBackgroundColour(wxColour(bgColStr));

  dcm.SelectObject(*m_memory);
  dcm.SetBackground(*(wxTheBrushList->FindOrCreateBrush(GetBackgroundColour(), wxSOLID)));
  dcm.Clear();
  PrepareDC(dcm);
  dcm.SetMapMode(wxMM_TEXT);
  dcm.SetBackgroundMode(wxTRANSPARENT);

  CellParser parser(dcm);
  parser.SetBouns(top, bottom);

  // Draw content
  if (m_tree != NULL)
  {
    //
    // First draw selection under content with wxCOPY and selection brush/color
    //
    if (m_selectionStart != NULL)
    {
      MathCell* tmp = m_selectionStart;

      dcm.SetLogicalFunction(wxCOPY);
#if defined(__WXMAC__)
      dcm.SetPen(wxNullPen); // wxmac doesn't like a border with wxXOR
#else
      dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, 1)));
// window linux, set a pen
#endif
      dcm.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION)))); //highlight c.

      if (m_selectionStart->GetType() == MC_TYPE_GROUP) // selection of groups
      {
        while (tmp != NULL)
        {

          wxRect rect = tmp->GetRect();
          // TODO globally define x coordinates of the left GC brackets
          dcm.DrawRectangle( 3, rect.GetTop() - 2, MC_GROUP_LEFT_INDENT, rect.GetHeight() + 5);

          if (tmp == m_selectionEnd)
              break;
          tmp = tmp->m_next;
        }

      }
      else {  // We have a selection of output
        while (tmp != NULL) {
          if (!tmp->m_isBroken && !tmp->m_isHidden && tmp->GetType() != MC_TYPE_SLIDE &&
              m_activeCell != tmp)
            tmp->DrawBoundingBox(dcm, false);
          if (tmp == m_selectionEnd)
            break;
          tmp = tmp->m_nextToDraw;
        } // end while (1)
      }
    }
    // draw content over selection
    wxPoint point;
    point.x = MC_GROUP_LEFT_INDENT;
    point.y = MC_BASE_INDENT + m_tree->GetMaxCenter();
    // Draw tree
    MathCell* tmp = m_tree;
    drop = tmp->GetMaxDrop();
    bool changeAsterisk = false;
    config->Read(wxT("changeAsterisk"), &changeAsterisk);
    parser.SetChangeAsterisk(changeAsterisk);

    while (tmp != NULL)
    {
      tmp->m_currentPoint.x = point.x;
      tmp->m_currentPoint.y = point.y;
      if (tmp->DrawThisCell(parser, point))
        tmp->Draw(parser, point, MAX(fontsize, MC_MIN_SIZE), false);
      if (tmp->m_next != NULL) {
        point.x = MC_GROUP_LEFT_INDENT;
        point.y += drop + tmp->m_next->GetMaxCenter();
        point.y += MC_GROUP_SKIP;
        drop = tmp->m_next->GetMaxDrop();
      }
      tmp = tmp->m_next;
    }

  }
  //
  // Draw horizontal caret
  //
  if (m_hCaretActive && m_hCaretPositionStart == NULL)
  {
    dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CURSOR), 1, wxSOLID))); // TODO is there more efficient way to do this?

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
void MathCtrl::InsertLine(MathCell *newCell, bool forceNewLine, bool hide)
{
  SetActiveCell(NULL);

  m_saved = false;

  GroupCell *tmp = m_insertPoint;

  if (tmp == NULL)
    tmp = m_last;

  if (tmp == NULL && newCell->GetType() != MC_TYPE_MAIN_PROMPT)
  {
    GroupCell *newGroup = new GroupCell;
    TextCell *prompt = new TextCell;
    prompt->SetValue(wxEmptyString);
    prompt->SetType(MC_TYPE_MAIN_PROMPT);
    newGroup->SetInput(prompt);
    newGroup->SetSpecial(true);
    tmp = m_tree = m_last = newGroup;
  }

  newCell->ForceBreakLine(forceNewLine);

  if (newCell->GetType() == MC_TYPE_MAIN_PROMPT)
  {
    GroupCell *newGroup = new GroupCell;
    if (hide)
      newGroup->Hide(true);
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

  else if (newCell->GetType() == MC_TYPE_INPUT)
  {
    tmp->AppendInput(newCell);
  }

  else
  {
    if (newCell->GetType() == MC_TYPE_TITLE ||
        newCell->GetType() == MC_TYPE_SECTION ||
        newCell->GetType() == MC_TYPE_TEXT ||
        newCell->GetType() == MC_TYPE_HEADER)
      tmp->SetSpecial(true);
    tmp->AppendOutput(newCell);
    if (newCell->GetType() == MC_TYPE_PROMPT)
    {
      m_workingGroup = tmp;
      ScrollToCell(tmp->GetParent());
      OpenHCaret();
    }
  }

  while (newCell != NULL)
  {
    newCell->SetParent(tmp, false);
    newCell = newCell->m_next;
  }

  m_selectionStart = NULL;
  m_selectionEnd = NULL;

  tmp->ResetSize();
  Recalculate();
  ScrollToCell(tmp);

  Refresh();
}

/***
 * Prepend a new cell
 */
GroupCell* MathCtrl::PrependGroup(int type, wxString value, bool refresh, bool prepend)
{
  GroupCell *where;

  if (m_hCaretActive) {
    if (m_hCaretPosition == NULL)
      where = m_tree;
    else
      where = m_hCaretPosition;
  }
  else if (m_selectionStart != NULL)
    where = (GroupCell *)m_selectionStart->GetParent();
  else
    where = m_last;

  GroupCell *newGroup = new GroupCell;

  TextCell *prompt = new TextCell;
  if (type == MC_TYPE_INPUT)
    prompt->SetValue(wxT(">> "));
  else {
    newGroup->SetSpecial(true);
    prompt->SetValue(wxEmptyString);
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

  if (m_tree == NULL) {
    m_tree = m_last = newGroup;
  }
  else {
    if (where == m_tree && prepend) {
      newGroup->m_next = m_tree;
      newGroup->m_nextToDraw = m_tree;
      m_tree->m_previous = newGroup;
      m_tree->m_previousToDraw = newGroup;
      m_tree = newGroup;
    }

    else {
      if (prepend) {
        where->m_previous->m_next = newGroup;
        where->m_previous->m_nextToDraw = newGroup;
        newGroup->m_previous = where->m_previous;
        newGroup->m_previousToDraw = where->m_previous;

        newGroup->m_next = where;
        newGroup->m_nextToDraw = where;
        where->m_previous = newGroup;
        where->m_previousToDraw = newGroup;
      }
      else {
        if (where->m_next != NULL)
          where->m_next->m_previous = newGroup;
        if (where->m_nextToDraw != NULL)
          where->m_next->m_previousToDraw = newGroup;
        newGroup->m_next = where->m_next;
        newGroup->m_nextToDraw = where->m_nextToDraw;

        newGroup->m_previous = where;
        newGroup->m_previousToDraw = where;
        where->m_next = newGroup;
        where->m_nextToDraw = newGroup;

        if (where == m_last)
          m_last = newGroup;
      }
    }
  }

  SetActiveCell(NULL);

  Recalculate();

  if (refresh)
    Refresh();

  return newGroup;
}

/***
 * Recalculate dimensions of cells
 */
void MathCtrl::RecalculateForce() {
  Recalculate(true);
}

void MathCtrl::Recalculate(bool force) {

  MathCell *tmp = m_tree;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(force);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);

  wxPoint point;
  point.x = MC_GROUP_LEFT_INDENT;
  point.y = MC_BASE_INDENT ;

  if (tmp != NULL)
    point.y += m_tree->GetMaxCenter();

  while (tmp != NULL) {
    tmp->m_currentPoint.x = point.x;
    tmp->m_currentPoint.y = point.y;
    tmp->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
    tmp->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);
    point.y += tmp->GetMaxDrop();
    tmp = tmp->m_next;
    if (tmp != NULL)
      point.y += tmp->GetMaxCenter();
    point.y += MC_GROUP_SKIP;
  }

  AdjustSize();
}

/***
 * Resize the controll
 */
void MathCtrl::OnSize(wxSizeEvent& event) {
  wxDELETE(m_memory);

  if (m_tree != NULL) {
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    RecalculateForce();
  }
  else
    AdjustSize();

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
  Recalculate();
  Refresh();
  Scroll(0, 0);
}

/***
 * Reset all input promts to ">> "
 * Called when Restart Maxima is called from Maxima menu
 */
void MathCtrl::ResetInputPrompts() {
  MathCell* tmp = m_tree;

  while (tmp != NULL)
  {
    if ( ((GroupCell*)tmp)->GetInput() != NULL) {
      ((TextCell*) ( ((GroupCell*)tmp)->GetPrompt() ))->SetValue(wxT(">> "));
    }
    tmp = tmp->m_next;
  }

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

    if (IsSelected(MC_TYPE_IMAGE) || IsSelected(MC_TYPE_SLIDE)) {
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
      }

      if (IsSelected(MC_TYPE_DEFAULT) || IsSelected(MC_TYPE_LABEL)) {
        popupMenu->AppendSeparator();
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

  // popup menu in active cell
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
 * Sets m_clickType and m_clickInGC according to what we clicked,
 * and selects appropriately.
 * m_clickType is used in ClickNDrag when click-draging to determine what kind of selection
 * behaviour we want.
 *
 * - check in which GroupCell it falls
 * - if it falls between groupCells activate caret and CLICK_TYPE_GROUP_SELECTION
 * - if it falls within a groupcell investigate where did it fall (input or output)
 */
void MathCtrl::OnMouseLeftDown(wxMouseEvent& event) {
  m_animate = false;
  m_leftDown = true;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

  if (m_tree == NULL)
    return ;

  // default when clicking
  m_clickType = CLICK_TYPE_NONE;
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
    SetHCaret(tmp->m_previous);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
  } // end if (clickedBeforeGC != NULL) // we clicked between groupcells, set hCaret

  else if (clickedInGC != NULL) { // we clicked in a groupcell, find out where

    if (m_down.x <= MC_GROUP_LEFT_INDENT) { // we clicked in left bracket area
      if ((clickedInGC->HideRect()).Contains(m_down)) // did we hit the hide rectancle
      {
        clickedInGC->SwitchHide(); // todo if there's nothin to hide, select as normal
        clickedInGC->ResetSize();
        Recalculate();
        m_clickType = CLICK_TYPE_NONE; // ignore drag-select
      }
      else {
        m_clickType = CLICK_TYPE_GROUP_SELECTION;
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
          m_clickType = CLICK_TYPE_INPUT_SELECTION;
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
            m_clickType = CLICK_TYPE_INPUT_SELECTION;
            Refresh();
            return;
          }
          else {
            m_clickType = CLICK_TYPE_OUTPUT_SELECTION;
            m_clickInGC = clickedInGC;
          }
        }
      }

    } // end we didn't click in left bracket space

  } // end if (clickedInGC != NULL) // we clicked in a groupcell, find out where

  else { // we clicked below last groupcell (both clickedInGC and clickedBeforeGC == NULL)
    // set hCaret (or activate last cell?)
    SetHCaret(m_last);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
  }
  Refresh();
}

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  m_animate = false;
  m_leftDown = false;
  m_mouseDrag = false;
  m_clickInGC = NULL; // pointer to NULL to prevent crashes if the cell is deleted
  CheckUnixCopy();
  SetFocus();
}

void MathCtrl::OnMouseMotion(wxMouseEvent& event) {
  if (m_tree == NULL || !m_leftDown)
    return;
  m_mouseDrag = true;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_up.x, &m_up.y);
  if (m_mouseOutside) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
  ClickNDrag(m_down, m_up);
}

/***
 * Select the rectangle surounded by down and up. Called from OnMouseMotion.
 *
 * The method decides what to do, based on the value of m_clickType which
 * was set previously in OnMouseLeftDown. This enables different selection behaviours
 * depending on where we first clicked. If m_clickType equals
 * CLICK_TYPE_NONE - click-draging does not result in a selection (we clicked in hideRect for instance)
 * CLICK_TYPE_GROUP_SELECTION - we are selecting full groupcells only. Only y-coordinate matters.
 * CLICK_TYPE_INPUT_SELECTION - we clicked in an editor (GroupCell::GetEditable()) and draging
 *   results in selecting text in EditorCell
 * CLICK_TYPE_OUTPUT_SELECTION - we clicked in an output, we want selection to be confined to that
 *   GroupCell's output. GC we first clicked in was stored in OnMouseMotion method
 *   into m_clickInGC pointer.
 */
void MathCtrl::ClickNDrag(wxPoint down, wxPoint up) {

  MathCell *st = m_selectionStart, *en = m_selectionEnd;
  wxRect rect;

  switch (m_clickType)
  {
    case CLICK_TYPE_NONE:
      return;

    case CLICK_TYPE_INPUT_SELECTION:
      if (m_activeCell != NULL) {
        wxClientDC dc(this);
        m_activeCell->SelectRectText(dc, down, up);
        m_switchDisplayCaret = false;
        wxRect rect = m_activeCell->GetRect();
        CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
        RefreshRect(rect);
        return;
      }
    case CLICK_TYPE_GROUP_SELECTION:
    {
      m_selectionStart = m_selectionEnd = NULL;
      int ytop    = MIN( down.y, up.y );
      int ybottom = MAX( down.y, up.y );
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
        SetHCaret(m_last);
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
        SetHCaret(m_selectionEnd);
        m_selectionStart = m_selectionEnd = NULL;
      }
      else {
        m_hCaretActive = false;
        m_hCaretPosition = NULL;
      }
      break;
    }

    case CLICK_TYPE_OUTPUT_SELECTION:
      m_selectionStart = m_selectionEnd = NULL;
      rect.x = MIN(down.x, up.x);
      rect.y = MIN(down.y, up.y);
      rect.width = MAX(ABS(down.x - up.x), 1);
      rect.height = MAX(ABS(down.y - up.y), 1);

      if (m_clickInGC != NULL)
        m_clickInGC->SelectRectInOutput(rect, down, up, &m_selectionStart, &m_selectionEnd);
      break;

    default:
      break;
  } // end switch

  // Refresh only if the selection has changed
  if (st != m_selectionStart || en != m_selectionEnd)
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

  while (tmp != NULL) {

    if (!tmp->IsSpecial())
    {
      s += wxT("/* [wxMaxima: input   start ] */\n");
      s += tmp->GetInput()->ToString(false) + wxT("\n");
      s += wxT("/* [wxMaxima: input   end   ] */\n");
    }
    if (tmp == end)
      break;
    tmp = (GroupCell *)tmp->m_next;
  }

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

/***
 * CanDeleteSelection
 * Returns true if we have a selection of groupcells and we have no working group!
 */
bool MathCtrl::CanDeleteSelection() {
  if (m_selectionStart == NULL || m_selectionEnd == NULL ||
      m_insertPoint != NULL || m_workingGroup != NULL)
    return false;

  if ((m_selectionStart->GetType() != MC_TYPE_GROUP) || (m_selectionEnd->GetType() != MC_TYPE_GROUP))
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

  m_saved = false;

  SetActiveCell(NULL);
  m_hCaretActive = false;
  m_hCaretPosition = NULL;

  GroupCell *newSelection = (GroupCell *)end->m_next;

  if (end == m_last)
    m_last = (GroupCell *)start->m_previous;

  if (start == m_tree) {
    if (end->m_previous != NULL) {
      end->m_previous->m_nextToDraw = NULL;
      end->m_previous->m_next = NULL;
    }
    if (end->m_next != NULL) {
      end->m_next->m_previous = NULL;
      end->m_next->m_previousToDraw = NULL;
    }

    m_tree = (GroupCell *)end->m_next;
    end->m_next = NULL;
    DestroyTree(start);
  }

  else {
    start->m_previous->m_next = end->m_next;
    start->m_previous->m_nextToDraw = end->m_next;
    if (end->m_next != NULL) {
      end->m_next->m_previous = start->m_previous;
      end->m_next->m_previousToDraw = start->m_previous;
      end->m_next = NULL;
    }
    DestroyTree(start);
  }

  m_selectionStart = m_selectionEnd = NULL;
  if (newSelection != NULL)
    SetHCaret(newSelection->m_previous);
  else
    SetHCaret(m_last);

  Recalculate();
  Refresh();
}

void MathCtrl::OpenHCaret(wxString txt, int type)
{
  m_saved = false;

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

  if (m_activeCell != NULL)
    SetHCaret(m_activeCell->GetParent());
  else if (m_selectionStart != NULL)
    SetHCaret(m_selectionStart->GetParent());

  if (!m_hCaretActive) {
    if (m_last == NULL)
      return;
    SetHCaret(m_last);
  }

  if (m_hCaretPosition != NULL) {
    SetSelection(m_hCaretPosition);
    GroupCell *group = PrependGroup(type, txt, false, false);
    SetActiveCell(group->GetEditable());
    ((EditorCell *)m_activeCell)->CaretToEnd();
    ((EditorCell *)m_activeCell)->ClearUndo();
    ScrollToCell(group);
  }
  else {
    SetSelection(m_tree);
    GroupCell *group = PrependGroup(type, txt, false, true);
    SetActiveCell(group->GetEditable());
    ((EditorCell *)m_activeCell)->CaretToEnd();
    ((EditorCell *)m_activeCell)->ClearUndo();
    ScrollToCell(group);
  }

  Refresh();
}

/***
 * Support for copying and deleting with keyboard
 */
void MathCtrl::OnKeyDown(wxKeyEvent& event) {
  switch (event.GetKeyCode()) {

    case WXK_DELETE:
    case WXK_BACK:
      if (CanDeleteSelection()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
        GetParent()->ProcessEvent(ev);
      } else
        event.Skip();
      break;

    case WXK_RETURN:
      if (m_activeCell != NULL) {
        bool enterEvaluates = false;
        wxConfig::Get()->Read(wxT("enterEvaluates"), &enterEvaluates);
        if (!enterEvaluates &&  (event.ControlDown() || event.ShiftDown()) ||
             enterEvaluates && !(event.ControlDown() || event.ShiftDown()))
        {
          wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, menu_reeval_input);
          GetParent()->ProcessEvent(ev);
        } else
          event.Skip();
      }

      else if (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_DEFAULT) {
        PrependGroup(MC_TYPE_INPUT, GetString(), true, false);
        ActivateNextInput();
      }

      else
        event.Skip();
      break;

    case WXK_ESCAPE:
      if (m_activeCell == NULL) {
        SetSelection(NULL);
        Refresh();
      }

      else
        SetHCaret(m_activeCell->GetParent());

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
      SetHCaret((m_activeCell->GetParent())->m_previous);
      return;
    }

    if (event.GetKeyCode() == WXK_DOWN &&
        ((EditorCell *)m_activeCell)->CaretAtEnd() &&
        !event.ShiftDown()) {
      SetHCaret(m_activeCell->GetParent());
      return;
    }

    m_activeCell->ProcessEvent(event);

    // CTRL+"s deactivates on MAC
    if (m_activeCell == NULL)
      return;

    m_switchDisplayCaret = false;

    wxClientDC dc(this);
    CellParser parser(dc);
    parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);

    wxPoint point = m_activeCell->PositionToPoint(parser);

    if (m_activeCell->IsDirty()) {
      m_saved = false;

      int height = m_activeCell->GetHeight();

      int fontsize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontsize);

      m_activeCell->ResetData();
      GroupCell *group = (GroupCell *)m_activeCell->GetParent();
      group->ResetSize();
      group->ResetData();
      group->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
      group->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);

      if (height != m_activeCell->GetHeight())
        hasHeightChanged = true;
    }

    if (hasHeightChanged) {
      Recalculate();
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
              ScrollToCell(m_hCaretPositionEnd);
          }

          else {
            if (m_selectionStart != NULL) { // if we have selection set hCaret at the top, deselect
              SetHCaret(m_selectionStart->GetParent()->m_previous);
            }
            else if (m_hCaretPosition != NULL)
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
          if (m_selectionStart != NULL) { // if we have selection set hCaret at the top, deselect
            SetHCaret(m_selectionStart->GetParent()->m_previous);
          }
          else if (!ActivatePrevInput())
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
              else if (m_hCaretPosition->m_next != NULL)
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
              ScrollToCell(m_hCaretPositionEnd);
          }

          else {
            if (m_selectionEnd != NULL) { // if we have selection set hCaret at the top, deselect
              SetHCaret(m_selectionEnd->GetParent());
            }
            else if (m_tree != NULL && m_hCaretPosition == NULL)
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

            else if (m_hCaretPosition != NULL && m_hCaretPosition->m_next != NULL)
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

            else
              event.Skip();
          }
        }

        else {
          if (m_selectionEnd != NULL) { // if we have selection set hCaret at the top, deselect
            SetHCaret(m_selectionEnd->GetParent());
          }
          else if (!ActivateNextInput())
            event.Skip();
          else
            Refresh();
        }

        break;

      case WXK_HOME: // TODO: if shift down, select.
        SetHCaret(NULL);
        if (m_tree != NULL)
          ScrollToCell(m_tree);
        break;

      case WXK_END:
        SetHCaret(m_last);
        if (m_last != NULL)
          ScrollToCell(m_last);
        break;

      default:
        m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

        switch (event.GetKeyCode())
        {
          // keycodes which are ignored
          case WXK_PAGEUP:
          case WXK_PAGEDOWN:
          case WXK_LEFT:
          case WXK_RIGHT:
            event.Skip();
            break;
          // delete key and backspace select cell, so pressing key twice deletes the cell
          case WXK_BACK:
            if (m_hCaretPosition != NULL) {
              SetSelection(m_hCaretPosition);
              m_hCaretActive = false;
              Refresh();
              return;
            }
            break;
          case WXK_DELETE:
            if (m_hCaretPosition == NULL) {
              if (m_tree != NULL) {
                SetSelection(m_tree);
                m_hCaretActive = false;
                Refresh();
                return;
              }
            }
            else if (m_hCaretPosition->m_next != NULL) {
              SetSelection(m_hCaretPosition->m_next);
              m_hCaretActive = false;
              Refresh();
              return;
            }
            break;
          case WXK_RETURN:
            OpenHCaret(wxT("\n"));
            break;
					// keycodes which open hCaret with initial content
          default:
          {
#if wxUSE_UNICODE
            wxString txt(event.GetUnicodeKey());
#else
            wxString txt = wxString::Format(wxT("%c"), event.GetKeyCode());
#endif
            OpenHCaret(txt);
          }
        }
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
 * Adjust the virtual size and scrollbars.
 */
void MathCtrl::AdjustSize() {
  int width= MC_BASE_INDENT, height= MC_BASE_INDENT;
  int clientWidth, clientHeight, virtualHeight;

  GetClientSize(&clientWidth, &clientHeight);
  if (m_tree != NULL)
    GetMaxPoint(&width, &height);
  // when window is scrolled all the way down, document occupies top 1/8 of clientHeight
  height += clientHeight - (int)(1.0/8.0*(float)clientHeight);
  virtualHeight = MAX(clientHeight  + 10 , height); // ensure we always have VSCROLL active

  SetVirtualSize(width, virtualHeight);
  SetScrollRate(SCROLL_UNIT, SCROLL_UNIT);
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

  tmp1 = tmp1->m_next;
  while (tmp1 != NULL) {
    tmp->AppendCell(tmp1->Copy(false));
    tmp = tmp->m_next;
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
          case MC_TYPE_TEXT:
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
  GroupCell *tmp = (GroupCell *)m_tree;
  int count = 0;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxT("/") + filename + wxT("_img");
  int imgCounter = 0;

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
    wxString s = tmp->ToTeX(false, imgDir, filename, &imgCounter);
    AddLineToFile(output, s);
    tmp = (GroupCell *)tmp->m_next;
  }

  //
  // Close document
  //
  AddLineToFile(output, wxT("\\end{document}"));

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

bool MathCtrl::ExportToMAC(wxString file)
{
  m_saved = true;

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

  m_saved = true;

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

    if (tmp->IsHidden())
    {
      AddLineToFile(output, wxEmptyString, false);
      AddLineToFile(output, wxT("/* [wxMaxima: hide output   ] */"), false);
    }
    else
      AddLineToFile(output, wxEmptyString, false);

    // Write input
    if (!tmp->IsSpecial()) {
      MathCell *txt = tmp->GetInput();
      if (txt != NULL) {
        wxString input = txt->ToString(false);
        if (input.Length()>0) {
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
      MathCell *txt = tmp->GetLabel();

      if (wxm) {
        switch (txt->GetType()) {
          case MC_TYPE_TEXT:
            AddLineToFile(output, wxT("/* [wxMaxima: comment start ]"), false);
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("/* [wxMaxima: section start ]"), false);
            break;
          case MC_TYPE_TITLE:
            AddLineToFile(output, wxT("/* [wxMaxima: title   start ]"), false);
            break;
          default:
            AddLineToFile(output, wxT("/*"), false);
        }
      }
      else
        AddLineToFile(output, wxT("/*"), false);

      wxString comment = txt->ToString(false);
      AddLineToFile(output, comment, false);

      if (wxm) {
        switch (txt->GetType()) {
          case MC_TYPE_TEXT:
            AddLineToFile(output, wxT("   [wxMaxima: comment end   ] */"), false);
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("   [wxMaxima: section end   ] */"), false);
            break;
          case MC_TYPE_TITLE:
            AddLineToFile(output, wxT("   [wxMaxima: title   end   ] */"), false);
            break;
          default:
            AddLineToFile(output, wxT("*/"), false);
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

bool MathCtrl::ExportToWDR(wxString file)
{
  m_saved = true;

  if(wxFileExists(file))
    if(!wxRemoveFile(file))
      return false;

  wxFFileOutputStream out(file);
  wxZipOutputStream zip(out);
  wxTextOutputStream output(zip);

  zip.PutNextEntry(_T("out"));
  output << _T("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
  output << _T("<wxMaxima>\n");

  GroupCell* tmp = (GroupCell *)m_tree;
  // Write contents //
  while (tmp != NULL) {
    // Write input
    if (!tmp->IsSpecial()) {
      MathCell *txt = tmp->GetInput();
      if (txt != NULL) {
        wxString input = txt->ToString(false);
        if (input.Length()>0) {
          output << wxT("<input>\n");
          output << input;
          output << wxT("\n</input>\n");
        }
      }
	    // Write output
      txt = tmp->GetLabel();
      if (txt != NULL) {
        wxString out = txt->ToXml(true);
        if (out.Length()>0) {
          output << wxT("<mth>\n");
          output << out;
          output << wxT("\n</mth>\n");
        }
      }
    }
    else {
      // Write text
      MathCell *txt = tmp->GetLabel();
      switch (txt->GetType()) {
        case MC_TYPE_TEXT:
          output << wxT("<comment>\n");
          break;
        case MC_TYPE_SECTION:
         output << wxT("<section>\n");
          break;
        case MC_TYPE_TITLE:
          output << wxT("<title>\n");
          break;
      }
      wxString comment = txt->ToString(false);
      output << comment;
      switch (txt->GetType()) {
        case MC_TYPE_TEXT:
          output << wxT("\n</comment>\n");
          break;
        case MC_TYPE_SECTION:
         output << wxT("\n</section>\n");
          break;
        case MC_TYPE_TITLE:
          output << wxT("\n</title>\n");
          break;
      }
    }
    tmp = (GroupCell *)tmp->m_next;
  }
  output << _T("</wxMaxima>");

  wxString images = _T("image");
  int i = 1;
  images<<i++;
  while( wxFileExists(images) ){
    zip.PutNextEntry(images);
    wxFileInputStream png(images);
    while(!png.Eof())
      png.Read(zip);
    wxRemoveFile(images);
    images = _T("image");
    images<<i++;
  }

  return true;
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

void MathCtrl::OnDoubleClick(wxMouseEvent &event) {
  if (m_activeCell != NULL) {
    ((EditorCell *) m_activeCell)->SelectWordUnderCaret();
    Refresh();
  }
}

bool MathCtrl::ActivatePrevInput() {
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

  ScrollToCell(inpt);

  SetActiveCell(inpt);
  ((EditorCell *)m_activeCell)->CaretToEnd();

  Refresh();

  return true;
}

bool MathCtrl::ActivateNextInput(bool input) {
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

  ScrollToCell(inpt);

  SetActiveCell(inpt);
  ((EditorCell *)m_activeCell)->CaretToStart();

  Refresh();

  return true;
}

bool MathCtrl::ActivateLastInput() {
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

  ScrollToCell(inpt);

  SetActiveCell(inpt);
  ((EditorCell *)m_activeCell)->CaretToStart();

  return true;
}

bool MathCtrl::ActivateFirstInput() {
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

  ScrollToCell(input);
  SetActiveCell(input);
  Refresh();

  return true;
}

void MathCtrl::ScrollToSelectionStart() {
  ScrollToCell(m_selectionStart);
}

void MathCtrl::ScrollToCell(MathCell *cell)
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

  if (cellY + cellDrop + SCROLL_UNIT > view_y + height - height / 10)
    Scroll(-1, MAX((cellY + cellDrop - height + height / 10)/SCROLL_UNIT + 4, 0));
  else if (cellY - cellCenter - SCROLL_UNIT < view_y && cellDrop + cellCenter < height)
    Scroll(-1, MAX(cellY/SCROLL_UNIT - 2, 0));

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
  m_activeCell->GetParent()->ResetSize();
  Recalculate();
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
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
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

void MathCtrl::SelectAll()
{
  if (m_activeCell == NULL && m_tree != NULL)
  {
    m_selectionStart = m_tree;
    m_selectionEnd = m_last;
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    m_hCaretActive = false;
  }
  else if (m_activeCell != NULL)
    m_activeCell->SelectAll();

  Refresh();
}

void MathCtrl::OnSetFocus(wxFocusEvent& event)
{
  if (m_activeCell != NULL)
    m_activeCell->SetFocus(true);
}

void MathCtrl::OnKillFocus(wxFocusEvent& event)
{
  if (m_activeCell != NULL)
    m_activeCell->SetFocus(false);
}

void MathCtrl::CheckUnixCopy()
{
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
  if (m_selectionStart == NULL)
    return false;

  else if (type == MC_TYPE_IMAGE || type == MC_TYPE_SLIDE) {
    if (m_selectionStart != m_selectionEnd || m_selectionStart->GetType() != type)
      return false;
    return true;
  }

  else if (m_selectionStart->GetType() != type)
    return false;

  return true;
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

void MathCtrl::SetHCaret(MathCell *where) {
  m_selectionStart = m_selectionEnd = NULL;
  SetActiveCell(NULL);
  m_hCaretPosition = (GroupCell *)where;
  m_hCaretActive = true;
}

bool MathCtrl::CanUndo()
{
  if (m_activeCell == NULL)
    return false;
  return ((EditorCell *)m_activeCell)->CanUndo();
}

void MathCtrl::Undo()
{
  if (m_activeCell != NULL) {
    ((EditorCell *)m_activeCell)->Undo();
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
    Refresh();
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
