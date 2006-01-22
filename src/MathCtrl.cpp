/*
 *  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "MathCtrl.h"
#include "Bitmap.h"

#include <wx/clipbrd.h>
#include <wx/config.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include <wx/textfile.h>

#if wxUSE_DRAG_AND_DROP
#include <wx/dnd.h>
#endif

void AddLineToFile(wxTextFile& output, wxString s);

enum {
  TIMER_ID
};

MathCtrl::MathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size):
wxScrolledWindow(parent, id, position, size,
                 wxVSCROLL|wxHSCROLL|wxSUNKEN_BORDER)
{
  m_tree = NULL;
  m_memory = NULL;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_last = NULL;
  m_leftDown = false;
  m_mouseDrag = false;
  m_mouseOutside = false;
  m_forceUpdate = false;
  m_timer.SetOwner(this, TIMER_ID);
  AdjustSize(false);
}


MathCtrl::~MathCtrl ()
{
  if (m_tree != NULL)
    DestroyTree();
  if (m_memory != NULL)
    delete m_memory;
}


/***
 * Redraw the control
 */
void MathCtrl::OnPaint(wxPaintEvent& event)
{
  wxPaintDC dc(this);

  wxMemoryDC dcm;

  // Get the font size
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  // Prepare data
  wxRect rect = GetUpdateRegion().GetBox();
  wxSize sz = GetSize();
  int tmp, top, bottom, drop;
  CalcUnscrolledPosition(0, rect.GetTop(), &tmp, &top);
  CalcUnscrolledPosition(0, rect.GetBottom(), &tmp, &bottom);

  // Thest if m_memory is NULL (resize event)
  if (m_memory == NULL)
    m_memory = new wxBitmap(sz.x, sz.y);

  // Prepare memory DC
  wxString bgColStr = wxT("white");
  config->Read(wxT("Style/Background/color"), &bgColStr);
#if wxCHECK_VERSION(2, 5, 2)
  SetBackgroundColour(wxTheColourDatabase->Find(bgColStr));
#else
  SetBackgroundColour(*(wxTheColourDatabase->FindColour(bgColStr)));
#endif
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
    dcm.BeginDrawing();
    // Draw tree
    MathCell* tmp = m_tree;
    drop = tmp->GetMaxDrop();
    CellParser parser(dcm);
    parser.SetBouns(top, bottom);
    while (tmp != NULL) {
      if (!tmp->m_isBroken) {
        tmp->m_currentPoint.x = point.x;
        tmp->m_currentPoint.y = point.y;
        if (tmp->DrawThisCell(parser, point))
          tmp->Draw(parser, point, fontsize, false);
        if (tmp->m_nextToDraw != NULL) {
          if (tmp->m_nextToDraw->BreakLineHere()) {
            point.x = MC_BASE_INDENT;
            point.y += drop + tmp->m_nextToDraw->GetMaxCenter();
            if (tmp->m_bigSkip)
              point.y += MC_LINE_SKIP;
            drop = tmp->m_nextToDraw->GetMaxDrop();
          }
          else
            point.x += (tmp->GetWidth() + MC_CELL_SKIP);
        }
      }
      else {
        if (tmp->m_nextToDraw != NULL && tmp->m_nextToDraw->BreakLineHere()) {
          point.x = MC_BASE_INDENT;
          point.y += drop + tmp->m_nextToDraw->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_nextToDraw->GetMaxDrop();
        }
      }
      tmp = tmp->m_nextToDraw;
    }
    // Draw selection
    if (m_selectionStart != NULL) {
      MathCell* tmp = m_selectionStart;
      // We have a selection with click
      if (m_selectWholeLine) {
        if (m_selectionStart == m_selectionEnd)
          m_selectionStart->DrawBoundingBox(dcm);
        else {
          if (!tmp->m_isBroken)
            tmp->DrawBoundingBox(dcm, true);
          while (1) {
            tmp = tmp->m_nextToDraw;
            if (tmp == NULL) break;
            if (tmp->BreakLineHere() && !tmp->m_isBroken)
              tmp->DrawBoundingBox(dcm, true);
            if (tmp == m_selectionEnd) break;
          }
        }
      }
      // We have a selection by dragging
      else {
        while (1) {
          if (!tmp->m_isBroken && !tmp->m_isHidden)
            tmp->DrawBoundingBox(dcm, false);
          if (tmp == m_selectionEnd)
            break;
          tmp = tmp->m_nextToDraw;
          if (tmp == NULL) break;
        }
      }
    }
    dcm.EndDrawing();
  }

  // Blit the memory image to the window
  dcm.SetDeviceOrigin(0,0);
  dc.BeginDrawing();
  dc.Blit(0, rect.GetTop(),
          sz.x, rect.GetBottom() - rect.GetTop() + 1,
          &dcm,
          0, rect.GetTop());
  dc.EndDrawing();
}

/***
 * Add a new line
 */
void MathCtrl::AddLine(MathCell *newNode, bool forceNewLine)
{
  Freeze();
  if (newNode == NULL)
    return;
  if (m_tree == NULL) {
    m_tree = newNode;
    m_last = m_tree;
  }
  else {
    while (m_last->m_next != NULL)
      m_last = m_last->m_next;
    m_last->AppendCell(newNode);
  }
  newNode->ForceBreakLine(forceNewLine);
  Recalculate(newNode, true);
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  Thaw();
  Refresh();
}

/***
 * Recalculate sizes of cells
 */
void MathCtrl::RecalculateForce()
{
  if (m_tree != NULL) {
    m_forceUpdate = true;
    Recalculate(m_tree, false);
    m_forceUpdate = false;
  }
}

void MathCtrl::Recalculate(bool scroll)
{
  UnBreakUpCells();
  if (m_tree != NULL)
    Recalculate(m_tree, scroll);
}

/***
 * Recalculate size of this line
 */
void MathCtrl::Recalculate(MathCell *cell, bool scroll)
{
  RecalculateWidths(cell);
  RecalculateSize(cell);
  BreakUpCells(cell);
  BreakLines(cell);
  AdjustSize(scroll);
}

/***
 * Recalculate widths of cells
 */
void MathCtrl::RecalculateWidths()
{
  if (m_tree != NULL)
    RecalculateWidths(m_tree);
}

void MathCtrl::RecalculateWidths(MathCell* tmp)
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(m_forceUpdate);

  while (tmp != NULL) {
    tmp->RecalculateWidths(parser, fontsize, false);
    tmp = tmp->m_next;
  }
}

/***
 * Recalculate sizes of cells
 */
void MathCtrl::RecalculateSize()
{
  if (m_tree != NULL)
    RecalculateSize(m_tree);
}

void MathCtrl::RecalculateSize(MathCell* tmp)
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(m_forceUpdate);

  while (tmp != NULL) {
    if (!tmp->m_isBroken)
      tmp->RecalculateSize(parser, fontsize, false);
    tmp = tmp->m_nextToDraw;
  }
}

/***
 * Resize the controll
 */
void MathCtrl::OnSize(wxSizeEvent& event)
{
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
void MathCtrl::ClearWindow()
{
  if (m_tree != NULL) {
    DestroyTree();
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    m_last = NULL;
  }
  Refresh();
  Scroll(0, 0);
}

/***
 * Left mouse - selection handling
 */
void MathCtrl::OnMouseLeftDown(wxMouseEvent& event)
{
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);
#if wxUSE_DRAG_AND_DROP
  if (m_selectionStart != NULL) {
    MathCell *tmp = NULL;
    for (tmp = m_selectionStart; tmp != NULL, tmp != m_selectionEnd; tmp = tmp->m_nextToDraw)
      if (tmp->ContainsPoint(m_down))
        break;
    if (tmp != NULL && (tmp != m_selectionEnd || (tmp == m_selectionEnd && tmp->ContainsPoint(m_down)))) {
      wxDropSource dragSource(this);
      wxTextDataObject my_data(GetString());
	    dragSource.SetData( my_data );
	    wxDragResult result = dragSource.DoDragDrop(true);
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

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event)
{
  if (!m_mouseDrag)
    SelectPoint(m_down);
  m_leftDown = false;
  m_mouseDrag = false;
  SetFocus();
}

void MathCtrl::OnMouseMotion(wxMouseEvent& event)
{
  if (m_tree == NULL || !m_leftDown)
    return;
  m_mouseDrag = true;
  m_selectWholeLine = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_up.x, &m_up.y);
  if (m_mouseOutside) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
  SelectRect(m_down, m_up);
}

/***
 * Select the rectangle sorounded by point one and two.
 */
void MathCtrl::SelectRect(wxPoint one, wxPoint two)
{
  if (m_tree == NULL)
    return;

  MathCell* tmp;
  wxPoint start, end;
  wxRect rect;
  MathCell* st = m_selectionStart;
  MathCell* en = m_selectionEnd;

  m_selectionStart = m_selectionEnd = NULL;

  if (one.y < two.y || (one.y == two.y && one.x < two.x)) {
    start = one;
    end = two;
  }
  else {
    start = two;
    end = one;
  }

  rect.x = MIN(m_down.x, m_up.x);
  rect.y = MIN(m_down.y, m_up.y);
  rect.width = MAX(ABS(m_down.x-m_up.x), 1);
  rect.height = MAX(ABS(m_down.y-m_up.y), 1);

  // Lets select a rectangle
  tmp = m_tree;
  while (tmp != NULL && !rect.Intersects(tmp->GetRect()))
    tmp = tmp->m_nextToDraw;
  m_selectionStart = tmp;
  m_selectionEnd = tmp;
  while (tmp != NULL) {
    if (rect.Intersects(tmp->GetRect()))
      m_selectionEnd = tmp;
    tmp = tmp->m_nextToDraw;
  }

  if (m_selectionStart != NULL && m_selectionEnd != NULL) {

    // If selection is on multiple lines, we need to correct it
    if (m_selectionStart->GetCurrentY() != m_selectionEnd->GetCurrentY()) {
      MathCell *tmp = m_selectionEnd;
      MathCell *curr;

      // Find the first cell in selection
      while (m_selectionStart != tmp &&
             (m_selectionStart->GetCurrentX()+
                m_selectionStart->GetWidth() < start.x ||
               m_selectionStart->GetCurrentY()+
                m_selectionStart->GetDrop() < start.y))
        m_selectionStart = m_selectionStart->m_nextToDraw;

      // Find the last cell in selection
      curr = m_selectionEnd = m_selectionStart;
      while (1) {
        curr = curr->m_nextToDraw;
        if (curr == NULL)
          break;
        if ((curr->GetCurrentX() <= end.x &&
             curr->GetCurrentY()-curr->GetMaxCenter() <= end.y))
          m_selectionEnd = curr;
        if (curr == tmp)
          break;
      }
    }

    if (m_selectionStart == m_selectionEnd)
      m_selectionStart->SelectInner(rect, &m_selectionStart, &m_selectionEnd);
  }

  // Refresh only if the selection has changed
  if (st != m_selectionStart || en != m_selectionEnd)
    Refresh();
}

/***
 * Do selection when selecting by click
 */
void MathCtrl::SelectPoint(wxPoint& point)
{
  if (m_tree == NULL) return;
  MathCell* tmp = NULL;
  m_selectWholeLine = true;

  //
  // Which cell did we select.
  //
  tmp = m_tree;
  while (tmp != NULL) {
    if (tmp->ContainsPoint(point))
      break;
    tmp = tmp->m_nextToDraw;
  }
  if (tmp == NULL) {
    if (m_selectionStart != NULL) {
      m_selectionStart = NULL;
      m_selectionEnd = NULL;
      Refresh();
    }
    return;
  }

  m_selectionStart = tmp;
  m_selectionEnd = tmp;

  //
  // We selected output - select whole line.
  //
  if (m_selectionStart->GetType() == MC_TYPE_TEXT) {
    while (m_selectionStart->m_previousToDraw != NULL &&
           m_selectionStart->m_previousToDraw->GetType() == MC_TYPE_TEXT)
      m_selectionStart = m_selectionStart->m_previousToDraw;
    while (m_selectionEnd->m_nextToDraw != NULL &&
           m_selectionEnd->m_nextToDraw->GetType() == MC_TYPE_TEXT)
      m_selectionEnd = m_selectionEnd->m_nextToDraw;
  }
  //
  // We selected a label - fold the output.
  //
  else if (m_selectionStart->GetType() == MC_TYPE_LABEL) {
    if (m_selectionStart->m_isFolded) {
      m_selectionStart->m_nextToDraw = m_selectionStart->m_next;
      m_selectionStart->Fold(false);
      m_selectionStart->ResetData();
    }
    else {
      while(m_selectionStart->m_nextToDraw != NULL) {
        m_selectionStart->m_nextToDraw = m_selectionStart->m_nextToDraw->m_nextToDraw;
        if (m_selectionStart->m_nextToDraw != NULL &&
            m_selectionStart->m_nextToDraw->GetType() != MC_TYPE_TEXT) /// BUG ...
           break;
      }
      m_selectionStart->Fold(true);
      m_selectionStart->ResetData();
    }
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    Recalculate(false);
    Refresh();
    return;
  }
  //
  // We selected prompt - fold everything to the prompt.
  //
  else if (m_selectionStart->GetType() == MC_TYPE_MAIN_PROMPT) {
    if (m_selectionStart->m_nextToDraw == NULL) {
      m_selectionStart = NULL;
      m_selectionEnd = NULL;
      Refresh();
      return;
    }
    if (m_selectionStart->m_isFolded) {
      m_selectionStart->m_nextToDraw = m_selectionStart->m_next;
      m_selectionStart->Fold(false);
      m_selectionStart->ResetData();
    }
    else {
      while(m_selectionStart->m_nextToDraw != NULL) {
        m_selectionStart->m_nextToDraw = m_selectionStart->m_nextToDraw->m_nextToDraw;
        if (m_selectionStart->m_nextToDraw != NULL &&
            m_selectionStart->m_nextToDraw->GetType() == MC_TYPE_MAIN_PROMPT)
           break;
      }
      m_selectionStart->Fold(true);
      m_selectionStart->ResetData();
    }
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    Recalculate(false);
    Refresh();
    return;
  }

  Refresh();
}

/***
 * Get the string representation of the selection
 */
wxString MathCtrl::GetString()
{
  if (m_selectionStart == NULL)
    return wxEmptyString;
  wxString s;
  MathCell* tmp = m_selectionStart;
  while (tmp != NULL) {
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
bool MathCtrl::Copy(bool lb)
{
  if (m_selectionStart == NULL) return false;
  wxString s;
  MathCell* tmp = m_selectionStart;

  while (tmp != NULL) {
    if (lb && tmp->BreakLineHere() && s.Length()>0)
      s += wxT("\n");
    if (lb && (tmp->GetType() == MC_TYPE_PROMPT || tmp->GetType() == MC_TYPE_INPUT)) {
      if (s.Length()>0 && s.Right(1) != wxT("\n") && s.Right(1) != wxT(" "))
        s += wxT(" ");
    }
    s += tmp->ToString(false);
    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_nextToDraw;
  }

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
bool MathCtrl::CanDeleteSelection()
{
  if (m_selectionStart == NULL || m_selectionEnd == NULL)
    return false;
  MathCell* end = m_selectionEnd;
  while (end->m_nextToDraw != NULL &&
         end->m_nextToDraw->m_isHidden)
    end = end->m_nextToDraw;
  if (end->m_nextToDraw == NULL)
    return false;
  if (m_selectionStart->GetType() == MC_TYPE_MAIN_PROMPT &&
      (end == m_selectionStart ||
       end->m_nextToDraw->GetType() == MC_TYPE_MAIN_PROMPT))
    return true;
  return false;
}

/***
 * Delete the selection
 */
void MathCtrl::DeleteSelection()
{
  if (!CanDeleteSelection())
    return;
  MathCell *start = m_selectionStart;
  MathCell *end = m_selectionEnd->m_nextToDraw;
  while (end != NULL && end->GetType() != MC_TYPE_MAIN_PROMPT)
    end = end->m_nextToDraw;
  if (end == NULL || start == NULL || end->m_previous == NULL)
    return;
  // We are deleting the first cell in the tree
  if (start == m_tree) {
    end->m_previous->m_next = NULL;
    m_tree = end;
    delete start;
  }
  // the cell to be deleted is not the first in the tree
  else {
    MathCell* previous = start->m_previous;
    MathCell* previousToDraw = start->m_previousToDraw;

    end->m_previous->m_next = NULL;
    end->m_previous = previous;
    previous->m_next = end;
    previousToDraw->m_nextToDraw = end;
    end->m_previousToDraw = previousToDraw;
    // We have to correct the m_nextToDraw for hidden group just before
    // the first to be deleted - check previous label and main prompt.
    while (previous != NULL && previous->GetType() != MC_TYPE_LABEL)
      previous = previous->m_previous;
    if (previous != NULL && previous->m_isFolded)
      previous->m_nextToDraw = end;
    while (previous != NULL && previous->GetType() != MC_TYPE_MAIN_PROMPT)
      previous = previous->m_previous;
    if (previous != NULL && previous->m_isFolded)
      previous->m_nextToDraw = end;
    delete start;
  }
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_last = m_tree;
  AdjustSize(false);
  Refresh();
}


/***
 * Support for copying and deleting with keyboard
 */
void MathCtrl::OnKeyUp(wxKeyEvent& event)
{
  switch(event.GetKeyCode())
  {
    case 'c':
    case 'C':
      if (event.ControlDown() && CanCopy())
        Copy();
      else
        event.Skip();
      break;
    case WXK_DELETE:
      if (CanDeleteSelection())
        DeleteSelection();
      else
        event.Skip();
      break;
    default:
      event.Skip();
  }
}

/***
 * Get maximum x and y in the tree.
 */
void MathCtrl::GetMaxPoint(int* width, int* height)
{
  MathCell* tmp = m_tree;
  int currentHeight = MC_BASE_INDENT;
  int currentWidth = MC_BASE_INDENT;
  *width = MC_BASE_INDENT;
  *height = MC_BASE_INDENT;
  bool bigSkip = false;
  while (tmp != NULL) {
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere()) {
        currentHeight += tmp->GetMaxHeight();
        if (bigSkip)
          currentHeight += MC_LINE_SKIP;
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
    tmp = tmp->m_nextToDraw;
  }
  //*width = *width+10;
}

/***
 * Break lines.
 *
 * m_widths must be set before calling BreakLines.
 * Only the top line is broken.
 */

void MathCtrl::BreakLines(MathCell* tmp)
{
  int fullWidth = GetClientSize().GetWidth() - 9;
  int currentWidth = MC_BASE_INDENT;

  while (tmp != NULL) {
    tmp->ResetData();
    tmp->BreakLine(false);
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere() ||
          (currentWidth + tmp->GetWidth() >= fullWidth)) {
        currentWidth = MC_BASE_INDENT + tmp->GetWidth();
        tmp->BreakLine(true);
      }
      else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_nextToDraw;
  }
}

/***
 * Adjust the virtual size and scrollbars.
 */
void MathCtrl::AdjustSize(bool scroll)
{
  int width = MC_BASE_INDENT, height = MC_BASE_INDENT;
  int clientWidth, clientHeight, virtualHeight;

  GetClientSize(&clientWidth, &clientHeight);
  if (m_tree!=NULL) {
    GetMaxPoint(&width, &height);
  }
  virtualHeight = MAX(clientHeight, height) + 10;

  SetVirtualSize(width + 9, virtualHeight + 9);
  SetScrollRate(10, 10);
  if (scroll && height>clientHeight)
    Scroll(0, height);
}


/***
 * Support for selecting cells outside display
 */
void MathCtrl::OnMouseExit(wxMouseEvent& event)
{
  m_mouseOutside = true;
  if (m_leftDown) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
    m_timer.Start(200, true);
  }
}

void MathCtrl::OnMouseEnter(wxMouseEvent& event)
{
  m_mouseOutside = false;
}

void MathCtrl::OnTimer(wxTimerEvent& event)
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

  Scroll((currX + dx)/10, (currY + dy)/10);
  m_timer.Start(50, true);
}

/***
 * Destroy the tree
 */
void MathCtrl::DestroyTree()
{
  DestroyTree(m_tree);
  m_tree = NULL;
}

void MathCtrl::DestroyTree(MathCell* tmp)
{
  MathCell* tmp1;
  while (tmp!=NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
}

/***
 * Copy tree
 */
MathCell* MathCtrl::CopyTree()
{
  if (m_tree == NULL)
    return (MathCell*)NULL;

  MathCell* tmp1 = m_tree;
  MathCell* tmp;
  MathCell* copy;
  tmp = tmp1->Copy(false);
  copy = tmp;

  if (tmp1->m_isFolded)
    tmp1 = tmp1->m_nextToDraw;
  else
    tmp1 = tmp1->m_next;
  while (tmp1!=NULL) {
    tmp->AppendCell(tmp1->Copy(false));
    tmp = tmp->m_next;
    if (tmp1->m_isFolded)
      tmp1 = tmp1->m_nextToDraw;
    else
      tmp1 = tmp1->m_next;
  }
  return copy;
}

/***
 * Copy selection as bitmap
 */
bool MathCtrl::CopyBitmap()
{
  MathCell* tmp = CopySelection();

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToClipboard();
}

bool MathCtrl::CopyToFile(wxString file)
{
  MathCell* tmp = CopySelection();

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToFile(file);
}

bool MathCtrl::CopyToFile(wxString file, MathCell* start, MathCell* end,
                          bool asData)
{
  MathCell* tmp = CopySelection(start, end, asData);

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToFile(file);
}

/***
 * Copy selection
 */
MathCell* MathCtrl::CopySelection()
{
  return CopySelection(m_selectionStart, m_selectionEnd);
}

MathCell* MathCtrl::CopySelection(MathCell* start, MathCell* end, bool asData)
{
  MathCell *tmp, *tmp1 = NULL, *tmp2 = NULL;
  tmp = start;

  while (tmp != NULL) {
    if (tmp1 == NULL) {
      tmp1 = tmp->Copy(false);
      tmp2 = tmp1;
    }
    else {
      tmp2->AppendCell(tmp->Copy(false));
      tmp2 = tmp2->m_next;
    }
    if (tmp == end)
      break;
    if (asData)
      tmp = tmp->m_next;
    else
      tmp = tmp->m_nextToDraw;
  }

  return tmp1;
}

/***
 * Export content to a HTML file.
 */

void AddLineToFile(wxTextFile& output, wxString s)
{
#if wxUNICODE
  output.AddLine(s);
#else
  wxString t(s.wc_str(wxConvLocal), wxConvUTF8);
  output.AddLine(t);
#endif
}

bool MathCtrl::ExportToHTML(wxString file)
{
  wxString dir = wxPathOnly(file);
  wxString imgDir = dir + wxT("/img");
  wxString path, filename, ext;
  int count = 0;
  MathCell *tmp = m_tree, *start = NULL, *end = NULL;

  wxFileName::SplitPath(file, &path, &filename, &ext);

  if (!wxDirExists(imgDir))
    if (!wxMkdir(imgDir))
      return false;

  wxTextFile output;
	if (wxFileExists(file))	{
		wxRemoveFile(file);
	}
	if (!output.Create(file)) {
		return false;
	}

  AddLineToFile(output, wxT("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"));
  AddLineToFile(output, wxT("<HTML>"));
  AddLineToFile(output, wxT(" <HEAD>"));
  AddLineToFile(output, wxT("  <TITLE>wxMaxima HTML export</TITLE>"));
  AddLineToFile(output, wxT("  <META NAME=\"generator\" CONTENT=\"wxMaxima\">"));
  AddLineToFile(output, wxT("  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">"));

  //
  // Write styles
  //
  wxString font;
  wxString colorInput;
  wxString colorPrompt;
  wxString colorMain;
  bool italicInput = false;
  bool boldInput = false;
  bool italicPrompt = false;
  bool boldPrompt = false;
  bool italicHidden = false;
  bool boldHidden = false;
  bool underlinedHidden = false;
  int fontSize = 12;
  wxConfigBase* config = wxConfig::Get();

  config->Read(wxT("Style/fontname"), &font);
  config->Read(wxT("Style/Input/color"), &colorInput);
  config->Read(wxT("Style/MainPrompt/color"), &colorPrompt);
  config->Read(wxT("Style/NormalText/color"), &colorMain);
  config->Read(wxT("fontSize"), &fontSize);
  config->Read(wxT("Style/Input/bold"), &boldInput);
  config->Read(wxT("Style/Input/italic"), &italicInput);
  config->Read(wxT("Style/MainPrompt/bold"), &boldPrompt);
  config->Read(wxT("Style/MainPrompt/italic"), &italicPrompt);
  config->Read(wxT("Style/HiddenText/bold"), &boldHidden);
  config->Read(wxT("Style/HiddenText/italic"), &italicHidden);
  config->Read(wxT("Style/HiddenText/underlined"), &underlinedHidden);


  AddLineToFile(output, wxT("  <STYLE TYPE=\"text/css\">"));

  // BODY STYLE
  AddLineToFile(output, wxT("body {"));
  if (font.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
                          font +
                          wxT(";"));
  }
  if (colorMain.Length()) {
    AddLineToFile(output, wxT("  color: ") +
                          colorMain +
                          wxT(";"));
  }
  AddLineToFile(output, wxT("}"));

  // INPUT STYLE
  AddLineToFile(output, wxT(".input {"));
  if (colorInput.Length()) {
    AddLineToFile(output, wxT("  color: ") +
                          colorInput +
                          wxT(";"));
  }
  if (boldInput)
    AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicInput)
    AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("}"));

  // PROMPT STYLE
  AddLineToFile(output, wxT(".prompt {"));
  if (colorPrompt.Length()) {
    AddLineToFile(output, wxT("  color: ") +
                          colorPrompt +
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
    AddLineToFile(output, wxT("  color: ") +
                          colorPrompt +
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

  //
  // Write maxima header
  //
  if (tmp!=NULL && tmp->GetType()!=MC_TYPE_MAIN_PROMPT) {
    AddLineToFile(output, wxEmptyString);
    AddLineToFile(output, wxT(" <P>"));
    while(tmp!=NULL && tmp->GetType()!=MC_TYPE_MAIN_PROMPT) {
      AddLineToFile(output, wxT("   ") + tmp->ToString(false));
      AddLineToFile(output, wxT("   <BR/>"));
      tmp = tmp->m_nextToDraw;
    }
    AddLineToFile(output, wxT(" </P>"));
  }

  AddLineToFile(output, wxEmptyString);

  //
  // Write contents
  //
  while(tmp!=NULL) {
    AddLineToFile(output, wxT("<!-- Input/output group -->"));
    AddLineToFile(output, wxEmptyString);
    AddLineToFile(output, wxT(" <P>"));

    // PROMPT
    if (!tmp->m_isFolded)
      AddLineToFile(output, wxT("  <SPAN CLASS=\"prompt\">"));
    else
      AddLineToFile(output, wxT("  <SPAN CLASS=\"hidden\">"));
    AddLineToFile(output, wxT("  ") + tmp->ToString(false));
    tmp = tmp->m_nextToDraw;
    AddLineToFile(output, wxT("  </SPAN>"));

    // INPUT
    AddLineToFile(output, wxT("  <SPAN CLASS=\"input\">"));
    while(tmp!=NULL && tmp->GetType()==MC_TYPE_INPUT) {
      wxString input = tmp->ToString(false);
      wxString line = wxEmptyString;
      for (unsigned int i=0; i<input.Length(); i++) {
        if (input.GetChar(i) == ' ')
          line += wxT("&nbsp;");
        else
          break;
      }
      input = input.Trim(false);
      line += input;
      AddLineToFile(output, wxT("   ") + line);
      AddLineToFile(output, wxT("   <BR>"));
      tmp = tmp->m_nextToDraw;
    }
    AddLineToFile(output, wxT("  </SPAN>"));

    // Check if there is no optput (commands with $)
    if (tmp!=NULL && tmp->GetType()==MC_TYPE_MAIN_PROMPT) {
      AddLineToFile(output, wxT(" </P>"));
      AddLineToFile(output, wxEmptyString);
      continue;
    }

    // OUTPUT
    start = tmp;
    if (tmp != NULL && tmp->m_isFolded) {
      end = tmp;
    }
    else {
      while (tmp!=NULL && tmp->m_next!=NULL &&
             tmp->m_next->GetType()!=MC_TYPE_MAIN_PROMPT)
        tmp = tmp->m_next;
      end = tmp;
    }
    if (start!=NULL && end!=NULL) {
      if (!CopyToFile(imgDir + wxT("/") +filename +
                      wxString::Format(wxT("_%d.png"), count),
                      start, end, true))
        return false;
      AddLineToFile(output, wxT("  <BR>"));
      AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"img/") +
                            filename +
                            wxString::Format(wxT("_%d.png\">"), count));
      count++;
    }
    AddLineToFile(output, wxT(" </P>"));
    AddLineToFile(output, wxEmptyString);
    if (tmp != NULL) {
      if (start->m_isFolded)
        tmp = tmp->m_nextToDraw;
      else
        tmp = tmp->m_next;
    }
  }

  //
  // Footer
  //
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

void MathCtrl::BreakUpCells()
{
  if (m_tree != NULL)
    BreakUpCells(m_tree);
}

void MathCtrl::BreakUpCells(MathCell *cell)
{
  wxClientDC dc(this);
  CellParser parser(dc);
  int fontsize = 12;
  MathCell *tmp = cell;

  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int clientWidth = GetClientSize().GetWidth() - 9;

  while (tmp != NULL) {
    if (tmp->GetWidth()>clientWidth) {
      if (tmp->BreakUp()) {
        tmp->RecalculateWidths(parser, fontsize, false);
        tmp->RecalculateSize(parser, fontsize, false);
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}

void MathCtrl::UnBreakUpCells()
{
  wxClientDC dc(this);
  CellParser parser(dc);
  int fontsize = 12;

  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int clientWidth = GetClientSize().GetWidth() - 9;

  MathCell *tmp = m_tree;
  while (tmp != NULL) {
    if (tmp->m_isBroken) {
      tmp->Unbreak(false);
    }
    tmp = tmp->m_next;
  }
}

BEGIN_EVENT_TABLE(MathCtrl, wxScrolledWindow)
  EVT_SIZE(MathCtrl::OnSize)
  EVT_PAINT(MathCtrl::OnPaint)
  EVT_LEFT_UP(MathCtrl::OnMouseLeftUp)
  EVT_LEFT_DOWN(MathCtrl::OnMouseLeftDown)
  EVT_MOTION(MathCtrl::OnMouseMotion)
  EVT_ENTER_WINDOW(MathCtrl::OnMouseEnter)
  EVT_LEAVE_WINDOW(MathCtrl::OnMouseExit)
  EVT_TIMER(TIMER_ID, MathCtrl::OnTimer)
  EVT_KEY_UP(MathCtrl::OnKeyUp)
  EVT_ERASE_BACKGROUND(MathCtrl::OnEraseBackground)
END_EVENT_TABLE()
