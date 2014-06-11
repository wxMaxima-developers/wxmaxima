///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
///            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
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
#include "ImgCell.h"

#include <wx/clipbrd.h>
#include <wx/config.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include <wx/tokenzr.h>

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>

#define SCROLL_UNIT 10
#define CARET_TIMER_TIMEOUT 500
#define ANIMATION_TIMER_TIMEOUT 300
#define AC_MENU_LENGTH 25

void AddLineToFile(wxTextFile& output, wxString s, bool unicode = true);

enum
{
  TIMER_ID,
  CARET_TIMER_ID,
  ANIMATION_TIMER_ID
};

MathCtrl::MathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size) :
  wxScrolledCanvas(
      parent, id, position, size,
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
  m_hCaretActive = true;
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
  m_zoomFactor = 1.0; // set zoom to 100%
  m_evaluationQueue = new EvaluationQueue();
  AdjustSize();

#if wxCHECK_VERSION(2,9,1)
  DisableKeyboardScrolling();
#endif

  // hack to workaround problems in RtL locales, http://bugzilla.redhat.com/455863
  SetLayoutDirection(wxLayout_LeftToRight);
}

MathCtrl::~MathCtrl() {
  if (m_tree != NULL)
    DestroyTree();
  if (m_memory != NULL)
    delete m_memory;

  delete m_evaluationQueue;
}

/***
 * Redraw the control
 */
void MathCtrl::OnPaint(wxPaintEvent& event) {
  wxPaintDC dc(this);

  wxMemoryDC dcm;

  // Get the font size
  wxConfig *config = (wxConfig *)wxConfig::Get();

  // Prepare data
  wxRect rect = GetUpdateRegion().GetBox();
  //printf("Updating rect [%d, %d] -> [%d, %d]\n", rect.x, rect.y, rect.width, rect.height);
  wxSize sz = GetSize();
  int tmp, top, bottom, drop;
  CalcUnscrolledPosition(0, rect.GetTop(), &tmp, &top);
  CalcUnscrolledPosition(0, rect.GetBottom(), &tmp, &bottom);

  // Thest if m_memory is NULL (resize event)
  if (m_memory == NULL) {
    m_memory = new wxBitmap();
#if wxCHECK_VERSION(3,0,0)
    m_memory->CreateScaled (sz.x, sz.y, -1, dc.GetContentScaleFactor ());
#else
    m_memory->Create(sz.x, sz.y);
#endif
  }
  // Prepare memory DC
  wxString bgColStr= wxT("white");
  config->Read(wxT("Style/Background/color"), &bgColStr);
  SetBackgroundColour(wxColour(bgColStr));

  dcm.SelectObject(*m_memory);
  dcm.SetBackground(*(wxTheBrushList->FindOrCreateBrush(GetBackgroundColour(), wxBRUSHSTYLE_SOLID)));
  dcm.Clear();
  PrepareDC(dcm);
  dcm.SetMapMode(wxMM_TEXT);
  dcm.SetBackgroundMode(wxTRANSPARENT);
  dcm.SetLogicalFunction(wxCOPY);

  CellParser parser(dcm);
  parser.SetBounds(top, bottom);
  parser.SetZoomFactor(m_zoomFactor);
  int fontsize = parser.GetDefaultFontSize(); // apply zoomfactor to defaultfontsize

  // Draw content
  if (m_tree != NULL)
  {
    //
    // First draw selection under content with wxCOPY and selection brush/color
    //
    if (m_selectionStart != NULL)
    {
      MathCell* tmp = m_selectionStart;

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
          if (!tmp->m_isBroken && !tmp->m_isHidden && m_activeCell != tmp) {
            if ((tmp->GetType() == MC_TYPE_IMAGE) || (tmp->GetType() == MC_TYPE_SLIDE))
              tmp->DrawBoundingBox(dcm, false, 5); // draw 5 pixels of border for img/slide cells
            else
              tmp->DrawBoundingBox(dcm, false);
          }
          if (tmp == m_selectionEnd)
            break;
          tmp = tmp->m_nextToDraw;
        } // end while (1)
      }
    }
    //
    // Mark groupcells currently in queue. TODO better in gc::draw?
    //
    if (m_evaluationQueue->GetFirst() != NULL) {
      MathCell* tmp = m_tree;
      dcm.SetBrush(*wxTRANSPARENT_BRUSH);
      while (tmp != NULL)
      {
        if (m_evaluationQueue->IsInQueue(dynamic_cast<GroupCell*>(tmp))) {
          if (m_evaluationQueue->GetFirst() == tmp)
          {
            wxRect rect = tmp->GetRect();
            dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CELL_BRACKET), 2, wxPENSTYLE_SOLID)));
            dcm.DrawRectangle( 3, rect.GetTop() - 2, MC_GROUP_LEFT_INDENT, rect.GetHeight() + 5);
          }
          else
          {
            wxRect rect = tmp->GetRect();
            dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CELL_BRACKET), 1, wxPENSTYLE_SOLID)));
            dcm.DrawRectangle( 3, rect.GetTop() - 2, MC_GROUP_LEFT_INDENT, rect.GetHeight() + 5);
          }
        }
        tmp = tmp->m_next;
      }
    }
    //
    // Draw content over
    //
    wxPoint point;
    point.x = MC_GROUP_LEFT_INDENT;
    point.y = MC_BASE_INDENT + m_tree->GetMaxCenter();
    // Draw tree
    MathCell* tmp = m_tree;
    drop = tmp->GetMaxDrop();

    dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_DEFAULT), 1, wxPENSTYLE_SOLID)));
    dcm.SetBrush(*(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_DEFAULT))));

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
    dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CURSOR), 1, wxPENSTYLE_SOLID))); // TODO is there more efficient way to do this?

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

// InsertGroupCells
// inserts groupcells after position "where" (NULL = top of the document)
// Multiple groupcells can be inserted when tree->m_next != NULL
// Returns the pointer to the last inserted group cell to have fun with
GroupCell *MathCtrl::InsertGroupCells(GroupCell* tree, GroupCell* where)
{
  if (!tree)
    return NULL; // nothing to insert
  bool renumbersections = false; // only renumber when true
  GroupCell *next; // next gc to insertion point
  GroupCell *prev;
  // last in the tree to insert
  GroupCell* last = tree;
  if (last->IsFoldable() || (last->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (last->m_next) {
    last = dynamic_cast<GroupCell*>(last->m_next);
    if (last->IsFoldable() || (last->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
  }

  if (m_tree == NULL)
    where = NULL;

  if (where)
    next = dynamic_cast<GroupCell*>(where->m_next);
  else {
    next = m_tree; // where == NULL
    m_tree = tree;
  }
  prev = where;

  tree->m_previous = tree->m_previousToDraw = where;
  last->m_next     = last->m_nextToDraw     = next;

  if (prev)
    prev->m_next     = prev->m_nextToDraw     = tree;
  if (next)
    next->m_previous = next->m_previousToDraw = last;
  // make sure m_last is correct!!
  if (!next) // if there were no further cells
    m_last = last;

  if (renumbersections)
    NumberSections();
  Recalculate();
  m_saved = false; // document has been modified
  return last;
}

// this goes through m_tree with m_next, to set the correct m_last
// you can call this after folding, unfolding cells to make sure
// m_last is correct
GroupCell *MathCtrl::UpdateMLast()
{
  if (!m_tree) {
    m_last = NULL;
    return NULL;
  }

  MathCell *tmp = m_tree;
  while (tmp->m_next)
    tmp = tmp->m_next;

  m_last = dynamic_cast<GroupCell*>(tmp);

  return m_last;
}

/***
 * Add a new line to working group or m_last
 */
void MathCtrl::InsertLine(MathCell *newCell, bool forceNewLine)
{
  SetActiveCell(NULL, false);

  m_saved = false;

  GroupCell *tmp = m_workingGroup;

  if (tmp == NULL)
    tmp = m_last;

  newCell->ForceBreakLine(forceNewLine);

  tmp->AppendOutput(newCell);
  if (newCell->GetType() == MC_TYPE_PROMPT)
  {
    m_workingGroup = tmp;
    ScrollToCell(tmp->GetParent());
    OpenHCaret();
  }

  while (newCell != NULL)
  {
    newCell->SetParent(tmp, false);
    newCell = newCell->m_next;
  }

  m_selectionStart = NULL;
  m_selectionEnd = NULL;

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetZoomFactor(m_zoomFactor);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);

  tmp->RecalculateAppended(parser);
  Recalculate();

  ScrollToCell(tmp); // also refreshes
}

/***
 * Recalculate dimensions of cells
 */
void MathCtrl::RecalculateForce() {
  Recalculate(true);
}

void MathCtrl::Recalculate(bool force)
{
  GroupCell *tmp = m_tree;

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetZoomFactor(m_zoomFactor);
  parser.SetForceUpdate(force);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);
  int d_fontsize = parser.GetDefaultFontSize();
  int m_fontsize = parser.GetMathFontSize();

  wxPoint point;
  point.x = MC_GROUP_LEFT_INDENT;
  point.y = MC_BASE_INDENT ;

  while (tmp != NULL) {
    tmp->Recalculate(parser, d_fontsize, m_fontsize);
//    tmp->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
//    tmp->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);
    point.y += tmp->GetMaxCenter();
    tmp->m_currentPoint.x = point.x;
    tmp->m_currentPoint.y = point.y;
    point.y += tmp->GetMaxDrop();
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
    point.y += MC_GROUP_SKIP;
  }

  AdjustSize();
}

/***
 * Resize the control
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
  //wxScrolledCanvas::OnSize(event);
}

/***
 * Clear document
 * Basically set everything to the state as if MathCtrl
 * was just created, so there is a blank document.
 * Called when opening a new file into existing MathCtrl.
 */
void MathCtrl::ClearDocument() {

  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_clickType = CLICK_TYPE_NONE;
  m_clickInGC = NULL;
  m_hCaretActive = false;
  m_hCaretPosition = NULL; // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_activeCell = NULL;
  m_workingGroup = NULL;

  ClearEvaluationQueue();

  DestroyTree();

  m_editingEnabled = true;
  m_switchDisplayCaret = true;
  m_animate = false;
  m_saved = true;

  Recalculate();
  Scroll(0, 0);
}

/***
 * Reset all input promts to "-->  "
 * Called when Restart Maxima is called from Maxima menu
 */
void MathCtrl::ResetInputPrompts() {
  if (m_tree)
    m_tree->ResetInputLabel(true); // recursivly reset prompts
}

//
// support for numbered sections with hiding
//
void MathCtrl::NumberSections() {
  int s, sub, i;
  s = sub = i = 0;
  if (m_tree)
    m_tree->Number(s, sub, i);
}

bool MathCtrl::IsLesserGCType(int type, int comparedTo) {
  switch (type) {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_IMAGE:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
          (comparedTo == GC_TYPE_SUBSECTION))
        return true;
      else
        return false;
    case GC_TYPE_SUBSECTION:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION))
        return true;
      else
        return false;
    case GC_TYPE_SECTION:
      if (comparedTo == GC_TYPE_TITLE)
        return true;
      else
        return false;
    case GC_TYPE_TITLE:
      return false;
    default:
      return false;
  }
}

/**
 * Call when a fold action was detected, to update the state in response
 * to a fold occurring.
 */
void MathCtrl::FoldOccurred() {
  SetSaved(false);
  UpdateMLast();
}

/**
 * Toggles the status of the fold for the given GroupCell.
 * If the cell is folded, it will be unfolded; otherwise it will be folded.
 *
 * @param which   The GroupCell to fold or unfold.
 * @return        A pointer to a GroupCell if the action succeeded;
 *                NULL otherwise.
 */
GroupCell *MathCtrl::ToggleFold(GroupCell *which) {
  if (!which)
    return NULL;

  GroupCell *result = NULL;
  if (which->IsFoldable())
    if (which->GetHiddenTree())
      result = which->Unfold();
    else
      result = which->Fold();
  else
    return NULL;

  if (result) // something has folded/unfolded
    FoldOccurred();

  return result;
}

/**
 * Toggles the status of the fold for the given GroupCell and its children.
 * If the cell is folded, it will be recursively unfolded;
 * otherwise it will be recursively folded.
 *
 * @param which   The GroupCell to recursively fold or unfold.
 * @return        A pointer to a GroupCell if the action succeeded;
 *                NULL otherwise.
 */
GroupCell *MathCtrl::ToggleFoldAll(GroupCell *which) {
  if (!which)
    return NULL;

  GroupCell *result = NULL;
  if (which->IsFoldable())
    if (which->GetHiddenTree())
      result = which->UnfoldAll(false);
    else
      result = which->FoldAll(false);
  else
    return NULL;

  if (result) // something has folded/unfolded
    FoldOccurred();

  return result;
}

/**
 * Recursively folds the whole document.
 */
void MathCtrl::FoldAll() {
  if (m_tree) {
    m_tree->FoldAll(true);
    FoldOccurred();
  }
}

/**
 * Recursively unfolds the whole document.
 */
void MathCtrl::UnfoldAll() {
  if (m_tree) {
    m_tree->UnfoldAll(true);
    FoldOccurred();
  }
}

// Returns the tree from start to end and connets the pointers the right way
// so that m_tree stays 'correct' - also works in hidden trees
GroupCell *MathCtrl::TearOutTree(GroupCell *start, GroupCell *end) {
  if ((!start) || (!end))
    return NULL;
  MathCell *prev = start->m_previous;
  MathCell *next = end->m_next;

  end->m_next = end->m_nextToDraw = NULL;
  start->m_previous = start->m_previousToDraw = NULL;

  if (prev)
    prev->m_next = prev->m_nextToDraw = next;
  if (next)
    next->m_previous = next->m_previousToDraw = prev;
  // fix m_last if we tore it
  if (end == m_last)
    m_last = dynamic_cast<GroupCell*>(prev);

  return start;
}

/***
 * Right mouse - popup-menu
 */
void MathCtrl::OnMouseRightDown(wxMouseEvent& event) {
  wxMenu* popupMenu = new wxMenu();

  int downx, downy;

  // find out if clicked into existing selection, if not, reselect with leftdown
  //
  bool clickInSelection = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &downx, &downy);
  if ((m_selectionStart != NULL) && (m_selectionEnd != NULL)) {
    // SELECTION OF GROUPCELLS
    if (m_selectionStart->GetType() == MC_TYPE_GROUP) { //a selection of groups
      if ( downx <= MC_GROUP_LEFT_INDENT + 3) {
        wxRect rectStart = m_selectionStart->GetRect();
        wxRect rectEnd = m_selectionEnd->GetRect();
        if (((downy >= rectStart.GetTop()) && (downy <= rectEnd.GetBottom())) ||
            ((downy >= rectEnd.GetTop()) && (downy <= rectStart.GetBottom())))
          clickInSelection = true;
      }
    }
    // SELECTION OF OUTPUT
    else {
      MathCell * tmp = m_selectionStart;
      wxRect rect;
      while (tmp != NULL) {
        rect = tmp->GetRect();
        if (rect.Contains(downx,downy))
          clickInSelection = true;

        if (tmp == m_selectionEnd)
          break;
        tmp = tmp->m_nextToDraw;
      }

    }
  }
  // SELECTION IN EDITORCELL
  else if (m_activeCell != NULL) {
    wxClientDC dc(this);
    if (m_activeCell->IsPointInSelection(dc, wxPoint(downx, downy)))
      clickInSelection = true;
  }

  // emulate a left click to set the cursor
  if (!clickInSelection) {
    OnMouseLeftDown(event);
    m_leftDown = false;
    m_clickType = CLICK_TYPE_NONE;
  }

  // construct a menu appropriate to what we have
  //
  /* If we have no selection or we are not in editing mode don't popup a menu!*/
  if (m_editingEnabled == false) {
    delete popupMenu;
    return;
  }
  if (m_activeCell == NULL) {

    if (IsSelected(MC_TYPE_IMAGE) || IsSelected(MC_TYPE_SLIDE)) {
      popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_image, _("Save Image..."), wxEmptyString, wxITEM_NORMAL);
      if (IsSelected(MC_TYPE_SLIDE)) {
        popupMenu->Append(popid_animation_save, _("Save Animation..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_animation_start, _("Start Animation"), wxEmptyString, wxITEM_NORMAL);
      }
    }

    else if (m_selectionStart != NULL)
    {
      if (m_selectionStart->GetType() == MC_TYPE_GROUP) {

        if (CanCopy()) {
          popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_tex, _("Copy LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_image, _("Copy As Image"),
              wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu->Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_evaluate, _("Evaluate Cell(s)"), wxEmptyString, wxITEM_NORMAL);

        if (m_selectionStart != m_selectionEnd)
          popupMenu->Append(popid_merge_cells, _("Merge Cells"), wxEmptyString, wxITEM_NORMAL);
      }

      else {
        if (CanCopy()) {
          popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_tex, _("Copy LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_image, _("Copy As Image"),
              wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu->Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }

        if (IsSelected(MC_TYPE_DEFAULT) || IsSelected(MC_TYPE_LABEL)) {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_float, _("To Float"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_solve, _("Solve..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_solve_num, _("Find Root..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_simplify, _("Simplify Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_factor, _("Factor Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_expand, _("Expand Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_subst, _("Substitute..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_integrate, _("Integrate..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_diff, _("Differentiate..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_plot2d, _("Plot 2d..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_plot3d, _("Plot 3d..."), wxEmptyString, wxITEM_NORMAL);
        }
      }
    }

    else if (m_hCaretActive == true) {
      popupMenu->Append(popid_paste, _("Paste"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_select_all, _("Select All"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->AppendSeparator();
      popupMenu->Append(popid_insert_text, _("Insert Text Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_title, _("Insert Title Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_section, _("Insert Section Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_subsection, _("Insert Subsection Cell"), wxEmptyString, wxITEM_NORMAL);
    }
  }

  // popup menu in active cell
  else {
    popupMenu->Append(popid_cut, _("Cut"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_paste, _("Paste"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->AppendSeparator();
    popupMenu->Append(popid_select_all, _("Select All"), wxEmptyString, wxITEM_NORMAL);
    if ((clickInSelection) &&
        dynamic_cast<GroupCell*>(m_activeCell->GetParent())->GetGroupType() == GC_TYPE_CODE)
      popupMenu->Append(popid_comment_selection, _("Comment Selection"), wxEmptyString, wxITEM_NORMAL);
    if (!clickInSelection)
      popupMenu->Append(popid_divide_cell, _("Divide Cell"), wxEmptyString, wxITEM_NORMAL);

    popupMenu->Enable(popid_copy, m_activeCell->CanCopy());
    popupMenu->Enable(popid_cut, m_activeCell->CanCopy());
  }

  // create menu if we have any items
  if (popupMenu->GetMenuItemCount() > 0 )
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
  SetActiveCell(NULL, false);

  GroupCell * tmp = m_tree;
  wxRect rect;
  GroupCell * clickedBeforeGC = NULL;
  GroupCell * clickedInGC = NULL;
  while (tmp != NULL) { // go through all groupcells
    rect = tmp->GetRect();
    if (m_down.y < rect.GetTop() )
    {
      clickedBeforeGC = dynamic_cast<GroupCell*>(tmp);
      break;
    }
    else if (m_down.y <= rect.GetBottom() )
    {
      clickedInGC = dynamic_cast<GroupCell*>(tmp);
      break;
    }
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  if (clickedBeforeGC != NULL) { // we clicked between groupcells, set hCaret
    SetHCaret(tmp->m_previous, false);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
  } // end if (clickedBeforeGC != NULL) // we clicked between groupcells, set hCaret

  else if (clickedInGC != NULL) { // we clicked in a groupcell, find out where

    if (m_down.x <= MC_GROUP_LEFT_INDENT) { // we clicked in left bracket area
      if ((clickedInGC->HideRect()).Contains(m_down)) // did we hit the hide rectancle
      {
        if (clickedInGC->IsFoldable()) {
          if (event.ShiftDown())
            ToggleFoldAll(clickedInGC);
          else
            ToggleFold(clickedInGC);
          Recalculate(true);
        }
        else {
          clickedInGC->SwitchHide(); // todo if there's nothin to hide, select as normal
          clickedInGC->ResetSize();
          Recalculate();
          m_clickType = CLICK_TYPE_NONE; // ignore drag-select
        }
      }
      else {
        m_clickType = CLICK_TYPE_GROUP_SELECTION;
        m_selectionStart = m_selectionEnd = clickedInGC;
      }
    } // end we clicked in left bracket area

    else { // we didn't click in left bracket space
      EditorCell * editor = clickedInGC->GetEditable();
      if (editor != NULL) {
        rect = editor->GetRect();
        if ((m_down.y >= rect.GetTop()) && (m_down.y <= rect.GetBottom())) {
          SetActiveCell(editor, false); // do not refresh
          wxClientDC dc(this);
          m_activeCell->SelectPointText(dc, m_down);
          m_switchDisplayCaret = false;
          m_clickType = CLICK_TYPE_INPUT_SELECTION;
          if (editor->GetWidth() == -1)
            Recalculate();
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
            SetActiveCell(dynamic_cast<EditorCell*>(m_selectionStart), false);
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
    SetHCaret(m_last, false);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
  }
  Refresh();
}

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  m_animate = false;
  m_leftDown = false;
  m_mouseDrag = false;
  m_clickInGC = NULL; // pointer to NULL to prevent crashes if the cell is deleted
  m_clickType = CLICK_TYPE_NONE;
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
 * Select the rectangle surrounded by down and up. Called from OnMouseMotion.
 *
 * The method decides what to do, based on the value of m_clickType which
 * was set previously in OnMouseLeftDown. This enables different selection behaviours
 * depending on where we first clicked. If m_clickType equals
 * CLICK_TYPE_NONE - click-dragging does not result in a selection (we clicked in hideRect for instance)
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
      GroupCell * tmp = m_tree;
      while (tmp != NULL) {
        rect = tmp->GetRect();
        if (ytop <= rect.GetBottom()) {
          m_selectionStart = tmp;
          break;
        }
        tmp = dynamic_cast<GroupCell*>(tmp->m_next);
      }

      if (tmp == NULL) { // below last cell, handle with care
        SetHCaret(m_last); // also refreshes
        return;
      }

      tmp = m_tree;
      while (tmp != NULL) {
        rect = tmp->GetRect();
        if (ybottom < rect.GetTop()) {
          m_selectionEnd = tmp->m_previous;
          break;
        }
        tmp = dynamic_cast<GroupCell*>(tmp->m_next);
      }
      if (tmp == NULL)
        m_selectionEnd = m_last;
      if (m_selectionEnd == (m_selectionStart->m_previous)) {
        SetHCaret(m_selectionEnd, false); // will refresh at the end of function
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
bool MathCtrl::Copy(bool astext) {
  if (m_activeCell != NULL) {
    return m_activeCell->CopyToClipboard();
  }

  if (m_selectionStart == NULL)
    return false;

  if (!astext && m_selectionStart->GetType() == MC_TYPE_GROUP)
    return CopyCells();
  /// If the selection is IMAGE or SLIDESHOW, copy it to clipboard
  /// as image.
  else if (m_selectionStart == m_selectionEnd &&
      m_selectionStart->GetType() == MC_TYPE_IMAGE) {
    ((ImgCell *)m_selectionStart)->CopyToClipboard();
	return true;
  }
  else if (m_selectionStart == m_selectionEnd &&
        m_selectionStart->GetType() == MC_TYPE_SLIDE) {
    ((SlideShow *)m_selectionStart)->CopyToClipboard();
	return true;
  }
  else {
    wxString s = GetString(true);

    if (wxTheClipboard->Open()) {
      wxTheClipboard->SetData(new wxTextDataObject(s));
      wxTheClipboard->Close();
      return true;
    }
    return false;
  }
}

bool MathCtrl::CopyTeX() {
  if (m_activeCell != NULL)
    return false;

  if (m_selectionStart == NULL)
    return false;

  wxString s;
  MathCell* tmp = m_selectionStart;

  bool inMath = false;
  wxString label;

  if (tmp->GetType() != MC_TYPE_GROUP) {
    inMath = true;
    s = wxT("\\[");
  }

  while (tmp != NULL) {
    s += tmp->ToTeX(false);
    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_next;
  }

  if (inMath == true)
    s += wxT("\\]");

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool MathCtrl::CopyCells()
{
  if (m_selectionStart == NULL)
      return false;

  wxString s;
  GroupCell *tmp = dynamic_cast<GroupCell*>(m_selectionStart->GetParent());
  GroupCell *end = dynamic_cast<GroupCell*>(m_selectionEnd->GetParent());

  while (tmp != NULL) {

    switch (tmp->GetGroupType())
    {
      case GC_TYPE_CODE:
        s += wxT("/* [wxMaxima: input   start ] */\n");
        s += tmp->GetEditable()->ToString(false) + wxT("\n");
        s += wxT("/* [wxMaxima: input   end   ] */\n");
        break;
      case GC_TYPE_TEXT:
        s += wxT("/* [wxMaxima: comment start ]\n");
        s += tmp->GetEditable()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: comment end   ] */\n");
        break;
      case GC_TYPE_SECTION:
        s += wxT("/* [wxMaxima: section start ]\n");
        s += tmp->GetEditable()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: section end   ] */\n");
        break;
      case GC_TYPE_SUBSECTION:
        s += wxT("/* [wxMaxima: subsect start ]\n");
        s += tmp->GetEditable()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: subsect end   ] */\n");
        break;
      case GC_TYPE_TITLE:
        s += wxT("/* [wxMaxima: title   start ]\n");
        s += tmp->GetEditable()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: title   end   ] */\n");
        break;
    }
    if (tmp == end)
      break;
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  if (wxTheClipboard->Open())
  {
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
      m_workingGroup != NULL)
    return false;

  if ((m_selectionStart->GetType() != MC_TYPE_GROUP) || (m_selectionEnd->GetType() != MC_TYPE_GROUP))
    return false;
  else { // a fine selection of groupcells
    MathCell* tmp = m_selectionStart;
    while (tmp != NULL)
    {
      if (m_evaluationQueue->IsInQueue(dynamic_cast<GroupCell*>(tmp)))
        return false;

      if (tmp == m_selectionEnd)
        break;
      tmp = tmp->m_next;
    }
  }

  return true;
}

/***
 * Delete the selection
 */
void MathCtrl::DeleteSelection(bool deletePrompt) {
  if (m_selectionStart == NULL || m_selectionEnd == NULL ||
      m_workingGroup != NULL)
    return;

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  GroupCell *start = dynamic_cast<GroupCell*>(m_selectionStart->GetParent());
  GroupCell *end = dynamic_cast<GroupCell*>(m_selectionEnd->GetParent());

  if (start == NULL || end == NULL)
    return;

  m_saved = false;

  bool renumber = false;

  SetActiveCell(NULL, false);
  m_hCaretActive = false;
  m_hCaretPosition = NULL;

  // check for renumbering
  GroupCell *tmp = start;
  while (tmp) {
    if (tmp->IsFoldable() || (tmp->GetGroupType() == GC_TYPE_IMAGE)) {
      renumber = true;
      break;
    }
    if (tmp == end)
      break;
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  GroupCell *newSelection = dynamic_cast<GroupCell*>(end->m_next);

  if (end == m_last)
    m_last = dynamic_cast<GroupCell*>(start->m_previous);

  if (start == m_tree) {
    if (end->m_previous != NULL) {
      end->m_previous->m_nextToDraw = NULL;
      end->m_previous->m_next = NULL;
    }
    if (end->m_next != NULL) {
      end->m_next->m_previous = NULL;
      end->m_next->m_previousToDraw = NULL;
    }

    m_tree = dynamic_cast<GroupCell*>(end->m_next);
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
    SetHCaret(newSelection->m_previous, false);
  else
    SetHCaret(m_last, false);

  if (renumber)
    NumberSections();

  Recalculate();
  Refresh();
}

void MathCtrl::OpenHCaret(wxString txt, int type)
{
  // if we have a working group, bypass normal behaviour
  // and insert an EditorCell into the output
  // of the working group.
  if (m_workingGroup != NULL) {
    if (m_workingGroup->RevealHidden()) {
      FoldOccurred();
      Recalculate(true);
    }

    EditorCell *newInput = new EditorCell;
    newInput->SetType(MC_TYPE_INPUT);
    newInput->SetValue(txt);
    newInput->CaretToEnd();

    m_workingGroup->AppendOutput(newInput);
    newInput->SetParent(m_workingGroup, false);
    SetActiveCell(newInput, false);

    wxClientDC dc(this);
    CellParser parser(dc);
    parser.SetZoomFactor(m_zoomFactor);
    parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);
    m_workingGroup->RecalculateAppended(parser);

    Recalculate();
    Refresh();

    return;
  }

  // set m_hCaretPosition to a sensible value
  if (m_activeCell != NULL)
    SetHCaret(m_activeCell->GetParent(), false);
  else if (m_selectionStart != NULL)
    SetHCaret(m_selectionStart->GetParent(), false);

  if (!m_hCaretActive) {
    if (m_last == NULL)
      return;
    SetHCaret(m_last, false);
  }

  // insert a new group cell
  GroupCell *group = new GroupCell(type, txt);
  // check how much to unfold for this type
  if (m_hCaretPosition) {
    while (IsLesserGCType(type, m_hCaretPosition->GetGroupType())) {
      GroupCell *result = m_hCaretPosition->Unfold();
      if (result == NULL) // assumes that unfold sets hcaret to the end of unfolded cells
        break; // unfold returns NULL when it cannot unfold
      SetHCaret(result, false);
    }
  }
  InsertGroupCells(group, m_hCaretPosition);

  // activate editor
  SetActiveCell(group->GetEditable(), false);
  m_activeCell->ClearUndo();
  ScrollToCell(group);

  Refresh();
}

/***
 * Support for copying and deleting with keyboard
 */
void MathCtrl::OnKeyDown(wxKeyEvent& event) {
  switch (event.GetKeyCode()) {

    case WXK_DELETE:
      if (event.ShiftDown()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_cut);
#if wxCHECK_VERSION(2,9,0)
        GetParent()->ProcessWindowEvent(ev);
#else
        GetParent()->ProcessEvent(ev);
#endif
      } else if (CanDeleteSelection()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
#if wxCHECK_VERSION(2,9,0)
        GetParent()->ProcessWindowEvent(ev);
#else
        GetParent()->ProcessEvent(ev);
#endif
      } else
        event.Skip();
      break;

    case WXK_INSERT:
      if (event.ControlDown()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_copy);
#if wxCHECK_VERSION(2,9,0)
        GetParent()->ProcessWindowEvent(ev);
#else
        GetParent()->ProcessEvent(ev);
#endif
      } else if (event.ShiftDown()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_paste);
#if wxCHECK_VERSION(2,9,0)
        GetParent()->ProcessWindowEvent(ev);
#else
        GetParent()->ProcessEvent(ev);
#endif
      } else
        event.Skip();
      break;

    case WXK_BACK:
      if (CanDeleteSelection()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
#if wxCHECK_VERSION(2,9,0)
        GetParent()->ProcessWindowEvent(ev);
#else
        GetParent()->ProcessEvent(ev);
#endif
      }
      else
        event.Skip();
      break;

    case WXK_NUMPAD_ENTER:
      if (m_activeCell != NULL && m_activeCell->GetType() == MC_TYPE_INPUT)
        dynamic_cast<wxFrame*>(GetParent())->ProcessCommand(menu_evaluate);
      else if (m_hCaretActive)
        OpenHCaret(wxT("%"));
      else
        event.Skip();
      break;

    case WXK_RETURN:
      if ((m_activeCell != NULL) && (m_activeCell->GetType() != MC_TYPE_INPUT))
        event.Skip(); // if enter pressed in text, title, section cell, pass the event
      else {
        bool enterEvaluates = false;
        bool controlOrShift = event.ControlDown() || event.ShiftDown();
        wxConfig::Get()->Read(wxT("enterEvaluates"), &enterEvaluates);
        if ((!enterEvaluates &&  controlOrShift) ||
            ( enterEvaluates && !controlOrShift) )
        { // shift-enter pressed === menu_evaluate event
          dynamic_cast<wxFrame*>(GetParent())->ProcessCommand(menu_evaluate);
        } else
          event.Skip();
      }
      break;

#ifndef wxUSE_UNICODE
    case WXK_ESCAPE:
      if (m_activeCell == NULL) {
        SetSelection(NULL);
        Refresh();
      }

      else
        SetHCaret(m_activeCell->GetParent()); // also refreshes

      break;
#endif

    default:
      event.Skip();
  }
}

/*****
 * OnChar handles key events. If we have an active cell, sends the
 * event to the active cell, else moves the cursor between groups.
 *
 * TODO: this function is should be reimplemented so that it is more
 *  readable!
 */
void MathCtrl::OnChar(wxKeyEvent& event) {

#if defined __WXMSW__
  if (event.GetKeyCode() == WXK_NUMPAD_DECIMAL) {
    return;
  }
#endif

  if (event.CmdDown() && !event.AltDown()) {
    event.Skip();
    return;
  }

  if (m_activeCell != NULL) { // we are in an active cell
    bool needRecalculate = false;

    if (event.GetKeyCode() == WXK_UP &&
        m_activeCell->CaretAtStart() &&
        !event.ShiftDown()) { // don't exit the cell if we are making a selection
      SetHCaret((m_activeCell->GetParent())->m_previous);
      return;
    }

    if (event.GetKeyCode() == WXK_DOWN &&
        m_activeCell->CaretAtEnd() &&
        !event.ShiftDown()) {
      SetHCaret(m_activeCell->GetParent());
      return;
    }

    if ((event.GetKeyCode() == WXK_BACK || event.GetKeyCode() == WXK_DELETE) &&
        m_activeCell->GetValue() == wxEmptyString) {
      m_selectionStart = m_selectionEnd = m_activeCell->GetParent();
      DeleteSelection();
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

    if (m_activeCell->IsDirty()) {
      m_saved = false;

      int height = m_activeCell->GetHeight();

      int fontsize = parser.GetDefaultFontSize();

      m_activeCell->ResetData();
      m_activeCell->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
      m_activeCell->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);

      if (height != m_activeCell->GetHeight() ||
          m_activeCell->GetWidth() + m_activeCell->m_currentPoint.x >=
            GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT)
        needRecalculate = true;
    }

    /// If we need to recalculate then refresh the window
    if (needRecalculate) {
      GroupCell *group = dynamic_cast<GroupCell*>(m_activeCell->GetParent());
      group->ResetSize();
      group->ResetData();
      if (m_activeCell->CheckChanges() &&
          (group->GetGroupType() == GC_TYPE_CODE) &&
          (m_activeCell == group->GetEditable()))
        group->ResetInputLabel();
      Recalculate();
      Refresh();
    }

    /// Otherwise refresh only the active cell
    else {
      wxRect rect;
      if (m_activeCell->CheckChanges()) {
        GroupCell *group = dynamic_cast<GroupCell*>(m_activeCell->GetParent());
        if ((group->GetGroupType() == GC_TYPE_CODE) &&
            (m_activeCell == group->GetEditable()))
          group->ResetInputLabel();
        rect = group->GetRect();
        rect.width = GetVirtualSize().x;
      }
      else {
        rect = m_activeCell->GetRect();
        rect.width = GetVirtualSize().x;
      }
      CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
      RefreshRect(rect);
    }

    wxPoint point = m_activeCell->PositionToPoint(parser);
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
                if (m_hCaretPosition != NULL &&
                    m_hCaretPosition->m_next == m_hCaretPositionEnd)
                  m_hCaretPositionStart = dynamic_cast<GroupCell*>(m_hCaretPositionStart->m_previous);
                m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPositionEnd->m_previous);
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
              EditorCell * editor = m_hCaretPosition->GetEditable();
              if(editor != NULL && m_workingGroup == NULL)
              {
                SetActiveCell(editor, false);
                m_hCaretActive = false;
                m_activeCell->CaretToEnd();
                ShowPoint(m_activeCell->PositionToPoint(parser));
                if (editor->GetWidth() == -1)
                  Recalculate();
                Refresh();
              }
              else { // can't get editor... jump over cell..
                m_hCaretPosition = dynamic_cast<GroupCell*>( m_hCaretPosition->m_previous);
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
                m_hCaretPositionStart = m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPosition->m_next);
            }
            else {
              if (m_hCaretPositionEnd->m_next != NULL) {
                if (m_hCaretPosition == m_hCaretPositionEnd)
                  m_hCaretPositionStart = dynamic_cast<GroupCell*>(m_hCaretPositionStart->m_next);
                m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPositionEnd->m_next);
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
              EditorCell *editor = m_tree->GetEditable();
              if (editor != NULL && m_workingGroup == NULL) // try to edit the first cell
              {
                SetActiveCell(editor, false);
                m_activeCell->CaretToStart();
                ShowPoint(m_activeCell->PositionToPoint(parser));
                if (editor->GetWidth() == -1)
                  Recalculate();
                Refresh();
              }
              else { // else jump over
                m_hCaretPosition = m_tree;
                Refresh();
              }
            }

            else if (m_hCaretPosition != NULL && m_hCaretPosition->m_next != NULL)
            {
              EditorCell *editor = dynamic_cast<GroupCell*>(m_hCaretPosition->m_next)->GetEditable();
              if( editor != NULL && m_workingGroup == NULL)
              {
                SetActiveCell(editor, false);
                m_activeCell->CaretToStart();
                ShowPoint(m_activeCell->PositionToPoint(parser));
                if (editor->GetWidth() == -1)
                  Recalculate();
                Refresh();
              }
              else { // can't get editor.. jump over cell..
                m_hCaretPosition = dynamic_cast<GroupCell*>( m_hCaretPosition->m_next);
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
          case WXK_WINDOWS_LEFT:
          case WXK_WINDOWS_RIGHT:
          case WXK_WINDOWS_MENU:
          case WXK_COMMAND:
          case WXK_START:
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
            if (m_selectionStart == NULL || m_selectionEnd == NULL)
              OpenHCaret(wxEmptyString);
            else
              OpenHCaret(GetString());
            break;

#if wxUSE_UNICODE
            // ESCAPE is handled by the new cell
          case WXK_ESCAPE:
            OpenHCaret(wxEmptyString);
            if (m_activeCell != NULL)
              m_activeCell->ProcessEvent(event);
            break;
#endif

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

  while (tmp != NULL) {
    currentHeight += tmp->GetMaxHeight();
    currentHeight += MC_GROUP_SKIP;
    *height = currentHeight;
    currentWidth = MC_BASE_INDENT + tmp->GetWidth();
    *width = MAX(currentWidth + MC_BASE_INDENT, *width);
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

  if (m_selectionStart != NULL &&
      m_selectionStart == m_selectionEnd &&
      (m_selectionStart->GetType() == MC_TYPE_IMAGE ||
       m_selectionStart->GetType() == MC_TYPE_SLIDE))
  {
    if (m_selectionStart->GetType() == MC_TYPE_IMAGE)
      return ((ImgCell *)m_selectionStart)->ToImageFile(file);
    else
      return ((SlideShow *)m_selectionStart)->ToImageFile(file);
  }
  else
  {
    MathCell* tmp = CopySelection();

    Bitmap bmp;
    bmp.SetData(tmp);

    return bmp.ToFile(file);
  }
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
      tmp = tmp->m_nextToDraw;
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

//Simple iterator over a Maxima input string, skipping comments and strings
struct SimpleMathParserIterator{
  const wxString &input; //reference to input string (must be a reference, so it can be modified)
  int pos;

  SimpleMathParserIterator(const wxString& ainput): input(ainput), pos(0){
    if (isValid() && (input[0] == '"' || (input[0] == '/' && input.length() > 1 && input[1] == '*') )) {
      //skip strings or comments at string start
      pos--;
      ++(*this);
    }
  }
  bool isValid(){
    return pos < input.length();
  }
  void operator++(){
    int oldpos = pos;
    pos++;
    while (pos < input.length() && oldpos != pos) {
      oldpos = pos;
      if (input[pos] == '"') { //skip strings
        pos++; //skip leading "
        while (pos < input.length() && input[pos] != '"')
          pos++;
        pos++;//skip trailing "
      }
      if (pos + 1 < input.length() && input[pos] == '/' && input[pos+1] == '*' ) { //skip comments
        pos += 2; //skip /*
        while (pos < input.length() && (input[pos] != '*' || input[pos+1] != '/'))
          pos++;
        pos += 2; //skip */
      }
    }
  }
  inline wxChar operator*() {
    return input[pos];
  }
};

//returns the index in (%i...) or (%o...)
int getMathCellIndex(MathCell* cell){
  if (!cell) return -1;
  wxString strindex = cell->ToString(false).Trim(); //(%i...)
  long temp;
  if (!strindex.Mid(3, strindex.Len()-4).ToLong(&temp)) return -1;
  return temp;
}

void MathCtrl::CalculateReorderedCellIndices(MathCell *tree, int &cellIndex, std::vector<int>& cellMap){
  GroupCell* tmp = dynamic_cast<GroupCell*>(tree);
  while (tmp != NULL) {
    if (!tmp->IsHidden() && tmp->GetGroupType() == GC_TYPE_CODE) {
      MathCell *prompt = tmp->GetPrompt();
      MathCell *cell = tmp->GetEditable();

      //fprintf(stderr, ">%ls< >%ls<\n", prompt?prompt->ToString(false).wc_str():(wchar_t*)"n\0i\0l\0\0", cell?cell->ToString(false).wc_str():(wchar_t*)"n\0i\0l\0\0" );
      //fprintf(stderr, ">%ls< >%ls<\n", tmp->GetLabel()?tmp->GetLabel()->ToString(false).wc_str():(wchar_t*)"n\0i\0l\0\0", tmp->GetOutput()?tmp->GetOutput()->ToString(false).wc_str():(wchar_t*)"n\0i\0l\0\0" );

      wxString input = cell->ToString(false);
      if (prompt && cell && input.Len() > 0) {
        int outputExpressions = 0;
        int initialHiddenExpressions = 0;
        for (SimpleMathParserIterator it = input; it.isValid(); ++it)
          switch (*it) {
          case '$': if (initialHiddenExpressions == outputExpressions) initialHiddenExpressions++; //fallthrough
          case ';': outputExpressions++;
          }

        int promptIndex = getMathCellIndex(prompt);
        int outputIndex = getMathCellIndex(tmp->GetLabel()) - initialHiddenExpressions;
        int index = promptIndex;
        if (promptIndex < 0) index = outputIndex; //no input index => use output index
        else if (outputIndex < 0 && initialHiddenExpressions < outputExpressions) {
          //input index, but no output index means the expression was evaluated, but produced no result
          // => it is invalid and should be ignored
          index = -1;
          outputExpressions = 0;
        }

        if (index > 0) {
          if (index + outputExpressions > cellMap.size()) cellMap.resize(index+outputExpressions);
          for (int i=0; i < outputExpressions; i++)
            cellMap[index + i] = cellIndex + i;
        }

        cellIndex += outputExpressions; //new cell index
      }
    }

    if (tmp->GetHiddenTree() != NULL)
      CalculateReorderedCellIndices(tmp->GetHiddenTree(), cellIndex, cellMap);

    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }
}

bool MathCtrl::ExportToHTML(wxString file) {
  wxString imgDir;
  wxString path, filename, ext;
  int count = 0;
  GroupCell *tmp = m_tree;

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
  AddLineToFile(output, wxT("  <TITLE>") + filename + wxT("</TITLE>"));
  AddLineToFile(output, wxT("  <META NAME=\"generator\" CONTENT=\"wxMaxima\">"));
  AddLineToFile(
      output,
      wxT("  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=utf-8\">"));

//////////////////////////////////////////////
// Write styles
//////////////////////////////////////////////

  wxString font, fontTitle, fontSection, fontSubsection, fontText;
  wxString colorInput(wxT("blue"));
  wxString colorPrompt(wxT("red"));
  wxString colorText(wxT("black")), colorTitle(wxT("black")), colorSection(wxT("black")),
           colorSubSec(wxT("black"));
  wxString colorTextBg(wxT("white"));
  wxString colorBg(wxT("white"));

  // bold and italic
  bool   boldInput = false;
  bool italicInput = false;
  bool   boldPrompt = false;
  bool italicPrompt = false;
  bool   boldString = false;
  bool italicString = false;

  bool   boldTitle = false;
  bool italicTitle = false;
  bool  underTitle = false;
  bool   boldSection = false;
  bool italicSection = false;
  bool  underSection = false;
  bool   boldSubsection = false;
  bool italicSubsection = false;
  bool  underSubsection = false;

  int fontSize = 12;
  wxConfigBase* config= wxConfig::Get();
  // main fontsize
  config->Read(wxT("fontSize"), &fontSize);

  // read fonts
  config->Read(wxT("Style/fontname"), &font);
  config->Read(wxT("Style/Title/fontname"), &fontTitle);
  config->Read(wxT("Style/Section/fontname"), &fontSection);
  config->Read(wxT("Style/Subsection/fontname"), &fontSubsection);
  config->Read(wxT("Style/Text/fontname"), &fontText);

  // read colors
  config->Read(wxT("Style/Input/color"), &colorInput);
  config->Read(wxT("Style/MainPrompt/color"), &colorPrompt);
  config->Read(wxT("Style/Text/color"), &colorText);
  config->Read(wxT("Style/Section/color"), &colorSection);
  config->Read(wxT("Style/Subsection/color"), &colorSubSec);
  config->Read(wxT("Style/Title/color"), &colorTitle);
  config->Read(wxT("Style/TextBackground/color"), &colorTextBg);
  config->Read(wxT("Style/Background/color"), &colorBg);

  // read bold and italic
  config->Read(wxT("Style/Input/bold"), &boldInput);
  config->Read(wxT("Style/String/bold"), &boldString);
  config->Read(wxT("Style/Input/italic"), &italicInput);
  config->Read(wxT("Style/String/italic"), &italicString);
  config->Read(wxT("Style/MainPrompt/bold"), &boldPrompt);
  config->Read(wxT("Style/MainPrompt/italic"), &italicPrompt);

  config->Read(wxT("Style/Title/bold"),     &boldTitle);
  config->Read(wxT("Style/Title/italic"), &italicTitle);
  config->Read(wxT("Style/Title/underlined"), &underTitle);
  config->Read(wxT("Style/Section/bold"),     &boldSection);
  config->Read(wxT("Style/Section/italic"), &italicSection);
  config->Read(wxT("Style/Section/underlined"), &underSection);
  config->Read(wxT("Style/Subsection/bold"),     &boldSubsection);
  config->Read(wxT("Style/Subsection/italic"), &italicSubsection);
  config->Read(wxT("Style/Subsection/underlined"), &underSubsection);

  AddLineToFile(output, wxT("  <STYLE TYPE=\"text/css\">"));

  // BODY STYLE
  AddLineToFile(output, wxT("body {"));
  if (font.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    font +
    wxT(";"));
  }
  if (colorBg.Length()) {
    wxColour color(colorBg);
    AddLineToFile(output, wxT("  background-color: ") +
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
  if   (boldInput) AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicInput) AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("}"));

  // COMMENT STYLE
  AddLineToFile(output, wxT(".comment {"));
  if (fontText.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    fontText +
    wxT(";"));
  }
  if (colorText.Length()) {
    wxColour color(colorText);
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

  // IMAGE STYLE
  AddLineToFile(output, wxT(".image {"));
  if (fontText.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    fontText + wxT(";"));
  }
  if (colorText.Length()) {
    wxColour color(colorText);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));

  // SECTION STYLE
  AddLineToFile(output, wxT(".section {"));
  if (fontSection.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    fontSection + wxT(";"));
  }
  if (colorSection.Length()) {
    wxColour color(colorSection);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if   (boldSection) AddLineToFile(output, wxT("  font-weight: bold;"));
  if  (underSection) AddLineToFile(output, wxT("  text-decoration: underline;"));
  if (italicSection) AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("  font-size: 1.5em;"));
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));


  // SUBSECTION STYLE
  AddLineToFile(output, wxT(".subsect {"));
  if (fontSubsection.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    fontSubsection + wxT(";"));
  }
  if (colorSubSec.Length()) {
    wxColour color(colorSubSec);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if   (boldSubsection) AddLineToFile(output, wxT("  font-weight: bold;"));
  if  (underSubsection) AddLineToFile(output, wxT("  text-decoration: underline;"));
  if (italicSubsection) AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("  font-size: 1.2em;"));
  AddLineToFile(output, wxT("  padding: 2mm;"));
  AddLineToFile(output, wxT("}"));

  // TITLE STYLE
  AddLineToFile(output, wxT(".title {"));
  if (fontTitle.Length()) {
    AddLineToFile(output, wxT("  font-family: ") +
    fontTitle + wxT(";"));
  }
  if (colorTitle.Length()) {
    wxColour color(colorTitle);
    AddLineToFile(output, wxT("  color: ") +
    wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
    wxT(";"));
  }
  if   (boldTitle) AddLineToFile(output, wxT("  font-weight: bold;"));
  if  (underTitle) AddLineToFile(output, wxT("  text-decoration: underline;"));
  if (italicTitle) AddLineToFile(output, wxT("  font-style: italic;"));
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
  if   (boldPrompt) AddLineToFile(output, wxT("  font-weight: bold;"));
  if (italicPrompt) AddLineToFile(output, wxT("  font-style: italic;"));
  AddLineToFile(output, wxT("}"));

  // TABLES
  AddLineToFile(output, wxT("table {"));
  AddLineToFile(output, wxT("  border: 0px;"));
  AddLineToFile(output, wxT("}"));
  AddLineToFile(output, wxT("td {"));
  AddLineToFile(output, wxT("  vertical-align: top;"));
  AddLineToFile(output, wxT("  padding: 1mm;"));
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
    if (tmp->GetGroupType() == GC_TYPE_CODE)
    {
      AddLineToFile(output, wxT("\n\n<!-- Code cell -->\n\n"));
      MathCell *prompt = tmp->GetPrompt();
      AddLineToFile(output, wxT("<P><TABLE><TR><TD>"));
      AddLineToFile(output, wxT("  <SPAN CLASS=\"prompt\">"));
      AddLineToFile(output, prompt->ToString(false));
      AddLineToFile(output, wxT("  </SPAN></TD>"));

      MathCell *input = tmp->GetInput();
      if (input != NULL) {
        AddLineToFile(output, wxT("  <TD><SPAN CLASS=\"input\">"));
        AddLineToFile(output, PrependNBSP(input->ToString(false)));
        AddLineToFile(output, wxT("  </SPAN></TD>"));
      }
      AddLineToFile(output, wxT("</TR></TABLE>"));

      MathCell *out = tmp->GetLabel();

      if (out == NULL) {
        AddLineToFile(output, wxEmptyString);
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

    else
    {
      switch(tmp->GetGroupType()) {
        case GC_TYPE_TEXT:
          AddLineToFile(output, wxT("\n\n<!-- Text cell -->\n\n"));
          AddLineToFile(output, wxT("<P CLASS=\"comment\">"));
          AddLineToFile(output, PrependNBSP(tmp->GetEditable()->ToString(false)));
          break;
        case GC_TYPE_SECTION:
          AddLineToFile(output, wxT("\n\n<!-- Section cell -->\n\n"));
          AddLineToFile(output, wxT("<P CLASS=\"section\">"));
          AddLineToFile(output, PrependNBSP(tmp->GetPrompt()->ToString(false) + tmp->GetEditable()->ToString(false)));
          break;
        case GC_TYPE_SUBSECTION:
          AddLineToFile(output, wxT("\n\n<!-- Subsection cell -->\n\n"));
          AddLineToFile(output, wxT("<P CLASS=\"subsect\">"));
          AddLineToFile(output, PrependNBSP(tmp->GetPrompt()->ToString(false) + tmp->GetEditable()->ToString(false)));
          break;
        case GC_TYPE_TITLE:
          AddLineToFile(output, wxT("\n\n<!-- Title cell -->\n\n"));
          AddLineToFile(output, wxT("<P CLASS=\"title\">"));
          AddLineToFile(output, PrependNBSP(tmp->GetEditable()->ToString(false)));
          break;
        case GC_TYPE_PAGEBREAK:
          AddLineToFile(output, wxT("\n\n<!-- Page break cell -->\n\n"));
          AddLineToFile(output, wxT("<P CLASS=\"comment\">"));
          AddLineToFile(output, wxT("<hr/>"));
          break;
        case GC_TYPE_IMAGE:
        {
          AddLineToFile(output, wxT("\n\n<!-- Image cell -->\n\n"));
          MathCell *out = tmp->GetLabel();
          AddLineToFile(output, wxT("<P CLASS=\"image\">"));
          AddLineToFile(output, PrependNBSP(tmp->GetPrompt()->ToString(false) +
                                            wxT(" ") +
                                            tmp->GetEditable()->ToString(false)));
          AddLineToFile(output, wxT("<BR>"));
          CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count), out, NULL, true);
          AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_img/") +
              filename +
              wxString::Format(wxT("_%d.png\">"), count));
          count++;
        }
        break;
      }
    }

    AddLineToFile(output, wxT("</P>"));

    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

//////////////////////////////////////////////
// Footer
//////////////////////////////////////////////

  AddLineToFile(output, wxEmptyString);
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
  GroupCell *tmp = m_tree;

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
  AddLineToFile(output, wxT("%% Created with wxMaxima " VERSION ));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\setlength{\\parskip}{\\medskipamount}"));
  AddLineToFile(output, wxT("\\setlength{\\parindent}{0pt}"));
  AddLineToFile(output, wxT("\\usepackage[utf8]{inputenc}"));
  AddLineToFile(output, wxT("\\usepackage{graphicx}"));
  AddLineToFile(output, wxT("\\usepackage{color}"));
  AddLineToFile(output, wxT("\\usepackage{amsmath}"));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\definecolor{labelcolor}{RGB}{100,0,0}"));
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\begin{document}"));

  //
  // Write contents
  //

  while (tmp != NULL) {
    wxString s = tmp->ToTeX(false, imgDir, filename, &imgCounter);
    AddLineToFile(output, s);
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  //
  // Close document
  //
  AddLineToFile(output, wxT("\\end{document}"));

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

void MathCtrl::ExportToMAC(wxTextFile& output, MathCell *tree, bool wxm, const std::vector<int>& cellMap, bool fixReorderedIndices)
{
  GroupCell* tmp = dynamic_cast<GroupCell*>(tree);

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
    if (tmp->GetGroupType() == GC_TYPE_CODE) {
      MathCell *txt = tmp->GetEditable();
      if (txt != NULL) {
        wxString input = txt->ToString(false);

        if (fixReorderedIndices)
          for (SimpleMathParserIterator it = input; it.pos + 1 < it.input.length(); ++it)
            if (*it == '%' && (input[it.pos+1] == 'i' || input[it.pos+1] == 'o') && (it.pos == 0 || input[it.pos-1] != '%')){
              it.pos += 2;
              int startPos = it.pos;
              int temp = 0;
              for (; it.pos < input.Length() && (*it >= '0' && *it <= '9'); ++it.pos)
                temp = temp * 10 + (*it - '0');
              if (temp >= cellMap.size() || cellMap[temp] < 1) continue;
              wxString tempstr; tempstr << cellMap[temp];
              input.replace(startPos, it.pos - startPos, tempstr);
              it.pos = startPos + tempstr.length();
            }


        if (input.Length()>0) {
          if (wxm)
            AddLineToFile(output, wxT("/* [wxMaxima: input   start ] */"), false);
          AddLineToFile(output, input, false);
          if (wxm)
            AddLineToFile(output, wxT("/* [wxMaxima: input   end   ] */"), false);
        }
      }
    }

    else if (tmp->GetGroupType() == GC_TYPE_PAGEBREAK) {
      AddLineToFile(output, wxT("/* [wxMaxima: page break    ] */"), false);
    }

    // Write text
    else {
      MathCell *txt = tmp->GetEditable();

      if (wxm) {
        switch (txt->GetType()) {
          case MC_TYPE_TEXT:
            AddLineToFile(output, wxT("/* [wxMaxima: comment start ]"), false);
            break;
          case MC_TYPE_SECTION:
            AddLineToFile(output, wxT("/* [wxMaxima: section start ]"), false);
            break;
          case MC_TYPE_SUBSECTION:
            AddLineToFile(output, wxT("/* [wxMaxima: subsect start ]"), false);
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
          case MC_TYPE_SUBSECTION:
            AddLineToFile(output, wxT("   [wxMaxima: subsect end   ] */"), false);
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

    if (tmp->GetHiddenTree() != NULL)
    {
      AddLineToFile(output, wxEmptyString);
      AddLineToFile(output, wxT("/* [wxMaxima: fold    start ] */"));
      ExportToMAC(output, tmp->GetHiddenTree(), wxm, cellMap, fixReorderedIndices);
      AddLineToFile(output, wxEmptyString);
      AddLineToFile(output, wxT("/* [wxMaxima: fold    end   ] */"));
    }

    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

}

bool MathCtrl::ExportToMAC(wxString file)
{
  m_saved = true;

  bool wxm = false;
  if (file.Right(4) == wxT(".wxm"))
    wxm = true;

  wxTextFile output(file);
  if (output.Exists())
  {
    if (!output.Open(file))
      return false;
    output.Clear();
  }
  else if (!output.Create(file))
    return false;

  m_saved = true;

  if (wxm) {
    AddLineToFile(output, wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"), false);
    wxString version(wxT(VERSION));
    AddLineToFile(output, wxT("/* [ Created with wxMaxima version ") + version + wxT(" ] */"), false);
  }

  bool fixReorderedIndices;
  wxConfig::Get()->Read(wxT("fixReorderedIndices"), &fixReorderedIndices);
  std::vector<int> cellMap;
  if (fixReorderedIndices) {
    int cellIndex = 1;
    CalculateReorderedCellIndices(m_tree, cellIndex,  cellMap);
  }
  ExportToMAC(output, m_tree, wxm, cellMap, fixReorderedIndices);

  AddLineToFile(output, wxEmptyString, false);
  if (wxm) {
    AddLineToFile(output, wxT("/* Maxima can't load/batch files which end with a comment! */"), false);
    AddLineToFile(output, wxT("\"Created with wxMaxima\"$"), false);
  }

  bool done = output.Write(wxTextFileType_None);
  output.Close();

  return done;
}

wxString ConvertToUnicode(wxString str)
{
#if wxUSE_UNICODE
  return str;
#else
  wxString str1(str.wc_str(*wxConvCurrent), wxConvUTF8);
  return str1;
#endif
}

bool MathCtrl::ExportToWXMX(wxString file)
{
  // delete file if it already exists
  if(wxFileExists(file))
    if(!wxRemoveFile(file))
      return false;

  wxFFileOutputStream out(file);
  wxZipOutputStream zip(out);
  wxTextOutputStream output(zip);

  // first zip entry is "content.xml", xml of m_tree
  zip.PutNextEntry(wxT("content.xml"));
  output << wxT("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  // TODO write DOCTYPE
  output << wxT("\n<!--   Created by wxMaxima ") << wxT(VERSION) << wxT("   -->");
  output << wxT("\n<!--http://wxmaxima.sourceforge.net-->\n");

  // write document
  output << wxT("\n<wxMaximaDocument version=\"");
  output << DOCUMENT_VERSION_MAJOR << wxT(".");
  output << DOCUMENT_VERSION_MINOR << wxT("\" zoom=\"");
  output << int(100.0 * m_zoomFactor) << wxT("\">\n");

  // Reset image counter
  ImgCell::WXMXResetCounter();

  GroupCell* tmp = m_tree;
  // Write contents //
  while (tmp != NULL) {
    output << ConvertToUnicode(tmp->ToXML(false));
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  output << wxT("\n</wxMaximaDocument>");

  // save images from memory to zip file
  wxFileSystem *fsystem = new wxFileSystem();
  fsystem->AddHandler(new wxMemoryFSHandler);
  fsystem->ChangePathTo(wxT("memory:"), true);

  for (int i=1; i<=ImgCell::WXMXImageCount(); i++)
  {
    wxString name = wxT("image");
    name << i << wxT(".png");

    wxFSFile *fsfile = fsystem->OpenFile(name);

    if (fsfile) {
      zip.PutNextEntry(name);
      wxInputStream *imagefile = fsfile->GetStream();

      while (!(imagefile->Eof()))
        imagefile->Read(zip);

      delete imagefile;
      wxMemoryFSHandler::RemoveFile(name);
    }
  }

  delete fsystem;
  m_saved = true;
  return true;
}

/**
 * CanEdit: we can edit the input if the we have the whole input in selection!
 */
bool MathCtrl::CanEdit() {
  if (m_selectionStart == NULL || m_selectionEnd != m_selectionStart
      || m_workingGroup != NULL || m_editingEnabled == false)
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
    m_activeCell->SelectWordUnderCaret();
    Refresh();
  }
  else if (m_selectionStart != NULL) {
    GroupCell *parent = dynamic_cast<GroupCell*>(m_selectionStart->GetParent());
    parent->SelectOutput(&m_selectionStart, &m_selectionEnd);
    Refresh();
  }
}

bool MathCtrl::ActivatePrevInput() {
  if (m_selectionStart == NULL && m_activeCell == NULL)
    return false;

  GroupCell *tmp;
  if (m_selectionStart != NULL)
    tmp = dynamic_cast<GroupCell*>(m_selectionStart->GetParent());
  else {
    tmp = dynamic_cast<GroupCell*>(m_activeCell->GetParent());
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = dynamic_cast<GroupCell*>(tmp->m_previous);
  if (tmp == NULL)
    return false;

  EditorCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL) {
    inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = dynamic_cast<GroupCell*>(tmp->m_previous);
  }

  if (inpt == NULL)
    return false;

  ScrollToCell(inpt);

  SetActiveCell(inpt, false);
  m_activeCell->CaretToEnd();

  Refresh();

  return true;
}

bool MathCtrl::ActivateNextInput(bool input) {
  if (m_selectionStart == NULL && m_activeCell == NULL)
    return false;

  GroupCell *tmp;
  if (m_selectionStart != NULL)
    tmp = dynamic_cast<GroupCell*>(m_selectionStart->GetParent());
  else {
    tmp = dynamic_cast<GroupCell*>(m_activeCell->GetParent());
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  if (tmp == NULL)
    return false;

  EditorCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL) {
    if (input)
      inpt = dynamic_cast<EditorCell*>(tmp->GetInput());
    else
      inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  if (inpt == NULL)
    return false;

  ScrollToCell(inpt);

  SetActiveCell(inpt, false);
  m_activeCell->CaretToStart();

  Refresh();

  return true;
}

/////////////////////////////////////////////////////////////
// methods related to evaluation queue
//
void MathCtrl::AddDocumentToEvaluationQueue()
{
  GroupCell* tmp = m_tree;
  while (tmp != NULL) {
      m_evaluationQueue->AddToQueue((GroupCell*) tmp);
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }
  SetHCaret(m_last);
}

/**
 * Add the entire document, including hidden cells, to the evaluation queue.
 */
void MathCtrl::AddEntireDocumentToEvaluationQueue()
{
  GroupCell* tmp = m_tree;
  while (tmp != NULL) {
    m_evaluationQueue->AddToQueue((GroupCell*) tmp);
    m_evaluationQueue->AddHiddenTreeToQueue((GroupCell*) tmp);
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }
  SetHCaret(m_last);
}

void MathCtrl::AddSelectionToEvaluationQueue()
{
  if ((m_selectionStart == NULL) || (m_selectionEnd == NULL))
    return;
  if (m_selectionStart->GetType() != MC_TYPE_GROUP)
    return;
  GroupCell* tmp = dynamic_cast<GroupCell*>(m_selectionStart);
  while (tmp != NULL) {
      m_evaluationQueue->AddToQueue((GroupCell*) tmp);
    if (tmp == m_selectionEnd)
      break;
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }
  SetHCaret(m_selectionEnd);
}

void MathCtrl::AddCellToEvaluationQueue(GroupCell* gc)
{
    m_evaluationQueue->AddToQueue((GroupCell*) gc);
    SetHCaret((MathCell *) gc);
}
void MathCtrl::ClearEvaluationQueue()
{
  while (m_evaluationQueue->GetFirst() != NULL)
    m_evaluationQueue->RemoveFirst();
}
//////// end of EvaluationQueue related stuff ////////////////

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

void MathCtrl::SetActiveCell(EditorCell *cell, bool callRefresh) {
  if (m_activeCell != NULL)
    m_activeCell->ActivateCell();

  m_activeCell = cell;

  if (m_activeCell != NULL) {
    SetSelection(NULL);
    bool match = false;
    bool insertAns = false;
    if (m_activeCell->GetType() == MC_TYPE_INPUT)
    {
      wxConfig::Get()->Read(wxT("matchParens"), &match);
      wxConfig::Get()->Read(wxT("insertAns"), &insertAns);
    }
    m_activeCell->ActivateCell();
    m_activeCell->SetMatchParens(match);
    m_activeCell->SetInsertAns(insertAns);
    m_switchDisplayCaret = false;
    m_caretTimer.Start(CARET_TIMER_TIMEOUT, true);
  }

  if (cell != NULL)
    m_hCaretActive = false; // we have activeted a cell .. disable caret

  if (callRefresh) // = true default
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
  if (m_activeCell != NULL)
  {
    m_activeCell->CutToClipboard();
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
    Refresh();
    return true;
  }
  else if (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_GROUP)
  {
    CopyCells();
    DeleteSelection();
    return true;
  }
  return false;
}

/****
 * PasteFromClipboard
 * Checks if we have cell structure in the clipboard.
 * If not, then pastes text into activeCell or opens a new cell
 * if hCaretActive == true. If yes, copies the cell structure.
 */
void MathCtrl::PasteFromClipboard(bool primary) {

  bool cells = false;

  if (primary)
    wxTheClipboard->UsePrimarySelection(true);

  // Check for cell structure
  if (wxTheClipboard->Open())
  {
    // Check if the clipboard contains text.
    if (wxTheClipboard->IsSupported( wxDF_TEXT ))
    {
      wxTextDataObject data;
      wxTheClipboard->GetData(data);
      wxString inputs(data.GetText());

      if (inputs.StartsWith(wxT("/* [wxMaxima: ")))
      {
        cells = true;

        wxStringTokenizer lines(inputs, wxT("\n"));
        wxString input, line;
        wxArrayString inp;

        // Read the content from clipboard
        while (lines.HasMoreTokens())
        {
          // Go to the beginning of the cell
          do {
            line = lines.GetNextToken();
          } while (lines.HasMoreTokens() &&
                   line != wxT("/* [wxMaxima: input   start ] */") &&
                   line != wxT("/* [wxMaxima: comment start ]") &&
                   line != wxT("/* [wxMaxima: section start ]") &&
                   line != wxT("/* [wxMaxima: subsect start ]") &&
                   line != wxT("/* [wxMaxima: title   start ]"));

          // Read the cell content
          do {
            line = lines.GetNextToken();
            if (line == wxT("/* [wxMaxima: input   end   ] */"))
            {
              inp.Add(wxT("input"));
              inp.Add(input);
              input = wxEmptyString;
            }
            else if (line == wxT("   [wxMaxima: comment end   ] */"))
            {
              inp.Add(wxT("comment"));
              inp.Add(input);
              input = wxEmptyString;
            }
            else if (line == wxT("   [wxMaxima: section end   ] */"))
            {
              inp.Add(wxT("section"));
              inp.Add(input);
              input = wxEmptyString;
            }
            else if (line == wxT("   [wxMaxima: subsect end   ] */"))
            {
              inp.Add(wxT("subsection"));
              inp.Add(input);
              input = wxEmptyString;
            }
            else if (line == wxT("   [wxMaxima: title   end   ] */"))
            {
              inp.Add(wxT("title"));
              inp.Add(input);
              input = wxEmptyString;
            }
            else
            {
              if (input.Length()>0)
                input = input + wxT("\n") + line;
              else
                input = line;
            }
          } while (lines.HasMoreTokens() &&
                   line != wxT("/* [wxMaxima: input   end   ] */") &&
                   line != wxT("   [wxMaxima: comment end   ] */") &&
                   line != wxT("   [wxMaxima: section end   ] */") &&
                   line != wxT("   [wxMaxima: subsect end   ] */") &&
                   line != wxT("   [wxMaxima: title   end   ] */"));
        }

        // Paste the content into the document.
        Freeze();
        for (unsigned int i=0; i<inp.Count(); i = i+2)
        {
          if (inp[i] == wxT("input"))
            OpenHCaret(inp[i+1]);
          else if (inp[i] == wxT("comment"))
            OpenHCaret(inp[i+1], GC_TYPE_TEXT);
          else if (inp[i] == wxT("section"))
            OpenHCaret(inp[i+1], GC_TYPE_SECTION);
          else if (inp[i] == wxT("subsection"))
            OpenHCaret(inp[i+1], GC_TYPE_SUBSECTION);
          else if (inp[i] == wxT("title"))
            OpenHCaret(inp[i+1], GC_TYPE_TITLE);
        }
        Thaw();
      }
    }

    // Check if the clipboard contains an image.
    else if (wxTheClipboard->IsSupported(wxDF_BITMAP))
    {
      OpenHCaret(wxEmptyString, GC_TYPE_IMAGE);
      GroupCell *group = dynamic_cast<GroupCell*>(m_activeCell->GetParent());

      if (group != NULL)
      {
        wxBitmapDataObject bitmap;
        wxTheClipboard->GetData(bitmap);
        ImgCell *ic = new ImgCell(wxEmptyString, false);
        ic->DrawRectangle(false);
        ic->SetBitmap(bitmap.GetBitmap());

        group->AppendOutput(ic);
      }
    }

    // Make sure the clipboard is closed!
    wxTheClipboard->Close();
  }

  // Clipboard does not have the cell structure.
  if (!cells)
  {
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
  if (primary)
    wxTheClipboard->UsePrimarySelection(false);
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

void MathCtrl::DivideCell()
{
  if (m_activeCell == NULL)
    return;

  GroupCell *parent = dynamic_cast<GroupCell*>(m_activeCell->GetParent());
  if (parent->GetEditable() != m_activeCell)
    return;

  int gctype = parent->GetGroupType();
  if (gctype == GC_TYPE_IMAGE)
    return;

  if (m_activeCell->CaretAtStart() || m_activeCell->CaretAtEnd())
    return;

  wxString newcellstring = m_activeCell->DivideAtCaret();

  SetHCaret(parent, false);
  OpenHCaret(newcellstring, gctype);
  if (m_activeCell)
    m_activeCell->CaretToStart();

}

void MathCtrl::MergeCells()
{
  wxString newcell = wxEmptyString;
  MathCell *tmp = m_selectionStart;
  if (!tmp)
    return;
  if (tmp->GetType() != MC_TYPE_GROUP)
    return; // should not happen

  while (tmp) {
    if (newcell.Length() > 0)
      newcell += wxT("\n");
    newcell += dynamic_cast<GroupCell*>(tmp)->GetEditable()->GetValue();

    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_next;
  }

  EditorCell *editor = dynamic_cast<GroupCell*>(m_selectionStart)->GetEditable();
  editor->SetValue(newcell);

  m_selectionStart = m_selectionStart->m_next;
  DeleteSelection();
  editor->GetParent()->ResetSize();
  dynamic_cast<GroupCell*>(editor->GetParent())->ResetInputLabel();
  editor->ResetSize();
  Recalculate();
  SetActiveCell(editor, true);
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
#if defined __WXGTK__ && wxCHECK_VERSION(2,9,0)
  if (CanCopy(true)) {
    wxTheClipboard->UsePrimarySelection(true);
    if (wxTheClipboard->Open()) {
      wxTheClipboard->SetData(new wxTextDataObject(GetString()));
      wxTheClipboard->Close();
    }
    wxTheClipboard->UsePrimarySelection(false);
  }
#endif
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

GroupCell *MathCtrl::GetHCaret()
{
  if (m_hCaretActive)
    return m_hCaretPosition;

  if (m_activeCell)
    return dynamic_cast<GroupCell*>(m_activeCell->GetParent());

  return m_last;
}

/**
 * Set the HCaret to its default location, at the end of the document.
 */
void MathCtrl::SetDefaultHCaret()
{
  SetHCaret(m_last);
}

/**
 * Set the HCaret at the location of the given MathCell.
 *
 * @param where   The cell to place the cursor before.
 * @param callRefresh   Call with false when manually refreshing.
 */
void MathCtrl::SetHCaret(MathCell *where, bool callRefresh)
{
  m_selectionStart = m_selectionEnd = NULL;
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  SetActiveCell(NULL, false);
  m_hCaretPosition = dynamic_cast<GroupCell*>(where);
  m_hCaretActive = true;

  if (callRefresh) // = true default
    Refresh();
}

void MathCtrl::ShowHCaret()
{
  if (m_hCaretPosition == NULL)
  {
    if (m_workingGroup != NULL)
      m_hCaretPosition = m_workingGroup;
    else if (m_last != NULL)
      m_hCaretPosition = m_last;
    else m_hCaretPosition = NULL;
  }

  m_hCaretActive = true;
}

bool MathCtrl::CanUndo()
{
  if (m_activeCell == NULL)
    return false;
  return m_activeCell->CanUndo();
}

void MathCtrl::Undo()
{
  if (m_activeCell != NULL) {
    m_activeCell->Undo();
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
    Refresh();
  }
}

bool MathCtrl::CanRedo()
{
  if (m_activeCell == NULL)
    return false;
  return m_activeCell->CanRedo();
}

void MathCtrl::Redo()
{
  if (m_activeCell != NULL) {
    m_activeCell->Redo();
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
    Refresh();
  }
}

void MathCtrl::SaveValue()
{
  if (m_activeCell != NULL)
    m_activeCell->SaveValue();
}

void MathCtrl::RemoveAllOutput()
{
  if (m_workingGroup != NULL)
    return;

  GroupCell *tmp = m_tree;
  SetSelection(NULL); // TODO only setselection NULL when selection is in the output
  SetActiveCell(NULL);

  while (tmp != NULL)
  {
    if (tmp->GetGroupType() == GC_TYPE_CODE)
      tmp->RemoveOutput();
    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  Recalculate();
  Refresh();
}

void MathCtrl::OnMouseMiddleUp(wxMouseEvent& event)
{
#if defined __WXGTK__
  OnMouseLeftDown(event);
  m_leftDown = false;
  if (m_clickType != CLICK_TYPE_NONE)
    PasteFromClipboard(true);
  m_clickType = CLICK_TYPE_NONE;
#endif
}

void MathCtrl::CommentSelection()
{
  if (GetActiveCell())
  {
    EditorCell *active = GetActiveCell();
    active->CommentSelection();
    active->ResetSize();
    active->GetParent()->ResetSize();
    Recalculate();
  }
}

void MathCtrl::OnMouseWheel(wxMouseEvent &ev)
{
  if (m_selectionStart == NULL || m_selectionStart != m_selectionEnd ||
      m_selectionStart->GetType() != MC_TYPE_SLIDE || m_animate)
    ev.Skip();
  else
  {
    int rot = ev.GetWheelRotation();

    SlideShow *tmp = (SlideShow *)m_selectionStart;

    if (rot > 0)
      tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());
    else
      tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() - 1) % tmp->Length());

    wxRect rect = m_selectionStart->GetRect();
    CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
    RefreshRect(rect);
  }
}

wxString MathCtrl::GetInputAboveCaret()
{
  if (!m_hCaretActive || m_hCaretPosition == NULL)
    return wxEmptyString;

  MathCell *editor = m_hCaretPosition->GetEditable();

  if (editor != NULL)
    return editor->ToString(false);
  return wxEmptyString;
}

wxString MathCtrl::GetOutputAboveCaret()
{
  if (!m_hCaretActive || m_hCaretPosition == NULL)
    return wxEmptyString;

  m_hCaretPosition->SelectOutput(&m_selectionStart, &m_selectionEnd);

  wxString output = GetString();

  m_selectionStart = m_selectionEnd = NULL;

  Refresh();

  return output;
}

bool MathCtrl::FindNext(wxString str, bool down, bool ignoreCase)
{
  if (m_tree == NULL)
    return false;

  GroupCell *tmp = m_tree;
  if (!down)
    tmp = m_last;

  if (m_activeCell != NULL)
    tmp = dynamic_cast<GroupCell*>(m_activeCell->GetParent());

  else if (m_hCaretActive)
  {
    if (down)
    {
      if (m_hCaretPosition != NULL)
        tmp = dynamic_cast<GroupCell*>(m_hCaretPosition->m_next);
    }
    else
    {
      tmp = m_hCaretPosition;
    }
  }

  while (tmp != NULL)
  {
    EditorCell *editor = (EditorCell *)(tmp->GetEditable());

    if (editor != NULL)
    {
      bool found = editor->FindNext(str, down, ignoreCase);

      if (found)
      {
        ScrollToCell(editor);
        int start, end;
        editor->GetSelection(&start, &end);
        SetActiveCell(editor);
        editor->SetSelection(start, end);
        Refresh();
        return true;
      }
    }

    if (down)
      tmp = dynamic_cast<GroupCell*>(tmp->m_next);
    else
      tmp = dynamic_cast<GroupCell*>(tmp->m_previous);
  }

  return false;
}

void MathCtrl::Replace(wxString oldString, wxString newString)
{
  if (m_activeCell != NULL)
  {
    if (m_activeCell->ReplaceSelection(oldString, newString))
    {
      m_saved = false;
      dynamic_cast<GroupCell*>(m_activeCell->GetParent())->ResetInputLabel();
      RecalculateForce();
      Refresh();
    }
  }
}

int MathCtrl::ReplaceAll(wxString oldString, wxString newString)
{
  if (m_tree == NULL)
    return 0;

  int count = 0;

  GroupCell *tmp = m_tree;

  while (tmp != NULL)
  {
    EditorCell *editor = (EditorCell *)(tmp->GetEditable());

    if (editor != NULL)
    {
      int replaced = editor->ReplaceAll(oldString, newString);
      if (replaced > 0)
      {
        count += replaced;
        tmp->ResetInputLabel();
      }
      count += editor->ReplaceAll(oldString, newString);
    }

    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }

  if (count > 0)
  {
    m_saved = false;
    RecalculateForce();
    Refresh();
  }

  return count;
}

bool MathCtrl::Autocomplete(bool templates)
{
  if (m_activeCell == NULL)
    return false;

  m_autocompleteTemplates = templates;

  EditorCell *editor = (EditorCell *)m_activeCell;

  editor->SelectWordUnderCaret(false, false);

  wxString partial = editor->GetSelectionString();

  m_completions = m_autocomplete.CompleteSymbol(partial, templates);

  /// No completions - clear the selection and return false
  if (m_completions.GetCount() == 0)
  {
    editor->ClearSelection();
    return false;
  }

  /// If there is only one completion, use it
  if (m_completions.GetCount() == 1)
  {
    int start, end;
    editor->GetSelection(&start, &end);

    editor->ReplaceSelection(editor->GetSelectionString(), m_completions[0]);
    editor->ClearSelection();
    editor->CaretToPosition(start);

    if (!templates || !editor->FindNextTemplate())
      editor->CaretToPosition(start + m_completions[0].Length());

    editor->ResetSize();
    editor->GetParent()->ResetSize();
    Recalculate();

    Refresh();
  }

  /// If there are more than one completions, popup a menu
  else {

    m_completions.Sort();

    wxMenu *popup = new wxMenu();

    for (int i=0; i<m_completions.GetCount() && i<AC_MENU_LENGTH; i++)
      popup->Append(popid_complete_00 + i, m_completions[i]);

    // Find the position for the popup menu
    wxClientDC dc(this);
    CellParser parser(dc);

    wxPoint pos = editor->PositionToPoint(parser, -1);

    CalcScrolledPosition(pos.x, pos.y, &pos.x, &pos.y);

    // Show popup menu
    PopupMenu(popup, pos.x, pos.y);

    delete popup;
  }

  return true;
}

void MathCtrl::OnComplete(wxCommandEvent &event)
{
  if (m_activeCell == NULL)
    return;

  EditorCell *editor = (EditorCell *)m_activeCell;
  int caret = editor->GetCaretPosition();

  editor->ReplaceSelection(editor->GetSelectionString(),
    m_completions[event.GetId() - popid_complete_00]);

  int sel_start, sel_end;
  editor->GetSelection(&sel_start, &sel_end);

  editor->ClearSelection();

  if (m_autocompleteTemplates)
  {
    editor->CaretToPosition(caret);
    if (!editor->FindNextTemplate())
      editor->CaretToPosition(sel_start + m_completions[event.GetId() - popid_complete_00].Length());
  }

  editor->ResetSize();
  editor->GetParent()->ResetSize();
  Recalculate();

  Refresh();
}

void MathCtrl::SetActiveCellText(wxString text)
{
  EditorCell* active = (EditorCell *)m_activeCell;
  if (active != NULL)
  {
    GroupCell *parent = dynamic_cast<GroupCell*>(active->GetParent());
    if (parent->GetGroupType() == GC_TYPE_CODE &&
        parent->IsMainInput(active))
    {
      active->SaveValue();
      active->SetValue(text);
      active->ResetSize();
      active->ResetData();
      parent->ResetSize();
      parent->ResetData();
      parent->ResetInputLabel();
      Recalculate();
      Refresh();
    }
  }
  else
    OpenHCaret(text);
}

bool MathCtrl::InsertText(wxString text)
{
  if (m_workingGroup != NULL)
    return false;

  if (m_activeCell == NULL)
    OpenHCaret(text);
  else {
    m_activeCell->InsertText(text);
    Refresh();
  }

  return true;
}

void MathCtrl::OpenNextOrCreateCell()
{
  if (m_hCaretPosition)
  {
    if (m_hCaretPosition->m_next)
    {
      m_selectionStart = m_selectionEnd = m_hCaretPosition;
      ActivateNextInput();
    }
    else
      OpenHCaret();
  }
  else
    OpenHCaret();
}

BEGIN_EVENT_TABLE(MathCtrl, wxScrolledCanvas)
  EVT_MENU_RANGE(popid_complete_00, popid_complete_00 + AC_MENU_LENGTH, MathCtrl::OnComplete)
  EVT_SIZE(MathCtrl::OnSize)
  EVT_PAINT(MathCtrl::OnPaint)
  EVT_LEFT_UP(MathCtrl::OnMouseLeftUp)
  EVT_LEFT_DOWN(MathCtrl::OnMouseLeftDown)
  EVT_RIGHT_DOWN(MathCtrl::OnMouseRightDown)
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
  EVT_MIDDLE_UP(MathCtrl::OnMouseMiddleUp)
  EVT_MOUSEWHEEL(MathCtrl::OnMouseWheel)
END_EVENT_TABLE()
