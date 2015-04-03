// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//


#include "wxMaxima.h"
#include "wxMaximaFrame.h"
#include "MathCtrl.h"
#include "Bitmap.h"
#include "Setup.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "SlideShowCell.h"
#include "ImgCell.h"
#include "MarkDown.h"

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

MathCtrl::MathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size) :
wxScrolledCanvas(
  parent, id, position, size,
  wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
  | wxSUNKEN_BORDER
#endif
  )
{
  TreeUndo_ActiveCell = NULL;
  m_TreeUndoMergeSubsequentEdits = false;
  m_questionPrompt = false;
  m_answerCell = NULL;
  m_scrolledAwayFromEvaluation = false;
  m_keyboardInactive = true;
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
  AnimationRunning(false);
  m_workingGroup = NULL;
  m_saved = false;
  m_zoomFactor = 1.0; // set zoom to 100%
  m_evaluationQueue = new EvaluationQueue();
  AdjustSize();

  DisableKeyboardScrolling();

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
    m_memory->CreateScaled (sz.x, sz.y, -1, dc.GetContentScaleFactor ());
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
      dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, wxPENSTYLE_SOLID)));
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
        tmp->Draw(parser, point, MAX(fontsize, MC_MIN_SIZE));
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

GroupCell *MathCtrl::InsertGroupCells(GroupCell* cells,GroupCell* where)
{
  InsertGroupCells(cells,where,&treeUndoActions);
}
  
// InsertGroupCells
// inserts groupcells after position "where" (NULL = top of the document)
// Multiple groupcells can be inserted when tree->m_next != NULL
// Returns the pointer to the last inserted group cell to have fun with
GroupCell *MathCtrl::InsertGroupCells(
  GroupCell* cells,
  GroupCell* where,
  std::list <TreeUndoAction *> *undoBuffer
  )
{
  if (!cells)
    return NULL; // nothing to insert
  
  bool renumbersections = false; // only renumber when true
  GroupCell *next; // next gc to insertion point
  GroupCell *prev;
  
  // Find the last cell in the tree that is to be inserted
  GroupCell* lastOfCellsToInsert = cells;
  if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (lastOfCellsToInsert->m_next) {
    lastOfCellsToInsert = dynamic_cast<GroupCell*>(lastOfCellsToInsert->m_next);
    if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
  }

  if (m_tree == NULL)
    where = NULL;

  if (where)
    next = dynamic_cast<GroupCell*>(where->m_next);
  else {
    next = m_tree; // where == NULL
    m_tree = cells;
  }
  prev = where;

  cells->m_previous = cells->m_previousToDraw = where;
  lastOfCellsToInsert->m_next     = lastOfCellsToInsert->m_nextToDraw     = next;

  if (prev)
    prev->m_next     = prev->m_nextToDraw     = cells;
  if (next)
    next->m_previous = next->m_previousToDraw = lastOfCellsToInsert;
  // make sure m_last still points to the last cell of the worksheet!!
  if (!next) // if there were no further cells
    m_last = lastOfCellsToInsert;
  
  if (renumbersections)
    NumberSections();
  Recalculate();
  m_saved = false; // document has been modified
  
  if(undoBuffer)
    TreeUndo_MarkCellsAsAdded(cells,lastOfCellsToInsert,undoBuffer);

  return lastOfCellsToInsert;
}

// this goes through m_tree with m_next, to set the correct m_last
// you can call this after folding, unfolding cells to make sure
// m_last is correct
GroupCell *MathCtrl::UpdateMLast()
{
  if (!m_tree)
    m_last = NULL;
  else
  {
    m_last = m_tree;
    while (m_last->m_next)
      m_last = dynamic_cast<GroupCell*>(m_last->m_next);
  }
  
  return m_last;
}

void MathCtrl::InsertLine(MathCell *newCell, bool forceNewLine)
{
  m_saved = false;

  GroupCell *tmp = m_workingGroup;

  // If there is no working group we take the last cell
  if (tmp == NULL)
    tmp = m_last;

  // If there is no last cell either the new one is used as the last cell.
  if (tmp == NULL)
    return;

  newCell->ForceBreakLine(forceNewLine);

  tmp->AppendOutput(newCell);

  newCell->SetParentList(tmp);

  m_selectionStart = NULL;
  m_selectionEnd = NULL;

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetZoomFactor(m_zoomFactor);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);

  tmp->RecalculateAppended(parser);
  Recalculate();

  if(FollowEvaluation()) {
    if(GCContainsCurrentQuestion(tmp))
      OpenQuestionCaret();
    ScrollToCell(tmp); // also refreshes
  }
  else
    Refresh();
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
  
  // Re-calculate the table of contents
  m_structure->Update(m_tree);

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
  TreeUndo_ClearBuffers();
  DestroyTree();

  EnableEdit(true);
  m_switchDisplayCaret = true;
  AnimationRunning(false);
  m_saved = false;

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
 * We have a mouse click to the left of a GroupCel.
 */
void MathCtrl::OnMouseLeftInGcLeft(wxMouseEvent& event, GroupCell *clickedInGC)
{
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
}

/***
 * We have a mouse click in the GroupCell.
 */
void MathCtrl::OnMouseLeftInGcCell(wxMouseEvent& event, GroupCell *clickedInGC)
{
  if(GCContainsCurrentQuestion(clickedInGC)) {
    // The user clicked at the cell maxima has asked a question in.
    FollowEvaluation(true);
    OpenQuestionCaret();
    return;
  }
  else {
    // The user clicked at a ordinary cell
    EditorCell * editor = clickedInGC->GetEditable();
    if (editor != NULL) {
      wxRect rect = editor->GetRect();
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
  }
  // what if we tried to select something in output, select it (or if editor, activate it)
  if ((clickedInGC->GetOutputRect()).Contains(m_down)) {
    wxRect rect2(m_down.x, m_down.y, 1,1);
    wxPoint mmm(m_down.x + 1, m_down.y +1);
    clickedInGC->SelectRectInOutput(rect2, m_down, mmm,
                                    &m_selectionStart, &m_selectionEnd);
    if (m_selectionStart != NULL) {
      if ((m_selectionStart == m_selectionEnd) && (m_selectionStart->GetType() == MC_TYPE_INPUT)
          && GCContainsCurrentQuestion(clickedInGC))// if we clicked an editor in output - activate it if working!
      {
        SetActiveCell(dynamic_cast<EditorCell*>(m_selectionStart), false);
        wxClientDC dc(this);
        m_activeCell->SelectPointText(dc, m_down);
        m_switchDisplayCaret = false;
        m_clickType = CLICK_TYPE_INPUT_SELECTION;
        FollowEvaluation(true);    
        OpenQuestionCaret();
        Refresh();
        return;
      }
      else {
        m_clickType = CLICK_TYPE_OUTPUT_SELECTION;
        m_clickInGC = clickedInGC;
      }
    }
  }
}

void MathCtrl::OnMouseLeftInGc(wxMouseEvent& event, GroupCell *clickedInGc)
{
  // The click has changed the cell which means the user works here and
  // doesn't want the evaluation mechanism to automatically follow the
  // evaluation any more.
  ScrolledAwayFromEvaluation(true);

  if (m_down.x <= MC_GROUP_LEFT_INDENT)
    OnMouseLeftInGcLeft(event, clickedInGc);
  else
    OnMouseLeftInGcCell(event, clickedInGc);
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
  AnimationRunning(false);
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

    // The click will has changed the position that is in focus so we assume
    // the user wants to work herr and doesn't want the evaluation mechanism
    // to automatically follow the evaluation any more.
    ScrolledAwayFromEvaluation(true);
  }

  else if (clickedInGC != NULL) {
    OnMouseLeftInGc(event, clickedInGC);
  }

  else { // we clicked below last groupcell (both clickedInGC and clickedBeforeGC == NULL)
    // set hCaret (or activate last cell?)
    SetHCaret(m_last, false);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
  }

  Refresh();
  // Re-calculate the table of contents
  m_structure->Update(m_tree);
}

void MathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  AnimationRunning(false);
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
      return m_activeCell->ToString();
  }

  wxString s;
  MathCell* tmp = m_selectionStart;
  while (tmp != NULL) {
    if (lb && tmp->BreakLineHere() && s.Length() > 0)
      s += wxT("\n");
    s += tmp->ToString();
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
	   m_selectionStart->GetType() == MC_TYPE_IMAGE)
  {
    ((ImgCell *)m_selectionStart)->CopyToClipboard();
    return true;
  }
  else if (m_selectionStart == m_selectionEnd &&
           m_selectionStart->GetType() == MC_TYPE_SLIDE)
  {
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
    s += tmp->ToTeX();
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
      s += tmp->GetEditable()->ToString() + wxT("\n");
      s += wxT("/* [wxMaxima: input   end   ] */\n");
      break;
    case GC_TYPE_TEXT:
      s += wxT("/* [wxMaxima: comment start ]\n");
      s += tmp->GetEditable()->ToString() + wxT("\n");
      s += wxT("   [wxMaxima: comment end   ] */\n");
      break;
    case GC_TYPE_SECTION:
      s += wxT("/* [wxMaxima: section start ]\n");
      s += tmp->GetEditable()->ToString() + wxT("\n");
      s += wxT("   [wxMaxima: section end   ] */\n");
      break;
    case GC_TYPE_SUBSECTION:
      s += wxT("/* [wxMaxima: subsect start ]\n");
      s += tmp->GetEditable()->ToString() + wxT("\n");
      s += wxT("   [wxMaxima: subsect end   ] */\n");
      break;
    case GC_TYPE_TITLE:
      s += wxT("/* [wxMaxima: title   start ]\n");
      s += tmp->GetEditable()->ToString() + wxT("\n");
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

bool MathCtrl::CanDeleteSelection()
{
  if((m_selectionStart==NULL) || (m_selectionEnd==NULL))
    return false;
  
  return CanDeleteRegion(
    dynamic_cast<GroupCell*>(m_selectionStart->GetParent()),
    dynamic_cast<GroupCell*>(m_selectionEnd->GetParent())
    );
}

void MathCtrl::DeleteSelection()
{
  DeleteRegion(
    dynamic_cast<GroupCell*>(m_selectionStart->GetParent()),
    dynamic_cast<GroupCell*>(m_selectionEnd->GetParent())
    );
  TreeUndo_ClearRedoActionList();
}

bool MathCtrl::CanDeleteRegion(GroupCell *start, GroupCell *end)
{
  if ((start == NULL)||(end == NULL))
    return false;
  
  GroupCell *tmp = start;
  
  // We refuse deletion of a cell maxima is currently evaluating
  if(tmp == m_workingGroup)
    return false;

  // We refuse deletion of a cell we are planning to evaluate
  while (tmp != NULL)
  {
    if(m_evaluationQueue->IsInQueue(tmp))
      return false;

    if (tmp == end)
      break;

    tmp = dynamic_cast<GroupCell*>(tmp->m_next);
  }
  
  return true;
}

void MathCtrl::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end)
{
  TreeUndo_MarkCellsAsAdded(start,end,&treeUndoActions);
}

void MathCtrl::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end,std::list <TreeUndoAction *> *undoBuffer)
{
  if(m_TreeUndoMergeSubsequentEdits)
  {
    if(!m_TreeUndoMergeStartIsSet)
    {
      m_currentUndoAction.m_start = start;
      m_TreeUndoMergeStartIsSet = true;
    }
    
    if(m_currentUndoAction.m_newCellsEnd)
      wxASSERT_MSG(end->m_previous == m_currentUndoAction.m_newCellsEnd,
                   _("Bug: Trying to merge individual cell adds to a region in the undo buffer but there are other cells between them."));
    m_currentUndoAction.m_newCellsEnd = end;
  }
  else
  {
    TreeUndoAction *undoAction = new TreeUndoAction;
    undoAction->m_start = start;
    undoAction->m_newCellsEnd = end;
    undoBuffer->push_front(undoAction);
    TreeUndo_ClearRedoActionList();
  }
}

void MathCtrl::TreeUndo_ClearRedoActionList()
{
  while(!treeRedoActions.empty())
  {
    TreeUndo_DiscardAction(&treeRedoActions);
  }
}

void MathCtrl::TreeUndo_ClearBuffers()
{
  m_currentUndoAction.Clear();
  TreeUndo_ClearRedoActionList();
  while(!treeUndoActions.empty())
  {
    TreeUndo_DiscardAction(&treeUndoActions);
  }
  TreeUndo_ActiveCell = NULL;
}

void MathCtrl::TreeUndo_DiscardAction(std::list <TreeUndoAction *> *actionList)
{
  TreeUndoAction *Action = actionList->back();

  if(Action->m_oldCells)
    DestroyTree(Action->m_oldCells);
  delete Action;

  actionList->pop_back();
}

void MathCtrl::TreeUndo_CellLeft()
{
  if(m_TreeUndoMergeSubsequentEdits) return;

  GroupCell *activeCell = dynamic_cast<GroupCell*>(GetActiveCell()->m_group);

  // If no cell is active we didn't leave a cell and return from this function.
  if(activeCell==NULL)
  {
    return;
  }
  
  if(TreeUndo_ActiveCell)
    wxASSERT_MSG(TreeUndo_ActiveCell == activeCell,_("Bug: Cell left but not entered."));

  // We only can undo a text change if the text has actually changed.
  if(
    (m_currentUndoAction.m_oldText != wxEmptyString) &&
    (m_currentUndoAction.m_oldText != activeCell->GetEditable()->GetValue()) &&
    (m_currentUndoAction.m_oldText + wxT(";") != activeCell->GetEditable()->GetValue())
    )
  {
    TreeUndoAction *undoAction = new TreeUndoAction(m_currentUndoAction);
    wxASSERT_MSG(activeCell != NULL,_("Bug: Text changed, but no active cell."));    
    undoAction->m_start = activeCell;
    wxASSERT_MSG(undoAction->m_start != NULL,_("Bug: Trying to record a cell contents change without a cell."));    
    treeUndoActions.push_front(undoAction);
    m_currentUndoAction.Clear();
    TreeUndo_ClearRedoActionList();
  }
  else
  {
    m_currentUndoAction.m_oldText = wxEmptyString;
  }
}

void MathCtrl::TreeUndo_CellEntered()
{
  if(m_TreeUndoMergeSubsequentEdits) return;
  if(GetActiveCell())
  {
    TreeUndo_ActiveCell = dynamic_cast<GroupCell*>(GetActiveCell()->m_group);
    m_currentUndoAction.m_oldText = TreeUndo_ActiveCell->GetEditable()->GetValue();
  }
}

bool MathCtrl::TreeUndo_MergeSubsequentEdits(bool mergeRequest)
{
  TreeUndo_MergeSubsequentEdits(mergeRequest,&treeUndoActions);
}

bool MathCtrl::TreeUndo_MergeSubsequentEdits(bool mergeRequest,std::list <TreeUndoAction *> *undoList)
{
  wxASSERT_MSG(mergeRequest != m_TreeUndoMergeSubsequentEdits,_("Bug: Start or end of merging of subsequent editing actions was requested two times in a row."));
  
  // If we have just finished collecting data for a undo action it is time to
  // create an item for the undo buffer.
  if(mergeRequest)
  {
    m_currentUndoAction.m_start = NULL;
    m_TreeUndoMergeStartIsSet = false;
  }
  else
  {
    if(m_TreeUndoMergeStartIsSet)
    {
      TreeUndoAction *undoAction=new TreeUndoAction(m_currentUndoAction);
      undoList->push_front(undoAction);
      m_currentUndoAction.Clear();
      TreeUndo_ActiveCell = NULL;
   }
  }

  // Remember if we are currently merging undo info.
  m_TreeUndoMergeSubsequentEdits = mergeRequest;
}


void MathCtrl::DeleteRegion(
  GroupCell *start,
  GroupCell *end
  )
{
  DeleteRegion(start,end,&treeUndoActions);
}

void MathCtrl::DeleteRegion(GroupCell *start,GroupCell *end,std::list <TreeUndoAction *> *undoBuffer) {

  // Abort deletion if there is no valid selection or if we cannot
  // delete it.
  if(!CanDeleteRegion(start,end))
    return;
  
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  m_saved = false;

  SetActiveCell(NULL, false);
  m_hCaretActive = false;
  m_hCaretPosition = NULL;

  // check if chapters or sections need to be renumbered
  bool renumber = false;
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

  // If the selection ends with the last file of the file m_last has to be
  // set to the last cell that isn't deleted.
  if (end == m_last)
    m_last = dynamic_cast<GroupCell*>(start->m_previous);

  if (start == m_tree) {
    // The deleted cells include the first cell of the worksheet.
    // Unlink the selected cells from the worksheet.
    if (end->m_previous != NULL) {
      end->m_previous->m_nextToDraw = NULL;
      end->m_previous->m_next = NULL;
    }
    if (end->m_next != NULL) {
      end->m_next->m_previous = NULL;
      end->m_next->m_previousToDraw = NULL;
    }

    m_tree = dynamic_cast<GroupCell*>(end->m_next);

    // Put an end-of-list-marker to the deleted cells.
    end->m_next = NULL;
    end->m_nextToDraw = NULL;

    // Move the deleted cells into a action in the undo buffer.
    // If we have a undo buffer to put it into, that is.
    if(undoBuffer)
    {
      if(!m_TreeUndoMergeSubsequentEdits)
      {
        TreeUndoAction *undoAction = new TreeUndoAction;
        m_TreeUndoMergeStartIsSet = true;
        undoAction->m_start = NULL;
        undoAction->m_oldCells = start;
        
        undoBuffer->push_front(undoAction);
      }
      else
      {
        if(!m_TreeUndoMergeStartIsSet)
        {
          m_currentUndoAction.m_start = NULL;
          m_TreeUndoMergeStartIsSet = true;
        }
        m_currentUndoAction.m_oldCells = start;
      }
    }
    else
      // We don't want to be able to undo this => delete the cells.
      DestroyTree(start);
  }
  else {
    // The deleted cells don't include the first cell of the worksheet.

    // Move the deleted cells into a action in the undo buffer.
    // If we have a undo buffer to put it into, that is.
    if(undoBuffer)
    {
      // Unlink the to-be-deleted cells from the worksheet.
      start->m_previous->m_next = end->m_next;
      start->m_previous->m_nextToDraw = end->m_next;
      if (end->m_next != NULL) {
        end->m_next->m_previous = start->m_previous;
        end->m_next->m_previousToDraw = start->m_previous;
      }

      // Add an "end of tree" marker to the end of the list of deleted cells
      end->m_next = NULL;
      end->m_nextToDraw = NULL;

      // Now let's put the unlinked cells into an undo buffer.
      if(!m_TreeUndoMergeSubsequentEdits)
      {
        // Create a new undo action.
        TreeUndoAction *undoAction = new TreeUndoAction;
        undoAction->m_start = dynamic_cast<GroupCell*>(start->m_previous);
        undoAction->m_oldCells = start;
        undoBuffer->push_front(undoAction);
      }
      else
      {
        // Add the cells that are to be deleted to the undo action.
        if(!m_TreeUndoMergeStartIsSet)
        {
          m_currentUndoAction.m_start = dynamic_cast<GroupCell*>(start->m_previous);
          m_TreeUndoMergeStartIsSet = false;
        }
        m_currentUndoAction.m_oldCells = start;
      }

    }
    else
      // We don't want to be able to undo this => delete the cells.
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

void MathCtrl::OpenQuestionCaret(wxString txt)
{
  // We are leaving the input part of the current cell in this step.
  TreeUndo_CellLeft();
    
  // We don't need an undo action for this cell.
  TreeUndo_ActiveCell = NULL;

  // Make sure that the cell containing the question is visible
  if (m_workingGroup->RevealHidden())
    FoldOccurred();
 
  if(m_answerCell == NULL)
  {      
    m_answerCell = new EditorCell;
    m_answerCell->SetType(MC_TYPE_INPUT);
    m_answerCell->SetValue(txt);
    m_answerCell->CaretToEnd();
      
    m_workingGroup->AppendOutput(m_answerCell);
    m_answerCell->SetParent(m_workingGroup);
  }

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetZoomFactor(m_zoomFactor);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);
  int fontsize = parser.GetDefaultFontSize();
  m_workingGroup->RecalculateWidths(parser, fontsize);
  m_workingGroup->RecalculateSize(parser, fontsize);

  if(FollowEvaluation())
    SetActiveCell(m_answerCell, false);

  Refresh();
}

void MathCtrl::OpenHCaret(wxString txt, int type)
{
  // if we are inside cell maxima is currently evaluating
  // bypass normal behaviour and insert an EditorCell into
  // the output of the working group.
  if(m_workingGroup) {
     if(m_activeCell!=NULL) {
      if((m_activeCell->GetParent() == m_workingGroup)&&(m_questionPrompt)) {
	OpenQuestionCaret(txt);
	return;
      }
    }
    if(m_hCaretPosition!=NULL) {	  
      if((m_hCaretPosition == m_workingGroup->m_next)&&(m_questionPrompt)) {
	OpenQuestionCaret(txt);
	return;
      }
    }
  }
  // set m_hCaretPosition to a sensible value
  if (m_activeCell != NULL)
  {
    SetHCaret(m_activeCell->GetParent(), false);
  }
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

  // Track the activity of the keyboard. Setting the keyboard
  // to inactive again is done in wxMaxima.cpp
  m_keyboardInactiveTimer.StartOnce(10000);
  m_keyboardInactive = false;

  // Handling of the keys this class has to handle
  switch (event.GetKeyCode()) {

  case WXK_DELETE:
    if (event.ShiftDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_cut);
      GetParent()->ProcessWindowEvent(ev);
    } else if (CanDeleteSelection()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
      GetParent()->ProcessWindowEvent(ev);
    } else
      event.Skip();
    break;

  case WXK_INSERT:
    if (event.ControlDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_copy);
      GetParent()->ProcessWindowEvent(ev);
    } else if (event.ShiftDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_paste);
      GetParent()->ProcessWindowEvent(ev);
    } else
      event.Skip();
    break;

  case WXK_BACK:
    if (CanDeleteSelection()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
      GetParent()->ProcessWindowEvent(ev);
    }
    else
      event.Skip();
    break;

  case WXK_NUMPAD_ENTER:
    if (m_activeCell != NULL && m_activeCell->GetType() == MC_TYPE_INPUT)
      dynamic_cast<wxFrame*>(GetParent())->ProcessCommand(wxMaximaFrame::menu_evaluate);
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
        dynamic_cast<wxFrame*>(GetParent())->ProcessCommand(wxMaximaFrame::menu_evaluate);
      } else
        event.Skip();
    }
    break;

  case WXK_ESCAPE:
#ifndef wxUSE_UNICODE
    if (m_activeCell == NULL) {
      SetSelection(NULL);
      Refresh();
    }
    else
      SetHCaret(m_activeCell->GetParent()); // also refreshes
#else
    event.Skip();
#endif
    break;

  default:
    event.Skip();
  }
}

bool MathCtrl::GCContainsCurrentQuestion(GroupCell *cell)
{
  if (m_workingGroup)
    return ((cell == m_workingGroup) && m_questionPrompt);
  else
    return false;
}

void MathCtrl::QuestionAnswered()
{
  m_answerCell = NULL;
  m_questionPrompt = false;
  SetActiveCell(NULL);
}

/****
 * OnCharInActive is called when we have a wxKeyEvent and
 * an EditorCell is active.
 *
 * OnCharInActive sends the event to the active EditorCell
 * and then updates the window.
 */
void MathCtrl::OnCharInActive(wxKeyEvent& event) {
  bool needRecalculate = false;

  // don't exit the cell if we are making a selection
  if (event.GetKeyCode() == WXK_UP &&
      m_activeCell->CaretAtStart() &&
      !event.ShiftDown()) {
    // Re-calculate the table of contents as we possibly leave a cell that is
    // to be found here.
    m_structure->Update(m_tree);

    // If we scrolled away from the cell that is currently being evaluated
    // we need to enable the button that brings us back
    ScrolledAwayFromEvaluation();

    GroupCell *previous=dynamic_cast<GroupCell*>((m_activeCell->GetParent())->m_previous);
    if (GCContainsCurrentQuestion(previous)) {
      // The user moved into the cell maxima has asked a question in.
      FollowEvaluation(true);
      OpenQuestionCaret();
      return;
    }
    else
      SetHCaret(previous);
    return;
  }

  if (event.GetKeyCode() == WXK_DOWN &&
      m_activeCell->CaretAtEnd() &&
      !event.ShiftDown()) {
    // Re-calculate the table of contents as we possibly leave a cell that is
    // to be found here.
    m_structure->Update(m_tree);
    ScrolledAwayFromEvaluation();

    GroupCell *next=dynamic_cast<GroupCell*>(m_activeCell->GetParent());
    if (GCContainsCurrentQuestion(next)) {
      // The user moved into the cell maxima has asked a question in.
      FollowEvaluation(true);
      OpenQuestionCaret();
      return;
    }
    else
      SetHCaret(next);
    return;
  }

  // an empty cell is removed on backspace/delete
  if ((event.GetKeyCode() == WXK_BACK || event.GetKeyCode() == WXK_DELETE) &&
      m_activeCell->GetValue() == wxEmptyString) {
    m_selectionStart = m_selectionEnd = m_activeCell->GetParent();
    DeleteSelection();
    return;
  }

  ///
  /// send event to active cell
  ///
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
    m_activeCell->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE));
    m_activeCell->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE));

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

void MathCtrl::SelectWithChar(int ccode) {
  // start making a selection
  // m_hCaretPositionStart is the first group selected
  // m_hCaretPositionEnd is tle last group selected
  // we always move m_hCaretPosition
  if (m_hCaretPositionStart == NULL || m_hCaretPositionEnd == NULL) {
    if (m_hCaretPosition != NULL)
      m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPosition;
    else
      m_hCaretPositionStart = m_hCaretPositionEnd = m_tree;
    
    if (m_hCaretPositionStart == NULL)
      return;
    
    if (ccode == WXK_DOWN && m_hCaretPosition != NULL && m_hCaretPositionStart->m_next != NULL)
      m_hCaretPositionStart = m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPositionStart->m_next);
  }
  // extend/shorten selection up
  else if (ccode == WXK_UP) {
    if (m_hCaretPositionEnd->m_previous != NULL) {
      if (m_hCaretPosition != NULL && m_hCaretPosition->m_next == m_hCaretPositionEnd)
        m_hCaretPositionStart = dynamic_cast<GroupCell*>(m_hCaretPositionStart->m_previous);
      m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPositionEnd->m_previous);
    }
    if (m_hCaretPositionEnd != NULL)
      ScrollToCell(m_hCaretPositionEnd); 
  }
  // extend/shorten selection down
  else {
    if (m_hCaretPositionEnd->m_next != NULL) {
      if (m_hCaretPosition == m_hCaretPositionEnd)
        m_hCaretPositionStart = dynamic_cast<GroupCell*>(m_hCaretPositionStart->m_next);
      m_hCaretPositionEnd = dynamic_cast<GroupCell*>(m_hCaretPositionEnd->m_next);
    }
    if (m_hCaretPositionEnd != NULL)
      ScrollToCell(m_hCaretPositionEnd);
  }
  
  // m_hCaretPositionStart can be above or below m_hCaretPositionEnd
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

void MathCtrl::SelectEditable(EditorCell *editor, bool top) {
  if(editor != NULL)
  {
    wxClientDC dc(this);
    CellParser parser(dc);

    SetActiveCell(editor, false);
    m_hCaretActive = false;

    if (top)
      m_activeCell->CaretToStart();
    else
      m_activeCell->CaretToEnd();

    ShowPoint(m_activeCell->PositionToPoint(parser));
    if (editor->GetWidth() == -1)
      Recalculate();
    Refresh();
  }
  else { // can't get editor... jump over cell..
    if (top)
      m_hCaretPosition = dynamic_cast<GroupCell*>( m_hCaretPosition->m_next);
    else
      m_hCaretPosition = dynamic_cast<GroupCell*>( m_hCaretPosition->m_previous);
    Refresh();
  }
}

void MathCtrl::OnCharNoActive(wxKeyEvent& event) {
  int ccode = event.GetKeyCode();
  wxString txt; // Usually we open an Editor Cell with initial content txt

  // If Shift is down we are selecting with WXK_UP and WXK_DOWN
  if (event.ShiftDown() && (ccode == WXK_UP || ccode == WXK_DOWN)) {
    SelectWithChar(ccode);
    return;
  }

  if (m_selectionStart != NULL &&
      m_selectionStart->GetType() == MC_TYPE_SLIDE &&
      ccode == WXK_SPACE)
  {
    Animate(!AnimationRunning());
    return;
  }
    
  // Remove selection with shift+WXK_UP/WXK_DOWN
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  
  switch (ccode) {
    // These are ingored
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

  case WXK_UP:
    ScrolledAwayFromEvaluation(true);
    if (m_hCaretActive) {
      if (m_selectionStart != NULL) {
        if(event.CmdDown())
        {
          MathCell *tmp = m_selectionStart;
          if(tmp->m_previous)
          {
            do tmp = tmp->m_previous; while(
              (tmp->m_previous)&&(
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_TITLE) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SECTION) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SUBSECTION)
                )
              );
            SetHCaret(tmp);
          } else
            SelectEditable(dynamic_cast<GroupCell*>(tmp)->GetEditable(), false);
        }
        else
          SetHCaret(m_selectionStart->GetParent()->m_previous);
      }
      else if (m_hCaretPosition != NULL) {
        if(event.CmdDown())
        {
          MathCell *tmp = m_hCaretPosition;
          if(tmp->m_previous)
          {
            do tmp = tmp->m_previous; while(
              (tmp->m_previous)&&(
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_TITLE) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SECTION) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SUBSECTION)
                )
              ); 
            SetHCaret(tmp);
          }
          else
            SelectEditable(dynamic_cast<GroupCell*>(tmp)->GetEditable(), false);
        }
        else
          SelectEditable(dynamic_cast<GroupCell*>(m_hCaretPosition)->GetEditable(), false);
      }
      else
        event.Skip();
    }
    else if (m_selectionStart != NULL) 
      SetHCaret(m_selectionStart->GetParent()->m_previous);
    else if (!ActivatePrevInput())
      event.Skip();
    else
      Refresh();
    break;

  case WXK_DOWN:
    ScrolledAwayFromEvaluation(true);
    if (m_hCaretActive) {
      if (m_selectionEnd != NULL) {
        if(event.CmdDown())
        {
          MathCell *tmp = m_selectionEnd;
          if(tmp->m_next)
          {
            do tmp = tmp->m_next; while(
              (tmp->m_next)&&(
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_TITLE) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SECTION) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SUBSECTION)
                )
              );
            SetHCaret(tmp);
          } else
            SelectEditable(dynamic_cast<GroupCell*>(tmp)->GetEditable(), false);
        }
        else
          SetHCaret(m_selectionEnd->GetParent());
        
      }
      else if (m_hCaretPosition != NULL && m_hCaretPosition->m_next != NULL) {
        if(event.CmdDown())
        {
          MathCell *tmp = m_hCaretPosition;
          if(tmp->m_next)
          {
            do tmp = tmp->m_next; while(
              (tmp->m_next)&&(
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_TITLE) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SECTION) &&
                (dynamic_cast<GroupCell*>(tmp)->GetGroupType()!=GC_TYPE_SUBSECTION)
                )
              );
            SetHCaret(tmp);
          } else
            SelectEditable(dynamic_cast<GroupCell*>(tmp)->GetEditable(), false);
        }
        else
          SelectEditable(dynamic_cast<GroupCell*>(m_hCaretPosition->m_next)->GetEditable(), true);
      }
      else if (m_tree != NULL && m_hCaretPosition == NULL) {
        SelectEditable(dynamic_cast<GroupCell*>(m_tree)->GetEditable(), true);
      }

    }
    else if (m_selectionEnd != NULL)
      SetHCaret(m_selectionEnd->GetParent());
    else if (!ActivateNextInput())
      event.Skip();
    else
      Refresh();
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
#if wxUSE_UNICODE
    wxString txt(event.GetUnicodeKey());
#else
    wxString txt = wxString::Format(wxT("%c"), event.GetKeyCode());
#endif
    OpenHCaret(txt);
  }

  Refresh();
}

/*****
 * OnChar handles key events. If we have an active cell, sends the
 * event to the active cell, else moves the cursor between groups.
 */
void MathCtrl::OnChar(wxKeyEvent& event) {
#if defined __WXMSW__
  if (event.GetKeyCode() == WXK_NUMPAD_DECIMAL) {
    return;
  }
#endif

  // Skip all events that look like they might be hotkey invocations so they
  // are processed by the other receipients
  if (event.CmdDown() && !event.AltDown())
  {
    if (
      !(event.GetKeyCode() == WXK_LEFT)  &&
      !(event.GetKeyCode() == WXK_RIGHT) &&
      !(event.GetKeyCode() == WXK_UP)    &&
      !(event.GetKeyCode() == WXK_DOWN)  &&
      !(event.GetKeyCode() == WXK_BACK)
      )
    {
      event.Skip();
      return;
    }
  }

  if (m_activeCell != NULL)
    OnCharInActive(event);
  else
    OnCharNoActive(event);
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

void MathCtrl::StepAnimation(int pos)
{
  SlideShow *tmp = (SlideShow *)m_selectionStart;

  // Change the bitmap
  if(pos<0)pos = tmp->GetDisplayedIndex() + 1;
  if(pos>=tmp->Length())pos = 0;
  tmp->SetDisplayedIndex(pos);

  // Refresh the displayed bitmap
  wxRect rect = m_selectionStart->GetRect();
  CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
  RefreshRect(rect);

  // Set the slider to its new value
  m_mainToolBar->m_plotSlider->SetValue(tmp->GetDisplayedIndex());

  // Rearm the animation timer if necessary.
  if(AnimationRunning())
    m_animationTimer.StartOnce(1000/tmp->GetFrameRate());
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
    if (CanAnimate())
    {
      StepAnimation();            
    }
    else
      AnimationRunning(false);
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

  return m_tree->CopyList();
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
      return ((SlideShow *)m_selectionStart)->ToGif(file);
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
      tmp1 = tmp->Copy();
      tmp2 = tmp1;
    } else {
      tmp2->AppendCell(tmp->Copy());
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

void MathCtrl::AddLineToFile(wxTextFile& output, wxString s, bool unicode) {
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

wxString MathCtrl::EscapeHTMLChars(wxString input)
{
  input.Replace(wxT("&"), wxT("&amp;"));  
  input.Replace(wxT("<"), wxT("&lt;"));
  input.Replace(wxT(">"), wxT("&gt;"));
  input.Replace(wxT("\n"), wxT("<BR>"));

  return input;
}

wxString MathCtrl::PrependNBSP(wxString input)
{
  wxStringTokenizer stringLines(input,"\n");
  wxString line;
  wxString output=wxEmptyString;
  bool     multipleSpace;
  
  while(stringLines.HasMoreTokens())
  {
    wxString line=stringLines.GetNextToken();
      
    for (unsigned int i = 0; i < input.Length(); i++) {

      multipleSpace = false;

      while (i < input.Length() && input.GetChar(i) == ' ') {
        if(multipleSpace)
          output += wxT("&nbsp;");
        else
        {
          multipleSpace = true;
          output += wxT(" ");
        }
        i++;
      }
      output += input.GetChar(i);
    }
    if(stringLines.HasMoreTokens())output += wxT("\n");
  }
  return output;
}

//Simple iterator over a Maxima input string, skipping comments and strings
struct SimpleMathParserIterator{
  const wxString &input; //reference to input string (must be a reference, so it can be modified)
  unsigned int pos;

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
    unsigned int oldpos = pos;
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
  wxString strindex = cell->ToString().Trim(); //(%i...)
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

      wxString input = cell->ToString();
      if (prompt && cell && input.Len() > 0) {
        int outputExpressions = 0;
        int initialHiddenExpressions = 0;
        for (SimpleMathParserIterator it = input; it.isValid(); ++it) {
          switch (*it) {
          case '$': if (initialHiddenExpressions == outputExpressions) initialHiddenExpressions++; //fallthrough
          case ';': outputExpressions++;
          }
        }

        int promptIndex = getMathCellIndex(prompt);
        int outputIndex = getMathCellIndex(tmp->GetLabel()) - initialHiddenExpressions;
        int index = promptIndex;
        if (promptIndex < 0) index = outputIndex; //no input index => use output index
        else
        {
          if (outputIndex < 0 && initialHiddenExpressions < outputExpressions) {
            //input index, but no output index means the expression was evaluated, but produced no result
            // => it is invalid and should be ignored
            outputExpressions = 0;
          }
          else
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

/***
 * Export content to a HTML file.
 */
bool MathCtrl::ExportToHTML(wxString file) {
  wxString imgDir;
  wxString path, filename, ext;
  int count = 0;
  GroupCell *tmp = m_tree;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxT("/") + filename + wxT("_htmlimg");

  if (!wxDirExists(imgDir)) {
    if (!wxMkdir(imgDir))
      return false;
  }

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
      AddLineToFile(output, prompt->ToString());
      AddLineToFile(output, wxT("  </SPAN></TD>"));

      MathCell *input = tmp->GetInput();
      if (input != NULL) {
        AddLineToFile(output, wxT("  <TD><SPAN CLASS=\"input\">"));
        AddLineToFile(output, PrependNBSP(EscapeHTMLChars(input->ToString())));
        AddLineToFile(output, wxT("  </SPAN></TD>"));
      }
      AddLineToFile(output, wxT("</TR></TABLE>"));

      MathCell *out = tmp->GetLabel();

      if (out == NULL) {
        AddLineToFile(output, wxEmptyString);
      }
      else {
        if(tmp->GetOutput() != NULL && tmp->GetOutput()->GetType() == MC_TYPE_SLIDE)
        {
          ((SlideShow *)tmp->GetOutput())->ToGif(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.gif"), count));
          AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_htmlimg/") +
                        filename +
                        wxString::Format(wxT("_%d.gif\">"), count));
        }
        else
        {
          CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count), out, NULL, true);
          AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_htmlimg/") +
                        filename +
                        wxString::Format(wxT("_%d.png\">"), count));
        }
        count++;
      }
    }

    else
    {
      MarkDownHTML MarkDown;
      
      switch(tmp->GetGroupType()) {
      case GC_TYPE_TEXT:
        AddLineToFile(output, wxT("\n\n<!-- Text cell -->\n\n"));
        AddLineToFile(output, wxT("<P CLASS=\"comment\">"));
        AddLineToFile(output, PrependNBSP(MarkDown.MarkDown(EscapeHTMLChars(tmp->GetEditable()->ToString()))));
        break;
      case GC_TYPE_SECTION:
        AddLineToFile(output, wxT("\n\n<!-- Section cell -->\n\n"));
        AddLineToFile(output, wxT("<P CLASS=\"section\">"));
        AddLineToFile(output, PrependNBSP(EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString())));
        break;
      case GC_TYPE_SUBSECTION:
        AddLineToFile(output, wxT("\n\n<!-- Subsection cell -->\n\n"));
        AddLineToFile(output, wxT("<P CLASS=\"subsect\">"));
        AddLineToFile(output, PrependNBSP(EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString())));
        break;
      case GC_TYPE_TITLE:
        AddLineToFile(output, wxT("\n\n<!-- Title cell -->\n\n"));
        AddLineToFile(output, wxT("<P CLASS=\"title\">"));
        AddLineToFile(output, PrependNBSP(EscapeHTMLChars(tmp->GetEditable()->ToString())));
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
        AddLineToFile(output, PrependNBSP(EscapeHTMLChars(tmp->GetPrompt()->ToString() +
                                                          wxT(" ") +
                                                          tmp->GetEditable()->ToString())));
        AddLineToFile(output, wxT("<BR>"));
        if(tmp->GetLabel()->GetType() == MC_TYPE_SLIDE)
        {
          ((SlideShow *)tmp->GetOutput())->ToGif(imgDir + wxT("/") + filename +
                                                 wxString::Format(wxT("_%d.gif"), count));
          AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_htmlimg/") +
                        filename +
                        wxString::Format(wxT("_%d.gif\">"), count));
        }
        else
        {
          CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count),
                     out, NULL, true);
          AddLineToFile(output, wxT("  <IMG ALT=\"Result\" SRC=\"") + filename + wxT("_htmlimg/") +
                        filename +
                        wxString::Format(wxT("_%d.png\">"), count));
        }
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

/*! Export the file as TeX code
 */
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
  // Tell LaTeX how to handle a few special characters.
  AddLineToFile(output, wxT("\\DeclareUnicodeCharacter{00B5}{\\ensuremath{\\mu}}"));
  // The following line loads all code needed in order to include graphics.
  AddLineToFile(output, wxT("\\usepackage{graphicx}"));
  // We want to color the labels and text cells. The following line adds the necessary
  // logic for this to TeX.
  AddLineToFile(output, wxT("\\usepackage{color}"));
  AddLineToFile(output, wxT("\\usepackage{amsmath}"));

  // We want to shrink pictures the user has included if they are
  // higher or wider than the page.
  AddLineToFile(output, wxT("\\usepackage{ifthen}"));
  AddLineToFile(output, wxT("\\newsavebox{\\picturebox}"));
  AddLineToFile(output, wxT("\\newlength{\\pictureboxwidth}"));
  AddLineToFile(output, wxT("\\newlength{\\pictureboxheight}"));
  AddLineToFile(output, wxT("\\newcommand{\\includeimage}[1]{"));
  AddLineToFile(output, wxT("    \\savebox{\\picturebox}{\\includegraphics{#1}}"));
  AddLineToFile(output, wxT("    \\settoheight{\\pictureboxheight}{\\usebox{\\picturebox}}"));
  AddLineToFile(output, wxT("    \\settowidth{\\pictureboxwidth}{\\usebox{\\picturebox}}"));
  AddLineToFile(output, wxT("    \\ifthenelse{\\lengthtest{\\pictureboxwidth > .95\\linewidth}}"));
  AddLineToFile(output, wxT("    {"));
  AddLineToFile(output, wxT("        \\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{#1}"));
  AddLineToFile(output, wxT("    }"));
  AddLineToFile(output, wxT("    {"));
  AddLineToFile(output, wxT("        \\ifthenelse{\\lengthtest{\\pictureboxheight>.80\\textheight}}"));
  AddLineToFile(output, wxT("        {"));
  AddLineToFile(output, wxT("            \\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{#1}"));
  AddLineToFile(output, wxT("            "));
  AddLineToFile(output, wxT("        }"));
  AddLineToFile(output, wxT("        {"));
  AddLineToFile(output, wxT("            \\includegraphics{#1}"));
  AddLineToFile(output, wxT("        }"));
  AddLineToFile(output, wxT("    }"));
  AddLineToFile(output, wxT("}"));

  // Define an "abs" operator for abs commands that are long enough to be broken into
  // lines.
  AddLineToFile(output, wxT("\\DeclareMathOperator{\\abs}{abs}"));
  
  // The animate package is only needed if we actually want to output animations
  // to LaTeX. Don't drag in this dependency if this feature was disabled in the settings.
  bool AnimateLaTeX=true;
  wxConfig::Get()->Read(wxT("AnimateLaTeX"), &AnimateLaTeX);
  if(AnimateLaTeX)
  {
    AddLineToFile(output, wxT("\\usepackage{animate} % This package is required because the wxMaxima configuration option"));
    AddLineToFile(output, wxT("                      % \"Export animations to TeX\" was enabled when this file was generated."));
  }
  AddLineToFile(output, wxEmptyString);
  AddLineToFile(output, wxT("\\definecolor{labelcolor}{RGB}{100,0,0}"));
  AddLineToFile(output, wxEmptyString);

  // Add an eventual preamble requested by the user.
  wxString texPreamble;
  wxConfig::Get()->Read(wxT("texPreamble"), &texPreamble);
  if(texPreamble!=wxEmptyString)
  {
    AddLineToFile(output, texPreamble);
    AddLineToFile(output, wxEmptyString);
  }

  AddLineToFile(output, wxT("\\begin{document}"));

  //
  // Write contents
  //
  while (tmp != NULL) {
    wxString s = tmp->ToTeX(imgDir, filename, &imgCounter);
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
        wxString input = txt->ToString();

        if (fixReorderedIndices)
          for (SimpleMathParserIterator it = input; it.pos + 1 < it.input.length(); ++it)
            if (*it == '%' &&
                (input[it.pos+1] == 'i' || input[it.pos+1] == 'o') &&
                (it.pos == 0 || input[it.pos-1] != '%')){
              it.pos += 2;
              unsigned int startPos = it.pos;
              unsigned int temp = 0;
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

      wxString comment = txt->ToString();
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
  bool wxm;
  
  if (file.Right(4) == wxT(".wxm"))
    wxm = true;
  else
    wxm = false;

  
  wxTextFile backupfile(file+wxT("~"));
  if (backupfile.Exists())
  {
    if (!backupfile.Open())
      return false;
    backupfile.Clear();
  }
  else if (!backupfile.Create())
    return false;

  if (wxm) {
    AddLineToFile(backupfile, wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"), false);
    wxString version(wxT(VERSION));
    AddLineToFile(backupfile, wxT("/* [ Created with wxMaxima version ") + version + wxT(" ] */"), false);
  }

  bool fixReorderedIndices;
  wxConfig::Get()->Read(wxT("fixReorderedIndices"), &fixReorderedIndices);
  std::vector<int> cellMap;
  if (fixReorderedIndices) {
    int cellIndex = 1;
    CalculateReorderedCellIndices(m_tree, cellIndex,  cellMap);
  }
  ExportToMAC(backupfile, m_tree, wxm, cellMap, fixReorderedIndices);

  AddLineToFile(backupfile, wxEmptyString, false);
  if (wxm) {
    AddLineToFile(backupfile, wxT("/* Maxima can't load/batch files which end with a comment! */"), false);
    AddLineToFile(backupfile, wxT("\"Created with wxMaxima\"$"), false);
  }

  // Try to save the file.
  bool done=backupfile.Write(wxTextFileType_None);
  // Even if that failed we should perhaps still issue a Close() .
  if(!backupfile.Close()) return false;
  if(!done)return false;

  // If we succeeded in saving the backup file we now can overwrite the Real Thing.
  if(!wxRenameFile(file+wxT("~"),file,true)) return false;

  m_saved = true;
  return true;
}

wxString ConvertToUnicode(wxString str)
{
#ifndef wxUSE_UNICODE
  str=str.wc_str(*wxConvCurrent), wxConvUTF8;
#endif
  return str;
}

/*
  Save the data as wxmx file

  First saves the data to a backup file ending in .wxmx~ so if anything goes 
  horribly wrong in this stepp all that is lost is the data that was input 
  since the last save. Then the original .wxmx file is replaced in a 
  (hopefully) atomic operation.
*/
bool MathCtrl::ExportToWXMX(wxString file)
{
  // delete temp file if it already exists
  wxString backupfile=file+wxT("~");
  if(wxFileExists(backupfile))
  {
    if(!wxRemoveFile(backupfile))
      return false;
  }
  
  wxFFileOutputStream out(backupfile);
  if (!out.IsOk())
    return false;
  wxZipOutputStream zip(out);
  wxTextOutputStream output(zip);

  /* The first zip entry is a file named "mimetype": This makes sure that the mimetype 
     is always stored at the same position in the file. This is common practice. One 
     example from an ePub file:

     00000000  50 4b 03 04 14 00 00 08  00 00 cd bd 0a 43 6f 61  |PK...........Coa|
     00000010  ab 2c 14 00 00 00 14 00  00 00 08 00 00 00 6d 69  |.,............mi|
     00000020  6d 65 74 79 70 65 61 70  70 6c 69 63 61 74 69 6f  |metypeapplicatio|
     00000030  6e 2f 65 70 75 62 2b 7a  69 70 50 4b 03 04 14 00  |n/epub+zipPK....|

  */

  // Make sure that the mime type is stored as plain text.
  zip.SetLevel(0);
  zip.PutNextEntry(wxT("mimetype"));
  output << wxT("text/x-wxmathml");

  /* We might want to compress the rest of this file, though, if the user doesn't 
     use a version control system like git or svn:

     Compressed files tend to completely change their structure if actually only 
     a single line of the uncompressed file has been modified. This means that
     changing a line of input might lead to git or svn having to deal with 
     a file that has changes all over the place.

     If we don't use compression the increase of the file size might be small:
     - The images are saved in the png format and therefore are compressed and
     - content.xml typically is small and therefore won't get much smaller during
     compression.
  */
  bool VcFriendlyWXMX=true;
  wxConfig::Get()->Read(wxT("OptimizeForVersionControl"), &VcFriendlyWXMX);
  if(!VcFriendlyWXMX)
    zip.SetLevel(9);

  // next zip entry is "content.xml", xml of m_tree

  zip.PutNextEntry(wxT("content.xml"));
  output << wxT("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  output << wxT("\n<!--   Created by wxMaxima ") << wxT(VERSION) << wxT("   -->");
  output << wxT("\n<!--http://wxmaxima.sourceforge.net-->\n");

  // write document
  output << wxT("\n<wxMaximaDocument version=\"");
  output << DOCUMENT_VERSION_MAJOR << wxT(".");
  output << DOCUMENT_VERSION_MINOR << wxT("\" zoom=\"");
  output << int(100.0 * m_zoomFactor) << wxT("\">\n");

  // Reset image counter
  ImgCell::WXMXResetCounter();

  wxString xmlText = ConvertToUnicode(m_tree->ListToXML());
  size_t xmlLen = xmlText.Length();
  
  // Delete all but one control character from the string: there should be
  // no way for them to enter this string, anyway. But sometimes they still
  // do...
  for(size_t index = 0;index<xmlLen;index++)
  {
    wxChar c=xmlText[index];

    if(( c <  wxT('\t')) ||
       ((c >  wxT('\n')) &&(c < wxT(' '))) ||
       ( c == wxChar((char)0x7F))
      )
    {
      xmlText[index] = wxT(' '); 
    }   
  }
  
  if(m_tree!=NULL)output << xmlText;
  output << wxT("\n</wxMaximaDocument>");

  // save images from memory to zip file
  wxFileSystem *fsystem = new wxFileSystem();
  fsystem->AddHandler(new wxMemoryFSHandler);
  fsystem->ChangePathTo(wxT("memory:"), true);

  for (int i = 1; i<=ImgCell::WXMXImageCount(); i++)
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

  if(!zip.Close())
    return false;
  if (!out.Close())
    return false;
  
  // Now that all data is save we can overwrite the actual save file.
  if(!wxRenameFile(backupfile,file,true))
    return false;
  
  m_saved = true;
  return true;
}

/**!
 * CanEdit: we can edit the input if the we have the whole input in selection!
 */
bool MathCtrl::CanEdit() {
  if (m_selectionStart == NULL || m_selectionEnd != m_selectionStart
      || m_editingEnabled == false)
    return false;

  if (!m_selectionStart->IsEditable())
    return false;

  if (m_selectionStart->m_previous == NULL)
    return false;

  if (m_selectionStart->m_previous->GetType() != MC_TYPE_MAIN_PROMPT)
    return false;

  return true;
}

//! Is called on double click on a cell.
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
  // Re-calculate the table of contents
  m_structure->Update(m_tree);
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
  FollowEvaluation(true);
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
  FollowEvaluation(true);
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
  FollowEvaluation(true);
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

void MathCtrl::AddDocumentTillHereToEvaluationQueue()
{
  FollowEvaluation(true);
  GroupCell *stop;
  if(m_hCaretActive)
    stop=m_hCaretPosition;
  else
  {
    stop=dynamic_cast<GroupCell*>(GetActiveCell()->m_group);
    if(stop->m_previous!=NULL)
      stop=dynamic_cast<GroupCell*>(stop->m_previous);
  }
    
  if(stop!=NULL)
  {
    GroupCell* tmp = m_tree;
    while (tmp != NULL) {
      m_evaluationQueue->AddToQueue((GroupCell*) tmp);
      if (tmp == stop)
        break;
      tmp = dynamic_cast<GroupCell*>(tmp->m_next);
    } 
  }
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
void MathCtrl::ScrolledAwayFromEvaluation(bool ScrolledAway)
{
  if(ScrolledAway!=m_scrolledAwayFromEvaluation)
  {
    m_scrolledAwayFromEvaluation = ScrolledAway;
    if(FollowEvaluation()&&(ScrolledAway))
    {
      FollowEvaluation(false);
      m_mainToolBar->EnableTool(ToolBar::tb_follow,true);
    }
    else
      m_mainToolBar->EnableTool(ToolBar::tb_follow,false);
  }
}


void MathCtrl::FollowEvaluation(bool followEvaluation)
{
  m_followEvaluation = followEvaluation;
  if(followEvaluation)
    ScrolledAwayFromEvaluation(false);
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

void MathCtrl::Undo()
{
  if(CanUndoInsideCell())
    UndoInsideCell();
  else
  {
    if(CanTreeUndo())
      TreeUndo();
  }
}

bool MathCtrl::CanTreeUndo(){
  if(treeUndoActions.empty())
    return false;
  else
  {
    // If the next undo action will delete cells we have to look if we are allowed
    // to do this.
    if(treeUndoActions.front()->m_newCellsEnd)
      return CanDeleteRegion(
        treeUndoActions.front()->m_start,
        treeUndoActions.front()->m_newCellsEnd
        );
    else return true;
  }
}

bool MathCtrl::CanTreeRedo(){
  if(treeRedoActions.empty())
    return false;
  else
  {
    // If the next redo action will delete cells we have to look if we are allowed
    // to do this.
    if(treeRedoActions.front()->m_newCellsEnd)
      return CanDeleteRegion(
        treeRedoActions.front()->m_start,
        treeRedoActions.front()->m_newCellsEnd
        );
    else return true;
  }
}

void MathCtrl::Redo()
{
  if(CanRedoInsideCell())
    RedoInsideCell();
  else
  {
    if(CanTreeRedo())
      TreeRedo();
  }
}

bool MathCtrl::TreeUndo(std::list <TreeUndoAction *> *sourcelist,std::list <TreeUndoAction *> *undoForThisOperation)
{
  if(sourcelist->empty())
  {
    return false;
  }
  
  m_saved = false;
  
  TreeUndoAction *action=sourcelist->front();
  wxASSERT_MSG(action!=NULL,_("Trying to undo an action without starting cell."));

  // Do we have to undo a cell contents change?
  if(action->m_oldText != wxEmptyString)
  {
    wxASSERT_MSG(action->m_start!=NULL,_("Bug: Got a request to change the contents of the cell above the beginning of the worksheet."));


    if(!m_tree->Contains(action->m_start))
    {
      wxASSERT_MSG(m_tree->Contains(action->m_start),_("Bug: Undo request for cell outside worksheet."));
      return false;
    }
    
    if(action->m_start)
    {
      // If this action actually does do nothing - we have not done anything
      // and want to make another attempt on undoing things.
      if(
        (action->m_oldText == action->m_start->GetEditable()->GetValue())||
        (action->m_oldText + wxT(";") == action->m_start->GetEditable()->GetValue())
        )
      {
        sourcelist->pop_front();
        return TreeUndo(sourcelist,undoForThisOperation);
      }

      // Document the old state of this cell so the next action can be undone.
      TreeUndoAction *undoAction = new TreeUndoAction;
      undoAction->m_start = action->m_start;
      undoAction->m_oldText = action->m_start->GetEditable()->GetValue();
      undoForThisOperation->push_front(undoAction);
      
      // Revert the old cell state
      action->m_start->GetEditable()->SetValue(action->m_oldText);
      
      // Make sure that the cell we have to work on is in the visible part of the tree.
      if (action->m_start->RevealHidden())
        FoldOccurred();
      
      ScrollToCell(action->m_start);

      sourcelist->pop_front();

      wxASSERT_MSG(action->m_newCellsEnd==NULL,_("Bug: Got a request to first change the contents of a cell and to then undelete it."));
      wxASSERT_MSG(action->m_oldCells==NULL,_("Bug: Undo action with both cell contents change and cell addition."));
      return true;
    }
  }

  // We have to change the structure of the tree.

  // If the starting cell for this action isn't the beginning of an potentially
  // empty worksheet we make this cell visible.
  if(action->m_start)
  {
    // Make sure that the cell we have to work on is in the visible part of the tree.
    if (action->m_start->RevealHidden())
      FoldOccurred();    
  }
  
  wxASSERT_MSG(
    (action->m_newCellsEnd)||(action->m_oldCells),
    _("Trying to undo something but the undo action is empty.")
    );

  // We might add cells to the tree and delete other cells, but want this to be
  // a single undo action.
  TreeUndo_MergeSubsequentEdits(true,undoForThisOperation);

    
    GroupCell *parentOfInsert=action->m_start;
  // un-add new cells
  if(action->m_newCellsEnd)
  {
    wxASSERT_MSG(action->m_start!=NULL,_("Bug: Got a request to delete the cell above the beginning of the worksheet."));
    if(action->m_start)
    {
      if(!m_tree->Contains(action->m_start))
      {
        wxASSERT_MSG(m_tree->Contains(action->m_start),_("Bug: Undo request for cell outside worksheet."));
        return false;
      }

      // If we delete the start cell of this undo action we need to set a pointer
      // that tells where to add cells later if this request  is part of the 
      // current undo action, too.
      parentOfInsert=dynamic_cast<GroupCell*>(action->m_start->GetParent());
      
      // We make the cell we want to end the deletion with visible.
      if(action->m_newCellsEnd->RevealHidden())
        FoldOccurred();

      wxASSERT_MSG(CanDeleteRegion(action->m_start,action->m_newCellsEnd),_("Got a request to undo an action that involves an delete which isn't possible at this moment."));
      DeleteRegion(action->m_start,action->m_newCellsEnd,undoForThisOperation);
    }
  }
  
  // Add cells we want to undo a delete for.
  if(action->m_oldCells)
  {
    if(parentOfInsert)
      if(!parentOfInsert->Contains(action->m_start))
      {
        wxASSERT_MSG(parentOfInsert->Contains(action->m_start),_("Bug: Undo request for cell outside worksheet."));
        return false;
      }
 
    InsertGroupCells(action->m_oldCells,parentOfInsert,undoForThisOperation);
  }
  TreeUndo_MergeSubsequentEdits(false,undoForThisOperation);
    
  sourcelist->pop_front();

  Recalculate(true);
  Refresh();

  return true;
}

/*! Mark a editor cell as the active one
  
 */
void MathCtrl::SetActiveCell(EditorCell *cell, bool callRefresh) {
  if (m_activeCell != NULL)
  {
    TreeUndo_CellLeft();
    m_activeCell->ActivateCell();
  }
  
  m_activeCell = cell;
  TreeUndo_CellEntered();

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
    m_hCaretActive = false; // we have activated a cell .. disable caret

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

bool MathCtrl::CutToClipboard()
{
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
void MathCtrl::PasteFromClipboard(bool primary)
{
  // Collect all changes of this paste action
  TreeUndo_MergeSubsequentEdits(true);
  
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

  // Tell the undo functionality that the current paste action has finished now.
  TreeUndo_MergeSubsequentEdits(false);
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
#if defined __WXGTK__
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

//! Is this cell selected?
bool MathCtrl::IsSelected(int type)
{
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

//! Starts playing the animation of a cell generated with the with_slider_* commands
void MathCtrl::Animate(bool run)
{
  if (CanAnimate()) {
    if (run) {
      SlideShow *tmp = (SlideShow *)m_selectionStart;
      AnimationRunning(true);
      m_animationTimer.StartOnce(1000/tmp->GetFrameRate());
      StepAnimation();
 
      // I suspect that on WXMSW changing the slider from the program's side
      // generates a "slider changed" event - which is nearlly indistinguishable
      // from a manual slider change that is supposed to stop the animation =>
      // disallow manual slider changes and the problem disappears.
      #ifdef __WXMSW__
      m_mainToolBar->m_plotSlider->Enable(false);
      #endif
    }
    else
    {
      AnimationRunning(false);
      m_animationTimer.Stop();
      #ifdef __WXMSW__
      m_mainToolBar->m_plotSlider->Enable(true);
      #endif
    }
  }
  else
  {
    AnimationRunning(false);
    m_animationTimer.Stop();
     #ifdef __WXMSW__
     m_mainToolBar->m_plotSlider->Enable(true);
     #endif
  }
}

void MathCtrl::SetWorkingGroup(GroupCell *group)
 {
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
    if (m_last != NULL)
      m_hCaretPosition = m_last;
    else m_hCaretPosition = NULL;
  }

  m_hCaretActive = true;
}

bool MathCtrl::CanUndoInsideCell()
{
  if (m_activeCell == NULL)
    return false;
  return m_activeCell->CanUndo();
}

void MathCtrl::UndoInsideCell()
{
  if (m_activeCell != NULL) {
    m_activeCell->Undo();
    m_activeCell->GetParent()->ResetSize();
    Recalculate();
    Refresh();
  }
}

bool MathCtrl::CanRedoInsideCell()
{
  if (m_activeCell == NULL)
    return false;
  return m_activeCell->CanRedo();
}

void MathCtrl::RedoInsideCell()
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
  // We don't want to remove all output if maxima is currently evaluating.
  if (m_workingGroup != NULL)
    return;

  SetSelection(NULL); // TODO only setselection NULL when selection is in the output
  SetActiveCell(NULL);

  RemoveAllOutput(m_tree);

  Recalculate();
  Refresh();
}

void MathCtrl::RemoveAllOutput(GroupCell *tree)
{
  if (tree == NULL)
    tree = m_tree;

  while (tree != NULL)
  {
    if (tree->GetGroupType() == GC_TYPE_CODE)
      tree->RemoveOutput();
    GroupCell *sub = tree->GetHiddenTree();
    if (sub != NULL)
      RemoveAllOutput(sub);
    tree = dynamic_cast<GroupCell*>(tree->m_next);
  }
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

void MathCtrl::OnScrollChanged(wxScrollEvent &ev)
{
  // Did we scroll away from the cell that is being currently evaluated?
  // If yes we want to no more follow the evaluation with the scroll and
  // want to enable the button that brings us back.
  ScrolledAwayFromEvaluation();

  // We don't want to start the autosave while the user is scrolling through
  // the document since this will shortly halt the scroll
  m_keyboardInactiveTimer.StartOnce(10000);
  m_keyboardInactive = false;
  ev.Skip();
}

void MathCtrl::OnMouseWheel(wxMouseEvent &ev)
{
  ScrolledAwayFromEvaluation(true);
  
  m_keyboardInactiveTimer.StartOnce(10000);
  m_keyboardInactive = false;

  if ((!CanAnimate()) || AnimationRunning())
    ev.Skip();
  else
  {
    int rot = ev.GetWheelRotation();  
    SlideShow *tmp = (SlideShow *)m_selectionStart;
    StepAnimation(tmp->GetDisplayedIndex() + 1);
  }
}

wxString MathCtrl::GetInputAboveCaret()
{
  if (!m_hCaretActive || m_hCaretPosition == NULL)
    return wxEmptyString;

  MathCell *editor = m_hCaretPosition->GetEditable();

  if (editor != NULL)
    return editor->ToString();
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

    for (unsigned int i=0; i<m_completions.GetCount() && i<AC_MENU_LENGTH; i++)
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
  if (GCContainsCurrentQuestion(dynamic_cast<GroupCell*>(m_activeCell->GetParent()))) {
    m_followEvaluation = true;
    OpenQuestionCaret(text);
  }
  else if (m_activeCell == NULL)
    OpenHCaret(text);
  else {
    m_activeCell->InsertText(text);
    Refresh();
  }
  return true;
}

void MathCtrl::OpenNextOrCreateCell()
{
  if (m_hCaretPosition && m_hCaretPosition->m_next) {
    m_selectionStart = m_selectionEnd = m_hCaretPosition;
    ActivateNextInput();
  }
  else
    OpenHCaret();
}

void MathCtrl::SelectGroupCell(GroupCell *cell)
{
  m_selectionStart = m_selectionEnd = cell;
  m_hCaretActive = false;
  m_activeCell = NULL;
  if(GCContainsCurrentQuestion(cell))
  {
    FollowEvaluation(true);
    OpenQuestionCaret();
  }
}

void MathCtrl::OnFollow()
{
  if(m_workingGroup)
  {
    m_hCaretActive = true;
    if (m_workingGroup->RevealHidden()) {
      FoldOccurred();
      Recalculate(true);
    }
    SetSelection(GetWorkingGroup());
    ScrollToCell(GetWorkingGroup());
    FollowEvaluation(true);
    if(GCContainsCurrentQuestion(GetWorkingGroup()))
      OpenQuestionCaret();
  }
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
  EVT_SCROLL_CHANGED(MathCtrl::OnScrollChanged)
END_EVENT_TABLE()
