// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Worksheet

  Worksheet represents the worksheet.
 */


#include "wxMaxima.h"
#include "ErrorRedirector.h"
#include "MaxSizeChooser.h"
#include "SVGout.h"
#include "EMFout.h"
#include <wx/richtext/richtextbuffer.h>
#include <wx/tooltip.h>
#include <wx/dcbuffer.h>
#include "wxMaximaFrame.h"
#include "Worksheet.h"
#include "BitmapOut.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "SlideShowCell.h"
#include "ImgCell.h"
#include "MarkDown.h"
#include "ConfigDialogue.h"

#include <wx/clipbrd.h>
#include <wx/caret.h>
#include <wx/config.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include <wx/tokenzr.h>
#include <wx/xml/xml.h>
#include <wx/mstream.h>
#include <wx/dcgraph.h>
#include <wx/fileconf.h>
#include <wx/uri.h>

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <stdlib.h>

//! This class represents the worksheet shown in the middle of the wxMaxima window.
Worksheet::Worksheet(wxWindow *parent, int id, wxPoint position, wxSize size) :
  wxScrolled<wxWindow>(
    parent, id, position, size,
    wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
    | wxSUNKEN_BORDER
#endif
    ),m_cellPointers(this)
{
  #ifdef __WXGTK__
  wxString gtk_input_method;
  if(wxGetEnv(wxT("GTK_IM_MODULE"),&gtk_input_method))
  {
    if(gtk_input_method==wxT("xim"))
      wxLogMessage(_("Warning: GTK_IM_MODULE is set to \"xim\" which means that some hotkeys and non-ASCII characters might not work."));
  }
  #endif
  SetMinClientSize(wxSize(100,100));
  // This is somehow needed for wxAutoBufferedPaintDC
  SetBackgroundStyle(wxBG_STYLE_PAINT);
  SetBackgroundColour(*wxWHITE);
  m_virtualWidth_Last = -1;
  m_virtualHeight_Last = -1;
 
#if wxUSE_ACCESSIBILITY
  m_accessibilityInfo = NULL;
#endif
#if wxCHECK_VERSION(3,1,1)
  EnableTouchEvents(wxTOUCH_ZOOM_GESTURE);
#endif
  m_zoomAtGestureStart = 1.0;
  m_scrollToTopOfCell = false;
  m_pointer_x = -1;
  m_pointer_y = -1;
  m_recalculateStart = NULL;
  m_mouseMotionWas = false;
  m_rectToRefresh = wxRect(-1,-1,-1,-1);
  m_notificationMessage = NULL;
  m_configuration = &m_configurationTopInstance;
  m_configuration->SetBackgroundBrush(
    *(wxTheBrushList->FindOrCreateBrush(*wxWHITE, wxBRUSHSTYLE_SOLID)));

  m_dc = new wxClientDC(this);
  m_configuration->SetContext(*m_dc);
  m_autocomplete  = new AutoComplete(m_configuration);
  m_configuration->SetWorkSheet(this);
  m_configuration->ReadConfig();
  m_redrawStart = NULL;
  m_redrawRequested = false;
  m_autocompletePopup = NULL;

  m_wxmFormat = wxDataFormat(wxT("text/x-wxmaxima-batch"));
  m_mathmlFormat = wxDataFormat(wxT("MathML"));
  m_mathmlFormat2 = wxDataFormat(wxT("application/mathml-presentation+xml"));
  m_rtfFormat = wxDataFormat(wxT("application/rtf"));
  m_rtfFormat2 = wxDataFormat(wxT("text/rtf"));
  m_hCaretBlinkVisible = true;
  m_hasFocus = true;
  m_windowActive = true;
  m_lastTop = 0;
  m_lastBottom = 0;
  m_followEvaluation = true;
  TreeUndo_ActiveCell = NULL;
  m_questionPrompt = false;
  m_scheduleUpdateToc = false;
  m_scrolledAwayFromEvaluation = false;
  m_tree = NULL;
  m_mainToolBar = NULL;
  m_clickType = CLICK_TYPE_NONE;
  m_clickInGC = NULL;
  m_last = NULL;
  m_hCaretActive = true;
  m_hCaretPosition = NULL; // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_leftDown = false;
  m_mouseDrag = false;
  m_mouseOutside = false;
  m_blinkDisplayCaret = true;
  m_timer.SetOwner(this, TIMER_ID);
  m_caretTimer.SetOwner(this, CARET_TIMER_ID);
  m_saved = false;
  AdjustSize();
  m_autocompleteTemplates = false;

  int blinktime = wxCaret::GetBlinkTime();
  if (blinktime < 200)
    blinktime = 200;
  m_caretTimer.Start(blinktime);

  DisableKeyboardScrolling();

  // hack to workaround problems in RtL locales, https://bugzilla.redhat.com/show_bug.cgi?id=455863
  SetLayoutDirection(wxLayout_LeftToRight);

  // If the following option is missing a size change might cause the scrollbar
  // to be shown causing a size change causing a relayout causing the scrollbar
  // to disappear causing a size change... ...which might be an endless loop.
  ShowScrollbars(wxSHOW_SB_ALWAYS, wxSHOW_SB_ALWAYS);
  ClearDocument();
  #if wxUSE_ACCESSIBILITY
  m_accessibilityInfo = new AccessibilityInfo(GetTargetWindow(),this);
  #endif

  #if wxCHECK_VERSION(3,1,1)
  Connect(wxEVT_GESTURE_ZOOM,
          wxZoomGestureEventHandler(Worksheet::OnZoom),
          NULL, this);
  #endif
}

wxSize Worksheet::DoGetBestClientSize() const
{
  wxSize size(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )*.6,
              wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )*.6);
  if (size.x<800) size.x=800;
  if (size.y<600) size.y=600;
  return size;
}

bool Worksheet::RedrawIfRequested()
{
  bool redrawIssued = false;

  RecalculateIfNeeded();

  if(m_mouseMotionWas)
  {
    if ((m_cellPointers.m_groupCellUnderPointer == NULL) ||
        (m_pointer_y < m_cellPointers.m_groupCellUnderPointer->GetRect().GetTop()) ||
        (m_pointer_y > m_cellPointers.m_groupCellUnderPointer->GetRect().GetBottom())
      )
    {
      GroupCell *oldGroupCellUnderPointer = dynamic_cast<GroupCell *>(m_cellPointers.m_groupCellUnderPointer);

      // find out which group cell lies under the pointer
      GroupCell *tmp = m_tree;
      wxRect rect;

      while (tmp != NULL)
      {
        rect = tmp->GetRect();
        if (m_pointer_y <= rect.GetBottom())
          break;
        tmp = dynamic_cast<GroupCell *>(tmp->m_next);
      }
      if (m_tree)
        m_tree->CellUnderPointer(tmp);

      if ((m_configuration->HideBrackets()) && (oldGroupCellUnderPointer != m_cellPointers.m_groupCellUnderPointer))
      {
        if(oldGroupCellUnderPointer != NULL)
        {
          RequestRedraw(
            wxRect(
              0,
              oldGroupCellUnderPointer->GetRect().GetTop(),
              m_configuration->GetIndent() - 1,
              oldGroupCellUnderPointer->GetRect().GetBottom()
              )
            );
        }
        if(m_cellPointers.m_groupCellUnderPointer != NULL)
        {
          RequestRedraw(
            wxRect(
              0,
              m_cellPointers.m_groupCellUnderPointer->GetRect().GetTop(),
              m_configuration->GetIndent() -1,
              m_cellPointers.m_groupCellUnderPointer->GetRect().GetBottom()
              )
            );
        }
      }
    }

    if (m_cellPointers.m_groupCellUnderPointer != NULL)
    {
      if ((dynamic_cast<GroupCell *>(m_cellPointers.m_groupCellUnderPointer)->GetOutputRect()).Contains(wxPoint(m_pointer_x,m_pointer_y)))
      {
        m_cellPointers.m_cellUnderPointer = NULL;
        wxString toolTip = dynamic_cast<GroupCell *>(m_cellPointers.m_groupCellUnderPointer)->GetToolTip(wxPoint(m_pointer_x,m_pointer_y));

        if(toolTip != wxEmptyString)
        {
          if(GetToolTip() != NULL)
          {
            if(toolTip != GetToolTip()->GetTip())
            {
              // Disabling and re-enabling tooltips resets the tooltip poput delay timer.
              wxToolTip::Enable(false);
              wxToolTip::Enable(true);
              SetToolTip(toolTip);
            }
          }
          else
              SetToolTip(toolTip);
        }
        else
          UnsetToolTip();
      }
      else
        UnsetToolTip();
    }
    else
    {
      if(m_cellPointers.m_cellUnderPointer != NULL)
      {
        UnsetToolTip();
        m_cellPointers.m_cellUnderPointer = NULL;
      }
    }
    m_mouseMotionWas = false;
    redrawIssued = true;
  }
  if (m_redrawRequested)
  {
    Refresh();
    m_redrawRequested = false;
    m_redrawStart = NULL;
    redrawIssued = true;
    m_rectToRefresh = wxRect(-1, -1, -1, -1);
  }
  else
  {
    if(m_rectToRefresh.GetLeft()>=0)
    {
      CalcScrolledPosition(m_rectToRefresh.x, m_rectToRefresh.y, &m_rectToRefresh.x, &m_rectToRefresh.y);
      RefreshRect(m_rectToRefresh);
      redrawIssued = true;
    }
  }
  m_rectToRefresh = wxRect(-1, -1, -1, -1);

  return redrawIssued;
}

void Worksheet::RequestRedraw(GroupCell *start)
{
  m_redrawRequested = true;

  if (start == 0)
    m_redrawStart = m_tree;
  else
  {
    if (m_redrawStart != NULL)
    {
      // No need to waste time avoiding to waste time in a refresh when we don't
      // know our cell's position.
      if ((start->GetCurrentPoint().y < 0) || (m_redrawStart->GetCurrentPoint().y < 0))
      {
        m_redrawStart = m_tree;
      }
      else if (start->GetCurrentPoint().y < m_redrawStart->GetCurrentPoint().y)
        m_redrawStart = start;
    }
    else
      m_redrawStart = start;
  }

  // Make sure there is a timeout for the redraw
  if (!m_caretTimer.IsRunning())
  {
    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.Start(blinktime);
  }
}

Worksheet::~Worksheet()
{
  if(wxConfig::Get() != NULL)
    wxConfig::Get()->Flush();
  if (HasCapture())
    ReleaseMouse();

  m_mainToolBar = NULL;

  ClearDocument();

  m_configuration = NULL;
  m_dc = NULL;
}

#if wxCHECK_VERSION(3, 1, 2)
#define WORKING_DC_CLEAR 1
#else
#ifndef __WXGTK3__
#define WORKING_DC_CLEAR 1
#endif
#endif

#if wxCHECK_VERSION(3, 1, 2)
#define DC_ALREADY_SCROLLED 1
#else
#ifdef __WXGTK3__
#else
#endif
#endif

#define WORKING_DC_CLEAR 1

#ifdef __WXGTK__
#if wxCHECK_VERSION(3, 1, 0)
#else
#define ANTIALIASSING_DC_NOT_CORRECTLY_SCROLLED 1
#endif
#endif    

#define WORKING_AUTO_BUFFER 1

void Worksheet::OnPaint(wxPaintEvent &WXUNUSED(event))
{    
  wxAutoBufferedPaintDC dc(this);
  if(!dc.IsOk())
    return;

  // Some drawing contents 
  #ifndef DC_ALREADY_SCROLLED
  PrepareDC(dc);
  #endif

  #if wxUSE_ACCESSIBILITY
  if(m_accessibilityInfo != NULL)
    m_accessibilityInfo->NotifyEvent(0, this, wxOBJID_CLIENT, wxOBJID_CLIENT);
  #endif
  // Don't attempt to refresh the screen while we are trying to output the
  // worksheet on paper, on a bitmap or similar.
  if ((!m_configuration->ClipToDrawRegion()) || (m_configuration->GetPrinting()))
  {
    wxLogMessage(_("Suppressing a redraw during printing/export"));
    RequestRedraw();
    return;
  }

  // Don't attempt to draw in a window of the size 0.
  if( (GetClientSize().x < 1) || (GetClientSize().y < 1))
    return;

  // Inform all cells how wide our display is
  m_configuration->SetCanvasSize(GetClientSize());

  // Prepare data
  wxRect rect = GetUpdateRegion().GetBox();
  wxSize sz = GetSize();
  int xstart, xend, top, bottom;
  CalcUnscrolledPosition(rect.GetLeft(), rect.GetTop(), &xstart, &top);
  CalcUnscrolledPosition(rect.GetRight(), rect.GetBottom(), &xend, &bottom);
  wxRect updateRegion;
  updateRegion.SetLeft(xstart);
  updateRegion.SetRight(xend);
  updateRegion.SetTop(top);
  updateRegion.SetBottom(bottom);
  m_configuration->SetUpdateRegion(updateRegion);

  // Don't draw into a window of the size 0.
  if ((sz.x < 1) || (sz.y < 1))
    return;
  
#ifdef WORKING_AUTO_BUFFER
  m_configuration->SetContext(dc);

  // We might be triggered after someone changed the worksheet and before the idle
  // loop caused it to be recalculated => Ensure all sizes and positions to be known
  // before we proceed.
  RecalculateIfNeeded();

  // Create a graphics context that supports antialiassing, but on MSW
  // only supports fonts that come in the Right Format.
  wxGCDC antiAliassingDC(dc);
  #else
  wxMemoryDC dcm;
  // Test if m_memory is NULL or of the wrong size
  #ifdef __WXMAC__
  if ((!m_memory.IsOk()) || (m_memory.GetSize() != sz))
    m_memory = wxBitmap(sz*wxWindow::GetContentScaleFactor(),
                        wxBITMAP_SCREEN_DEPTH,
                        wxWindow::GetContentScaleFactor());
  #else
  if ((!m_memory.IsOk()) || (m_memory.GetSize() != sz))
    m_memory = wxBitmap(sz*wxWindow::GetContentScaleFactor(), wxBITMAP_SCREEN_DEPTH);
  #endif
  if(!m_memory.IsOk())
  {
    m_configuration->SetContext(*m_dc);
    return;
  }
  dcm.SetUserScale(wxWindow::GetContentScaleFactor(),wxWindow::GetContentScaleFactor());
  dcm.SelectObject(m_memory);
  if(!dcm.IsOk())
  {
    m_configuration->SetContext(*m_dc);
    return;
  }
  DoPrepareDC(dcm);
  m_configuration->SetContext(dcm);
  // Create a graphics context that supports antialiassing, but on MSW
  // only supports fonts that come in the Right Format.
  wxGCDC antiAliassingDC(dcm);
  #endif

  if(antiAliassingDC.IsOk())
  {
#ifdef ANTIALIASSING_DC_NOT_CORRECTLY_SCROLLED
    PrepareDC(antiAliassingDC);
#endif    
    m_configuration->SetAntialiassingDC(antiAliassingDC);
  }
  

  SetBackgroundColour(m_configuration->DefaultBackgroundColor());

  // Don't fill the text background with the background color
  m_configuration->GetDC()->SetMapMode(wxMM_TEXT);
  m_configuration->GetDC()->SetBackgroundMode(wxTRANSPARENT);
  m_configuration->GetDC()->SetBackground(m_configuration->GetBackgroundBrush());
  m_configuration->GetDC()->SetBrush(m_configuration->GetBackgroundBrush());
  m_configuration->GetDC()->SetPen(*wxTRANSPARENT_PEN);
  m_configuration->GetDC()->SetLogicalFunction(wxCOPY);

  // Clear the drawing area
#if WORKING_DC_CLEAR
  m_configuration->GetDC()->Clear();
#else
  m_configuration->GetDC()->DrawRectangle(updateRegion);
#endif

  //
  // Draw the horizontal caret
  //
  if ((m_hCaretActive) &&
      (m_hCaretPositionStart == NULL) &&
      (m_hCaretBlinkVisible) &&
      (m_hasFocus) &&
      (m_hCaretPosition != NULL))
  {
    m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_CURSOR), 1, wxPENSTYLE_SOLID)));
    m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_CURSOR), wxBRUSHSTYLE_SOLID)));
    
    wxRect currentGCRect = m_hCaretPosition->GetRect();
    int caretY = ((int) m_configuration->GetGroupSkip()) / 2 + currentGCRect.GetBottom() + 1;
    m_configuration->GetDC()->DrawRectangle(xstart + m_configuration->GetBaseIndent(),
                     caretY - m_configuration->GetCursorWidth() / 2,
                     MC_HCARET_WIDTH, m_configuration->GetCursorWidth());
  }
  
  if ((m_hCaretActive) && (m_hCaretPositionStart == NULL) && (m_hasFocus) && (m_hCaretPosition == NULL))
  {
    if (!m_hCaretBlinkVisible)
    {
      m_configuration->GetDC()->SetBrush(m_configuration->GetBackgroundBrush());
      m_configuration->GetDC()->SetPen(*wxThePenList->FindOrCreatePen(GetBackgroundColour(), m_configuration->Scale_Px(1)));
    }
    else
    {
      m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_CURSOR), m_configuration->Scale_Px(1), wxPENSTYLE_SOLID)));
      m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_CURSOR), wxBRUSHSTYLE_SOLID)));
    }
    
    wxRect cursor = wxRect(xstart + m_configuration->GetCellBracketWidth(),
                           (m_configuration->GetBaseIndent() - m_configuration->GetCursorWidth()) / 2,
                           MC_HCARET_WIDTH, m_configuration->GetCursorWidth());
    m_configuration->GetDC()->DrawRectangle(cursor);
  }
  
  if (m_tree == NULL)
  {
    m_configuration->SetContext(*m_dc);
    return;
  }
  
  //
  // Draw the selection marks
  //
  if (CellsSelected())
  {
    Cell *tmp = m_cellPointers.m_selectionStart;
    m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_SELECTION), 1, wxPENSTYLE_SOLID)));
    m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_SELECTION))));
    
    // Draw the marker that tells us which output cells are selected -
    // if output cells are selected, that is.
    if (m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP)
    {  // We have a selection of output
      while (tmp != NULL)
      {
        if (!tmp->m_isBrokenIntoLines && !tmp->m_isHidden && GetActiveCell() != tmp)
          tmp->DrawBoundingBox(dc, false);
        if (tmp == m_cellPointers.m_selectionEnd)
          break;
        tmp = tmp->m_nextToDraw;
      } // end while (1)
    }
  }
  
  //
  // Draw the cell contents
  //
  wxPoint point;
  point.x = m_configuration->GetIndent();
  point.y = m_configuration->GetBaseIndent() + m_tree->GetMaxCenter();
  // Draw tree
  GroupCell *tmp = m_tree;
  
  m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_DEFAULT), 1, wxPENSTYLE_SOLID)));
  m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_DEFAULT))));
  
  bool recalculateNecessaryWas = false;
  
  while (tmp != NULL)
  {
    if(
      (tmp->GetCurrentPoint().x < 0) ||
      (tmp->GetCurrentPoint().y < 0) ||
      (tmp->GetRect().GetWidth() < 0) ||
      (tmp->GetRect().GetHeight() < 0)
      )
    {
      tmp->Recalculate();
      recalculateNecessaryWas = true;
    }
    
    wxRect cellRect = tmp->GetRect();
    
    int width;
    int height;
    GetClientSize(&width, &height);
    
    wxPoint upperLeftScreenCorner;
    CalcScrolledPosition(0, 0,
                         &upperLeftScreenCorner.x, &upperLeftScreenCorner.y);
    (m_configuration)->SetVisibleRegion(wxRect(upperLeftScreenCorner,
                                               upperLeftScreenCorner + wxPoint(width,height)));
    (m_configuration)->SetWorksheetPosition(GetPosition());
    // Clear the image cache of all cells above or below the viewport.
    if ((cellRect.GetTop() >= bottom) || (cellRect.GetBottom() <= top))
    {
      // Only actually clear the image cache if there is a screen's height between
      // us and the image's position: Else the chance is too high that we will
      // very soon have to generated a scaled image again.
      if ((cellRect.GetBottom() <= m_lastBottom - height) || (cellRect.GetTop() >= m_lastTop + height))
      {
        if (tmp->GetOutput())
          tmp->GetOutput()->ClearCacheList();
      }
    }
    
    tmp->SetCurrentPoint(point);
    if (tmp->DrawThisCell(point))
    {
      tmp->InEvaluationQueue(m_evaluationQueue.IsInQueue(tmp));
      tmp->LastInEvaluationQueue(m_evaluationQueue.GetCell() == tmp);
    }
    tmp->Draw(point);
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    if (tmp != NULL)
    {
      tmp->UpdateYPosition();
      point = tmp->GetCurrentPoint();
    }
  }
  
  if(recalculateNecessaryWas)
    wxLogMessage(_("Cell wasn't recalculated on draw!"));
  
  #ifndef WORKING_AUTO_BUFFER
  // Blit the memory image to the window
  dcm.SetDeviceOrigin(0, 0);
  dc.Blit(0, rect.GetTop(), sz.x, rect.GetBottom() - rect.GetTop() + 1, &dcm,
          0, rect.GetTop());
  #endif
  
  m_configuration->SetContext(*m_dc);
  m_configuration->UnsetAntialiassingDC();
  m_lastTop = top;
  m_lastBottom = bottom;
}

GroupCell *Worksheet::InsertGroupCells(GroupCell *cells, GroupCell *where)
{
  return InsertGroupCells(cells, where, &treeUndoActions);
}

// InsertGroupCells
// inserts groupcells after position "where" (NULL = top of the document)
// Multiple groupcells can be inserted when tree->m_next != NULL
// Returns the pointer to the last inserted group cell to have fun with
GroupCell *Worksheet::InsertGroupCells(
        GroupCell *cells,
        GroupCell *where,
        std::list<TreeUndoAction *> *undoBuffer
)
{
  if (!cells)
    return NULL; // nothing to insert

  m_configuration->AdjustWorksheetSize(true);
  bool renumbersections = false; // only renumber when true
  GroupCell *next; // next gc to insertion point
  GroupCell *prev;

  // Find the last cell in the tree that is to be inserted
  GroupCell *lastOfCellsToInsert = cells;
  if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (lastOfCellsToInsert->m_next)
  {
    lastOfCellsToInsert = dynamic_cast<GroupCell *>(lastOfCellsToInsert->m_next);
    if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
  }

  if (m_tree == NULL)
    where = NULL;

  if (where)
    next = dynamic_cast<GroupCell *>(where->m_next);
  else
  {
    next = m_tree; // where == NULL
    m_tree = cells;
  }
  prev = where;

  cells->m_previous = cells->m_previousToDraw = where;
  lastOfCellsToInsert->m_next = lastOfCellsToInsert->m_nextToDraw = next;

  if (prev)
    prev->m_next = prev->m_nextToDraw = cells;
  if (next)
    next->m_previous = next->m_previousToDraw = lastOfCellsToInsert;
  // make sure m_last still points to the last cell of the worksheet!!
  if (!next) // if there were no further cells
    m_last = lastOfCellsToInsert;

  m_configuration->SetCanvasSize(GetClientSize());
  if (renumbersections)
    NumberSections();
  Recalculate(where, false);
  m_saved = false; // document has been modified

  if (undoBuffer)
    TreeUndo_MarkCellsAsAdded(cells, lastOfCellsToInsert, undoBuffer);

  RequestRedraw(where);
  return lastOfCellsToInsert;
}

// this goes through m_tree with m_next, to set the correct m_last
// you can call this after folding, unfolding cells to make sure
// m_last is correct
GroupCell *Worksheet::UpdateMLast()
{
  if (!m_tree)
    m_last = NULL;
  else
  {

    m_last = m_tree;
    while (m_last->m_next)
      m_last = dynamic_cast<GroupCell *>(m_last->m_next);
  }

  if(m_last != NULL)
    m_configuration->AdjustWorksheetSize(true);
  return m_last;
}

void Worksheet::ScrollToError()
{
  GroupCell *ErrorCell;

  ErrorCell = dynamic_cast<GroupCell *>(m_cellPointers.m_errorList.LastError());

  if (ErrorCell == NULL)
    ErrorCell = GetWorkingGroup(true);

  if (ErrorCell != NULL)
  {
    if (ErrorCell->RevealHidden())
    {
      FoldOccurred();
      Recalculate(true);
    }

    // Try to scroll to a place from which the full error message is visible
    ScrollToCaret();

    // Set the cursor as close to the error as possible.
    if(ErrorCell->GetEditable()->ErrorIndexSet())
    {
      SetSelection(NULL);
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
      SetActiveCell(ErrorCell->GetEditable());
      ErrorCell->GetEditable()->GotoError();
      ScrollToCaret();
    }
    else
      SetHCaret(ErrorCell);
  }
}

GroupCell *Worksheet::GetWorkingGroup(bool resortToLast)
{
  GroupCell *tmp = NULL;
  if(m_cellPointers.GetWorkingGroup(resortToLast) != NULL)
    tmp = dynamic_cast<GroupCell *>(m_cellPointers.GetWorkingGroup(resortToLast));
  if(!resortToLast)
    return tmp;

  // The last group maxima was working on no more exists or has been deleted.
  if (tmp == NULL)
  {
    if (m_hCaretActive)
      tmp = m_hCaretPosition;
  }

  // No such cursor? Perhaps there is a vertically drawn one.
  if (tmp == NULL)
  {
    if (GetActiveCell())
      tmp = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
  }

  // If there is no such cell, neither, we append the line to the end of the
  // worksheet.
  if (tmp == NULL)
  {
    tmp = m_last;
  }
  return tmp;
}

void Worksheet::InsertLine(Cell *newCell, bool forceNewLine)
{
  if (newCell == NULL)
    return;

  m_saved = false;

  GroupCell *tmp = GetWorkingGroup(true);

  if (tmp == NULL)
  {
    if (GetActiveCell())
      tmp = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
  }


  // If we still don't have a place to put the line we give up.
  if (tmp == NULL)
    return;

  if (m_tree->Contains(tmp))
  {
    newCell->ForceBreakLine(forceNewLine);
    newCell->SetGroupList(tmp);

    tmp->AppendOutput(newCell);

    UpdateConfigurationClientSize();

    tmp->RecalculateAppended();

    if (FollowEvaluation())
    {
      SetSelection(NULL);
      if (GCContainsCurrentQuestion(tmp))
        OpenQuestionCaret();
      else
        ScrollToCaret();
    }
    RequestRedraw(tmp);
  }
  else
  {
    wxASSERT_MSG(m_tree->Contains(tmp), _("Bug: Trying to append maxima's output to a cell outside the worksheet."));
  }
}

void Worksheet::SetZoomFactor(double newzoom, bool recalc)
{
  // Restrict zoom factors to tenths
  newzoom = round (newzoom * 10) / 10.0;

  if(newzoom < m_configuration->GetMinZoomFactor())
    newzoom = m_configuration->GetMinZoomFactor();
  if(newzoom > m_configuration->GetMaxZoomFactor())
    newzoom = m_configuration->GetMaxZoomFactor();

  // If the zoom factor hasn't changed return. We don't test for equality with zero
  // since in this case that might probably work. But testing floats for equality
  // is a bad habit.
  if(fabs(m_configuration->GetZoomFactor() - newzoom) < .00005)
    return;

  m_configuration->SetZoomFactor(newzoom);
  // Determine if we have a sane thing we can scroll to.
  Cell *cellToScrollTo = NULL;
  if (CaretVisibleIs())
  {
    cellToScrollTo = GetHCaret();
    cellToScrollTo = GetActiveCell();
  }
  if (!cellToScrollTo) cellToScrollTo = GetWorkingGroup(true);
  if (!cellToScrollTo)
  {
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    cellToScrollTo = GetTree();
    while (cellToScrollTo != NULL)
    {
      wxRect rect = cellToScrollTo->GetRect();
      if (rect.GetBottom() > topleft.y)
        break;
      cellToScrollTo = cellToScrollTo->m_next;
    }
  }
  if (recalc)
  {
    RecalculateForce();
    RequestRedraw();
  }
  ScheduleScrollToCell(cellToScrollTo);
}

bool Worksheet::RecalculateIfNeeded()
{
  bool recalculate = true;

  if((m_recalculateStart == NULL) || (m_tree == NULL))
    recalculate = false;

  if(m_dc == NULL)
    recalculate = false;

  if(!recalculate)
  {
    if(m_configuration->AdjustWorksheetSize())
      AdjustSize();
    return false;
  }
  m_configuration->AdjustWorksheetSize(false);

  if(m_configuration->RecalculationForce())
  {
    if(m_configuration->FontChanged())
      wxLogMessage(_("Recalculating all cell sizes inclusive text dimensions"));
    else
      wxLogMessage(_("Recalculating all cell sizes"));
  }
  else
    wxLogMessage(_("Recalculating changed cell sizes"));

  if(!m_tree->Contains(m_recalculateStart))
    m_recalculateStart = m_tree;

  GroupCell *tmp;
  m_configuration->SetCanvasSize(GetClientSize());

  if (m_recalculateStart == NULL)
    tmp = m_tree;
  else
    tmp = m_recalculateStart;

  m_configuration->SetCanvasSize(GetClientSize());

  UpdateConfigurationClientSize();

  int width;
  int height;
  GetClientSize(&width, &height);

  while (tmp != NULL)
  {
    wxPoint upperLeftScreenCorner;
    CalcScrolledPosition(0, 0,
                         &upperLeftScreenCorner.x, &upperLeftScreenCorner.y);
    m_configuration->SetVisibleRegion(wxRect(upperLeftScreenCorner,
                                      upperLeftScreenCorner + wxPoint(width,height)));
    m_configuration->SetWorksheetPosition(GetPosition());

    tmp->Recalculate();
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  AdjustSize();
  m_configuration->RecalculationForce(false);
  m_configuration->FontChanged(false);

  m_recalculateStart = NULL;
  return true;
}

void Worksheet::Recalculate(Cell *start, bool force)
{
  GroupCell *group = m_tree;
  if(start != NULL)
    group = dynamic_cast<GroupCell *>(start->GetGroup());

  if(force)
    m_configuration->RecalculationForce(force);

  GroupCell *tmp = m_tree;
  if(m_recalculateStart == NULL)
    m_recalculateStart = group;
  else
    // Move m_recalculateStart backwards to group, if group comes before m_recalculateStart.
    while(tmp != NULL)
    {
      if (tmp == group)
      {
        m_recalculateStart = group;
        return;
      }

      if (tmp == m_recalculateStart)
        return;

      tmp = dynamic_cast<GroupCell *>(tmp -> m_next);
    }

  // If the cells to recalculate neither contain the start nor the tree we should
  // better recalculate all.
  m_recalculateStart = m_tree;
}

/***
 * Resize the control
 */
void Worksheet::OnSize(wxSizeEvent& WXUNUSED(event))
{
  // Inform all cells how wide our display is now
  m_configuration->SetCanvasSize(GetClientSize());

  // Determine if we have a sane thing we can scroll to.
  Cell *CellToScrollTo = NULL;
  if (CaretVisibleIs())
  {
    CellToScrollTo = m_hCaretPosition;
    if (!CellToScrollTo) CellToScrollTo = GetActiveCell();
  }
  if (!CellToScrollTo) CellToScrollTo = GetWorkingGroup(true);

  if (!CellToScrollTo)
  {
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    CellToScrollTo = m_tree;
    while (CellToScrollTo != NULL)
    {
      wxRect rect = CellToScrollTo->GetRect();
      if (rect.GetBottom() > topleft.y)
        break;
      CellToScrollTo = CellToScrollTo->m_next;
    }
  }

  RecalculateForce();

  GroupCell *tmp = m_tree;
  GroupCell *prev = NULL;
  UpdateConfigurationClientSize();
  if (tmp != NULL)
  {
    SetSelection(NULL);
    while (tmp != NULL)
    {
      dynamic_cast<GroupCell*>(tmp)->OnSize();

      if (prev == NULL)
      {
        tmp->SetCurrentPoint(m_configuration->GetIndent(),
                             m_configuration->GetBaseIndent() + tmp->GetMaxCenter());
      }
      else
      {
        tmp->SetCurrentPoint(m_configuration->GetIndent(),
                             prev->GetCurrentPoint().y + prev->GetMaxDrop() + tmp->GetMaxCenter() +
                             m_configuration->GetGroupSkip());
      }

      prev = tmp;
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
  }

  m_configuration->AdjustWorksheetSize(true);
  RequestRedraw();
  if (CellToScrollTo)
    ScheduleScrollToCell(CellToScrollTo, false);
}

/***
 * Clear document
 * Basically set everything to the state as if Worksheet
 * was just created, so there is a blank document.
 * Called when opening a new file into existing Worksheet.
 */
void Worksheet::ClearDocument()
{
  CloseAutoCompletePopup();
  SetSelection(NULL);
  SetActiveCell(NULL, false);
  m_clickType = CLICK_TYPE_NONE;
  m_clickInGC = NULL;
  m_hCaretActive = false;
  SetHCaret(NULL); // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_recalculateStart = NULL;
  m_evaluationQueue.Clear();
  TreeUndo_ClearBuffers();
  DestroyTree();

  m_blinkDisplayCaret = true;
  m_saved = false;
  UpdateTableOfContents();

  Scroll(0, 0);
}

/***
 * Reset all input promts to "-->  "
 * Called when Restart Maxima is called from Maxima menu
 */
void Worksheet::ResetInputPrompts()
{
  if (m_tree)
    m_tree->ResetInputLabelList(); // recursivly reset prompts
}

//
// support for numbered sections with hiding
//
void Worksheet::NumberSections()
{
  int s, sub, subsub, h5, h6, i;
  s = sub = subsub = i = h5 = h6 = 0;
  if (m_tree)
    m_tree->Number(s, sub, subsub, h5, h6, i);
}

bool Worksheet::IsLesserGCType(int type, int comparedTo)
{
  switch (type)
  {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_IMAGE:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
          (comparedTo == GC_TYPE_SUBSECTION) || (comparedTo == GC_TYPE_SUBSUBSECTION) ||
          (comparedTo == GC_TYPE_HEADING5) || (comparedTo == GC_TYPE_HEADING6))
        return true;
      else
        return false;
    case GC_TYPE_HEADING6:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
          (comparedTo == GC_TYPE_SUBSECTION) || (comparedTo == GC_TYPE_HEADING5)
          || (comparedTo == GC_TYPE_SUBSUBSECTION))
        return true;
      else
        return false;
    case GC_TYPE_HEADING5:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
          (comparedTo == GC_TYPE_SUBSECTION) || (comparedTo == GC_TYPE_SUBSUBSECTION))
        return true;
      else
        return false;
    case GC_TYPE_SUBSUBSECTION:
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
void Worksheet::FoldOccurred()
{
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
GroupCell *Worksheet::ToggleFold(GroupCell *which)
{
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
GroupCell *Worksheet::ToggleFoldAll(GroupCell *which)
{
  if (!which)
    return NULL;

  GroupCell *result = NULL;
  if (which->IsFoldable())
    if (which->GetHiddenTree())
      result = which->UnfoldAll();
    else
      result = which->FoldAll();
  else
    return NULL;

  if (result) // something has folded/unfolded
    FoldOccurred();

  return result;
}

/**
 * Recursively folds the whole document.
 */
void Worksheet::FoldAll()
{
  if (m_tree)
  {
    m_tree->FoldAll();
    FoldOccurred();
  }
}

/**
 * Recursively unfolds the whole document.
 */
void Worksheet::UnfoldAll()
{
  if (m_tree)
  {
    m_tree->UnfoldAll();
    FoldOccurred();
  }
}

// Returns the tree from start to end and connets the pointers the right way
// so that m_tree stays 'correct' - also works in hidden trees
GroupCell *Worksheet::TearOutTree(GroupCell *start, GroupCell *end)
{
  if ((!start) || (!end))
    return NULL;
  Cell *prev = start->m_previous;
  Cell *next = end->m_next;

  end->m_next = end->m_nextToDraw = NULL;
  start->m_previous = start->m_previousToDraw = NULL;

  if (prev)
    prev->m_next = prev->m_nextToDraw = next;
  if (next)
    next->m_previous = next->m_previousToDraw = prev;
  // fix m_last if we tore it
  if (end == m_last)
    m_last = dynamic_cast<GroupCell *>(prev);

  return start;
}

/***
 * Right mouse - popup-menu
 */
void Worksheet::OnMouseRightDown(wxMouseEvent &event)
{
  RecalculateIfNeeded();
  ClearNotification();
  m_cellPointers.ResetSearchStart();

  wxMenu *popupMenu = new wxMenu();

  int downx, downy;

  // find out if clicked into existing selection, if not, reselect with leftdown
  //
  bool clickInSelection = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &downx, &downy);
  if ((m_cellPointers.m_selectionStart != NULL))
  {
    // SELECTION OF GROUPCELLS
    if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
    { //a selection of groups
      if (downx <= m_configuration->GetCellBracketWidth() + 3)
      {
        wxRect rectStart = m_cellPointers.m_selectionStart->GetRect();
        wxRect rectEnd = m_cellPointers.m_selectionEnd->GetRect();
        if (((downy >= rectStart.GetTop()) && (downy <= rectEnd.GetBottom())) ||
            ((downy >= rectEnd.GetTop()) && (downy <= rectStart.GetBottom())))
          clickInSelection = true;
      }
    }
      // SELECTION OF OUTPUT
    else
    {
      Cell *tmp = m_cellPointers.m_selectionStart;
      wxRect rect;
      while (tmp != NULL)
      {
        rect = tmp->GetRect();
        if (rect.Contains(downx, downy))
          clickInSelection = true;

        if (tmp == m_cellPointers.m_selectionEnd)
          break;
        tmp = tmp->m_nextToDraw;
      }
    }
  }
    // SELECTION IN EDITORCELL
  else if (GetActiveCell() != NULL)
  {
    if (GetActiveCell()->IsPointInSelection(wxPoint(downx, downy)))
      clickInSelection = true;
  }

  // emulate a left click to set the cursor
  if (!clickInSelection)
  {
    wxMouseEvent dummy = event;
    dummy.SetShiftDown(false);
    OnMouseLeftDown(dummy);
    m_leftDown = false;
    m_clickType = CLICK_TYPE_NONE;
    if (HasCapture())
      ReleaseMouse();
  }

  // construct a menu appropriate to what we have
  //
  if (GetActiveCell() == NULL)
  {
    if (IsSelected(MC_TYPE_IMAGE) || IsSelected(MC_TYPE_SLIDE))
    {
      popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_image, _("Save Image..."), wxEmptyString, wxITEM_NORMAL);
      if (IsSelected(MC_TYPE_SLIDE))
      {
        popupMenu->Append(popid_animation_save, _("Save Animation..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_copy_animation, _("Copy Animation"),
                          wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(popid_animation_start, _("Start Animation"), wxEmptyString, wxITEM_NORMAL);
      }
      else
      {
        if(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup())->GetGroupType() == GC_TYPE_IMAGE)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_maxsizechooser, _("Restrict Maximum size"), wxEmptyString, wxITEM_NORMAL);
        }
      }
      if (IsSelected(MC_TYPE_IMAGE))
      {
        if(dynamic_cast<ImgCell *>(m_cellPointers.m_selectionStart)->GnuplotSource() != wxEmptyString)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_popup_gnuplot, _("Popout interactively"), wxEmptyString, wxITEM_NORMAL);
        }
      }
    }
    else if (m_cellPointers.m_selectionStart != NULL)
    {
      if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
      {

        if (CanCopy())
        {
          popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
		  popupMenu->Append(popid_copy_matlab, _("Copy for Octave/Matlab"), wxEmptyString, wxITEM_NORMAL);
		  popupMenu->Append(popid_copy_tex, _("Copy as LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_text, _("Copy as plain text"), wxEmptyString, wxITEM_NORMAL);
          if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd)
            popupMenu->Append(popid_copy_mathml, _("Copy as MathML (e.g. to word processor)"), wxEmptyString,
                              wxITEM_NORMAL);
          popupMenu->Append(popid_copy_image, _("Copy as Image"),
                            wxEmptyString, wxITEM_NORMAL);
          if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
             (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu->Append(popid_copy_animation, _("Copy Animation"),
                              wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_svg, _("Copy as SVG"),
                            wxEmptyString, wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
          popupMenu->Append(popid_copy_emf, _("Copy as EMF"),
                            wxEmptyString, wxITEM_NORMAL);
#endif
          popupMenu->Append(popid_copy_rtf, _("Copy as RTF"),
                            wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu->Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }
        popupMenu->AppendSeparator();
        popupMenu->Append(popid_evaluate, _("Evaluate Cell(s)"), wxEmptyString, wxITEM_NORMAL);
        if(m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd)
          popupMenu->Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"), wxEmptyString, wxITEM_NORMAL);

        if (CanMergeSelection())
          popupMenu->Append(popid_merge_cells, _("Merge Cells"), wxEmptyString, wxITEM_NORMAL);

        // Add a "evaluate this <sectioning unit>" context menu entry.
        GroupCell *group;
        if (m_cellPointers.m_selectionEnd != NULL)
          group = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd);
        else
          group = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_TITLE)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString, wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SECTION)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Section\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SUBSECTION)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SUBSUBSECTION)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING5)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING6)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        popupMenu->AppendCheckItem(popid_auto_answer, _("Automatically answer questions"),
                                   _("Automatically fill in answers known from the last run"));
        if(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart)->GetGroupType() == GC_TYPE_IMAGE)
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_maxsizechooser, _("Restrict Maximum size"), wxEmptyString, wxITEM_NORMAL);
        }
        if((m_cellPointers.m_selectionStart != NULL) &&
           (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE) &&
           (dynamic_cast<ImgCell *>(m_cellPointers.m_selectionStart)->GnuplotSource() != wxEmptyString))
        {
          popupMenu->AppendSeparator();
          popupMenu->Append(popid_popup_gnuplot, _("Popout interactively"), wxEmptyString, wxITEM_NORMAL);
        }
      }

      else
      {
        if (CanCopy(true))
        {
          popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
		  popupMenu->Append(popid_copy_matlab, _("Copy for Octave/Matlab"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_tex, _("Copy as LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_text, _("Copy as plain text"), wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_mathml, _("Copy as MathML (e.g. to word processor)"), wxEmptyString,
                            wxITEM_NORMAL);

          popupMenu->Append(popid_copy_image, _("Copy as Image"),
                            wxEmptyString, wxITEM_NORMAL);
          if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
             (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu->Append(popid_copy_animation, _("Copy Animation"),
                              wxEmptyString, wxITEM_NORMAL);
          popupMenu->Append(popid_copy_svg, _("Copy as SVG"),
                            wxEmptyString, wxITEM_NORMAL);
  #if wxUSE_ENH_METAFILE
          popupMenu->Append(popid_copy_emf, _("Copy as EMF"),
                            wxEmptyString, wxITEM_NORMAL);
#endif
          popupMenu->Append(popid_copy_rtf, _("Copy as RTF"),
                            wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu->Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }
        if(IsSelected(MC_TYPE_LABEL))
        {
          if(popupMenu->GetMenuItemCount()>0)
            popupMenu->AppendSeparator();
          popupMenu->Append(popid_add_watch_label, _("Add to watchlist"), wxEmptyString, wxITEM_NORMAL);
        }

        if (IsSelected(MC_TYPE_DEFAULT) || IsSelected(MC_TYPE_LABEL))
        {
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

    else if (m_hCaretActive == true)
    {
      popupMenu->Append(popid_paste, _("Paste"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_select_all, _("Select All"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->AppendSeparator();
      popupMenu->Append(popid_insert_text, _("Insert Text Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_title, _("Insert Title Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_section, _("Insert Section Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_subsection, _("Insert Subsection Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_subsubsection, _("Insert Subsubsection Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_heading5, _("Insert Heading5 Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(popid_insert_heading6, _("Insert Heading6 Cell"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->AppendSeparator();
        popupMenu->Append(ToolBar::tb_evaltillhere, _("Evaluate Cells Above"), wxEmptyString, wxITEM_NORMAL);
        popupMenu->Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"), wxEmptyString, wxITEM_NORMAL);
    }
  }

    // popup menu in active cell
  else
  {
    popupMenu->Append(popid_cut, _("Cut"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_copy, _("Copy"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->Append(popid_paste, _("Paste"), wxEmptyString, wxITEM_NORMAL);
    popupMenu->AppendSeparator();
    popupMenu->Append(popid_select_all, _("Select All"), wxEmptyString, wxITEM_NORMAL);
    if ((clickInSelection) &&
        dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup())->GetGroupType() == GC_TYPE_CODE)
      popupMenu->Append(popid_comment_selection, _("Comment Selection"), wxEmptyString, wxITEM_NORMAL);
    wxString selectionString = GetActiveCell()->GetSelectionString();
    if(selectionString.IsEmpty())
      selectionString = GetActiveCell()->GetWordUnderCaret();
    if(!selectionString.IsEmpty() &&
       !selectionString.Contains("\n") &&
       !selectionString.Contains("\r") &&
       !selectionString.Contains(":") &&
       ((selectionString[0] < '0') || (selectionString[0] > '9'))
      )
      popupMenu->Append(popid_add_watch, _("Add to watchlist"), wxEmptyString, wxITEM_NORMAL);

    if (!clickInSelection)
      popupMenu->Append(popid_divide_cell, _("Divide Cell"), wxEmptyString, wxITEM_NORMAL);

    GroupCell *group = NULL;
    if (GetActiveCell() != NULL)
    {
      wxASSERT(GetActiveCell()->GetGroup() != NULL);
      group = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    }
    if (m_cellPointers.m_selectionStart != NULL)
    {
      if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
      {
        group = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
      }
    }
    if (group)
    {
      popupMenu->AppendSeparator();
      switch (StartOfSectioningUnit(group)->GetGroupType())
      {
        case GC_TYPE_TITLE:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SECTION:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Section\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSECTION:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSUBSECTION:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING5:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING6:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
      default:{}
      }
      switch (group->GetGroupType())
      {
        case GC_TYPE_CODE:
          popupMenu->AppendSeparator();
          popupMenu->AppendCheckItem(popid_auto_answer, _("Automatically answer questions"),
                                     _("Automatically fill in answers known from the last run"));
          popupMenu->Check(popid_auto_answer,group->AutoAnswer());
          break;
        case GC_TYPE_TITLE:
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Part"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Part"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SECTION:
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Section"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Section"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSECTION:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Subsection"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Subsection"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSUBSECTION:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Subsubsection"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Subsubsection"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING5:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Heading 5"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Heading 5"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING6:
          popupMenu->Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide Heading 6"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide Heading 6"), wxEmptyString, wxITEM_NORMAL);
          break;
        default:
          if (group->GetHiddenTree() != NULL)
            popupMenu->Append(popid_unfold,
                              _("Unhide contents"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu->Append(popid_fold,
                              _("Hide contents"), wxEmptyString, wxITEM_NORMAL);
      }
    }
  }
  // create menu if we have any items
  if (popupMenu->GetMenuItemCount() > 0)
    PopupMenu( popupMenu);
  wxDELETE(popupMenu);
}


/***
 * We have a mouse click to the left of a GroupCel.
 */
void Worksheet::OnMouseLeftInGcLeft(wxMouseEvent &event, GroupCell *clickedInGC)
{
  if ((clickedInGC->HideRect()).Contains(m_down)) // did we hit the hide rectancle
  {
    if (clickedInGC->IsFoldable())
    {
      if (event.ShiftDown())
        ToggleFoldAll(clickedInGC);
      else
        ToggleFold(clickedInGC);
      Recalculate(clickedInGC, true);
    }
    else
    {
      clickedInGC->SwitchHide();
      clickedInGC->ResetSize();
      Recalculate(clickedInGC, false);
      m_clickType = CLICK_TYPE_NONE; // ignore drag-select
    }
  }
  else
  {
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    SetSelection(clickedInGC);
  }
}

/***
 * We have a mouse click in the GroupCell.
 */
void Worksheet::OnMouseLeftInGcCell(wxMouseEvent &WXUNUSED(event), GroupCell *clickedInGC)
{
  if (GCContainsCurrentQuestion(clickedInGC))
  {
    // The user clicked at the cell maxima has asked a question in.
    FollowEvaluation(true);
    OpenQuestionCaret();
    return;
  }
  else
  {
    // The user clicked at a ordinary cell
    EditorCell *editor = clickedInGC->GetEditable();
    if (editor != NULL)
    {
      wxRect rect = editor->GetRect();
      if (
              ((m_down.y >= rect.GetTop()) && (m_down.y <= rect.GetBottom())) &&
              ((m_configuration->ShowCodeCells()) ||
               ((editor->GetType() != MC_TYPE_INPUT) || (clickedInGC->GetOutput() == NULL))
              )
              )
      {
        editor->MouseSelectionStartedHere();
        SetActiveCell(editor, false); // do not refresh as we will do so later
        GetActiveCell()->SelectPointText(m_down);
        m_blinkDisplayCaret = true;
        m_clickType = CLICK_TYPE_INPUT_SELECTION;
        if (editor->GetWidth() == -1)
          Recalculate(clickedInGC, false);
        ScrollToCaret();
        // Here we tend to get unacceptably long delays before the display is
        // refreshed by the idle loop => Trigger the refresh manually.
        ForceRedraw();
        return;
      }
    }
  }
  // what if we tried to select something in output, select it (or if editor, activate it)
  if ((clickedInGC->GetOutputRect()).Contains(m_down))
  {
    wxRect rect2(m_down.x, m_down.y, 1, 1);
    wxPoint mmm(m_down.x + 1, m_down.y + 1);
    clickedInGC->SelectRectInOutput(rect2, m_down, mmm,
                                    &m_cellPointers.m_selectionStart, &m_cellPointers.m_selectionEnd);
    if (m_cellPointers.m_selectionStart != NULL)
    {
      m_clickType = CLICK_TYPE_OUTPUT_SELECTION;
      m_clickInGC = clickedInGC;
    }
  }
}

void Worksheet::OnMouseLeftInGc(wxMouseEvent &event, GroupCell *clickedInGc)
{
  // The click has changed the cell which means the user works here and
  // doesn't want the evaluation mechanism to automatically follow the
  // evaluation any more.
  ScrolledAwayFromEvaluation(true);

  if (m_down.x <= m_configuration->GetIndent())
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
void Worksheet::OnMouseLeftDown(wxMouseEvent &event)
{
  RecalculateIfNeeded();
  CloseAutoCompletePopup();
  m_leftDownPosition = wxPoint(event.GetX(),event.GetY());
  ClearNotification();

  // During drag-and-drop We want to track the mouse position.
  if (event.LeftDown())
  {
    if (!HasCapture())
      CaptureMouse();
    m_leftDown = true;
  }

  m_cellPointers.ResetSearchStart();

  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

  if (m_tree == NULL)
    return;

  // Handle a shift-click when GroupCells were selected.
  if ((m_hCaretPositionStart != NULL) && (event.ShiftDown()))
  {
    // We were selecting group cells when the shift-click happened.
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    // Set a fake starting point for the selection that is inside the cell the selection started in.
    m_down = wxPoint(m_hCaretPositionStart->GetRect().GetLeft(), m_hCaretPositionStart->GetRect().GetTop());
    // Handle the mouse pointer position
    OnMouseMotion(event);
    return;
  }

  // Handle a shift-click when the text editor is active.
  if ((GetActiveCell() != NULL) && (event.ShiftDown()))
  {

    // We were within an input cell when the selection has started.
    m_clickType = CLICK_TYPE_INPUT_SELECTION;

    // The mouse selection was started in the currently active EditorCell
    GetActiveCell()->MouseSelectionStartedHere();

    // Set a fake starting point for the selection that is inside the cell the selection started in.
    int startingChar = GetActiveCell()->GetCaretPosition();
    if (GetActiveCell()->SelectionActive()) startingChar = dynamic_cast<EditorCell *>(GetActiveCell())->GetSelectionStart();
    m_down = wxPoint(GetActiveCell()->PositionToPoint(m_configuration->GetDefaultFontSize(), startingChar));
    GetActiveCell()->SelectNone();
    // Handle the mouse pointer position
    OnMouseMotion(event);

    // Did we shift-click in the currently active cell?

    return;
  }

  // Handle a shift-click when the hCaret is active
  if ((m_hCaretPosition != NULL) && (event.ShiftDown()))
  {
    // We were selecting group cells when the shift-click happened.
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    // Set a fake starting point for the selection that is inside the cell the selection started in.
    m_down = wxPoint(m_hCaretPosition->GetRect().GetLeft(), m_hCaretPosition->GetRect().GetBottom() + 1);
    // Handle the mouse pointer position
    OnMouseMotion(event);
    return;
  }


// default when clicking
  m_clickType = CLICK_TYPE_NONE;
  SetSelection(NULL);
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_hCaretPosition = NULL;
  m_hCaretActive = false;
  SetActiveCell(NULL, false);

  GroupCell *tmp = m_tree;
  wxRect rect;
  GroupCell *clickedBeforeGC = NULL;
  GroupCell *clickedInGC = NULL;
  while (tmp != NULL)
  { // go through all groupcells
    rect = tmp->GetRect();
    if (m_down.y < rect.GetTop())
    {
      clickedBeforeGC = dynamic_cast<GroupCell *>(tmp);
      break;
    }
    else if (m_down.y <= rect.GetBottom())
    {
      clickedInGC = dynamic_cast<GroupCell *>(tmp);
      break;
    }
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  if (clickedBeforeGC != NULL)
  { // we clicked between groupcells, set hCaret
    SetHCaret(dynamic_cast<GroupCell *>(tmp->m_previous));
    m_clickType = CLICK_TYPE_GROUP_SELECTION;

    // The click will has changed the position that is in focus so we assume
    // the user wants to work here and doesn't want the evaluation mechanism
    // to automatically follow the evaluation any more.
    ScrolledAwayFromEvaluation(true);
  }
  else if (clickedInGC != NULL)
  {
    ScrolledAwayFromEvaluation(true);
    OnMouseLeftInGc(event, clickedInGC);
  }

  else
  { // we clicked below last groupcell (both clickedInGC and clickedBeforeGC == NULL)
    // set hCaret (or activate last cell?)
    SetHCaret(m_last);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    ScrolledAwayFromEvaluation(true);
  }

  RequestRedraw();
  // Re-calculate the table of contents
  UpdateTableOfContents();
}


GroupCell *Worksheet::FirstVisibleGC()
{
  wxPoint point;
  CalcUnscrolledPosition(0, 0, &point.x, &point.y);
  wxRect rect;
  GroupCell *tmp = m_tree;

  while (tmp != NULL)
  { // go through all groupcells
    rect = tmp->GetRect();

    if (point.y < rect.GetBottom())
      return tmp;

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  return NULL;
}

void Worksheet::OnMouseLeftUp(wxMouseEvent &event)
{
  m_cellPointers.ResetSearchStart();
  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
     (m_leftDownPosition == wxPoint(event.GetX(),event.GetY())) &&
     (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
    dynamic_cast<SlideShow *>(GetSelectionStart())->AnimationRunning(
      !dynamic_cast<SlideShow *>(GetSelectionStart())->AnimationRunning());

  m_leftDown = false;
  m_mouseDrag = false;
  m_clickInGC = NULL; // pointer to NULL to prevent crashes if the cell is deleted
  m_clickType = CLICK_TYPE_NONE;
  CheckUnixCopy();
  SetFocus();
  m_cellPointers.ResetMouseSelectionStart();
  // Here we actually only want the toolbars and menus to redrawn (and therefore the "copy")
  // hotkey to be enabled: The rest already is in place.
  RequestRedraw();
}

void Worksheet::OnMouseWheel(wxMouseEvent &event)
{
  if (event.GetModifiers() & wxMOD_CONTROL)
  {
    wxCommandEvent *zoomEvent = new wxCommandEvent;
    zoomEvent->SetEventType(wxEVT_MENU);
    if (event.GetWheelRotation() > 0)
    {
      zoomEvent->SetId(menu_zoom_in);
      GetParent()->GetEventHandler()->QueueEvent(zoomEvent);
    }
    else
    {
      zoomEvent->SetId(menu_zoom_out);
      GetParent()->GetEventHandler()->QueueEvent(zoomEvent);
    }
  }
  else
  {
    if (CanAnimate())
    {

      //! Step the slide show.
      int rot = event.GetWheelRotation();

      SlideShow *tmp = dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart);
      tmp->AnimationRunning(false);

      if (rot > 0)
        tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());
      else
        tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() - 1) % tmp->Length());

      wxRect rect = m_cellPointers.m_selectionStart->GetRect();
      RequestRedraw(rect);

      if ((m_mainToolBar != NULL) && (m_mainToolBar->m_plotSlider != NULL))
      {
#ifdef __WXMSW__
        // On Windows: Set the focus to the slider so it handles further wheel events
        m_mainToolBar -> m_plotSlider -> SetFocus();
#endif

        if (m_mainToolBar->m_plotSlider)
          m_mainToolBar->m_plotSlider->SetValue(tmp->GetDisplayedIndex());
      }

#ifdef __WXMSW__
      // On Windows the first scroll event scrolls the canvas. Let's scroll it back again.
      int view_x,view_y;
      GetViewStart(&view_x, &view_y);
      if(rot>0)
        view_y ++;
      else
        view_y --;
      Scroll(view_x, view_y);
#endif
    }
    else
      event.Skip();
  }
}

void Worksheet::OnMouseMotion(wxMouseEvent &event)
{
    CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_pointer_x, &m_pointer_y);
    m_mouseMotionWas = true;

    if (m_tree == NULL || !m_leftDown)
      return;
    m_mouseDrag = true;
    m_up.x = m_pointer_x;
    m_up.y = m_pointer_y;
    if (m_mouseOutside)
    {
      m_mousePoint.x = event.GetX();
      m_mousePoint.y = event.GetY();
    }
    ClickNDrag(m_down, m_up);
}

void Worksheet::SelectGroupCells(wxPoint down, wxPoint up)
{
  // Calculate the rectangle that has been selected
  int ytop = wxMin(down.y, up.y);
  int ybottom = wxMax(down.y, up.y);
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = NULL;

  wxRect rect;

  // find out the group cell the selection begins in
  GroupCell *tmp = m_tree;
  while (tmp != NULL)
  {
    rect = tmp->GetRect();
    if (ytop <= rect.GetBottom())
    {
      m_cellPointers.m_selectionStart = tmp;
      break;
    }
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  // find out the group cell the selection ends in
  tmp = m_tree;
  while (tmp != NULL)
  {
    rect = tmp->GetRect();
    if (ybottom < rect.GetTop())
    {
      m_cellPointers.m_selectionEnd = tmp->m_previous;
      break;
    }
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  if (tmp == NULL)
    m_cellPointers.m_selectionEnd = m_last;

  if (m_cellPointers.m_selectionStart)
  {
    if (m_cellPointers.m_selectionEnd == (m_cellPointers.m_selectionStart->m_previous))
    {
      SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd));
    }
    else
    {
      m_hCaretActive = false;
      m_hCaretPosition = NULL;
    }
  }
  else
  {
    m_hCaretActive = true;
    m_hCaretPosition = m_last;
  }

  if (down.y > up.y)
  {
    m_hCaretPositionStart = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
    m_hCaretPositionEnd = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd);
  }
  else
  {
    m_hCaretPositionStart = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd);
    m_hCaretPositionEnd = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
  }
  SetSelection(m_cellPointers.m_selectionStart,m_cellPointers.m_selectionEnd);
}

void Worksheet::ClickNDrag(wxPoint down, wxPoint up)
{
  Cell *selectionStartOld = m_cellPointers.m_selectionStart, *selectionEndOld = m_cellPointers.m_selectionEnd;
  wxRect rect;

  int ytop = wxMin(down.y, up.y);
  int ybottom = wxMax(down.y, up.y);

  switch (m_clickType)
  {
    case CLICK_TYPE_NONE:
      return;

    case CLICK_TYPE_INPUT_SELECTION:
      wxASSERT_MSG(m_cellPointers.m_cellMouseSelectionStartedIn != NULL,
                   _("Bug: Trying to select inside a cell without having a current cell"));
      if (m_cellPointers.m_cellMouseSelectionStartedIn == NULL)
        return;

      {
        rect = dynamic_cast<EditorCell *>(m_cellPointers.m_cellMouseSelectionStartedIn)->GetRect();

        // Let's see if we are still inside the cell we started selecting in.
        if ((ytop < rect.GetTop()) || (ybottom > rect.GetBottom()))
        {
          // We have left the cell we started to select in =>
          // select all group cells between start and end of the selection.
          SelectGroupCells(up, down);

          // If we have just started selecting GroupCells we have to unselect
          // the already-selected text in the cell we have started selecting in.
          if (GetActiveCell())
          {
            GetActiveCell()->SelectNone();
            m_hCaretActive = true;
            SetActiveCell(NULL);
          }
        }
        else
        {
          // Clean up in case that we have re-entered the cell we started
          // selecting in.
          m_hCaretActive = false;
          SetSelection(NULL);
          SetActiveCell(dynamic_cast<EditorCell *>(m_cellPointers.m_cellMouseSelectionStartedIn));
          // We are still inside the cell => select inside the current cell.
          GetActiveCell()->SelectRectText(down, up);
          m_blinkDisplayCaret = true;
          RequestRedraw();

          // Remove the marker that we need to refresh
          selectionStartOld = m_cellPointers.m_selectionStart;
          selectionEndOld = m_cellPointers.m_selectionEnd;
        }
        break;
      }
    case CLICK_TYPE_GROUP_SELECTION:
      SelectGroupCells(up, down);
      break;

    case CLICK_TYPE_OUTPUT_SELECTION:
      SetSelection(NULL);
      rect.x = wxMin(down.x, up.x);
      rect.y = wxMin(down.y, up.y);
      rect.width = wxMax(abs(down.x - up.x), 1);
      rect.height = wxMax(abs(down.y - up.y), 1);

      if (m_clickInGC != NULL)
        m_clickInGC->SelectRectInOutput(rect, down, up, &m_cellPointers.m_selectionStart, &m_cellPointers.m_selectionEnd);
      break;

    default:
      break;
  } // end switch

  // Refresh only if the selection has changed
  if ((selectionStartOld != m_cellPointers.m_selectionStart) || (selectionEndOld != m_cellPointers.m_selectionEnd))
    RequestRedraw();
}

/***
 * Get the string representation of the selection
 */
wxString Worksheet::GetString(bool lb)
{

  if (m_cellPointers.m_selectionStart == NULL)
  {
    if (GetActiveCell() == NULL)
      return wxEmptyString;
    else
      return GetActiveCell()->ToString();
  }

  wxString s;
  Cell *tmp = m_cellPointers.m_selectionStart;
  while (tmp != NULL)
  {
    if (lb && tmp->BreakLineHere() && s.Length() > 0)
      s += wxT("\n");
    s += tmp->ToString();
    if (tmp == m_cellPointers.m_selectionEnd)
      break;
    tmp = tmp->m_nextToDraw;
  }
  return s;
}

/***
 * Copy selection to clipboard.
 */
bool Worksheet::Copy(bool astext)
{
  if (GetActiveCell() != NULL)
  {
    return GetActiveCell()->CopyToClipboard();
  }

  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  if (!astext && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
    return CopyCells();
    /// If the selection is IMAGE or SLIDESHOW, copy it to clipboard
    /// as image.
  else if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
           m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE)
  {
    dynamic_cast<ImgCell *>(m_cellPointers.m_selectionStart)->CopyToClipboard();
    return true;
  }
  else if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
           m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE)
  {
    dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart)->CopyToClipboard();
    return true;
  }
  else
  {
    wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
    if (wxTheClipboard->Open())
    {
      wxDataObjectComposite *data = new wxDataObjectComposite;

      // Add the wxm code corresponding to the selected output to the clipboard
      wxString s = GetString(true);
      data->Add(new wxmDataObject(s));

      if(m_configuration->CopyMathML())
      {
        // Add a mathML representation of the data to the clipboard
        s = ConvertSelectionToMathML();
        if (s != wxEmptyString)
        {
          // We mark the MathML version of the data on the clipboard as "preferred"
          // as if an application supports MathML neither bitmaps nor plain text
          // makes much sense.
          data->Add(new MathMLDataObject(s), true);
          data->Add(new MathMLDataObject2(s), true);
          if(m_configuration->CopyMathMLHTML())
            data->Add(new wxHTMLDataObject(s), true);
          // wxMathML is a HTML5 flavour, as well.
          // See https://github.com/fred-wang/Mathzilla/blob/master/mathml-copy/lib/copy-mathml.js#L21
          //
          // Unfortunately MS Word and Libreoffice Writer don't like this idea so I have
          // disabled the following line of code again:
          //
          // data->Add(new wxHTMLDataObject(s));
        }
      }

      if(m_configuration->CopyRTF())
      {
        // Add a RTF representation of the currently selected text
        // to the clipboard: For some reason libreoffice likes RTF more than
        // it likes the MathML - which is standartized.
        Cell *tmp = CopySelection();
        if (tmp != NULL)
        {
          wxString rtf;
          rtf = RTFStart() + tmp->ListToRTF() + wxT("\\par\n") + RTFEnd();
          data->Add(new RtfDataObject(rtf));
          data->Add(new RtfDataObject2(rtf), true);
        }
        wxDELETE(tmp);
      }

      // Add a string representation of the selected output to the clipboard
      Cell *tmp = CopySelection();
      s = tmp->ListToString();
      data->Add(new wxTextDataObject(s));
      wxDELETE(tmp);

      if(m_configuration->CopyBitmap())
      {
        // Try to fill bmp with a high-res version of the cells
        {
          // Add a bitmap representation of the selected output to the clipboard - if this
          // bitmap isn't way too large for this to make sense:
          wxBitmap bmp;
          int bitmapScale = 3;
          wxConfig::Get()->Read(wxT("bitmapScale"), &bitmapScale);
          BitmapOut bmp_scaled(&m_configuration, bitmapScale);
          Cell *tmp = CopySelection();
          if (bmp_scaled.SetData(tmp, 4000000))
          {
            bmp = bmp_scaled.GetBitmap();
            data->Add(new wxBitmapDataObject(bmp));
          }
        }
      }
      wxTheClipboard->SetData(data);
      wxTheClipboard->Close();
      Recalculate();
      return true;
    }
    Recalculate();
    return false;
  }
}

wxString Worksheet::ConvertSelectionToMathML()
{
  if (GetActiveCell() != NULL)
    return wxEmptyString;

  if ((m_cellPointers.m_selectionStart == NULL) || (m_cellPointers.m_selectionEnd == NULL))
    return wxEmptyString;

  wxString s;
  Cell *tmp = CopySelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd, true);

  s = wxString(wxT("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n")) +
      wxT("<semantics>") +
      tmp->ListToMathML(true) +
      wxT("<annotation encoding=\"application/x-maxima\">") +
      Cell::XMLescape(tmp->ListToString()) +
      wxT("</annotation>") +
      wxT("</semantics>") +
      wxT("</math>");

  // We might add indentation as additional eye candy to all but extremely long
  // xml data chunks.
  if (s.Length() < 1000000)
  {
    // Load the string into a wxXmlDocument that later can be output with
    // sensible indentation.
    wxXmlDocument doc;
    {
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(s);
      wxMemoryInputStream istream(ostream);
      doc.Load(istream);
    }

    // If we failed to load the document the word processor will most probably fail, too.
    // But we can still put it into the clipboard for debugging purposes.
    if (doc.IsOk())
    {
      wxMemoryOutputStream ostream;
      doc.Save(ostream);
      s = wxString::FromUTF8((char *) ostream.GetOutputStreamBuffer()->GetBufferStart(),
                             ostream.GetOutputStreamBuffer()->GetBufferSize());

      // Now the string has a header we want to get rid of again.
      s = s.SubString(s.Find("\n") + 1, s.Length());
      
    }
  }
  Recalculate();
  return s;
}

bool Worksheet::CopyMathML()
{
  wxString s = ConvertSelectionToMathML();

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));

  if (wxTheClipboard->Open())
  {
    wxDataObjectComposite *data = new wxDataObjectComposite;
    // The default clipboard slot for MathML
    data->Add(new MathMLDataObject(s), true);
    data->Add(new MathMLDataObject2(s), true);
    // A fallback for communicating with non-mathML-aware programs
    data->Add(new wxTextDataObject(s));
    // wxMathML is a HTML5 flavour, as well.
    // See https://github.com/fred-wang/Mathzilla/blob/master/mathml-copy/lib/copy-mathml.js#L21
    //
    // The \0 tries to work around a strange bug in wxWidgets that sometimes makes string
    // endings disappear
    data->Add(new wxHTMLDataObject(s + wxT('\0')));
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    Recalculate();
    return true;
  }
  return false;
}

bool Worksheet::CopyMatlab()
{
  if (GetActiveCell() != NULL)
	return false;

  if (m_cellPointers.m_selectionStart == NULL)
	return false;

  wxString result;
  Cell *tmp = m_cellPointers.m_selectionStart;

  bool firstcell = true;
  while (tmp != NULL)
  {
	if ((tmp->HardLineBreak()) && (!firstcell))
	  result += wxT("\n");
	result += tmp->ToMatlab();
	if (tmp == m_cellPointers.m_selectionEnd)
	  break;
	tmp = tmp->m_next;
	firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
	wxDataObjectComposite *data = new wxDataObjectComposite;
	data->Add(new wxTextDataObject(result));
	wxTheClipboard->SetData(data);
	wxTheClipboard->Close();
	return true;
  }

  return false;
}

bool Worksheet::CopyTeX()
{
  if (GetActiveCell() != NULL)
    return false;

  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  wxString s;
  Cell *tmp = m_cellPointers.m_selectionStart;

  bool inMath = false;
  wxString label;

  wxConfigBase *config = wxConfig::Get();
  bool wrapLatexMath = true;
  config->Read(wxT("wrapLatexMath"), &wrapLatexMath);

  if (tmp->GetType() != MC_TYPE_GROUP)
  {
    inMath = true;
    if (wrapLatexMath)
      s = wxT("\\[");
  }

  if (tmp->GetType() != MC_TYPE_GROUP)
  {
    while (tmp != NULL)
    {
      s += tmp->ToTeX();
      if (tmp == m_cellPointers.m_selectionEnd)
        break;
      tmp = tmp->m_next;
    }
  }
  else
  {
    GroupCell *gc = dynamic_cast<GroupCell *>(tmp);
    int imgCtr;
    while (gc != NULL)
    {
      s += gc->ToTeX(wxEmptyString,wxEmptyString,&imgCtr);
      if (gc == m_cellPointers.m_selectionEnd)
        break;
      gc = dynamic_cast<GroupCell *>(gc->m_next);
    }
  }

  if ((inMath == true) && (wrapLatexMath))
    s += wxT("\\]");

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxDataObjectComposite *data = new wxDataObjectComposite;
    data->Add(new wxTextDataObject(s));
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyText()
{
  if (GetActiveCell() != NULL)
    return false;

  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  wxString result;
  Cell *tmp = m_cellPointers.m_selectionStart;

  bool firstcell = true;
  while (tmp != NULL)
  {
    if ((tmp->HardLineBreak()) && (!firstcell))
      result += wxT("\n");
    result += tmp->ToString();
    if (tmp == m_cellPointers.m_selectionEnd)
      break;
    tmp = tmp->m_next;
    firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxDataObjectComposite *data = new wxDataObjectComposite;
    data->Add(new wxTextDataObject(result));
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyCells()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  if (wxTheClipboard->Open())
  {
    wxDataObjectComposite *data = new wxDataObjectComposite;
    wxString wxm;
    wxString str;
    wxString rtf = RTFStart();
    GroupCell *tmp = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
    GroupCell *end = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetGroup());

    bool firstcell = true;
    while (tmp != NULL)
    {
      if (!firstcell)
        str += wxT("\n");
      str += tmp->ToString();
      firstcell = false;

      if(m_configuration->CopyRTF())
        rtf += tmp->ToRTF();
      wxm += tmp->ToWXM();

      if (tmp == end)
        break;

      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }

    rtf += wxT("\\par") + RTFEnd();

    if(m_configuration->CopyRTF())
    {
      data->Add(new RtfDataObject(rtf), true);
      data->Add(new RtfDataObject2(rtf));
    }
    data->Add(new wxTextDataObject(str));
    data->Add(new wxmDataObject(wxm));

    if(m_configuration->CopyBitmap())
    {
      Cell *tmp = CopySelection();
      int bitmapScale = 3;
      wxConfig::Get()->Read(wxT("bitmapScale"), &bitmapScale);
      BitmapOut bmp(&m_configuration, bitmapScale);
      if (bmp.SetData(tmp, 4000000))
        data->Add(new wxBitmapDataObject(bmp.GetBitmap()));
    }

#if wxUSE_ENH_METAFILE
    if(m_configuration->CopyEMF())
    {
      Cell *tmp = CopySelection();

      Emfout emf(&m_configuration);
      emf.SetData(tmp);
      data->Add(emf.GetDataObject());
    }
#endif
    if(m_configuration->CopySVG())
    {
      Cell *tmp = CopySelection();

      Svgout svg(&m_configuration);
      svg.SetData(tmp);
      data->Add(svg.GetDataObject());
    }

    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    Recalculate();
    return true;
  }

  return false;
}

bool Worksheet::CanDeleteSelection()
{
  if ((m_cellPointers.m_selectionStart == NULL) || (m_cellPointers.m_selectionEnd == NULL))
    return false;

  return CanDeleteRegion(
          dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup()),
          dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetGroup())
  );
}

void Worksheet::DeleteSelection()
{
  DeleteRegion(
          dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup()),
          dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetGroup())
  );
  TreeUndo_ClearRedoActionList();
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = NULL;
  UpdateTableOfContents();
}

void Worksheet::DeleteCurrentCell()
{
  GroupCell *cellToDelete = NULL;
  if (m_hCaretActive)
    cellToDelete = m_hCaretPosition;
  else
    cellToDelete = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());

  if (cellToDelete)
    DeleteRegion(cellToDelete, cellToDelete);
}

bool Worksheet::CanDeleteRegion(GroupCell *start, GroupCell *end)
{
  if ((start == NULL) || (end == NULL))
    return false;

  GroupCell *tmp = start;

  // We refuse deletion of a cell we are planning to evaluate
  while (tmp != NULL)
  {
    // We refuse deletion of a cell maxima is currently evaluating
    if (tmp == GetWorkingGroup())
      return false;

    if (tmp == end)
      return true;

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  return true;
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end)
{
  TreeUndo_MarkCellsAsAdded(start, end, &treeUndoActions);
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end, std::list<TreeUndoAction *> *undoBuffer)
{
  TreeUndoAction *undoAction = new TreeUndoAction;
  undoAction->m_start = start;
  undoAction->m_newCellsEnd = end;
  undoBuffer->push_front(undoAction);
  TreeUndo_LimitUndoBuffer();
}

void Worksheet::TreeUndo_ClearRedoActionList()
{
  while (!treeRedoActions.empty())
  {
    TreeUndo_DiscardAction(&treeRedoActions);
  }
}

void Worksheet::TreeUndo_ClearUndoActionList()
{
  while (!treeUndoActions.empty())
  {
    TreeUndo_DiscardAction(&treeUndoActions);
  }
}

void Worksheet::TreeUndo_ClearBuffers()
{
  TreeUndo_ClearRedoActionList();
  while (!treeUndoActions.empty())
  {
    TreeUndo_DiscardAction(&treeUndoActions);
  }
  TreeUndo_ActiveCell = NULL;
}

void Worksheet::TreeUndo_DiscardAction(std::list<TreeUndoAction *> *actionList)
{
  if(!actionList->empty())
  {
    do
    {
      TreeUndoAction *Action = actionList->back();
      wxDELETE(Action);
      actionList->pop_back();
    }
    while(!actionList->empty() && (actionList->back()->m_partOfAtomicAction));
  }
}

void Worksheet::TreeUndo_CellLeft()
{
  // If no cell is active we didn't leave a cell and return from this function.
  if (GetActiveCell() == NULL)
  {
    return;
  }

  GroupCell *activeCell = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());

  if (TreeUndo_ActiveCell != NULL)
    wxASSERT_MSG(TreeUndo_ActiveCell == activeCell, _("Bug: Cell left but not entered."));

  // We only can undo a text change if the text has actually changed.
  if (
    (m_treeUndo_ActiveCellOldText            != activeCell->GetEditable()->GetValue()) &&
    (m_treeUndo_ActiveCellOldText + wxT(";") != activeCell->GetEditable()->GetValue()) &&
    (m_treeUndo_ActiveCellOldText.Length() > 1)
    )
  {
    TreeUndoAction *undoAction = new TreeUndoAction;
    wxASSERT_MSG(activeCell != NULL, _("Bug: Text changed, but no active cell."));
    undoAction->m_start = activeCell;
    wxASSERT_MSG(undoAction->m_start != NULL, _("Bug: Trying to record a cell contents change for undo without a cell."));
    undoAction->m_oldText = m_treeUndo_ActiveCellOldText;
    treeUndoActions.push_front(undoAction);
    TreeUndo_LimitUndoBuffer();
    TreeUndo_ClearRedoActionList();
  }
}

void Worksheet::TreeUndo_CellEntered()
{
  if (GetActiveCell())
  {
    if (GetActiveCell()->GetGroup() == NULL)
      return;
    TreeUndo_ActiveCell = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    m_treeUndo_ActiveCellOldText = GetActiveCell()->GetValue();
  }
}

void Worksheet::SetCellStyle(GroupCell *group, GroupType style)
{
  if(group == NULL)
    return;

  wxString cellContents;
  if(group->GetInput())
    cellContents = group->GetInput()->GetValue();
  GroupCell *newGroupCell = new GroupCell(&m_configuration, style,
                                          &m_cellPointers);
  newGroupCell->GetInput()->SetValue(cellContents);
  GroupCell *prev = dynamic_cast<GroupCell *>(group->m_previous);
  DeleteRegion(group,group);
  TreeUndo_AppendAction();
  InsertGroupCells(newGroupCell,prev);
  SetActiveCell(newGroupCell->GetEditable(), false);
  SetSaved(false);
  Recalculate(true);
  RequestRedraw();
}

void Worksheet::DeleteRegion(
        GroupCell *start,
        GroupCell *end
)
{
  DeleteRegion(start, end, &treeUndoActions);
}

void Worksheet::DeleteRegion(GroupCell *start, GroupCell *end, std::list<TreeUndoAction *> *undoBuffer)
{
  m_cellPointers.ResetSearchStart();
  if(end == NULL)
    return;

  // Abort deletion if there is no valid selection or if we cannot
  // delete it.
  if (!CanDeleteRegion(start, end))
    return;

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  //! Set the cursor to a sane place
  SetActiveCell(NULL, false);
  SetSelection(NULL);
  SetHCaret(dynamic_cast<GroupCell *>(start->m_previous));

  // check if chapters or sections need to be renumbered
  bool renumber = false;
  GroupCell *tmp = start;
  while (tmp != NULL)
  {
    m_evaluationQueue.Remove(tmp);

    if (tmp->IsFoldable() || (tmp->GetGroupType() == GC_TYPE_IMAGE))
      renumber = true;

    // Don't keep cached versions of scaled images around in the undo buffer.
    if (tmp->GetOutput())
      tmp->GetOutput()->ClearCacheList();

    // Tell the cells we don't want to keep pointers to them active
    tmp->MarkAsDeleted();

    if (tmp == end)
      break;
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  GroupCell *cellBeforeStart = dynamic_cast<GroupCell *>(start->m_previous);;

  // If the selection ends with the last file of the file m_last has to be
  // set to the last cell that isn't deleted.
  if (end == m_last)
    m_last = cellBeforeStart;

  // Unlink the to-be-deleted cells from the worksheet.
  if(start->m_previous == NULL)
    m_tree = dynamic_cast<GroupCell *>(end->m_next);
  else
    start->m_previous->m_next = start->m_previous->m_nextToDraw = end->m_next;

  if (end->m_next != NULL)
    end->m_next->m_previous = end->m_next->m_previousToDraw = start->m_previous;
  else
  {
    if(start->m_previous != NULL)
      start->m_previous->m_next = start->m_previous->m_nextToDraw =NULL;
  }

  // Add an "end of tree" marker to both ends of the list of deleted cells
  end->m_next = end->m_nextToDraw = NULL;
  start->m_previous = start->m_previousToDraw = NULL;

  // Do we have an undo buffer for this action?
  if (undoBuffer != NULL)
  {
    // We have an undo buffer => add the deleted cells there
    TreeUndoAction *undoAction = new TreeUndoAction;
    undoAction->m_start = cellBeforeStart;
    undoAction->m_oldCells = start;
    undoAction->m_newCellsEnd = NULL;
    undoBuffer->push_front(undoAction);
    TreeUndo_LimitUndoBuffer();
  }
  else
  {
    // We don't habe an undo buffer => really delete the cells
    wxDELETE(start);
  }


  if (renumber)
    NumberSections();
  UpdateTableOfContents();
  Recalculate();
  RequestRedraw();
  m_saved = false;
}

void Worksheet::SetAnswer(wxString answer)
{
  GroupCell *answerCell = GetWorkingGroup();
  if(answerCell == NULL)
    return;

  if(answer.IsEmpty())
    return;

  if(m_lastQuestion.IsEmpty())
    return;
  
  answerCell->SetAnswer(m_lastQuestion, answer);
}

void Worksheet::OpenQuestionCaret(wxString txt)
{
  GroupCell *group = GetWorkingGroup(true);
  wxASSERT_MSG(group != NULL, _("Bug: Got a question but no cell to answer it in"));

  if(group == NULL)
    return;

  // We are leaving the input part of the current cell in this step.
  TreeUndo_CellLeft();

  // We don't need an undo action for the thing we will do now.
  TreeUndo_ActiveCell = NULL;

  // Make sure that the cell containing the question is visible
  if (group->RevealHidden())
  {
    FoldOccurred();
    Recalculate(group, true);
  }

  // If we still haven't a cell to put the answer in we now create one.
  if (m_cellPointers.m_answerCell == NULL)
  {
    m_cellPointers.m_answerCell = new EditorCell(
      group,
      &m_configuration,
      &m_cellPointers);
    m_cellPointers.m_answerCell->SetType(MC_TYPE_INPUT);
    bool autoEvaluate = false;
    if(txt == wxEmptyString)
    {
      txt = group->GetAnswer(m_lastQuestion);
      if(!txt.IsEmpty())
        autoEvaluate = group->AutoAnswer();
    }
    m_cellPointers.m_answerCell->SetValue(txt);
    dynamic_cast<EditorCell *>(m_cellPointers.m_answerCell)->CaretToEnd();

    group->AppendOutput(m_cellPointers.m_answerCell);

    // If we filled in an answer and "AutoAnswer" is true we issue an evaluation event here.
    if(autoEvaluate)
    {
      wxMenuEvent *EvaluateEvent = new wxMenuEvent(wxEVT_MENU, wxMaximaFrame::menu_evaluate);
      GetParent()->GetEventHandler()->QueueEvent(EvaluateEvent);
    }
    RecalculateForce();
  }
  // If the user wants to be automatically scrolled to the cell evaluation takes place
  // we scroll to this cell.
  if (FollowEvaluation())
    SetActiveCell(dynamic_cast<EditorCell *>(m_cellPointers.m_answerCell), false);

  RequestRedraw();
}

void Worksheet::OpenHCaret(wxString txt, GroupType type)
{
  CloseAutoCompletePopup();

  // if we are inside cell maxima is currently evaluating
  // bypass normal behaviour and insert an EditorCell into
  // the output of the working group.
  if (GetWorkingGroup())
  {
    if (GetActiveCell() != NULL)
    {
      if ((GetActiveCell()->GetGroup() == GetWorkingGroup()) && (m_questionPrompt))
      {
        OpenQuestionCaret(txt);
        return;
      }
    }
    if (m_hCaretPosition != NULL)
    {
      if ((m_hCaretPosition == GetWorkingGroup()->m_next) && (m_questionPrompt))
      {
        OpenQuestionCaret(txt);
        return;
      }
    }
  }
  // set m_hCaretPosition to a sensible value
  if (GetActiveCell() != NULL)
  {
    SetHCaret(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
  }
  else if (m_cellPointers.m_selectionStart != NULL)
    SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup()));

  if (!m_hCaretActive)
  {
    if (m_last == NULL)
      return;
    SetHCaret(m_last);
  }

  // insert a new group cell
  GroupCell *group = new GroupCell(&m_configuration, type, &m_cellPointers, txt);
  // check how much to unfold for this type
  if (m_hCaretPosition != NULL)
  {
    while (IsLesserGCType(type, m_hCaretPosition->GetGroupType()))
    {
      GroupCell *result = m_hCaretPosition->Unfold();
      if (result == NULL) // assumes that unfold sets hcaret to the end of unfolded cells
        break; // unfold returns NULL when it cannot unfold
      SetHCaret(result);
    }
  }
  if((type == GC_TYPE_CODE) && !m_configuration->ShowCodeCells())
  {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }

  InsertGroupCells(group, m_hCaretPosition);
  // Recalculate(group, false);

  // activate editor
  SetActiveCell(group->GetEditable(), false);
  if (GetActiveCell() != NULL)
    GetActiveCell()->ClearUndo();
  // If we just have started typing inside a new cell we don't want the screen
  // to scroll away.
  ScrolledAwayFromEvaluation();

  // Here we tend to get unacceptably long delays before the display is
  // refreshed by the idle loop => Trigger the refresh manually.
  ForceRedraw();
}

void Worksheet::Evaluate()
{
  wxMenuEvent *EvaluateEvent = new wxMenuEvent(wxEVT_MENU, wxMaximaFrame::menu_evaluate);
  GetParent()->GetEventHandler()->QueueEvent(EvaluateEvent);
}

/***
 * Support for copying and deleting with keyboard
 */
void Worksheet::OnKeyDown(wxKeyEvent &event)
{
  ClearNotification();

  // Track the activity of the keyboard. Setting the keyboard
  // to inactive again is done in wxMaxima.cpp
  m_keyboardInactiveTimer.StartOnce(10000);

  // If Alt and Ctrl are down at the same time we are almost entirely sure that
  // this is a hotkey we need to pass to the main application. One exception is
  // curly brackets in - I think it was france.
  if (event.ControlDown() && event.AltDown())
  {
    if (
            (event.GetUnicodeKey() == wxT('{')) ||
            (event.GetUnicodeKey() == wxT('}'))
            )
    {
      event.Skip();
      return;
    }
  }

  if (m_autocompletePopup != NULL)
  {
    m_autocompletePopup->OnKeyDown(event);
    return;
  }

  // Alt+Up and Alt+Down are hotkeys, too.
  if(event.AltDown() && ((event.GetKeyCode()==WXK_UP)||(event.GetKeyCode()==WXK_DOWN)))
    {
      event.Skip();
      return;
    }

  // Handling of the keys this class has to handle
  switch (event.GetKeyCode())
  {

    case WXK_DELETE:
    case WXK_NUMPAD_DELETE:
      if (event.ShiftDown())
      {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_cut);
        GetParent()->ProcessWindowEvent(ev);
      }
      else if (CanDeleteSelection())
      {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
        GetParent()->ProcessWindowEvent(ev);
      }
      else
        event.Skip();
      break;

    case WXK_INSERT:
      if (event.ControlDown())
      {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_copy);
        GetParent()->ProcessWindowEvent(ev);
      }
      else if (event.ShiftDown())
      {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_paste);
        GetParent()->ProcessWindowEvent(ev);
      }
      else
        event.Skip();
      break;

    case WXK_BACK:
      if ((event.ControlDown()) && (event.ShiftDown()))
        DeleteCurrentCell();
      else
      {
        if (CanDeleteSelection())
        {
          wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, popid_delete);
          GetParent()->ProcessWindowEvent(ev);
        }
        else
          event.Skip();
      }
      break;

    case WXK_NUMPAD_ENTER:
      if (event.ControlDown() && event.ShiftDown())
      {
        // Queue an evaluate event for the window containing this worksheet.
        wxCommandEvent *evaluateEvent = new wxCommandEvent;
        evaluateEvent->SetEventType(wxEVT_MENU);
        evaluateEvent->SetId(popid_evaluate_section);
        GetParent()->GetEventHandler()->QueueEvent(evaluateEvent);
      }
      else
      {
        if (GetActiveCell() != NULL && GetActiveCell()->GetType() == MC_TYPE_INPUT)
          Evaluate();
        else if (m_hCaretActive)
          OpenHCaret(wxT("%"));
        else
          event.Skip();
      }
      break;

    case WXK_RETURN:
      if (event.ControlDown() && event.ShiftDown())
      {
        // Queue an evaluate event for the window containing this worksheet.
        wxCommandEvent *evaluateEvent = new wxCommandEvent;
        evaluateEvent->SetEventType(wxEVT_MENU);
        evaluateEvent->SetId(popid_evaluate_section);
        GetParent()->GetEventHandler()->QueueEvent(evaluateEvent);
      }
      else
      {
        if (GetActiveCell() == NULL)
        {
          // We are instructed to evaluate something - but we aren't inside a cell.
          // Let's see if there are selected cells we can evaluate.
          if (CellsSelected())
          {
            bool enterEvaluates = false;
            bool controlOrShift = event.ControlDown() || event.ShiftDown();
            wxConfig::Get()->Read(wxT("enterEvaluates"), &enterEvaluates);
            if ((!enterEvaluates && controlOrShift) ||
                (enterEvaluates && !controlOrShift))
            { // shift-enter pressed === menu_evaluate event
              Evaluate();
            }
            else
              event.Skip();
          }
          else
          {
            // We are instructed to evaluate something - but we aren't inside a cell
            // and we haven't selected one. Let's see if we are in front of a cell
            // we can jump into.
            if (m_hCaretActive)
            {
              if (GetHCaret())
              {
                if (GetHCaret()->m_next)
                {
                  SetActiveCell(dynamic_cast<GroupCell *>(GetHCaret()->m_next)->GetEditable());

                  // User has in a way moved the cursor manually and definitively doesn't want
                  // to be returned to the end of the cell being evaluated if the evaluation
                  // stops before the "evaluate" key can be pressed again.
                  FollowEvaluation(false);
                }
              }
              else
              {
                if (m_tree)
                {
                  SetActiveCell(m_tree->GetEditable());
                  ScrollToCaret();
                  // User has in a way moved the cursor manually and definitively doesn't want
                  // to be returned to the end of the cell being evaluated if the evaluation
                  // stops before the "evaluate" key can be pressed again.
                  FollowEvaluation(false);
                }
              }
            }
            else
              event.Skip();
          }
        }
        else
        {
          // User pressed "Evaluate" inside an active cell.
          if (GetActiveCell()->GetType() != MC_TYPE_INPUT)
          {
            // User pressed enter inside a cell that doesn't contain code.

            if ((event.ControlDown()) || (event.ShiftDown()))
            { // shift-enter pressed => The user doesn't want to make an ordinary
              // line break.
              //
              // In this cell there isn't anything to evaluate. But we can jump to the next
              // cell. Perhaps there is something there...
              if (GetActiveCell()->GetGroup()->m_next)
              {
                // Jump to the next cell.
                SetActiveCell(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()->m_next)->GetEditable());
                ScrollToCaret();
              }
              else
                // No next cell -> Jump to the end of the document.
                SetHCaret(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
            }
            else
            {
              GetActiveCell()->ProcessEvent(event);
              // Recalculate(dynamic_cast<GroupCell*>(GetActiveCell()->GetGroup()),false);
              GroupCell *parent = dynamic_cast<GroupCell*>(GetActiveCell()->GetGroup());
              parent->InputHeightChanged();
              RequestRedraw();
            }
          }
          else
          {
            // User pressed enter inside a cell that does contain code.

            bool enterEvaluates = false;
            bool controlOrShift = event.ControlDown() || event.ShiftDown();
            wxConfig::Get()->Read(wxT("enterEvaluates"), &enterEvaluates);
            if ((!enterEvaluates && controlOrShift) ||
                (enterEvaluates && !controlOrShift))
            { // shift-enter pressed === menu_evaluate event
              GroupCell *currentGroup = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
              if ((m_evaluationQueue.IsLastInQueue(currentGroup)) &&
                  (!QuestionPending()) && (
                          (GetWorkingGroup() != currentGroup) ||
                          !currentGroup->GetInput()->ContainsChanges())
                      )
              {
                // User tries to evaluate a cell that already is to be evaluated as the
                // last element of the evaluation queue => It is most probable that
                // the intention is to evaluate more than one cell => Move the cursor
                // forward a bit.
                //
                // If the current cell isn't the last cell in the evaluation queue
                // there is a chance tht the user has decided to re-evaluate something
                // So we just do what we are requested to in this case.
                SetHCaret(currentGroup);
              }
              else
              {
                // Finally evaluate the cell
                Evaluate();
              }
            }
            else
            {
              event.Skip();
              // Sometimes and only in certain zoom factors pressing enter doesn't change the
              // size of an EditorCell. Let's see if that helps...
              GetActiveCell()->RecalculateWidths(m_configuration->GetDefaultFontSize());
              Recalculate(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
              RequestRedraw();
            }
          }
        }
      }
      break;

    case WXK_ESCAPE:
      event.Skip();
      break;

    default:
      event.Skip();
  }
}

bool Worksheet::GCContainsCurrentQuestion(GroupCell *cell)
{
  if (GetWorkingGroup())
    return ((cell == GetWorkingGroup()) && m_questionPrompt);
  else
    return false;
}

void Worksheet::QuestionAnswered()
{
  if((m_cellPointers.m_answerCell != NULL) || m_questionPrompt)
  {
    SetActiveCell(NULL);
    GroupCell *wg = GetWorkingGroup(true);
    if(wg != NULL)
    {
      SetHCaret(wg);
      ScrollToCaret();
    }
  }
  m_cellPointers.m_answerCell = NULL;
  m_questionPrompt = false;
}

GroupCell *Worksheet::StartOfSectioningUnit(GroupCell *start)
{
  wxASSERT(start != NULL);
  // If the current cell is a sectioning cell we return this cell
  if (IsLesserGCType(GC_TYPE_TEXT, start->GetGroupType()))
    return start;

  GroupCell *end = start;
  while ((end != NULL) && (!IsLesserGCType(GC_TYPE_TEXT, end->GetGroupType())))
  {
    end = dynamic_cast<GroupCell *>(end->m_previous);
  }

  // Return the sectioning cell we found - or the current cell which is the
  // next equivalent to a sectioning cell.
  if (end != NULL)
    return end;
  else
    return start;
}

GroupCell *Worksheet::EndOfSectioningUnit(GroupCell *start)
{
  wxASSERT(start != NULL);
  GroupCell *sectionbegin = StartOfSectioningUnit(start);
  int endgrouptype = sectionbegin->GetGroupType();

  // Begin with the cell after the start cell - that might contain a section
  // start of any sorts.
  GroupCell *end = dynamic_cast<GroupCell *>(start->m_next);
  if (end == NULL)
    return start;

  // Find the end of the chapter/section/...
  while ((end->m_next != NULL) && (IsLesserGCType(end->GetGroupType(), endgrouptype)))
  {
    end = dynamic_cast<GroupCell *>(end->m_next);
  }
  return end;
}

void Worksheet::UpdateConfigurationClientSize()
{
  m_configuration->SetClientWidth(GetClientSize().GetWidth() -
                                  m_configuration->GetCellBracketWidth() -
                                  m_configuration->GetBaseIndent());
  m_configuration->SetClientHeight(GetClientSize().GetHeight());
}

/****
 * OnCharInActive is called when we have a wxKeyEvent and
 * an EditorCell is active.
 *
 * OnCharInActive sends the event to the active EditorCell
 * and then updates the window.
 */
void Worksheet::OnCharInActive(wxKeyEvent &event)
{
  bool needRecalculate = false;

  if ((event.GetKeyCode() == WXK_UP ||
       event.GetKeyCode() == WXK_PAGEUP
#ifdef WXK_PRIOR
                  || (event.GetKeyCode() != WXK_PRIOR)
#endif
#ifdef WXK_NUMPAD_PRIOR
                  || (event.GetKeyCode() != WXK_NUMPAD_PRIOR)
#endif
        ) && GetActiveCell()->CaretAtStart())
  {
    // Get the first previous cell that isn't hidden
    GroupCell *previous = dynamic_cast<GroupCell *>((GetActiveCell()->GetGroup())->m_previous);
    while ((previous != NULL) && (previous->GetMaxDrop() == 0))
      previous = dynamic_cast<GroupCell *>(previous->m_previous);

    if (event.ShiftDown())
    {
      SetSelection(previous, dynamic_cast<GroupCell *>((GetActiveCell()->GetGroup())));
      m_hCaretPosition = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
      m_hCaretPositionEnd = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
      m_hCaretPositionStart = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd);

      GetActiveCell()->KeyboardSelectionStartedHere();
      GetActiveCell()->SelectNone();
      SetActiveCell(NULL);
      RequestRedraw(m_hCaretPosition);
    }
    else
    {
      if (GCContainsCurrentQuestion(previous))
      {
        // The user moved into the cell maxima has asked a question in.
        FollowEvaluation(true);
        OpenQuestionCaret();
        return;
      }
      else
        SetHCaret(previous);
    }

    // Re-calculate the table of contents as we possibly leave a cell that is
    // to be found here.
    UpdateTableOfContents();

    // If we scrolled away from the cell that is currently being evaluated
    // we need to enable the button that brings us back
    ScrolledAwayFromEvaluation();

    return;
  }

  if ((event.GetKeyCode() == WXK_DOWN ||
       event.GetKeyCode() == WXK_PAGEDOWN
#ifdef WXK_NEXT
                  || (event.GetKeyCode() != WXK_NEXT)
#endif
#ifdef WXK_NUMPAD_NEXT
                  || (event.GetKeyCode() != WXK_NUMPAD_NEXT)
#endif
      ) && GetActiveCell()->CaretAtEnd())
  {
    // Get the first next cell that isn't hidden
    GroupCell *start = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    while ((start != NULL) && (start->m_next != NULL) && (start->m_next->GetMaxDrop() == 0))
      start = dynamic_cast<GroupCell *>(start->m_next);

    if (event.ShiftDown())
    {
      GroupCell *end = start;
      if (end->m_next != NULL)
        end = dynamic_cast<GroupCell *>(end->m_next);

      SetSelection(start, end);
      m_hCaretPosition = start;
      m_hCaretPositionStart = start;
      m_hCaretPositionEnd = end;

      GetActiveCell()->KeyboardSelectionStartedHere();
      GetActiveCell()->SelectNone();
      SetActiveCell(NULL);
      ScrolledAwayFromEvaluation();
      RequestRedraw();
    }
    else
    {
      if (GCContainsCurrentQuestion(start))
      {
        // The user moved into the cell maxima has asked a question in.
        FollowEvaluation(true);
        OpenQuestionCaret();
        return;
      }
      else
        SetHCaret(start);
    }
    // Re-calculate the table of contents as we possibly leave a cell that is
    // to be found here.
    UpdateTableOfContents();
    ScrolledAwayFromEvaluation();

    return;
  }

  m_cellPointers.ResetKeyboardSelectionStart();

  // an empty cell is removed on backspace/delete
  if ((event.GetKeyCode() == WXK_BACK || event.GetKeyCode() == WXK_DELETE || event.GetKeyCode() == WXK_NUMPAD_DELETE) &&
      GetActiveCell()->GetValue() == wxEmptyString)
  {
    SetSelection(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
    DeleteSelection();
    return;
  }

  // CTRL+"s deactivates on MAC
  if (GetActiveCell() == NULL)
    return;

  ///
  /// send event to active cell
  ///
  EditorCell *activeCell = GetActiveCell();
  wxString oldValue = activeCell->GetValue();

  switch(event.GetKeyCode())
  {
  case WXK_LEFT:
    if(GetActiveCell()->CaretAtStart())
    {
      GroupCell *newGroup = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()->m_previous);
      while ((newGroup != NULL) && (newGroup->GetMaxDrop() == 0))
        newGroup = dynamic_cast<GroupCell *>(newGroup->m_previous);
      SetHCaret(newGroup);
      return;
    }
    else
      GetActiveCell()->ProcessEvent(event);
    break;
  case WXK_RIGHT:
    if(GetActiveCell()->CaretAtEnd())
    {
      GroupCell *newGroup = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
      while ((newGroup != NULL) && (newGroup->m_next != NULL) &&
             (newGroup->m_next->GetMaxDrop() == 0))
        newGroup = dynamic_cast<GroupCell *>(newGroup->m_next);
      SetHCaret(newGroup);
      return;
    }
    else
      GetActiveCell()->ProcessEvent(event);
    break;
  default:
  {
    GetActiveCell()->ProcessEvent(event);
    GroupCell *parent = dynamic_cast<GroupCell*>(GetActiveCell()->GetGroup());
    parent->InputHeightChanged();
    RequestRedraw();
  }
  }

  // Update title and toolbar in order to reflect the "unsaved" state of the worksheet.
  if (IsSaved() && activeCell->GetValue() != oldValue)
  {
    m_saved = false;
    RequestRedraw();
  }
  // The keypress might have moved the cursor off-screen.
  ScrollToCaret();

  m_blinkDisplayCaret = true;

  UpdateConfigurationClientSize();

  if (activeCell->IsDirty())
  {
    m_saved = false;

    int height = activeCell->GetHeight();
    //   int fontsize = m_configuration->GetDefaultFontSize();
    int fontsize = m_configuration->GetDefaultFontSize();

    GetActiveCell()->RecalculateWidths(wxMax(fontsize, MC_MIN_SIZE));
    GetActiveCell()->RecalculateHeight(wxMax(fontsize, MC_MIN_SIZE));

    if (height != GetActiveCell()->GetHeight() ||
        GetActiveCell()->GetWidth() + GetActiveCell()->GetCurrentPoint().x >=
        GetClientSize().GetWidth() - m_configuration->GetCellBracketWidth() - m_configuration->GetBaseIndent())
      needRecalculate = true;
  }

  /// If we need to recalculate then refresh the window
  if (needRecalculate)
  {
    GroupCell *group = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    group->ResetData();
    if (GetActiveCell()->CheckChanges() &&
        (group->GetGroupType() == GC_TYPE_CODE) &&
        (GetActiveCell() == group->GetEditable()))
      group->ResetInputLabel();
//    Recalculate(group, false);
    RequestRedraw(group);
  }
  else
  {
    if (GetActiveCell()->m_selectionChanged)
    {
      RequestRedraw(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
    }
      /// Otherwise refresh only the active cell
    else
    {
      wxRect rect;
      if (GetActiveCell()->CheckChanges())
      {
        GroupCell *group = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
        if ((group->GetGroupType() == GC_TYPE_CODE) &&
            (GetActiveCell() == group->GetEditable()))
          group->ResetInputLabel();
        rect = group->GetRect();
        rect.width = GetVirtualSize().x;
      }
      else
      {
        rect = GetActiveCell()->GetRect();
        rect.width = GetVirtualSize().x;
      }
      rect.x -= m_configuration->GetCursorWidth() / 2;
      RequestRedraw(rect);
    }
  }

  if (GetActiveCell())
  {
    if (IsLesserGCType(GC_TYPE_TEXT, dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup())->GetGroupType()))
      UpdateTableOfContents();
  }
}

void Worksheet::SelectWithChar(int ccode)
{
  ScrolledAwayFromEvaluation();
  // start making a selection
  // m_hCaretPositionStart is the first group selected
  // m_hCaretPositionEnd is tle last group selected
  // we always move m_hCaretPosition
  if (m_hCaretPositionStart == NULL || m_hCaretPositionEnd == NULL)
  {
    if (m_hCaretPosition != NULL)
      m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPosition;
    else
      m_hCaretPositionStart = m_hCaretPositionEnd = m_tree;

    if (m_hCaretPositionStart == NULL)
      return;

    if (ccode == WXK_DOWN && m_hCaretPosition != NULL && m_hCaretPositionStart->m_next != NULL)
    {
      m_hCaretPositionStart = m_hCaretPositionEnd = dynamic_cast<GroupCell *>(m_hCaretPositionStart->m_next);
      while((m_hCaretPositionStart != NULL) && (m_hCaretPositionStart->GetMaxDrop() == 0) &&
            (m_hCaretPositionStart->m_next != 0))
        m_hCaretPositionStart = m_hCaretPositionEnd =
          dynamic_cast<GroupCell *>(m_hCaretPositionStart->m_next);
    }
  }
  else if (ccode == WXK_UP)
  {
    if ((KeyboardSelectionStart() != NULL) &&
        (m_hCaretPositionEnd == dynamic_cast<GroupCell *>(KeyboardSelectionStart()->GetGroup()->m_next)))
    {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      SetSelection(NULL);
      KeyboardSelectionStart()->ReturnToSelectionFromBot();
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
    }
    else
    {
      // extend / shorten up selection
      GroupCell *prev = dynamic_cast<GroupCell *>(m_hCaretPositionEnd->m_previous);
      while((prev != NULL) && (prev->GetMaxDrop() == 0))
        prev = dynamic_cast<GroupCell *>(prev->m_previous);

      if (prev != NULL)
      {
        if (m_hCaretPosition != NULL && m_hCaretPosition->m_next == m_hCaretPositionEnd)
          m_hCaretPositionStart = prev;
        if (m_hCaretPositionEnd != NULL)
          m_hCaretPositionEnd = prev;
      }
      if (m_hCaretPositionEnd != NULL)
        ScheduleScrollToCell(m_hCaretPositionEnd, false);
    }
  }
  else
  {
    // We arrive here on WXK_DOWN.
    if (
      (KeyboardSelectionStart() != NULL) &&
      (m_hCaretPositionEnd == dynamic_cast<GroupCell *>(KeyboardSelectionStart()->GetGroup()->m_previous))
      )
    {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      SetSelection(NULL);
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
      KeyboardSelectionStart()->ReturnToSelectionFromTop();
    }
    else
    {
      // extend/shorten down selection
      GroupCell *nxt = dynamic_cast<GroupCell *>(m_hCaretPositionEnd->m_next);
      while((nxt != NULL) && (nxt->GetMaxDrop() == 0))
        nxt = dynamic_cast<GroupCell *>(nxt->m_next);

      if (nxt != NULL)
      {
        if (m_hCaretPosition == m_hCaretPositionEnd)
          m_hCaretPositionStart = nxt;
        if (m_hCaretPositionEnd != NULL)
          m_hCaretPositionEnd = nxt;
      }
      if (m_hCaretPositionEnd != NULL)
        ScheduleScrollToCell(m_hCaretPositionEnd, false);
    }
  }

  if ((m_hCaretPositionStart) && (m_hCaretPositionEnd))
  {
    // m_hCaretPositionStart can be above or below m_hCaretPositionEnd
    if (m_hCaretPositionStart->GetCurrentY() < m_hCaretPositionEnd->GetCurrentY())
    {
      SetSelection(m_hCaretPositionStart, m_hCaretPositionEnd);
    }
    else
    {
      SetSelection(m_hCaretPositionEnd, m_hCaretPositionStart);
    }
  }
  RequestRedraw();
}

void Worksheet::SelectEditable(EditorCell *editor, bool top)
{
  if ((editor != NULL) &&
      (
              (m_configuration->ShowCodeCells()) ||
              (editor->GetType() != MC_TYPE_INPUT)
      )
          )
  {
    SetActiveCell(editor, false);
    m_hCaretActive = false;

    if (top)
      editor->CaretToStart();
    else
      editor->CaretToEnd();

    if (editor->GetWidth() == -1)
      Recalculate(dynamic_cast<GroupCell *>(editor->GetGroup()));

    ScrollToCaret();
  }
  else
  { // can't get editor... jump over to the next cell..
    if (top)
    {
      if (m_hCaretPosition == NULL)
        SetHCaret(m_tree);
      else
      {
        if (m_hCaretPosition->m_next != NULL)
        {
          SetHCaret(dynamic_cast<GroupCell *>( m_hCaretPosition->m_next));
        }
        else
          SetHCaret(m_last);
      }
    }
    else
      SetHCaret(dynamic_cast<GroupCell *>( m_hCaretPosition->m_previous));
  }
  RequestRedraw();
}

void Worksheet::OnCharNoActive(wxKeyEvent &event)
{
  int ccode = event.GetKeyCode();
  wxString txt; // Usually we open an Editor Cell with initial content txt

  // If Shift is down we are selecting with WXK_UP and WXK_DOWN
  if (event.ShiftDown() && (ccode == WXK_UP || ccode == WXK_DOWN))
  {
    SelectWithChar(ccode);
    return;
  }

  m_cellPointers.ResetKeyboardSelectionStart();

  if (m_cellPointers.m_selectionStart != NULL &&
      m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE &&
      ccode == WXK_SPACE)
  {
    dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart)->AnimationRunning(
      !dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart)->AnimationRunning());
    return;
  }

  // Remove selection with shift+WXK_UP/WXK_DOWN
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  switch (ccode)
  {
    case WXK_PAGEUP:
#ifdef WXK_PRIOR
      case WXK_PRIOR: // Is on some systems a replacement for WXK_PAGEUP
#endif
#ifdef WXK_NUMPAD_PRIOR
      case WXK_NUMPAD_PRIOR:
#endif
    {
      wxPoint topleft;
      int width;
      int height;
      CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
      GroupCell *CellToScrollTo = m_last;

      GetClientSize(&width, &height);

      // Scroll as many cells as are needed to make sure that
      // the new cell is the upmost cell that begins on the new page.
      while (CellToScrollTo != NULL)
      {
        CellToScrollTo = dynamic_cast<GroupCell *>(CellToScrollTo->m_previous);

        if ((CellToScrollTo != NULL) && (CellToScrollTo->GetRect().GetTop() < topleft.y - height))
          break;
      }
      // We want to put the cursor in the space above the cell we found.
      if (CellToScrollTo != NULL)
        CellToScrollTo = dynamic_cast<GroupCell *>(CellToScrollTo->m_previous);

      ScrolledAwayFromEvaluation();
      SetHCaret(CellToScrollTo);
      break;
    }
#ifdef WXK_NEXT   // Is on some systems a replacement for WXK_PAGEDOWN
      case WXK_NEXT:
#endif
#ifdef WXK_NUMPAD_NEXT
      case WXK_NUMPAD_NEXT:
#endif
    case WXK_PAGEDOWN:
    {
      wxPoint topleft;
      int width;
      int height;
      CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
      GroupCell *CellToScrollTo = m_tree;
      GetClientSize(&width, &height);

      // Make sure we scroll at least one cell
      if (CellToScrollTo != NULL)
        CellToScrollTo = dynamic_cast<GroupCell *>(CellToScrollTo->m_next);

      // Now scroll far enough that the bottom of the cell we reach is the last
      // bottom of a cell on the new page.
      while ((CellToScrollTo != NULL) && ((CellToScrollTo != m_last)))
      {
        if (CellToScrollTo->GetRect().GetBottom() > topleft.y + 2 * height)
          break;
        else
          CellToScrollTo = dynamic_cast<GroupCell *>(CellToScrollTo->m_next);
      }
      SetHCaret(CellToScrollTo);
      ScrollToCaret();
      ScrolledAwayFromEvaluation();
      break;
    }
      // These are ingored
    case WXK_WINDOWS_LEFT:
    case WXK_WINDOWS_RIGHT:
    case WXK_WINDOWS_MENU:
    case WXK_COMMAND:
    case WXK_START:
      event.Skip();
      break;

    case WXK_HOME:
      // Returning to the beginning of the worksheet on pressing POS1 isn't what one
      // would expect from an ordinary editor so we ignore the key if it is.
      // pressed alone. But on pressing Ctrl+POS1 one would expect to end up at the
      // beginning of the document...

      if (event.CmdDown())
      {
        GroupCell *oldCell = GetHCaret();
        SetHCaret(NULL);
        if (m_tree != NULL)
          ScheduleScrollToCell(m_tree, true);
        if (event.ShiftDown())
        {
          SetSelection(m_tree, oldCell);
          m_hCaretPositionStart = oldCell;
          m_hCaretPositionEnd = m_tree;
        }
        ScrolledAwayFromEvaluation();
      }
      break;

    case WXK_END:
      // Jumping to the end of the worksheet on pressing End isn't what one
      // would expect from an ordinary editor so we ignore the key if it is.
      // pressed alone. But on pressing Ctrl+End one would expect to end up at the
      // end of the document...

      if (event.CmdDown())
      {
        GroupCell *oldCell = GetHCaret();
        SetHCaret(m_last);
        if (event.ShiftDown())
        {
          if (oldCell != NULL)
            oldCell = dynamic_cast<GroupCell *>(oldCell->m_next);
          SetSelection(oldCell, m_last);
          m_hCaretPositionStart = oldCell;
          m_hCaretPositionEnd = m_last;
        }
        ScrolledAwayFromEvaluation();
      }
      break;

    case WXK_BACK:
      if (m_hCaretPosition != NULL)
      {
        SetSelection(m_hCaretPosition);
        RequestRedraw();
        m_hCaretActive = false;
        return;
      }
      break;

    case WXK_DELETE:
    case WXK_NUMPAD_DELETE:
      if (m_hCaretPosition == NULL)
      {
        if (m_tree != NULL)
        {
          SetSelection(m_tree);
          m_hCaretActive = false;
          return;
        }
        ScrolledAwayFromEvaluation();
      }
      else if (m_hCaretPosition->m_next != NULL)
      {
        SetSelection(dynamic_cast<GroupCell *>(m_hCaretPosition->m_next));
        m_hCaretActive = false;
        return;
      }
      break;

    case WXK_UP:
    case WXK_LEFT:
      if (CanAnimate())
      {
        SlideShow *slideShow = dynamic_cast<SlideShow *>(GetSelectionStart());
        slideShow->AnimationRunning(false);
        StepAnimation(-1);
        break;
      }

      ScrolledAwayFromEvaluation(true);
      if (m_hCaretActive)
      {
        if (m_cellPointers.m_selectionStart != NULL)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart);
            if (tmp->m_previous)
            {
              do tmp = dynamic_cast<GroupCell *>(tmp->m_previous);
              while (
                      (tmp->m_previous) && (
                              (tmp->GetGroupType() != GC_TYPE_TITLE) &&
                              (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                              (tmp->GetGroupType() != GC_TYPE_SUBSECTION)
                      )
                      );
              if (dynamic_cast<GroupCell *>(tmp)->GetEditable() != NULL)
                SetHCaret(dynamic_cast<GroupCell *>(tmp));
            }
            else
            {
              if (
                      (m_hCaretPosition != NULL) &&
                      (dynamic_cast<GroupCell *>(m_hCaretPosition)->GetEditable() != NULL)
                      )
                SelectEditable(dynamic_cast<GroupCell *>(tmp)->GetEditable(), false);
            }
          }
          else
            SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup()->m_previous));
        }
        else if (m_hCaretPosition != NULL)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_hCaretPosition;
            if (tmp->m_previous)
            {
              do tmp = dynamic_cast<GroupCell *>(tmp->m_previous);
              while (
                      (tmp->m_previous) && (
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_TITLE) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SECTION) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SUBSECTION)
                      )
                      );
              SetHCaret(tmp);
            }
            else if (dynamic_cast<GroupCell *>(tmp)->GetEditable() != NULL)
              SelectEditable(dynamic_cast<GroupCell *>(tmp)->GetEditable(), false);
          }
          else
          {
            if (
                    (m_hCaretPosition != NULL) &&
                    (dynamic_cast<GroupCell *>(m_hCaretPosition)->GetEditable() != NULL)
                    )
              SelectEditable(dynamic_cast<GroupCell *>(m_hCaretPosition)->GetEditable(), false);
          }
        }
//      This allows to use WXK_UP in order to move the cursorup from the worksheet to the toolbar.
//      But a down key doesn't go back from the toolbar to the worksheet this
//      is more a nuisance than a feature.
//        else
//          event.Skip();
      }
      else if (m_cellPointers.m_selectionStart != NULL)
        SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup()->m_previous));
      else if (!ActivatePrevInput())
        event.Skip();
      break;

    case WXK_DOWN:
    case WXK_RIGHT:
      if (CanAnimate())
      {
        SlideShow *slideShow = dynamic_cast<SlideShow *>(GetSelectionStart());
        slideShow->AnimationRunning(false);
        StepAnimation(1);
        break;
      }
      ScrolledAwayFromEvaluation(true);
      if (m_hCaretActive)
      {
        if (m_cellPointers.m_selectionEnd != NULL)
        {
          if (event.CmdDown())
          {
            Cell *tmp = m_cellPointers.m_selectionEnd;
            if (tmp->m_next)
            {
              do tmp = dynamic_cast<GroupCell *>(tmp->m_next);
              while (
                (tmp->m_next) && (
                  (
                    (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_TITLE) &&
                    (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SECTION) &&
                    (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SUBSECTION)
                    ) ||
                  (dynamic_cast<GroupCell *>(tmp->m_next)->GetMaxDrop() == 0)
                  )
                );
              SetHCaret(dynamic_cast<GroupCell *>(tmp));
            }
            else
            {
              SelectEditable(dynamic_cast<GroupCell *>(tmp)->GetEditable(), false);
            }
          }
          else
            SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd));

        }
        else if (m_hCaretPosition != NULL && m_hCaretPosition->m_next != NULL)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_hCaretPosition;
            if (tmp->m_next)
            {
              do tmp = dynamic_cast<GroupCell *>(tmp->m_next);
              while (
                      (tmp->m_next) && (
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_TITLE) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SECTION) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SUBSECTION) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_SUBSUBSECTION) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_HEADING5) &&
                              (dynamic_cast<GroupCell *>(tmp)->GetGroupType() != GC_TYPE_HEADING6)
                      )
                      );
              SetHCaret(tmp);
            }
            else
              SelectEditable(dynamic_cast<GroupCell *>(tmp)->GetEditable(), false);
          }
          else
            SelectEditable(dynamic_cast<GroupCell *>(m_hCaretPosition->m_next)->GetEditable(), true);
        }
        else if (m_tree != NULL && m_hCaretPosition == NULL)
        {
          SelectEditable(dynamic_cast<GroupCell *>(m_tree)->GetEditable(), true);
        }

      }
      else if (m_cellPointers.m_selectionEnd != NULL)
        SetHCaret(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetGroup()));
      else if (!ActivateNextInput())
        event.Skip();
      break;

    case WXK_RETURN:
      ScrolledAwayFromEvaluation();
      if (m_cellPointers.m_selectionStart == NULL || m_cellPointers.m_selectionEnd == NULL)
        OpenHCaret(wxEmptyString);
      else
        OpenHCaret(GetString());
      break;

      // ESCAPE is handled by the new cell
    case WXK_ESCAPE:
      OpenHCaret(wxEmptyString);
      if (GetActiveCell() != NULL)
        Autocomplete(AutoComplete::esccommand);
      break;

      // keycodes which open hCaret with initial content
    default:
      wxChar txt(event.GetUnicodeKey());
      if (txt == WXK_NONE)
      {
        event.Skip();
        return;
      }
      else
        OpenHCaret(txt);
  }

  RequestRedraw();
}

void Worksheet::ClearNotification()
{
  if(m_notificationMessage != NULL)
  {
    m_notificationMessage->Close();
    delete m_notificationMessage;
    m_notificationMessage = NULL;
  }
}

void Worksheet::SetNotification(wxString message, int flags)
{
  if(m_windowActive)
    return;

  ClearNotification();

  m_notificationMessage = new Notification(wxT("wxMaxima"),
                                           message,
                                           GetParent(),
                                           flags);

  if(m_notificationMessage != NULL)
  {
    m_notificationMessage->Show();

    // In wxGTK 3.1.0 Leaving the notification message object alive until the message
    // hits its timeout causes a crash (https://trac.wxwidgets.org/ticket/17876).
    // Let's work around this crash by deleting the object as fast as we can.
    // The crash is fixed in version 3.1.1.
#if wxCHECK_VERSION(3, 1, 2)
#else
#ifdef __WXGTK__
    m_notificationMessage->Close();
    delete m_notificationMessage;
    m_notificationMessage = NULL;
#endif
#endif
  }
}
/*****
 * OnChar handles key events. If we have an active cell, sends the
 * event to the active cell, else moves the cursor between groups.
 */
void Worksheet::OnChar(wxKeyEvent &event)
{
  // Alt+Up and Alt+Down are hotkeys. In order for the main application to realize
  // them they need to be passed to it using the event's Skip() function.
  if(event.AltDown() && ((event.GetKeyCode()==WXK_UP)||(event.GetKeyCode()==WXK_DOWN)))
  {
    event.Skip();
    return;
  }

  if (m_autocompletePopup != NULL)
  {
    // We don't want autocompletion to be able to trigger another autocompletion:
    // On keypress Autocompletion processes the keypress and issues another autocompletion
    // that otherwise would result in an endless loop.
    if(
      !((event.GetKeyCode() == WXK_TAB) && (event.AltDown())) &&
      !((event.GetKeyCode() == 'k') && (event.AltDown())) &&
      !((event.GetKeyCode() == 'K') && (event.AltDown()))
      )
    m_autocompletePopup->OnChar(event);
    return;
  }

  m_cellPointers.ResetSearchStart();
#if defined __WXMSW__
  if (event.GetKeyCode() == WXK_NUMPAD_DECIMAL) {
    event.Skip();
  }
#endif

  // Skip all events that look like they might be hotkey invocations so they
  // are processed by the other recipients
  if (event.CmdDown() && !event.AltDown())
  {
    if (
            !(event.GetKeyCode() == WXK_LEFT) &&
            !(event.GetKeyCode() == WXK_RIGHT) &&
            !(event.GetKeyCode() == WXK_UP) &&
            !(event.GetKeyCode() == WXK_DOWN) &&
            !(event.GetKeyCode() == WXK_BACK) &&
            !(event.GetKeyCode() == WXK_NUMPAD_DELETE) &&
            !(event.GetKeyCode() == WXK_DELETE) &&
            !(event.GetKeyCode() == WXK_HOME) &&
            !(event.GetKeyCode() == WXK_END)
            )
    {
      event.Skip();
      return;
    }
  }

  // Forward cell creation hotkeys to the class wxMaxima
  if (event.CmdDown() && event.AltDown())
  {
    if (
            (event.GetKeyCode() == WXK_ESCAPE) ||
            (event.GetKeyCode() == wxT('1')) ||
            (event.GetKeyCode() == wxT('2')) ||
            (event.GetKeyCode() == wxT('3')) ||
            (event.GetKeyCode() == wxT('4')) ||
            (event.GetKeyCode() == wxT('5'))
            )
    {
      event.Skip();
      return;
    }
  }

  // If the find dialogue is open we use the ESC key as a hotkey that closes
  // the dialogue. If it isn't it is used as part of the shortcuts for
  // entering unicode characters instead.
  if ((m_findDialog != NULL) && (event.GetKeyCode() == WXK_ESCAPE))
  {
    m_findDialog->Destroy();
    m_findDialog = NULL;
    return;
  }

  if (GetActiveCell() != NULL)
  {
    if(event.GetKeyCode() != WXK_ESCAPE)
      OnCharInActive(event);
    else
      Autocomplete(AutoComplete::esccommand);
  }
  else
    OnCharNoActive(event);
}

/***
 * Get maximum x and y in the tree.
 */
void Worksheet::GetMaxPoint(int *width, int *height)
{
  Cell *tmp = m_tree;
  int currentHeight = m_configuration->GetIndent();
  int currentWidth = m_configuration->GetBaseIndent();
  *width = m_configuration->GetBaseIndent();

  while (tmp != NULL)
  {
    currentHeight += tmp->GetMaxHeight();
    currentHeight += m_configuration->GetGroupSkip();
    currentWidth = m_configuration->Scale_Px(m_configuration->GetIndent() + m_configuration->GetDefaultFontSize()) + tmp->GetWidth() + m_configuration->Scale_Px(m_configuration->GetIndent() + m_configuration->GetDefaultFontSize());
    *width = wxMax(currentWidth, *width);
    tmp = tmp->m_next;
  }
  *height = currentHeight;
}

/***
 * Adjust the virtual size and scrollbars.
 */
void Worksheet::AdjustSize()
{
  int width = 40, height = 40;
  int virtualHeight = 40;
  int clientWidth, clientHeight;
  GetClientSize(&clientWidth, &clientHeight);
  if (m_tree != NULL)
  {
    width = m_configuration->GetBaseIndent();
    height = width;

    GetMaxPoint(&width, &height);
    // when window is scrolled all the way down, document occupies top 1/8 of clientHeight
    height += clientHeight - clientHeight / 8;
    virtualHeight = wxMax(clientHeight + 10, height); // ensure we always have VSCROLL active

    // Don't set m_scrollUnit too high for big windows on hi-res screens:
    // Allow scrolling by a tenth of a line doesn't make too much sense,
    // but will make scrolling feel sluggish.
    height = GetClientSize().y;
  }
  if((m_virtualWidth_Last != width) || (m_virtualHeight_Last != virtualHeight))
  {
    m_virtualWidth_Last = width;
    m_virtualHeight_Last = height;
    SetVirtualSize(width, virtualHeight);
    m_scrollUnit = height / 30;
    // Ensure a sane scroll unit even for the fringe case of a very small
    // screen.
    if (m_scrollUnit < 10)
      m_scrollUnit = 10;
    
    SetScrollRate(m_scrollUnit, m_scrollUnit);
  }
}

/***
 * Support for selecting cells outside display
 */
void Worksheet::OnMouseExit(wxMouseEvent &event)
{
  m_mouseOutside = true;
  if (m_leftDown)
  {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
    m_timer.Start(200, true);
  }

  // If only the bracket of the cell under the mouse pointer is shown perhaps it
  // is logical to stop displaying it if the mouse pointer is outside the window.
  // If this isn't the case I'm not against the following block being deleted.
  if(m_configuration->HideBrackets())
  {
    if (m_tree)
      m_tree->CellUnderPointer(NULL);
    RequestRedraw();
  }
}

#if wxCHECK_VERSION(3,1,1)
void Worksheet::OnZoom(wxZoomGestureEvent &event)
{
  if(event.IsGestureStart())
    m_zoomAtGestureStart = m_configuration->GetZoomFactor();

  SetZoomFactor(m_zoomAtGestureStart*event.GetZoomFactor());
}
#endif

void Worksheet::OnMouseEnter(wxMouseEvent &WXUNUSED(event))
{
  m_mouseOutside = false;
}

void Worksheet::StepAnimation(int change)
{
  if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
     (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
  {
    SlideShow *tmp = dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart);
    int pos = tmp->GetDisplayedIndex() + change;

    if(change != 0)
    {
      // Change the bitmap
      if (pos < 0)
        pos = tmp->Length() - 1;
      if (pos >= tmp->Length())
        pos = 0;
      tmp->SetDisplayedIndex(pos);

      // Refresh the displayed bitmap
      wxRect rect = m_cellPointers.m_selectionStart->GetRect();
      RequestRedraw(rect);

      // Set the slider to its new value
      if (m_mainToolBar)
        if (m_mainToolBar->m_plotSlider)
          m_mainToolBar->m_plotSlider->SetValue(tmp->GetDisplayedIndex());
    }
  }
}

void Worksheet::OnTimer(wxTimerEvent &event)
{
  switch (event.GetId())
  {
    case TIMER_ID:
    {
      if (!m_leftDown || !m_mouseOutside)
        return;
      int dx = 0, dy = 0;
      int currX, currY;

      wxSize size = GetClientSize();
      CalcUnscrolledPosition(0, 0, &currX, &currY);

      if (m_mousePoint.x <= 0)
        dx = -m_scrollUnit;
      else if (m_mousePoint.x >= size.GetWidth())
        dx = m_scrollUnit;
      if (m_mousePoint.y <= 0)
        dy = -m_scrollUnit;
      else if (m_mousePoint.y >= size.GetHeight())
        dy = m_scrollUnit;
      Scroll((currX + dx * 2) / m_scrollUnit, (currY + dy * 2) / m_scrollUnit);
      m_timer.Start(50, true);
    }
    break;
  case CARET_TIMER_ID:
    {
      int virtualsize_x;
      int virtualsize_y;
      GetVirtualSize(&virtualsize_x, &virtualsize_y);

      if (m_blinkDisplayCaret)
      {
        wxRect rect;

        if (GetActiveCell() != NULL)
        {
          rect = GetActiveCell()->GetRect();
          GetActiveCell()->SwitchCaretDisplay();
        }
        else
        {
          m_hCaretBlinkVisible = !m_hCaretBlinkVisible;
          if (m_hCaretPosition == NULL)
          {
            rect.SetTop(0);
            rect.SetBottom(m_configuration->GetGroupSkip());
          }
          else
          {
            rect = m_hCaretPosition->GetRect();
            int caretY = ((int) m_configuration->GetGroupSkip()) / 2 + rect.GetBottom() + 1;
            rect.SetTop(caretY - m_configuration->GetCursorWidth() / 2);
            rect.SetBottom(caretY + (m_configuration->GetCursorWidth() + 1) / 2);
          }
        }
        rect.SetLeft(0);
        rect.SetRight(virtualsize_x + m_configuration->Scale_Px(10));
        RequestRedraw(rect);
      }

      // We only blink the cursor if we have the focus => If we loose the focus
      // we can save batteries by not waking up the CPU unnecessarily.
      if (!m_hasFocus)
        m_caretTimer.Stop();
    }
    break;
  default:
  {
      SlideShow *slideshow = NULL;

      // Determine if the timer that has expired belongs to a slide show cell.
      for(Cell::CellPointers::SlideShowTimersList::iterator it = m_cellPointers.m_slideShowTimers.begin();it != m_cellPointers.m_slideShowTimers.end() ; ++it)
      {
        if(it->second == event.GetId())
        {
          slideshow = (SlideShow *) it->first;
          break;
        }
      }
      if(slideshow != NULL)
      {
        int pos = slideshow->GetDisplayedIndex() + 1;

        if (pos >= slideshow->Length())
          pos = 0;
        slideshow->SetDisplayedIndex(pos);

        // Refresh the displayed bitmap
        if (!m_configuration->ClipToDrawRegion())
          slideshow->ReloadTimer();
        else
        {
          wxRect rect = slideshow->GetRect();
          RequestRedraw(rect);
        }

        if ((m_mainToolBar) && (GetSelectionStart() == slideshow))
        {
          if (m_mainToolBar->m_plotSlider)
            m_mainToolBar->UpdateSlider(slideshow);
        }
      }
      break;
    }
  }
}

void Worksheet::RequestRedraw(wxRect rect)
{
  if(m_rectToRefresh.IsEmpty())
    m_rectToRefresh = rect;
  else
    m_rectToRefresh = m_rectToRefresh.Union(rect);
}

/***
 * Destroy the tree
 */
void Worksheet::DestroyTree()
{
  m_hCaretActive = false;
  SetHCaret(NULL);
  TreeUndo_ClearUndoActionList();
  TreeUndo_ClearRedoActionList();
  wxDELETE(m_tree);
  m_tree = m_last = NULL;
}

/***
 * Copy tree
 */
GroupCell *Worksheet::CopyTree()
{
  if (m_tree == NULL)
    return (GroupCell *) NULL;

  return dynamic_cast<GroupCell *>(m_tree->CopyList());
}

/***
 * Copy selection as bitmap
 */
bool Worksheet::CopyBitmap()
{
  Cell *tmp = CopySelection();

  int bitmapScale = 3;
  wxConfig::Get()->Read(wxT("bitmapScale"), &bitmapScale);

  BitmapOut bmp(&m_configuration, bitmapScale);
  bmp.SetData(tmp);

  bool retval = bmp.ToClipboard();
  Recalculate();

  return retval;
}

bool Worksheet::CopyAnimation()
{
  if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
     (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
    return dynamic_cast<SlideShow *>(GetSelectionStart())->CopyAnimationToClipboard();
  else
    return false;
}

bool Worksheet::CopySVG()
{
  Cell *tmp = CopySelection();

  Svgout svg(&m_configuration);
  svg.SetData(tmp);

  bool retval = svg.ToClipboard();
  Recalculate();

  return retval;
}
#if wxUSE_ENH_METAFILE
bool Worksheet::CopyEMF()
{
  Cell *tmp = CopySelection();

  Emfout emf(&m_configuration);
  emf.SetData(tmp);

  bool retval = emf.ToClipboard();
  Recalculate();

  return retval;
}
#endif

bool Worksheet::CopyRTF()
{
  if(!CellsSelected())
    return false;

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (!wxTheClipboard->Open())
    return false;

  wxDataObjectComposite *data = new wxDataObjectComposite;

  wxString rtf = RTFStart();
  GroupCell *tmp = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
  GroupCell *end = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetGroup());

  while (tmp != NULL)
  {
    rtf += tmp->ToRTF();
    if (tmp == end)
      break;
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  rtf += wxT("\\par") + RTFEnd();

  data->Add(new RtfDataObject(rtf), true);
  data->Add(new RtfDataObject2(rtf));

  wxTheClipboard->SetData(data);
  wxTheClipboard->Close();
  return true;
}

wxSize Worksheet::CopyToFile(wxString file)
{

  if (m_cellPointers.m_selectionStart != NULL &&
      m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
      (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE ||
       m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE))
  {
    if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE)
      return dynamic_cast<ImgCell *>(m_cellPointers.m_selectionStart)->ToImageFile(file);
    else
      return dynamic_cast<SlideShow *>(m_cellPointers.m_selectionStart)->ToImageFile(file);
  }
  else
  {
    Cell *tmp = CopySelection();

    BitmapOut bmp(&m_configuration);
    bmp.SetData(tmp);

    wxSize retval = bmp.ToFile(file);

    return retval;
  }
}

wxSize Worksheet::CopyToFile(wxString file, Cell *start, Cell *end,
                            bool asData, int scale)
{
  Cell *tmp = CopySelection(start, end, asData);

  BitmapOut bmp(&m_configuration, scale);
  bmp.SetData(tmp);

  wxSize retval = bmp.ToFile(file);

  return retval;
}

/***
 * Copy selection
 */
Cell *Worksheet::CopySelection(bool asData)
{
  return CopySelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd, asData);
}

Cell *Worksheet::CopySelection(Cell *start, Cell *end, bool asData)
{
  Cell *tmp, *out = NULL, *outEnd = NULL;
  tmp = start;

  while (tmp != NULL)
  {
    if (out == NULL)
    {
      out = tmp->Copy();
      outEnd = out;
    }
    else
    {
      outEnd->AppendCell(tmp->Copy());
      outEnd = outEnd->m_next;
    }
    if (tmp == end)
      break;
    if (asData)
      tmp = tmp->m_next;
    else
      tmp = tmp->m_nextToDraw;
  }

  return out;
}

void Worksheet::AddLineToFile(wxTextFile &output, wxString s, bool unicode)
{
  if (s == wxT("\n") || s == wxEmptyString)
    output.AddLine(wxEmptyString);
  else
  {
    wxStringTokenizer lines(s, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
    wxString line;

    while (lines.HasMoreTokens())
    {
      line = lines.GetNextToken();
      if (unicode)
      {
#if wxUNICODE
        output.AddLine(line);
#else
        wxString t(line.wc_str(wxConvLocal), wxConvUTF8);
        output.AddLine(t);
#endif
      }
      else
        output.AddLine(line);
    }
  }
}

//Simple iterator over a Maxima input string, skipping comments and strings
struct SimpleMathConfigurationIterator
{
  const wxString &input; //reference to input string (must be a reference, so it can be modified)
  unsigned int pos;

  SimpleMathConfigurationIterator(const wxString &ainput) : input(ainput), pos(0)
  {
    if (isValid() && (input[0] == '"' || (input[0] == '/' && input.length() > 1 && input[1] == '*')))
    {
      //skip strings or comments at string start
      pos--;
      ++(*this);
    }
  }

  bool isValid()
  {
    return pos < input.length();
  }

  void operator++()
  {
    unsigned int oldpos = pos;
    pos++;
    while (pos < input.length() && oldpos != pos)
    {
      oldpos = pos;
      if (input[pos] == '"')
      { //skip strings
        pos++; //skip leading "
        while (pos < input.length() && input[pos] != '"')
          pos++;
        pos++;//skip trailing "
      }
      if (pos + 1 < input.length() && input[pos] == '/' && input[pos + 1] == '*')
      { //skip comments
        pos += 2; //skip /*
        while (pos < input.length() && (input[pos] != '*' || input[pos + 1] != '/'))
          pos++;
        pos += 2; //skip */
      }
    }
  }

  inline wxChar operator*()
  {
    return input[pos];
  }
};

//returns the index in (%i...) or (%o...)
int getCellIndex(Cell *cell)
{
  if (!cell) return -1;
  wxString strindex = cell->ToString().Trim(); //(%i...)
  long temp;
  if (!strindex.Mid(3, strindex.Len() - 4).ToLong(&temp)) return -1;
  return temp;
}

void Worksheet::CalculateReorderedCellIndices(Cell *tree, int &cellIndex, std::vector<int> &cellMap)
{
  GroupCell *tmp = dynamic_cast<GroupCell *>(tree);
  while (tmp != NULL)
  {
    if (!tmp->IsHidden() && tmp->GetGroupType() == GC_TYPE_CODE)
    {
      Cell *prompt = tmp->GetPrompt();
      Cell *cell = tmp->GetEditable();

      wxString input = cell->ToString();
      if (prompt && cell && input.Len() > 0)
      {
        int outputExpressions = 0;
        int initialHiddenExpressions = 0;
        for (SimpleMathConfigurationIterator it = input; it.isValid(); ++it)
        {
          switch (*it)
          {
            case '$':
              if (initialHiddenExpressions == outputExpressions) initialHiddenExpressions++; //fallthrough
            case ';':
              outputExpressions++;
          }
        }

        long promptIndex = getCellIndex(prompt);
        long outputIndex = getCellIndex(tmp->GetLabel()) - initialHiddenExpressions;
        long index = promptIndex;
        if (promptIndex < 0) index = outputIndex; //no input index => use output index
        else
        {
          if (outputIndex < 0 && initialHiddenExpressions < outputExpressions)
          {
            //input index, but no output index means the expression was evaluated, but produced no result
            // => it is invalid and should be ignored
            outputExpressions = 0;
          }
          else if (index + outputExpressions > (long) cellMap.size()) cellMap.resize(index + outputExpressions);
          for (int i = 0; i < outputExpressions; i++)
            cellMap[index + i] = cellIndex + i;
        }

        cellIndex += outputExpressions; //new cell index
      }
    }

    if (tmp->GetHiddenTree() != NULL)
      CalculateReorderedCellIndices(tmp->GetHiddenTree(), cellIndex, cellMap);

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
}

/***
 * Export content to a HTML file.
 */
bool Worksheet::ExportToHTML(wxString file)
{
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;

  // The path to the image directory as seen from the html directory
  wxString imgDir_rel;
  // The absolute path to the image directory
  wxString imgDir;
  // What happens if we split the filename into several parts.
  wxString path, filename, ext;
  wxConfigBase *config = wxConfig::Get();

  int count = 0;
  GroupCell *tmp = m_tree;
  MarkDownHTML MarkDown(m_configuration);

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir_rel = filename + wxT("_htmlimg");
  imgDir = path + wxT("/") + imgDir_rel;

  if (!wxDirExists(imgDir))
  {
    if (!wxMkdir(imgDir))
      return false;
  }

  wxString cssfileName_rel = imgDir_rel + wxT("/") + filename + wxT(".css");
  wxString cssfileName = path + wxT("/") + cssfileName_rel;
  wxFileOutputStream cssfile(cssfileName);
  if (!cssfile.IsOk())
    return false;

  wxURI filename_uri(filename);
  wxString filename_encoded = filename_uri.BuildURI(); /* handle HTML entities like " " => "%20" */


  wxTextOutputStream css(cssfile);

  wxString output;
  

  m_configuration->ClipToDrawRegion(false);
  output << wxT("<!DOCTYPE html>\n");
  output << wxT("<html lang=\"\">\n"); // We do not know the language of the exported document.
  output << wxT(" <head>\n");
  output << wxT("  <title>") + filename + wxT("</title>\n");
  output << wxT("  <meta name=\"generator\" content=\"wxMaxima\"/>\n");
  output << wxT("  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n");

//////////////////////////////////////////////
// Write styles
//////////////////////////////////////////////

  if ((m_configuration->HTMLequationFormat() == Configuration::mathML_mathJaX) ||
      (m_configuration->HTMLequationFormat() == Configuration::mathJaX_TeX))
  {
    output << wxT("<script type=\"text/x-mathjax-config\">\n");
    output << wxT("  MathJax.Hub.Config({\n");
    output << wxT("    displayAlign: \"left\",\n");
    output << wxT("    context: \"MathJax\",\n");
    output << wxT("    TeX: {TagSide: \"left\"}\n");
    output << wxT("  })\n");
    output << wxT("</script>\n");
    output << wxT("<script src=\"") + m_configuration->MathJaXURL() + wxT("\" async=\"async\">\n");
    // prevent optimizing <srcipt src="..."><script> to <script src=..."/>
    output << wxT("  // A comment that hinders wxWidgets from optimizing this tag too much.\n");
    output << wxT("</script>\n");
  }

  wxString font, fontTitle, fontSection, fontSubsection, fontSubsubsection, fontHeading5, fontHeading6, fontText;
  wxString colorInput(wxT("blue"));
  wxString colorPrompt(wxT("red"));
  wxString colorText(wxT("black")), colorTitle(wxT("black")), colorSection(wxT("black")),
    colorSubSec(wxT("black")), colorSubsubSec(wxT("black")), colorHeading5(wxT("black")),colorHeading6(wxT("black"));
  wxString colorCodeVariable = wxT("rgb(0,128,0)");
  wxString colorCodeFunction = wxT("rgb(128,0,0)");
  wxString colorCodeComment = wxT("rgb(64,64,64)");
  wxString colorCodeNumber = wxT("rgb(128,64,0)");
  wxString colorCodeString = wxT("rgb(0,0,128)");
  wxString colorCodeOperator = wxT("rgb(0,0,128)");
  wxString colorCodeLisp = wxT("rgb(255,0,128)");
  wxString colorCodeEndOfLine = wxT("rgb(192,192,192)");


  wxString colorTextBg(wxT("white"));
  wxString colorBg(wxT("white"));

  // bold and italic
  bool boldInput = false;
  bool italicInput = false;
  bool boldPrompt = false;
  bool italicPrompt = false;
  bool boldString = false;
  bool italicString = false;

  bool boldTitle = false;
  bool italicTitle = false;
  bool underTitle = false;
  bool boldSection = false;
  bool italicSection = false;
  bool underSection = false;
  bool boldSubsection = false;
  bool boldSubsubsection = false;
  bool boldHeading5 = false;
  bool boldHeading6 = false;
  bool italicSubsection = false;
  bool italicSubsubsection = false;
  bool italicHeading5 = false;
  bool italicHeading6 = false;
  bool underSubsection = false;
  bool underSubsubsection = false;
  bool underHeading5 = false;
  bool underHeading6 = false;

  int fontSize = 12;
  // main fontsize
  config->Read(wxT("fontSize"), &fontSize);

  // read fonts
  config->Read(wxT("Style/fontname"), &font);
  config->Read(wxT("Style/Title/fontname"), &fontTitle);
  config->Read(wxT("Style/Section/fontname"), &fontSection);
  config->Read(wxT("Style/Subsection/fontname"), &fontSubsection);
  config->Read(wxT("Style/Subsubsection/fontname"), &fontSubsubsection);
  config->Read(wxT("Style/Heading5/fontname"), &fontHeading5);
  config->Read(wxT("Style/Heading6/fontname"), &fontHeading6);
  config->Read(wxT("Style/Text/fontname"), &fontText);

  // read colors
  config->Read(wxT("Style/Input/color"), &colorInput);
  config->Read(wxT("Style/MainPrompt/color"), &colorPrompt);
  config->Read(wxT("Style/Text/color"), &colorText);
  config->Read(wxT("Style/Section/color"), &colorSection);
  config->Read(wxT("Style/Subsection/color"), &colorSubSec);
  config->Read(wxT("Style/Subsubsection/color"), &colorSubsubSec);
  config->Read(wxT("Style/Heading5/color"), &colorHeading5);
  config->Read(wxT("Style/Heading6/color"), &colorHeading6);
  config->Read(wxT("Style/Title/color"), &colorTitle);
  config->Read(wxT("Style/TextBackground/color"), &colorTextBg);
  config->Read(wxT("Style/Background/color"), &colorBg);

  config->Read(wxT("Style/CodeHighlighting/Variable/color"), &colorCodeVariable);
  config->Read(wxT("Style/CodeHighlighting/Function/color"), &colorCodeFunction);
  config->Read(wxT("Style/CodeHighlighting/Comment/color"), &colorCodeComment);
  config->Read(wxT("Style/CodeHighlighting/Number/color"), &colorCodeNumber);
  config->Read(wxT("Style/CodeHighlighting/String/color"), &colorCodeString);
  config->Read(wxT("Style/CodeHighlighting/Operator/color"), &colorCodeOperator);
  config->Read(wxT("Style/CodeHighlighting/Lisp/color"), &colorCodeLisp);

  // read bold and italic
  config->Read(wxT("Style/Input/bold"), &boldInput);
  config->Read(wxT("Style/String/bold"), &boldString);
  config->Read(wxT("Style/Input/italic"), &italicInput);
  config->Read(wxT("Style/String/italic"), &italicString);
  config->Read(wxT("Style/MainPrompt/bold"), &boldPrompt);
  config->Read(wxT("Style/MainPrompt/italic"), &italicPrompt);

  config->Read(wxT("Style/Title/bold"), &boldTitle);
  config->Read(wxT("Style/Title/italic"), &italicTitle);
  config->Read(wxT("Style/Title/underlined"), &underTitle);
  config->Read(wxT("Style/Section/bold"), &boldSection);
  config->Read(wxT("Style/Section/italic"), &italicSection);
  config->Read(wxT("Style/Section/underlined"), &underSection);
  config->Read(wxT("Style/Subsection/bold"), &boldSubsection);
  config->Read(wxT("Style/Subsection/italic"), &italicSubsection);
  config->Read(wxT("Style/Subsection/underlined"), &underSubsection);
  config->Read(wxT("Style/Subsubsection/bold"), &boldSubsubsection);
  config->Read(wxT("Style/Subsubsection/italic"), &italicSubsubsection);
  config->Read(wxT("Style/Subsubsection/underlined"), &underSubsubsection);
  config->Read(wxT("Style/Heading5/bold"), &boldHeading5);
  config->Read(wxT("Style/Heading5/italic"), &italicHeading5);
  config->Read(wxT("Style/Heading5/underlined"), &underHeading5);
  config->Read(wxT("Style/Heading6/bold"), &boldHeading6);
  config->Read(wxT("Style/Heading6/italic"), &italicHeading6);
  config->Read(wxT("Style/Heading6/underlined"), &underHeading6);

  wxURI css_url (cssfileName_rel);
  wxString encoded_css_url = css_url.BuildURI(); /* handle HTML entities like " " => "%20" */
  output << wxT("  <link rel=\"stylesheet\" type=\"text/css\" href=\"") + encoded_css_url + wxT("\"/>\n");

  wxString version(wxT(GITVERSION));

  wxString versionString = "Created with wxMaxima version " + version;
  wxString versionPad;
  for(unsigned int i = 0; i<versionString.Length(); i++)
    versionPad += "*";

  css << wxT("\n");
  css << wxT("/* *********") + versionPad    + wxT("******** \n");
  css << wxT("   *        ") + versionString + wxT("       * \n");
  css << wxT("   *********") + versionPad    + wxT("******** */\n");

  // BODY STYLE
  css << wxT("body {\n");
  if (font.Length())
  {
    css << wxT("  font-family: ") +
           font +
           wxT(";\n");
  }
  if (colorBg.Length())
  {
    wxColour color(colorBg);
    css << wxT("  background-color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  css << wxT("}\n");


  // INPUT STYLE
  css << wxT(".input {\n");
  if (colorInput.Length())
  {
    wxColour color(colorInput);
    css << wxT("  color: \n") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldInput) css << wxT("  font-weight: bold;\n");
  if (italicInput) css << wxT("  font-style: italic;\n");
  css << wxT("}\n");

  // COMMENT STYLE
  css << wxT(".comment {\n");
  if (fontText.Length())
  {
    css << wxT("  font-family: ") +
           fontText +
           wxT(";\n");
  }

  if (colorText.Length())
  {
    wxColour color(colorText);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (colorTextBg.Length())
  {
    wxColour color(colorTextBg);
    css << wxT("  background-color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // Colors for code highlighting
  if (colorCodeVariable.Length())
  {
    wxColour color(colorCodeVariable);
    css << wxT(".code_variable {\n");
    css << wxT("  color: \n") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  css << wxT("p {\n");
  css << wxT("  margin-top: 0em;\n");
  css << wxT("  margin-bottom: 0em;\n");
  css << wxT("}\n");

  if (colorCodeFunction.Length())
  {
    wxColour color(colorCodeFunction);
    css << wxT(".code_function {\n\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeComment.Length())
  {
    wxColour color(colorCodeComment);
    css << wxT(".code_comment {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeNumber.Length())
  {
    wxColour color(colorCodeNumber);
    css << wxT(".code_number {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeString.Length())
  {
    wxColour color(colorCodeString);
    css << wxT(".code_string {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeOperator.Length())
  {
    wxColour color(colorCodeOperator);
    css << wxT(".code_operator {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeLisp.Length())
  {
    wxColour color(colorCodeLisp);
    css << wxT(".code_lisp {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";\n");
    css << wxT("}\n");
  }

  if (colorCodeEndOfLine.Length())
  {
    wxColour color(colorCodeEndOfLine);
    css << wxT(".code_endofline {\n");
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"),
                            color.Red(),
                            color.Green(),
                            color.Blue()) +
           wxT(";");
    css << wxT("}\n");
  }

  // SMOOTHER IMAGE SCALING FOR THE IE
  css << "img {\n";
  css << wxT("  -ms-interpolation-mode: bicubic;\n");
  css << wxT("}\n");

  // IMAGE STYLE
  css << wxT(".image {\n");
  if (fontText.Length())
  {
    css << wxT("  font-family: ") +
           fontText + wxT(";\n");
  }
  if (colorText.Length())
  {
    wxColour color(colorText);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // SECTION STYLE
  css << wxT(".section {\n");
  if (fontSection.Length())
  {
    css << wxT("  font-family: ") +
           fontSection + wxT(";\\");
  }
  if (colorSection.Length())
  {
    wxColour color(colorSection);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldSection) css << wxT("  font-weight: bold;\n");
  if (underSection) css << wxT("  text-decoration: underline;\n");
  if (italicSection) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 1.5em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");


  // SUBSECTION STYLE
  css << wxT(".subsect {\n");
  if (fontSubsection.Length())
  {
    css << wxT("  font-family: ") +
           fontSubsection + wxT(";\n");
  }
  if (colorSubSec.Length())
  {
    wxColour color(colorSubSec);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldSubsection) css << wxT("  font-weight: bold;\n");
  if (underSubsection) css << wxT("  text-decoration: underline;\n");
  if (italicSubsection) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 1.2em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // SUBSUBSECTION STYLE
  css << wxT(".subsubsect {\n");
  if (fontSubsubsection.Length())
  {
    css << wxT("  font-family: ") +
           fontSubsubsection + wxT(";\n");
  }
  if (colorSubsubSec.Length())
  {
    wxColour color(colorSubsubSec);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldSubsubsection) css << wxT("  font-weight: bold;\n");
  if (underSubsubsection) css << wxT("  text-decoration: underline;\n");
  if (italicSubsubsection) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 1.2em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // HEADING5 STYLE
  css << wxT(".heading5 {\n");
  if (fontHeading5.Length())
  {
    css << wxT("  font-family: ") +
           fontHeading5 + wxT(";\n");
  }
  if (colorHeading5.Length())
  {
    wxColour color(colorHeading5);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldHeading5) css << wxT("  font-weight: bold;\n");
  if (underHeading5) css << wxT("  text-decoration: underline;\n");
  if (italicHeading5) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 1.2em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // HEADING6 STYLE
  css << wxT(".heading6 {\n");
  if (fontHeading6.Length())
  {
    css << wxT("  font-family: ") +
           fontHeading6 + wxT(";\n");
  }
  if (colorHeading6.Length())
  {
    wxColour color(colorHeading6);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldHeading6) css << wxT("  font-weight: bold;\n");
  if (underHeading6) css << wxT("  text-decoration: underline;\n");
  if (italicHeading6) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 1.2em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");



  // TITLE STYLE
  css << wxT(".title {\n");
  if (fontTitle.Length())
  {
    css << wxT("  font-family: ") +
           fontTitle + wxT(";\n");
  }
  if (colorTitle.Length())
  {
    wxColour color(colorTitle);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldTitle) css << wxT("  font-weight: bold;\n");
  if (underTitle) css << wxT("  text-decoration: underline;\n");
  if (italicTitle) css << wxT("  font-style: italic;\n");
  css << wxT("  font-size: 2em;\n");
  css << wxT("  padding: 2mm;\n");
  css << wxT("}\n");

  // PROMPT STYLE
  css << wxT(".prompt {\n");
  if (colorPrompt.Length())
  {
    wxColour color(colorPrompt);
    css << wxT("  color: ") +
           wxString::Format(wxT("rgb(%d,%d,%d)"), color.Red(), color.Green(), color.Blue()) +
           wxT(";\n");
  }
  if (boldPrompt) css << wxT("  font-weight: bold;\n");
  if (italicPrompt) css << wxT("  font-style: italic;\n");
  css << wxT("}\n");

  // TABLES
  css << wxT("table {\n");
  css << wxT("  border: 0px;\n");
  css << wxT("}\n");
  css << wxT("td {\n");
  css << wxT("  vertical-align: top;\n");
  css << wxT("  padding: 1mm;\n");
  css << wxT("}\n");

  output << wxT(" </head>\n");
  output << wxT(" <body>\n");

  output << wxT("\n");
  output << wxT("<!-- *********") + versionPad    + wxT("******** -->\n");
  output << wxT("<!-- *        ") + versionString + wxT("       * -->\n");
  output << wxT("<!-- *********") + versionPad    + wxT("******** -->\n");

  if ((m_configuration->HTMLequationFormat() != Configuration::bitmap) &&
      (m_configuration->HTMLequationFormat() != Configuration::svg))
  {
    // Tell users that have disabled JavaScript why they don't get 2d maths.
    output << wxT("<noscript>");
    output << wxT("<div class=\"error message\">");
    output
            << wxT("<p>Please enable JavaScript in order to get a 2d display of the equations embedded in this web page.</p>");
    output << wxT("</div>");
    output << wxT("</noscript>");

    // Tell mathJax about the \abs{} operator we define for LaTeX.
    output << wxT("<p hidden = \"hidden\">\\(");
    output << wxT("      \\DeclareMathOperator{\\abs}{abs}\n");
    output << wxT("      \\newcommand{\\ensuremath}[1]{\\mbox{$#1$}}\n");
    output << wxT("\\)</p>");

  }

  //////////////////////////////////////////////
  // Write the actual contents
  //////////////////////////////////////////////

  while (tmp != NULL)
  {

    // Handle a code cell
    if (tmp->GetGroupType() == GC_TYPE_CODE)
    {

      // Handle the label
      Cell *out = tmp->GetLabel();

      if (out || (m_configuration->ShowCodeCells()))
        output << wxT("\n\n<!-- Code cell -->\n\n\n");

      // Handle the input
      if (m_configuration->ShowCodeCells())
      {
        Cell *prompt = tmp->GetPrompt();
        output << wxT("<table><tr><td>\n");
        output << wxT("  <span class=\"prompt\">\n");
        output << prompt->ToString();
        output << wxT("\n  </span></td>\n");

        EditorCell *input = tmp->GetInput();
        if (input != NULL)
        {
          output << wxT("  <td><span class=\"input\">\n");
          output << input->ToHTML();
          output << wxT("  </span></td>\n");
        }
        output << wxT("</tr></table>\n");
      }

      // Handle the output - if output exists.
      if (out == NULL)
      {
        // No output to export.x
        output << wxT("\n");
      }
      else
      {

        // We got output.
        // Output is a list that can consist of equations, images and slideshows.
        // We need to handle each of these item types separately => break down the list
        // into chunks of one type.
        Cell *chunkStart = tmp->GetLabel();
        while (chunkStart != NULL)
        {
          Cell *chunkEnd = chunkStart;

          if (
                  (chunkEnd->GetType() != MC_TYPE_SLIDE) &&
                  (chunkEnd->GetType() != MC_TYPE_IMAGE)
                  )
            while (chunkEnd->m_next != NULL)
            {
              if (
                      (chunkEnd->m_next->GetType() == MC_TYPE_SLIDE) ||
                      (chunkEnd->m_next->GetType() == MC_TYPE_IMAGE) ||
                      (chunkEnd->m_next->GetStyle() == TS_LABEL) ||
                      (chunkEnd->m_next->GetStyle() == TS_USERLABEL)
                      )
                break;
              chunkEnd = chunkEnd->m_next;
            }

          // Create a list containing only our chunk.
          Cell *chunk = CopySelection(chunkStart, chunkEnd);

          // Export the chunk.

          if (chunk->GetType() == MC_TYPE_SLIDE)
          {
            dynamic_cast<SlideShow *>(chunk)->ToGif(
                    imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.gif"), count));
            output << wxT("  <img src=\"") + filename_encoded + wxT("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(_("_%d.gif\"  alt=\"Animated Diagram\" loading=\"lazy\" style=\"max-width:90%%;\" />\n"), count);
          }
          else if (chunk->GetType() != MC_TYPE_IMAGE)
          {
            switch(m_configuration->HTMLequationFormat())
            {
            case Configuration::mathJaX_TeX:
            {
              wxString line = chunk->ListToTeX();

              line.Replace(wxT("<"), wxT("&lt;"));
              line.Replace(wxT("&"), wxT("&amp;"));
              line.Replace(wxT(">"), wxT("&gt;"));
              // Work around a known limitation in MathJaX: According to
              // https://github.com/mathjax/MathJax/issues/569 Non-Math Text will still
              // be interpreted as Text, not as TeX for a long while.
              //
              // So instead of  "\%o1" print "%o1" - that works fine now.
	      // Since we are using a *fixed* Mathjax version for an export,
	      // nothing will happen, if Mathjax changes that behaviour and would interpret
	      // the % as TeX comment. When we would upgrade to the new MathJax version
	      // we would need to escape the % with \%, but now that is not necessary.
              line.Replace(wxT("\\tag{\\% "), wxT("\\tag{%"));

              output << wxT("<p>\n\\[") << line << wxT("\\]\n</p>\n");
              wxDELETE(chunk);
              break;
            }

            case Configuration::svg:
            {
              wxString alttext = _("Result");
              alttext = chunk->ListToString();
              alttext = EditorCell::EscapeHTMLChars(alttext);
              Svgout svgout(&m_configuration, imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.svg"), count));
              wxSize size = svgout.SetData(chunk);
              wxString line = wxT("  <img src=\"") +
                filename_encoded + wxT("_htmlimg/") + filename_encoded +
                wxString::Format(wxT("_%d.svg\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\"" ),
                                 count, size.x) +
                alttext +
                wxT("\" /><br/>\n");

              output << line + "\n";
              break;
            }

            case Configuration::bitmap:
            {
              wxString ext;
              wxSize size;
              int bitmapScale = 3;
              ext = wxT(".png");
              wxConfig::Get()->Read(wxT("bitmapScale"), &bitmapScale);
              size = CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count),
                                chunk,
                                NULL, true, bitmapScale);
              int borderwidth = 0;
              wxString alttext = _("Result");
              alttext = chunk->ListToString();
              alttext = EditorCell::EscapeHTMLChars(alttext);
              borderwidth = chunk->m_imageBorderWidth;

              wxString line = wxT("  <img src=\"") +
                filename_encoded + wxT("_htmlimg/") + filename_encoded +
                wxString::Format(wxT("_%d%s\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\" "),
                                 count, ext, size.x / bitmapScale - 2 * borderwidth) +
                alttext +
                wxT("\" /><br/>\n");

              output << line + "\n";
              wxDELETE(chunk);
              break;
            }

            default:
            {
              wxString line = chunk->ListToMathML();
              output << wxT("<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"block\">") << line
                     << wxT("</math>\n");
              wxDELETE(chunk);
            }
            }
          }
          else
          {
            wxString ext;
            wxSize size;
            ext = wxT(".") + dynamic_cast<ImgCell *>(chunk)->GetExtension();
            size = dynamic_cast<ImgCell *>(chunk)->ToImageFile(
              imgDir + wxT("/") + filename + wxString::Format(wxT("_%d"), count) + ext);
            int borderwidth = 0;
            wxString alttext = _("Image");
            alttext = chunk->ListToString();
            alttext = EditorCell::EscapeHTMLChars(alttext);
            borderwidth = chunk->m_imageBorderWidth;

            wxString line = wxT("  <img src=\"") +
              filename_encoded + wxT("_htmlimg/") + filename_encoded +
              wxString::Format(wxT("_%d%s\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\""),
                               count, ext, size.x - 2 * borderwidth) +
              alttext +
              wxT("\" /><br/>\n");

            output << line + "\n";
            wxDELETE(chunk);

          }
          count++;

          chunkStart = chunkEnd->m_next;
        }
      }
    }
    else // No code cell
    {
      switch (tmp->GetGroupType())
      {
        case GC_TYPE_TEXT:
          output << wxT("\n\n<!-- Text cell -->\n\n\n");
          output << wxT("<div class=\"comment\">\n");
          output << wxT("<p>\n");
          output << MarkDown.MarkDown(EditorCell::EscapeHTMLChars(tmp->GetEditable()->ToString())) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SECTION:
          output << wxT("\n\n<!-- Section cell -->\n\n\n");
          output << wxT("<div class=\"section\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SUBSECTION:
          output << wxT("\n\n<!-- Subsection cell -->\n\n\n");
          output << wxT("<div class=\"subsect\">\n");
          output << wxT("<p>\n");
          output << 
                  EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SUBSUBSECTION:
          output << wxT("\n\n<!-- Subsubsection cell -->\n\n\n");
          output << wxT("<div class=\"subsubsect\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_HEADING5:
          output << wxT("\n\n<!-- Heading5 cell -->\n\n\n");
          output << wxT("<div class=\"heading5\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_HEADING6:
          output << wxT("\n\n<!-- Heading6 cell -->\n\n\n");
          output << wxT("<div class=\"heading6\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() + tmp->GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_TITLE:
          output << wxT("\n\n<!-- Title cell -->\n\n\n");
          output << wxT("<div class=\"title\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetEditable()->ToString()) +"\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_PAGEBREAK:
          output << wxT("\n\n<!-- Page break cell -->\n\n\n");
          output << wxT("<div class=\"comment\">\n");
          output << wxT("<hr/>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_IMAGE:
        {
          output << wxT("\n\n<!-- Image cell -->\n\n\n");
          Cell *out = tmp->GetLabel();
          output << wxT("<div class=\"image\">\n");
          output << EditorCell::EscapeHTMLChars(tmp->GetPrompt()->ToString() +
                                                tmp->GetEditable()->ToString()) << wxT("\n");
          output << wxT("<br/>\n");
          if (tmp->GetLabel()->GetType() == MC_TYPE_SLIDE)
          {
            dynamic_cast<SlideShow *>(tmp->GetOutput())->ToGif(imgDir + wxT("/") + filename +
                                                               wxString::Format(wxT("_%d.gif"), count));
            output << wxT("  <img src=\"") + filename_encoded + wxT("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(_("_%d.gif\" alt=\"Animated Diagram\" style=\"max-width:90%%;\" loading=\"lazy\" />"), count)
                   << wxT("\n");
          }
          else
          {
            ImgCell *imgCell = dynamic_cast<ImgCell *>(out);
            imgCell->ToImageFile(
                    imgDir + wxT("/") + filename + wxString::Format(wxT("_%d."), count) +
                    imgCell->GetExtension());
            output << wxT("  <img src=\"") + filename_encoded + wxT("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(wxT("_%d.%s\" alt=\"Diagram\" style=\"max-width:90%%;\" loading=\"lazy\" />"), count,
                                       imgCell->GetExtension());
          }
          output << wxT("</div>\n");
          count++;
        }
          break;
      case GC_TYPE_CODE:{}
      }
    }

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

//////////////////////////////////////////////
// Footer
//////////////////////////////////////////////

  output << wxT("\n");
  output << wxT(" <hr/>\n");
  output << wxT(" <p><small> Created with "
                        "<a href=\"https://wxMaxima-developers.github.io/wxmaxima/\">"
                        "wxMaxima</a>.</small></p>\n");
  output << wxEmptyString;

  bool exportContainsWXMX = false;
  wxConfig::Get()->Read(wxT("exportContainsWXMX"), &exportContainsWXMX);

  if (exportContainsWXMX)
  {
    wxString wxmxfileName_rel = imgDir_rel + wxT("/") + filename + wxT(".wxmx");
    wxString wxmxfileName = path + wxT("/") + wxmxfileName_rel;
    ExportToWXMX(wxmxfileName, false);
    output << wxT(" <small> The source of this Maxima session can be downloaded "
                  "<a href=\"") + wxmxfileName_rel + wxT("\">here</a>.</small>\n");
  }

  //
  // Close the document
  //
  output << wxT(" </body>\n");
  output << wxT("</html>\n");

  m_configuration->ClipToDrawRegion(true);

  // Indent the document and test it for validity.
  wxXmlDocument doc;
  {
    wxMemoryOutputStream ostream;
    wxTextOutputStream txtstrm(ostream);
    txtstrm.WriteString(output);
    wxMemoryInputStream istream(ostream);
    doc.Load(istream);
  }
      
  // Replace the raw document by the indented one. If that step worked, that ist.
  if (doc.IsOk())
  {
    wxMemoryOutputStream ostream;
    doc.Save(ostream);
    output = wxString::FromUTF8((char *) ostream.GetOutputStreamBuffer()->GetBufferStart(),
                                ostream.GetOutputStreamBuffer()->GetBufferSize());
    
    // Now the string has a header we want to drop again.
    output = output.SubString(output.Find("\n") + 1, output.Length());
  }
  else
    wxLogMessage(_("Bug: HTML output is no valid XML"));
  
  wxFileOutputStream outfile(file);
  if (!outfile.IsOk())
  {
    return false;
  }
  
  wxTextOutputStream outstream(outfile);
  outstream << "<!DOCTYPE html>\n";
  outstream << output;

  bool outfileOK = !outfile.GetFile()->Error();
  bool cssOK = !cssfile.GetFile()->Error();
  outfile.Close();
  cssfile.Close();

  m_configuration->ClipToDrawRegion(true);
  RecalculateForce();
  return outfileOK && cssOK;
}

void Worksheet::CodeCellVisibilityChanged()
{
  // Move the cursor out of the currently active cell if we are about to
  // hide it
  if ((GetActiveCell() != NULL) &&
      (GetActiveCell()->GetType() == MC_TYPE_INPUT) &&
      (!m_configuration->ShowCodeCells())
          )
    SetHCaret(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
  RecalculateForce();
  ScrollToCaret();
}

GroupCell *Worksheet::CreateTreeFromWXMCode(wxArrayString *wxmLines)
{
  bool hide = false;
  GroupCell *tree = NULL;
  GroupCell *last = NULL;
  GroupCell *cell = NULL;

  wxString question;
  
  while (!wxmLines->IsEmpty())
  {
    cell = NULL;

    if (wxmLines->Item(0) == wxT("/* [wxMaxima: hide output   ] */"))
      hide = true;

      // Print title
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: title   start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: title   end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_TITLE, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

      // Print section
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: section start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: section end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_SECTION, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

      // Print subsection
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: subsect start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: subsect end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_SUBSECTION, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

    // print subsubsection
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: subsubsect start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: subsubsect end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_SUBSUBSECTION, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

    // print heading5
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: heading5 start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: heading5 end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_HEADING5, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

    // print heading6
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: heading6 start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: heading6 end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_HEADING6, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

      // Print comment
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: comment start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: comment end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_TEXT, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }

      // Print an image
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: caption start ]"))
    {
      wxmLines->RemoveAt(0);

      wxString line;

      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: caption end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_IMAGE, &m_cellPointers);
      cell->GetEditable()->SetValue(line);

      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }

      // Gracefully handle captions without images
      if(wxmLines->IsEmpty())
        break;

      wxmLines->RemoveAt(0);
      if (wxmLines->Item(0) == wxT("/* [wxMaxima: image   start ]"))
      {
        wxmLines->RemoveAt(0);

        // Read the image type
        wxString imgtype = wxmLines->Item(0);
        wxmLines->RemoveAt(0);

        wxString line;
        while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("   [wxMaxima: image   end   ] */")))
        {
          if (line.Length() == 0)
            line += wxmLines->Item(0);
          else
            line += wxT("\n") + wxmLines->Item(0);

          wxmLines->RemoveAt(0);
        }

        cell->SetOutput(new ImgCell(NULL, &m_configuration, &m_cellPointers, wxBase64Decode(line), imgtype));
      }
    }
      // Print input
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: input   start ] */"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("/* [wxMaxima: input   end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }

      cell = new GroupCell(&m_configuration, GC_TYPE_CODE, &m_cellPointers, line);
      if (hide)
      {
        cell->Hide(true);
        hide = false;
      }
    }
    if (wxmLines->Item(0) == wxT("/* [wxMaxima: answer  start ] */"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("/* [wxMaxima: answer  end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }
      if((last != NULL) && (!question.IsEmpty()))
        last->SetAnswer(question, line);
    }
    if (wxmLines->Item(0) == wxT("/* [wxMaxima: question  start ] */"))
    {
      wxmLines->RemoveAt(0);

      wxString line;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("/* [wxMaxima: question  end   ] */")))
      {
        if (line.Length() == 0)
          line += wxmLines->Item(0);
        else
          line += wxT("\n") + wxmLines->Item(0);

        wxmLines->RemoveAt(0);
      }
      question = line;
    }
    if (wxmLines->Item(0) == wxT("/* [wxMaxima: autoanswer    ] */"))
    {
      if(last != NULL)
        last->AutoAnswer(true);
    }
    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: page break    ] */"))
    {
      wxmLines->RemoveAt(0);

      cell = new GroupCell(&m_configuration, GC_TYPE_PAGEBREAK, &m_cellPointers);
    }

    else if (wxmLines->Item(0) == wxT("/* [wxMaxima: fold    start ] */"))
    {
      wxmLines->RemoveAt(0);

      wxArrayString hiddenTree;
      while ((!wxmLines->IsEmpty()) && (wxmLines->Item(0) != wxT("/* [wxMaxima: fold    end   ] */")))
      {
        hiddenTree.Add(wxmLines->Item(0));
        wxmLines->RemoveAt(0);
      }
      last->HideTree(CreateTreeFromWXMCode(&hiddenTree));
    }

    if (cell)
    { // if we have created a cell in this pass
      if (!tree)
        tree = last = cell;
      else
      {
        last->m_next = last->m_nextToDraw = cell;
        last->m_next->m_previous = last->m_next->m_previousToDraw = last;

        last = dynamic_cast<GroupCell *>(last->m_next);
      }
      cell = NULL;
    }

    if (!wxmLines->IsEmpty())
      wxmLines->RemoveAt(0);
  }

  return tree;
}

/*! Export the file as TeX code
 */
bool Worksheet::ExportToTeX(wxString file)
{
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;

  wxString imgDir;
  wxString path, filename, ext;
  GroupCell *tmp = m_tree;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxT("/") + filename + wxT("_img");
  int imgCounter = 0;

  wxFileOutputStream outfile(file);
  if (!outfile.IsOk())
    return false;

  wxTextOutputStream output(outfile);

  if(m_configuration->DocumentclassOptions().IsEmpty())
    output << "\\documentclass{" +
      m_configuration->Documentclass() +
      "}\n\n";
  else
    output << "\\documentclass["+ m_configuration->DocumentclassOptions() +"]{" +
      m_configuration->Documentclass() +
      "}\n\n";
  output << wxT("%% Created with wxMaxima "
                        GITVERSION
                        "\n\n");
  output << wxT("\\setlength{\\parskip}{\\medskipamount}\n");
  output << wxT("\\setlength{\\parindent}{0pt}\n");
  output << wxT("\\usepackage{iftex}\n");
  output << wxT("\\ifPDFTeX\n");
  output << wxT("  % PDFLaTeX or LaTeX \n");
  output << wxT("  \\usepackage[utf8]{inputenc}\n");
  output << wxT("  \\usepackage[T1]{fontenc}\n");
  output << wxT("  \\DeclareUnicodeCharacter{00B5}{\\ensuremath{\\mu}}\n");
  output << wxT("\\else\n");
  output << wxT("  %  XeLaTeX or LuaLaTeX\n");
  output << wxT("  \\usepackage{fontspec}\n");
  output << wxT("\\fi\n");

  // The following line loads all code needed in order to include graphics.
  output << wxT("\\usepackage{graphicx}\n");
  // We want to color the labels and text cells. The following line adds the necessary
  // logic for this to TeX.
  output << wxT("\\usepackage{color}\n");
  output << wxT("\\usepackage{amsmath}\n");
  // Support characters like spaces and underscores in graphic filenames
  output << wxT("\\usepackage{grffile}\n");

  // We want to shrink pictures the user has included if they are
  // higher or wider than the page.
  output << wxT("\\usepackage{ifthen}\n");
  output << wxT("\\newsavebox{\\picturebox}\n");
  output << wxT("\\newlength{\\pictureboxwidth}\n");
  output << wxT("\\newlength{\\pictureboxheight}\n");
  output << wxT("\\newcommand{\\includeimage}[1]{\n");
  output << wxT("    \\savebox{\\picturebox}{\\includegraphics{#1}}\n");
  output << wxT("    \\settoheight{\\pictureboxheight}{\\usebox{\\picturebox}}\n");
  output << wxT("    \\settowidth{\\pictureboxwidth}{\\usebox{\\picturebox}}\n");
  output << wxT("    \\ifthenelse{\\lengthtest{\\pictureboxwidth > .95\\linewidth}}\n");
  output << wxT("    {\n");
  output << wxT("        \\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{#1}\n");
  output << wxT("    }\n");
  output << wxT("    {\n");
  output << wxT("        \\ifthenelse{\\lengthtest{\\pictureboxheight>.80\\textheight}}\n");
  output << wxT("        {\n");
  output << wxT("            \\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{#1}\n");
  output << wxT("            \n");
  output << wxT("        }\n");
  output << wxT("        {\n");
  output << wxT("            \\includegraphics{#1}\n");
  output << wxT("        }\n");
  output << wxT("    }\n");
  output << wxT("}\n");
  output << wxT("\\newlength{\\thislabelwidth}\n");

  // Define an "abs" operator for abs commands that are long enough to be broken into
  // lines.
  output << wxT("\\DeclareMathOperator{\\abs}{abs}\n");

  // The animate package is only needed if we actually want to output animations
  // to LaTeX. Don't drag in this dependency if this feature was disabled in the settings.
  bool AnimateLaTeX = true;
  wxConfig::Get()->Read(wxT("AnimateLaTeX"), &AnimateLaTeX);
  if (AnimateLaTeX)
  {
    output << wxT("\\usepackage{animate} % This package is required because the wxMaxima configuration option\n");
    output << wxT("                      % \"Export animations to TeX\" was enabled when this file was generated.\n");
  }
  output << wxT("\n");
  output << wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");
  output << wxT("\n");

  // Add an eventual preamble requested by the user.
  wxString texPreamble;
  wxConfig::Get()->Read(wxT("texPreamble"), &texPreamble);
  if (texPreamble != wxEmptyString)
    output << texPreamble << wxT("\n\n");

  output << wxT("\\begin{document}\n");

  //
  // Write contents
  //
  while (tmp != NULL)
  {
    wxString s = tmp->ToTeX(imgDir, filename, &imgCounter);
    output << s << wxT("\n");
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  //
  // Close document
  //
  output << wxT("\\end{document}\n");


  bool done = !outfile.GetFile()->Error();
  outfile.Close();

  return done;
}

wxString Worksheet::UnicodeToMaxima(wxString s)
{
  wxString retval;

  wxChar ch;
  wxChar ch_Last = '\0';
  for (wxString::iterator it = s.begin(); it < s.end(); ++it)
  {
    ch = *it;
    switch (ch)
    {
    // Strings aren't converted
    case '\"':
      retval += ch;
      ++it;
      while(it < s.end())
      {
        retval += *it;
        if(*it == '\\')
        {
          ++it;
          if(it != s.end())
            retval += *it;
          else
            return retval;
          ++it;
        }
        else
        {
          if(*it == '\"')
          {
            if (it < s.end())
              break;
            else
              return retval;
           }
          else
            ++it;
        }
      }
      break;
    // Backslashed items aren't converted.
    case '\\':
      retval += ch;
      ++it;
      if(it != s.end())
        retval += *it;
      else
        return retval;
      break;
    case wxT('\x00B2'): retval += wxT("^2");break;
    case wxT('\x00B3'): retval += wxT("^3");break;
    case wxT('\x00BD'): retval += wxT("(1/2)");break;
    case wxT('\x221A'): retval += wxT("sqrt");break;
    case wxT('\x2148'): retval += wxT("%i");break;
    case wxT('\x2147'): retval += wxT("%e");break;
    case wxT('\x221E'): retval += wxT("inf");break;
    case wxT('\x22C0'): retval += wxT(" and ");break;
    case wxT('\x22C1'): retval += wxT(" or ");break;
    case wxT('\x22BB'): retval += wxT(" xor ");break;
    case wxT('\x22BC'): retval += wxT(" nand ");break;
    case wxT('\x22BD'): retval += wxT(" nor ");break;
    case wxT('\x21D2'): retval += wxT(" implies ");break;
    case wxT('\x21D4'): retval += wxT(" equiv ");break;
    case wxT('\x00AC'): retval += wxT(" not ");break;
    case wxT('\x2260'): retval += wxT(" # ");break;
    case wxT('\x2264'): retval += wxT(" <= ");break;
    case wxT('\x2265'): retval += wxT(" >= ");break;
    case wxT('\x2212'): retval += wxT("-");break; // An unicode minus sinbreak;
    case wxT('\x00B7'): retval += wxT("*");break; // An unicode multiplication sinbreak;
    case wxT('\xDCB6'): retval += wxT(" ");break; // A non-breakable spaebreak;
    case wxT('\r'): retval += wxT(" ");break; // A soft linebreak
    // Convert \x03C0 to %pi if it isn't part of a symbol name
    case wxT('\x03C0'):
    {
      wxString::iterator it2 = it;
      ++it2;
      if (
        (!wxIsalnum(ch_Last)) &&
        ((it2 == s.end()) || (!wxIsalnum(*it2)))
        )
        retval += wxT("%pi");
      else
        retval += *it;
    }
    break;
    default:
      retval += *it;
    }
    ch_Last = ch;
  }
  return retval;
}

void Worksheet::ExportToMAC(wxTextFile &output, GroupCell *tree, bool wxm, const std::vector<int> &cellMap,
                           bool fixReorderedIndices)
{
  // Show a busy cursor as long as we open a file.
  wxBusyCursor crs;

  GroupCell *tmp = tree;

  //
  // Write contents
  //
  while (tmp != NULL)
  {

    AddLineToFile(output, tmp->ToWXM(wxm));

    if ((wxm)&&(tmp->GetGroupType() == GC_TYPE_CODE))
    {
      EditorCell *txt = tmp->GetEditable();
      if (txt != NULL)
      {
        wxString input = txt->ToString(true);

        if (fixReorderedIndices)
          for (SimpleMathConfigurationIterator it = input; it.pos + 1 < it.input.length(); ++it)
            if (*it == '%' &&
                (input[it.pos + 1] == 'i' || input[it.pos + 1] == 'o') &&
                (it.pos == 0 || input[it.pos - 1] != '%'))
            {
              it.pos += 2;
              unsigned int startPos = it.pos;
              unsigned int temp = 0;
              for (; it.pos < input.Length() && (*it >= '0' && *it <= '9'); ++it.pos)
                temp = temp * 10 + (*it - '0');
              if (temp >= cellMap.size() || cellMap[temp] < 1) continue;
              wxString tempstr;
              tempstr << cellMap[temp];
              input.replace(startPos, it.pos - startPos, tempstr);
              it.pos = startPos + tempstr.length();
            }

      }
    }
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
}

bool Worksheet::ExportToMAC(wxString file)
{
  // Show a busy cursor as long as we export or save.
  wxBusyCursor crs;

  bool wxm;

  if (file.Right(4) == wxT(".wxm"))
    wxm = true;
  else
    wxm = false;


  wxTextFile backupfile(file + wxT("~"));
  if (backupfile.Exists())
  {
    if (!backupfile.Open())
      return false;
    backupfile.Clear();
  }
  else if (!backupfile.Create())
    return false;

  if (wxm)
  {
    AddLineToFile(backupfile, wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"), false);
    wxString version(wxT(GITVERSION));
    AddLineToFile(backupfile, wxT("/* [ Created with wxMaxima version ") + version + wxT(" ] */"), false);
  }

  bool fixReorderedIndices = m_configuration->FixReorderedIndices();
  std::vector<int> cellMap;
  if (fixReorderedIndices)
  {
    int cellIndex = 1;
    CalculateReorderedCellIndices(m_tree, cellIndex, cellMap);
  }
  ExportToMAC(backupfile, m_tree, wxm, cellMap, fixReorderedIndices);

  AddLineToFile(backupfile, wxEmptyString, false);
  if (wxm)
  {
    AddLineToFile(backupfile, wxT("/* Old versions of Maxima abort on loading files that end in a comment. */"), false);
    AddLineToFile(backupfile, wxT("\"Created with wxMaxima " GITVERSION "\"$"), false);
  }

  // Try to save the file.
  bool done = backupfile.Write(wxTextFileType_None);
  // Even if that failed we should perhaps still issue a Close() .
  if (!backupfile.Close()) return false;
  if (!done)return false;

  {
    // We try a few times to overwrite the original file: On MSW sometimes
    // virus scanners lock files for a while
    SuppressErrorDialogs suppressor;
  // If we succeeded in saving the backup file we now can overwrite the Real Thing.
    done = wxRenameFile(file + wxT("~"), file, true);
    if(!done)
    {
      wxSleep(1);
      wxRenameFile(file + wxT("~"), file, true);
    }
  }
  if(!done)
  {
    wxSleep(1);
    if (!wxRenameFile(file + wxT("~"), file, true))
      return false;
  }

  if (wxm)
    m_saved = true;
  return true;
}

/*
  Save the data as wxmx file

  First saves the data to a backup file ending in .wxmx~ so if anything goes
  horribly wrong in this stepp all that is lost is the data that was input
  since the last save. Then the original .wxmx file is replaced in a
  (hopefully) atomic operation.
*/
bool Worksheet::ExportToWXMX(wxString file, bool markAsSaved)
{
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;
  wxLogMessage(_("Starting to save the worksheet as .wxmx"));
  // delete temp file if it already exists
  wxString backupfile = file + wxT("~");
  if (wxFileExists(backupfile))
  {
    if (!wxRemoveFile(backupfile))
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
  //
  // We will keep that setting for the rest of the file for the following reasons:
  //  - Compression of the .zip file won't improve compression of the embedded .png images
  //  - The text part of the file is too small to justify compression
  //  - not compressing the text part of the file allows version control systems to
  //    determine which lines have changed and to track differences between file versions
  //    efficiently (in a compressed text virtually every byte might change when one
  //    byte at the start of the uncompressed original is)
  //  - and if anything crashes in a bad way chances are high that the uncompressed
  //    contents of the .wxmx file can be rescued using a text editor.
  //  Who would - under these circumstances - care about a kilobyte?
  zip.SetLevel(0);
  zip.PutNextEntry(wxT("mimetype"));
  output << wxT("text/x-wxmathml");
  zip.CloseEntry();
  zip.PutNextEntry(wxT("format.txt"));
  output << wxT(
    "\n\nThis file contains a wxMaxima session in the .wxmx format.\n"
    ".wxmx files are .xml-based files contained in a .zip container like .odt\n"
    "or .docx files. After changing their name to end in .zip the .xml and\n"
    "eventual bitmap files inside them can be extracted using any .zip file\n"
    "viewer.\n"
    "The reason why part of a .wxmx file still might still seem to make sense in a\n"
    "ordinary text viewer is that the text portion of .wxmx by default\n"
    "isn't compressed: The text is typically small and compressing it would\n"
    "mean that changing a single character would (with a high probability) change\n"
    "big parts of the  whole contents of the compressed .zip archive.\n"
    "Even if version control tools like git and svn that remember all changes\n"
    "that were ever made to a file can handle binary files compression would\n"
    "make the changed part of the file bigger and therefore seriously reduce\n"
    "the efficiency of version control\n\n"
    "wxMaxima can be downloaded from https://github.com/wxMaxima-developers/wxmaxima.\n"
    "It also is part of the windows installer for maxima\n"
    "(https://wxmaxima-developers.github.io/wxmaxima/).\n\n"
    "If a .wxmx file is broken but the content.xml portion of the file can still be\n"
    "viewed using an text editor just save the xml's text as \"content.xml\"\n"
    "and try to open it using a recent version of wxMaxima.\n"
    "If it is valid XML (the XML header is intact, all opened tags are closed again,\n"
    "the text is saved with the text encoding \"UTF8 without BOM\" and the few\n"
    "special characters XML requires this for are properly escaped)\n"
    "chances are high that wxMaxima will be able to recover all code and text\n"
    "from the XML file.\n\n"
    );
  zip.CloseEntry();

  // next zip entry is "content.xml", xml of m_tree

  zip.PutNextEntry(wxT("content.xml"));
  wxString xmlText;

  xmlText << wxT("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  xmlText << wxT("\n<!--   Created using wxMaxima ") << wxT(GITVERSION) << wxT("   -->");
  xmlText << wxT("\n<!--https://wxMaxima-developers.github.io/wxmaxima/-->\n");

  // write document
  xmlText << wxT("\n<wxMaximaDocument version=\"");
  xmlText << DOCUMENT_VERSION_MAJOR << wxT(".");
  xmlText << DOCUMENT_VERSION_MINOR << wxT("\" zoom=\"");
  xmlText << int(100.0 * m_configuration->GetZoomFactor()) << wxT("\"");

  // **************************************************************************
  // Find out the number of the cell the cursor is at and save this information
  // if we find it

  // Determine which cell the cursor is at.
  long ActiveCellNumber = 1;
  GroupCell *cursorCell = NULL;
  if (m_hCaretActive)
  {
    cursorCell = GetHCaret();

    // If the cursor is before the 1st cell in the worksheet the cell number
    // is 0.
    if (!cursorCell)
      ActiveCellNumber = 0;
  }
  else
  {
    if (GetActiveCell())
      cursorCell = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
  }

  if (cursorCell == NULL)
    ActiveCellNumber = 0;
  // We want to save the information that the cursor is in the nth cell.
  // Count the cells until then.
  GroupCell *tmp = GetTree();
  if (tmp == NULL)
    ActiveCellNumber = -1;
  if (ActiveCellNumber > 0)
  {
    while ((tmp) && (tmp != cursorCell))
    {
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
      ActiveCellNumber++;
    }
  }
  // Paranoia: What happens if we didn't find the cursor?
  if (tmp == NULL) ActiveCellNumber = -1;

  // If we know where the cursor was we save this piece of information.
  // If not we omit it.
  if (ActiveCellNumber >= 0)
    xmlText << wxString::Format(wxT(" activecell=\"%li\""), ActiveCellNumber);


  // Save the variables list for the "variables" sidepane.
  wxArrayString variables = m_variablesPane->GetVarnames();
  if(variables.GetCount() > 1)
  {
    long varcount = variables.GetCount() - 1;
    xmlText += wxString::Format(" variables_num=\"%li\"", varcount);
    for(unsigned long i = 0; i<variables.GetCount(); i++)
      xmlText += wxString::Format(" variables_%li=\"%s\"", i, Cell::XMLescape(variables[i]));
  }
  
  xmlText << ">\n";

  // Reset image counter
  m_cellPointers.WXMXResetCounter();

  if (m_tree)
    xmlText += m_tree->ListToXML();

  // Delete all but one control character from the string: there should be
  // no way for them to enter this string, anyway. But sometimes they still
  // do...
  for (wxString::iterator it = xmlText.begin(); it != xmlText.end(); ++it)
  {
    wxChar c = *it;
    if ((c < wxT('\t')) ||
        ((c > wxT('\n')) && (c < wxT(' '))) ||
        (c == wxChar((char) 0x7F))
      )
    {
      // *it = wxT(' ');
    }
  }

  xmlText +=  wxT("\n</wxMaximaDocument>");

  // Prepare reading the files we have stored in memory
  wxFileSystem *fsystem = new wxFileSystem();
  fsystem->AddHandler(new wxMemoryFSHandler);
  fsystem->ChangePathTo(wxT("memory:"), true);

  // In wxWidgets 3.1.1 fsystem->FindFirst crashes if we don't have a file
  // in the memory filesystem => Let's create a file just to make sure
  // one exists.
  wxMemoryBuffer dummyBuf;
  wxMemoryFSHandler::AddFile("dummyfile",
                             dummyBuf.GetData(),
                             dummyBuf.GetDataLen());

  // Let wxWidgets test if the document can be read again by the XML parser before
  // the user finds out the hard way.
  {
    wxXmlDocument doc;
    {
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(xmlText);
      wxMemoryInputStream istream(ostream);
      doc.Load(istream);
    }

    // If we fail to load the document we abort the safe process as it will
    // only destroy data.
    // But we can still put the erroneous data into the clipboard for debugging purposes.
    if (!doc.IsOk())
    {
      if (wxTheClipboard->Open())
      {
        wxDataObjectComposite *data = new wxDataObjectComposite;
        data->Add(new wxTextDataObject(xmlText));
        wxTheClipboard->SetData(data);
        wxLogMessage(_("Save failed. The erroneous XML data has been put on the clipboard in order to allow to debug it."));
      }

      // Remove all files from our internal filesystem
      wxString memFsName = fsystem->FindFirst("*", wxFILE);
      while(memFsName != wxEmptyString)
      {
        wxString name = memFsName.Right(memFsName.Length()-7);
        wxMemoryFSHandler::RemoveFile(name);
        memFsName = fsystem->FindNext();
      }
      wxDELETE(fsystem);
      return false;
    }
  }

  // wxWidgets could pretty-print the XML document now. But as no-one will
  // look at it, anyway, there might be no good reason to do so.
  if (m_tree != NULL)output << xmlText;

  // Move all files we have stored in memory during saving to zip file
  wxString memFsName = fsystem->FindFirst("*", wxFILE);
  while(memFsName != wxEmptyString)
  {
    wxString name = memFsName.Right(memFsName.Length()-7);
    if(name != wxT("dummyfile"))
    {
      zip.CloseEntry();

      wxFSFile *fsfile = fsystem->OpenFile(memFsName);

      if (fsfile)
      {
        // The data for gnuplot is likely to change in its entirety if it
        // ever changes => We can store it in a compressed form.
        if(name.EndsWith(wxT(".data")))
          zip.SetLevel(9);
        else
          zip.SetLevel(0);

        zip.PutNextEntry(name);
        wxInputStream *imagefile = fsfile->GetStream();

        while (!(imagefile->Eof()))
          imagefile->Read(zip);

        wxDELETE(imagefile);
      }
    }
    wxMemoryFSHandler::RemoveFile(name);
    memFsName = fsystem->FindNext();
  }

  wxDELETE(fsystem);

  if (!zip.Close())
    return false;
  if (!out.Close())
    return false;

  // If all data is saved now we can overwrite the actual save file.
  // We will try to do so a few times if we suspect a MSW virus scanner or similar
  // temporarily hindering us from doing so.
  bool done = false;

  // The following line is paranoia as closing (and thus writing) the file has
  // succeeded.
  if(!wxFileExists(backupfile))
    return false;

  {
    SuppressErrorDialogs suppressor;
    done = wxRenameFile(backupfile, file, true);
    if(!done)
    {
      // We might have failed to move the file because an over-eager virus scanner wants to
      // scan it and a design decision of a filesystem driver might hinder us from moving
      // it during this action => Wait for a second and retry.
      wxSleep(1);
      done = wxRenameFile(backupfile, file, true);
    }
    if(!done)
    {
      // We might have failed to move the file because an over-eager virus scanner wants to
      // scan it and a design decision of a filesystem driver might hinder us from moving
      // it during this action => Wait for a second and retry.
      wxSleep(1);
      done = wxRenameFile(backupfile, file, true);
    }
  }
  if(!done)
  {
    wxSleep(1);
    if (!wxRenameFile(backupfile, file, true))
      return false;
  }
  if (markAsSaved)
    m_saved = true;

  wxLogMessage(_("wxmx file saved"));
  return true;
}

bool Worksheet::CanEdit()
{
  if (m_cellPointers.m_selectionStart == NULL || m_cellPointers.m_selectionEnd != m_cellPointers.m_selectionStart)
    return false;

  if (!m_cellPointers.m_selectionStart->IsEditable())
    return false;

  if (m_cellPointers.m_selectionStart->m_previous == NULL)
    return false;

  if (m_cellPointers.m_selectionStart->m_previous->GetType() != MC_TYPE_MAIN_PROMPT)
    return false;

  return true;
}

void Worksheet::OnDoubleClick(wxMouseEvent &WXUNUSED(event))
{

  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  if (GetActiveCell() != NULL)
    GetActiveCell()->SelectWordUnderCaret();
  else if (m_cellPointers.m_selectionStart != NULL)
  {
    GroupCell *parent = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
    Cell *selectionStart = m_cellPointers.m_selectionStart;
    Cell *selectionEnd = m_cellPointers.m_selectionEnd;
    parent->SelectOutput(&selectionStart, &selectionEnd);
  }

  RequestRedraw();
// Re-calculate the table of contents
  UpdateTableOfContents();
}

bool Worksheet::ActivatePrevInput()
{
  if (m_cellPointers.m_selectionStart == NULL && GetActiveCell() == NULL)
    return false;

  GroupCell *tmp;
  if (m_cellPointers.m_selectionStart != NULL)
    tmp = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
  else
  {
    tmp = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = dynamic_cast<GroupCell *>(tmp->m_previous);
  if (tmp == NULL)
    return false;

  while ((tmp != NULL) && (tmp->m_previous != NULL) && (tmp->m_previous->GetMaxDrop() == 0))
    tmp = dynamic_cast<GroupCell *>(tmp->m_previous);

  EditorCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL)
  {
    inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = dynamic_cast<GroupCell *>(tmp->m_previous);
  }

  if (inpt == NULL)
    return false;

  SetActiveCell(inpt, false);
  GetActiveCell()->CaretToEnd();

  RequestRedraw();

  return true;
}

bool Worksheet::ActivateNextInput(bool input)
{
  if (m_cellPointers.m_selectionStart == NULL && GetActiveCell() == NULL)
    return false;

  GroupCell *tmp;
  if (m_cellPointers.m_selectionStart != NULL)
    tmp = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
  else
  {
    tmp = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    SetActiveCell(NULL);
  }

  if (tmp == NULL)
    return false;

  tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  if (tmp == NULL)
    return false;

  while ((tmp != NULL) && (tmp->m_next != NULL) && (tmp->m_next->GetMaxDrop() == 0))
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);


  EditorCell *inpt = NULL;
  while (tmp != NULL && inpt == NULL)
  {
    if (input)
      inpt = dynamic_cast<EditorCell *>(tmp->GetInput());
    else
      inpt = tmp->GetEditable();
    if (inpt == NULL)
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  if (inpt == NULL)
    return false;

  SetActiveCell(inpt, false);
  GetActiveCell()->CaretToStart();

  RequestRedraw();

  return true;
}

/////////////////////////////////////////////////////////////
// methods related to evaluation queue
//
void Worksheet::AddDocumentToEvaluationQueue()
{
  FollowEvaluation(true);
  GroupCell *tmp = m_tree;
  while (tmp != NULL)
  {
    {
      AddToEvaluationQueue(tmp);
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
  }
  SetHCaret(m_last);
}

void Worksheet::AddToEvaluationQueue(GroupCell *cell)
{
  if (cell->GetGroupType() == GC_TYPE_CODE)
  {
    // Gray out the output of the cell in order to mark it as "not current".
    if (cell->GetInput())
    {
      cell->GetInput()->ContainsChanges(true);
      // ...and add it to the evaluation queue
      m_evaluationQueue.AddToQueue(cell);
    }
  }
}

/**
 * Add the entire document, including hidden cells, to the evaluation queue.
 */
void Worksheet::AddEntireDocumentToEvaluationQueue()
{
  FollowEvaluation(true);
  GroupCell *tmp = m_tree;
  while (tmp != NULL)
  {
    AddToEvaluationQueue(tmp);
    m_evaluationQueue.AddHiddenTreeToQueue(tmp);
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  SetHCaret(m_last);
}

void Worksheet::AddSectionToEvaluationQueue(GroupCell *start)
{
  // Find the begin of the current section
  start = StartOfSectioningUnit(start);

  // Find the end of the current section
  GroupCell *end = EndOfSectioningUnit(start);
  AddSelectionToEvaluationQueue(start, end);
}

void Worksheet::AddRestToEvaluationQueue()
{
  GroupCell *start = NULL;
  if(CellsSelected())
  {
    start = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());
  }

  if(GetActiveCell() != NULL)
    start = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()->m_previous);


  if(start == NULL)
    start = GetHCaret();

  if(start != NULL)
    start = dynamic_cast<GroupCell *>(start->m_next);

  if(start == NULL)
    return;

  AddSelectionToEvaluationQueue(start, m_last);
}

void Worksheet::AddSelectionToEvaluationQueue()
{
  AddSelectionToEvaluationQueue(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart), dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd));
}

void Worksheet::AddSelectionToEvaluationQueue(GroupCell *start, GroupCell *end)
{
  FollowEvaluation(true);
  if ((start == NULL) || (end == NULL))
    return;
  if (start->GetType() != MC_TYPE_GROUP)
    return;
  GroupCell *tmp = dynamic_cast<GroupCell *>(start);
  while (tmp != NULL)
  {
    AddToEvaluationQueue(tmp);
    if (tmp == end)
      break;
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  SetHCaret(dynamic_cast<GroupCell *>(end));
}

void Worksheet::AddDocumentTillHereToEvaluationQueue()
{
  FollowEvaluation(true);
  GroupCell *stop;
  if (m_hCaretActive)
    stop = m_hCaretPosition;
  else
  {
    stop = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
    if (stop->m_previous != NULL)
      stop = dynamic_cast<GroupCell *>(stop->m_previous);
  }

  if (stop != NULL)
  {
    GroupCell *tmp = m_tree;
    while (tmp != NULL)
    {
      AddToEvaluationQueue(tmp);
      if (tmp == stop)
        break;
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
  }
}

void Worksheet::AddCellToEvaluationQueue(GroupCell *gc)
{
  AddToEvaluationQueue(dynamic_cast<GroupCell *>(gc));
  SetHCaret(gc);
}

//////// end of EvaluationQueue related stuff ////////////////
void Worksheet::ScrolledAwayFromEvaluation(bool ScrolledAway)
{
  if (ScrolledAway != m_scrolledAwayFromEvaluation)
  {
    m_scrolledAwayFromEvaluation = ScrolledAway;
    if (FollowEvaluation() && (ScrolledAway))
    {
      FollowEvaluation(false);
      if ((m_mainToolBar) && (GetActiveCell() != NULL))
        m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    }
    else
    {
      if (m_mainToolBar)
        m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
    }
  }
}


void Worksheet::FollowEvaluation(bool followEvaluation)
{
  m_followEvaluation = followEvaluation;
  if (followEvaluation)
    ScrolledAwayFromEvaluation(false);
}

void Worksheet::ScrollToCellIfNeeded()
{
  if(!m_cellPointers.m_scrollToCell)
    return;
  m_cellPointers.m_scrollToCell = false;

  RecalculateIfNeeded();

  Cell *cell = m_cellPointers.CellToScrollTo();

  if (cell == NULL)
  {
    int view_x, view_y;
    GetViewStart(&view_x, &view_y);
    Scroll(view_x, 0);
    return;
  }

  if (cell == GetActiveCell())
  {
    ScrollToCaret();
    return;
  }

  int cellY = cell->GetCurrentY();

  if (cellY < 0)
  {
    RecalculateForce();
    cellY = cell->GetCurrentY();
  }

  if (cellY < 0)
    cellY = cell->GetGroup()->GetCurrentY();

  wxASSERT_MSG(cellY >= 0, _("Bug: Cell with negative y position!"));

  if (cellY < 0)
    return;

  int cellDrop = cell->GetDrop();
  int cellCenter = cell->GetCenter();

  int view_x, view_y;
  int height, width;

  GetViewStart(&view_x, &view_y);
  GetSize(&width, &height);

  view_y *= m_scrollUnit;

  int cellTop = cellY - cellCenter;
  int cellBottom = cellY + cellDrop;

  if (m_scrollToTopOfCell)
  {

    // Scroll upwards if the top of the thing we want to scroll to is less than 1/2
    // scroll unit away from the top of the page
    if (cellTop - m_scrollUnit < view_y)
      Scroll(-1, wxMax(cellTop / m_scrollUnit - 1, 0));

    // Scroll downwards if the top of the thing we want to scroll to is less than 1/2
    // scroll unit away from the bottom of the page
    if (cellTop + m_scrollUnit > view_y + height)
      Scroll(-1, wxMax(cellTop / m_scrollUnit - 1, 0));
  }
  else
  {
    // Scroll downwards if the bottom of the thing we want to scroll to is less
    // than 1/2 scroll unit away from the bottom of the page
    if (cellBottom + m_scrollUnit > view_y + height)
      Scroll(-1, wxMax(cellBottom / m_scrollUnit - 1, 0));

    // Scroll upwards if the bottom of the thing we want to scroll to is less than 1/2
    // scroll unit away from the top of the page
    if (cellBottom - m_scrollUnit < view_y)
      Scroll(-1, wxMax(cellBottom / m_scrollUnit - 1, 0));
  }
  RequestRedraw();
}

void Worksheet::Undo()
{
  if (CanUndoInsideCell())
  {
    UndoInsideCell();
    Recalculate();
  }
  else
  {
    if (CanTreeUndo())
    {
      TreeUndo();
      UpdateTableOfContents();
    }
  }
}

void Worksheet::TreeUndo_LimitUndoBuffer()
{

  wxConfigBase *config = wxConfig::Get();
  long undoLimit = 0;
  config->Read(wxT("undoLimit"), &undoLimit);

  if (undoLimit < 0)
    undoLimit = 0;

  if (undoLimit == 0)
    return;

  while ((long) treeUndoActions.size() > undoLimit)
    TreeUndo_DiscardAction(&treeUndoActions);
}

bool Worksheet::CanTreeUndo()
{
  if (treeUndoActions.empty())
    return false;
  else
  {
    // If the next undo action will delete cells we have to look if we are allowed
    // to do this.
    if (treeUndoActions.front()->m_newCellsEnd)
      return CanDeleteRegion(
              treeUndoActions.front()->m_start,
              treeUndoActions.front()->m_newCellsEnd
      );
    else return true;
  }
}

bool Worksheet::CanTreeRedo()
{
  if (treeRedoActions.empty())
  {
    return false;
  }
  else
  {
    // If the next redo action will delete cells we have to look if we are allowed
    // to do this.
    if (treeRedoActions.front()->m_newCellsEnd)
      return CanDeleteRegion(
              treeRedoActions.front()->m_start,
              treeRedoActions.front()->m_newCellsEnd
      );
    else return true;
  }
}

void Worksheet::Redo()
{
  if (CanRedoInsideCell())
  {
    RedoInsideCell();
  }
  else
  {
    if (CanTreeRedo())
    {
      TreeRedo();
      UpdateTableOfContents();
    }
  }
}

bool Worksheet::CanMergeSelection()
{
  // We cannot merge cells if not at least two cells are selected
  if (GetSelectionStart() == GetSelectionEnd())
    return false;

  // We cannot merge cells if we cannot delete the cells that are
  // removed during the merge.
  if (!CanDeleteSelection())
    return false;

  return true;
}

bool Worksheet::TreeUndoCellDeletion(std::list<TreeUndoAction *> *sourcelist, std::list<TreeUndoAction *> *undoForThisOperation)
{
  TreeUndoAction *action = sourcelist->front();
  GroupCell *newCursorPos = action->m_oldCells;
  if(newCursorPos != NULL)
    while(newCursorPos->m_next != NULL)
      newCursorPos = dynamic_cast<GroupCell *>(newCursorPos->m_next);
  InsertGroupCells(action->m_oldCells, action->m_start, undoForThisOperation);
  SetHCaret(newCursorPos);
  return true;
}

bool Worksheet::TreeUndoCellAddition(std::list<TreeUndoAction *> *sourcelist, std::list<TreeUndoAction *> *undoForThisOperation)
{
  TreeUndoAction *action = sourcelist->front();
  wxASSERT_MSG(action->m_start != NULL,
               _("Bug: Got a request to delete the cell above the beginning of the worksheet."));

  // We make the cell we want to end the deletion with visible.
  if (action->m_newCellsEnd->RevealHidden())
    FoldOccurred();

  wxASSERT_MSG(CanDeleteRegion(action->m_start, action->m_newCellsEnd),
               _("Got a request to undo an action that involves an delete which isn't possible at this moment."));

  // Set the cursor to a sane position.
  if (action->m_newCellsEnd->m_next)
    SetHCaret(dynamic_cast<GroupCell *>(action->m_newCellsEnd->m_next));
  else
    SetHCaret(dynamic_cast<GroupCell *>(action->m_start->m_previous));

  // Actually delete the cells we want to remove.
  DeleteRegion(action->m_start, action->m_newCellsEnd, undoForThisOperation);
  return true;
}

bool Worksheet::TreeUndoTextChange(std::list<TreeUndoAction *> *sourcelist, std::list<TreeUndoAction *> *undoForThisOperation)
{
  TreeUndoAction *action = sourcelist->front();

  wxASSERT_MSG(action->m_start != NULL,
               _("Bug: Got a request to change the contents of the cell above the beginning of the worksheet."));


  if (!m_tree->Contains(action->m_start))
  {
    wxASSERT_MSG(m_tree->Contains(action->m_start), _("Bug: Undo request for cell outside worksheet."));
    return false;
  }

  if (action->m_start)
  {

    // If this action actually does do nothing - we have not done anything
    // and want to make another attempt on undoing things.
    if (
      (action->m_oldText == action->m_start->GetEditable()->GetValue()) ||
      (action->m_oldText + wxT(";") == action->m_start->GetEditable()->GetValue())
      )
    {
      sourcelist->pop_front();
      return TreeUndo(sourcelist, undoForThisOperation);
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

    SetHCaret(action->m_start);

    Recalculate(true);
    RequestRedraw();

    wxASSERT_MSG(action->m_newCellsEnd == NULL,
                 _("Bug: Got a request to first change the contents of a cell and to then undelete it."));
    wxASSERT_MSG(action->m_oldCells == NULL, _("Bug: Undo action with both cell contents change and cell addition."));
    return true;
  }
  return false;
}

bool Worksheet::TreeUndo(std::list<TreeUndoAction *> *sourcelist, std::list<TreeUndoAction *> *undoForThisOperation)
{
  if (sourcelist->empty())
    return false;

  m_saved = false;

  // Seems like saving the current value of the currently active cell
  // in the tree undo buffer makes the behavior of TreeUndo feel
  // more predictable to the user.
  if (GetActiveCell())
    TreeUndo_CellLeft();

  if(sourcelist->empty())
    return false;
  TreeUndoAction *action = sourcelist->front();
  if(action == NULL)
    return false;

  if (action->m_start)
  {
    // Make sure that the cell we work on is in the visible part of the tree.
    if (action->m_start->RevealHidden())
      FoldOccurred();
  }

  bool actionContinues;
  do{
    action = sourcelist->front();
    if (action->m_newCellsEnd)
      TreeUndoCellAddition(sourcelist, undoForThisOperation);
    else
    {
      if (action->m_oldCells != NULL)
        TreeUndoCellDeletion(sourcelist, undoForThisOperation);
      else
        TreeUndoTextChange(sourcelist, undoForThisOperation);
    }
    TreeUndo_AppendAction(undoForThisOperation);
    sourcelist->pop_front();
    if(!sourcelist->empty())
      actionContinues = sourcelist->front()->m_partOfAtomicAction;
  } while (actionContinues && (!sourcelist->empty()));
  if(!undoForThisOperation->empty())
    undoForThisOperation->front()->m_partOfAtomicAction = false;
  Recalculate(true);
  RequestRedraw();
  return true;
}

/*! Mark a editor cell as the active one

 */
void Worksheet::SetActiveCell(EditorCell *cell, bool callRefresh)
{
  if(GetActiveCell() == cell)
    return;

  if (GetActiveCell() != NULL)
    TreeUndo_CellLeft();

  if(m_mainToolBar != NULL)
  {
    if(cell == NULL)
    {
      m_mainToolBar -> UnsetCellStyle();
    }
    else
      m_mainToolBar -> SetCellStyle(dynamic_cast<GroupCell *>(cell->GetGroup())->GetGroupType());
  }

  bool scrollneeded = ((GetActiveCell() != NULL) && (GetActiveCell() != cell));

  if (cell != NULL)
  {
    m_cellPointers.m_selectionStart = NULL;
    m_cellPointers.m_selectionEnd = NULL;
    cell->ActivateCursor();
    if (!m_redrawRequested) m_caretTimer.Stop();
  }
  else if (GetActiveCell())
    GetActiveCell()->DeactivateCursor();

  TreeUndo_CellEntered();

  if (cell != NULL)
  {
    m_blinkDisplayCaret = true;

    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.Start(blinktime);
    m_hCaretActive = false; // we have activated a cell .. disable caret
    m_hCaretPosition = NULL;
  }

  if (callRefresh) // = true default
    RequestRedraw();

  if ((cell != NULL) && (!m_configuration->ShowCodeCells()) &&
      (GetActiveCell()->GetType() == MC_TYPE_INPUT)
          )
  {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }
  if (scrollneeded && (cell != NULL))
    ScrollToCaret();
}


void Worksheet::SetSelection(Cell *start, Cell *end)
{
  if((m_cellPointers.m_selectionStart != start) || (m_cellPointers.m_selectionEnd != end))
    RequestRedraw();
  m_cellPointers.m_selectionStart = start;
  m_cellPointers.m_selectionEnd = end;

  if (m_cellPointers.m_selectionStart == NULL)
  {
    m_hCaretPositionStart = NULL;
    m_hCaretPositionEnd = NULL;
  }

  if(m_mainToolBar != NULL)
  {
    if ((start == NULL) && (end == NULL))
    {
      if(GetActiveCell() == NULL)
      {
        m_mainToolBar->UnsetCellStyle();
      }
    }
    else
    {
      if((start != end) && (GetActiveCell() == NULL))
      {
        m_mainToolBar->UnsetCellStyle();
      }
      else
        m_mainToolBar -> SetCellStyle(dynamic_cast<GroupCell *>(start)->GetGroupType());
    }
  }
}

bool Worksheet::PointVisibleIs(wxPoint point)
{
  int view_x, view_y;
  int height, width;

  CalcUnscrolledPosition(0, 0, &view_x, &view_y);

  GetSize(&width, &height);

  if ((point.y < view_y) ||
      (point.y > view_y + height
                 - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20))
    return false;

  if ((point.x < view_x) || (point.x > view_x + width
                                       - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20))
    return false;

  return true;
}

void Worksheet::ShowPoint(wxPoint point)
{
  wxASSERT((point.x >=0) && (point.y >=0));
  if (point.x < 0 || point.y < 0)
    return;

  int view_x, view_y;
  int height, width;
  bool sc = false;

  int scrollToX = -1, scrollToY = -1;

  // Get the position [in pixels] the visible portion of the worksheet starts at
  GetViewStart(&view_x, &view_y);
  view_x *= m_scrollUnit;
  view_y *= m_scrollUnit;

  // Get the size of the worksheet window
  GetSize(&width, &height);
  // The scrollbars make part of the window size, but not of the
  // size usable for text
  height -= wxSystemSettings::GetMetric(wxSYS_VTHUMB_Y);
  width -= wxSystemSettings::GetMetric(wxSYS_HTHUMB_X);

  Configuration *configuration = m_configuration;
  int fontsize_px = configuration->GetZoomFactor() * configuration->GetDefaultFontSize();
  if (
          (point.y - fontsize_px < view_y) ||
          (point.y + fontsize_px > view_y + height - 20)
          )
  {
    sc = true;
    scrollToY = point.y - height / 2;
  }
  else
    scrollToY = view_y;

  if (
          (point.x - fontsize_px < view_x) ||
          (point.x + 2 > view_x + width - 20)
          )
  {
    sc = true;
    scrollToX = point.x - width / 2;
  }
  else
    scrollToX = view_x;

  if (sc)
  {
    Scroll(scrollToX / m_scrollUnit, scrollToY / m_scrollUnit);
  }
}

bool Worksheet::CutToClipboard()
{
  if (GetActiveCell() != NULL)
  {
    GetActiveCell()->CutToClipboard();
    GetActiveCell()->GetGroup()->ResetSize();
    Recalculate();
    RequestRedraw();
    return true;
  }
  else if (m_cellPointers.m_selectionStart != NULL && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
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
void Worksheet::PasteFromClipboard()
{
  bool cells = false;

  // Check for cell structure
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (!wxTheClipboard->Open())
    return;

  // Check if the clipboard contains text.
  if ((wxTheClipboard->IsSupported(wxDF_TEXT)) || (wxTheClipboard->IsSupported(m_wxmFormat)))
  {
    wxString inputs;
    if (wxTheClipboard->IsSupported(m_wxmFormat))
    {
      wxmDataObject data;
      wxTheClipboard->GetData(data);
      inputs = wxString::FromUTF8((char *) data.GetData()) + wxT("\0");
    }
    else
    {
      wxTextDataObject data;
      wxTheClipboard->GetData(data);
      inputs = data.GetText();
    }

    if (inputs.StartsWith(wxT("/* [wxMaxima: ")))
    {

      // Convert the text from the clipboard into an array of lines
      wxStringTokenizer lines(inputs, wxT("\n"));
      wxArrayString lines_array;
      while (lines.HasMoreTokens())
        lines_array.Add(lines.GetNextToken());

      // Load the array like we would do with a .wxm file
      GroupCell *contents = CreateTreeFromWXMCode(&lines_array);

      // Add the result of the last operation to the worksheet.
      if (contents)
      {
        // ! Tell the rest of this function that we have found cells
        cells = true;

        // Search for the last cell we want to paste
        GroupCell *end = contents;
        while (end->m_next != NULL)
          end = dynamic_cast<GroupCell *>(end->m_next);

        // Now paste the cells
        if (m_tree == NULL)
        {
          // Empty work sheet => We paste cells as the new cells
          m_tree = contents;
          m_last = end;
        }
        else
        {
          if (m_hCaretActive)
          {
            if ((m_cellPointers.m_selectionStart != NULL) && (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP))
            {
              DeleteSelection();
              TreeUndo_AppendAction();
            }
            InsertGroupCells(contents,GetHCaret());
          }
          else
            InsertGroupCells(contents,dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
        }
        NumberSections();
        Recalculate();
        RequestRedraw();
        SetHCaret(end);
      }
    }
  }
  // Check if the clipboard contains an image.
  else if (wxTheClipboard->IsSupported(wxDF_BITMAP))
  {
    OpenHCaret(wxEmptyString, GC_TYPE_IMAGE);
    GroupCell *group = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());

    if (group != NULL)
    {
      wxBitmapDataObject bitmap;
      wxTheClipboard->GetData(bitmap);
      ImgCell *ic = new ImgCell(group, &m_configuration, &m_cellPointers, bitmap.GetBitmap());
      group->AppendOutput(ic);
    }
  }

  // Clipboard does not have the cell structure.
  if (!cells)
  {
    if (GetActiveCell() != NULL)
    {
      GetActiveCell()->PasteFromClipboard();
      GetActiveCell()->ResetSize();
      GetActiveCell()->GetGroup()->ResetSize();
      Recalculate(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
      RequestRedraw();
    }
    else
    {
      if (m_hCaretActive == true)
      {
        if (wxTheClipboard->IsSupported(wxDF_TEXT))
        {
          wxTextDataObject obj;
          wxTheClipboard->GetData(obj);
          wxString txt = obj.GetText();

          OpenHCaret(txt);
          RequestRedraw();
        }
      }
    }
  }

  // Make sure the clipboard is closed!
  wxTheClipboard->Close();

  UpdateMLast();
  UpdateTableOfContents();
  ScrolledAwayFromEvaluation();
}

void Worksheet::SelectAll()
{
  if (GetActiveCell() == NULL && m_tree != NULL)
  {
    SetSelection(m_tree, m_last);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    m_hCaretActive = false;
  }
  else if (GetActiveCell() != NULL)
  {
    if (!GetActiveCell()->AllSelected())
      GetActiveCell()->SelectAll();
    else
    {
      SetActiveCell(NULL);
      SetSelection(m_tree, m_last);
      m_clickType = CLICK_TYPE_GROUP_SELECTION;
      m_hCaretActive = false;
    }
  }
  ScrolledAwayFromEvaluation();
  RequestRedraw();
}

void Worksheet::DivideCell()
{
  if (GetActiveCell() == NULL)
    return;

  GroupCell *parent = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
  if (parent->GetEditable() != GetActiveCell())
    return;

  GroupType gctype = parent->GetGroupType();
  if (gctype == GC_TYPE_IMAGE)
    return;

  if (GetActiveCell()->CaretAtStart() || GetActiveCell()->CaretAtEnd())
    return;

  dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup())->RemoveOutput();

  wxString newcellstring = GetActiveCell()->DivideAtCaret();

  SetHCaret(parent);
  OpenHCaret(newcellstring, gctype);
  if (GetActiveCell())
    GetActiveCell()->CaretToStart();
  ScrolledAwayFromEvaluation();
  Recalculate();
}

void Worksheet::MergeCells()
{
  wxString newcell = wxEmptyString;
  Cell *tmp = m_cellPointers.m_selectionStart;
  if (!tmp)
    return;
  if (tmp->GetType() != MC_TYPE_GROUP)
    return; // should not happen

  while (tmp)
  {
    if (newcell.Length() > 0)
      newcell += wxT("\n");
    newcell += dynamic_cast<GroupCell *>(tmp)->GetEditable()->GetValue();

    if (tmp == m_cellPointers.m_selectionEnd)
      break;
    tmp = tmp->m_next;
  }

  EditorCell *editor = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart)->GetEditable();
  editor->SetValue(newcell);

  m_cellPointers.m_selectionStart = dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->m_next);
  DeleteSelection();
  editor->GetGroup()->ResetSize();
  dynamic_cast<GroupCell *>(editor->GetGroup())->ResetInputLabel();
  dynamic_cast<GroupCell *>(editor->GetGroup())->RemoveOutput();
  editor->ResetSize();
  Recalculate();
  SetActiveCell(editor, true);
  ScrolledAwayFromEvaluation();
}

void Worksheet::OnSetFocus(wxFocusEvent &event)
{
  m_hasFocus = true;
  // We want the cursor to blink in this case
  int blinktime = wxCaret::GetBlinkTime();
  if (blinktime < 200)
    blinktime = 200;
  m_caretTimer.Start(blinktime);
  if (GetActiveCell() != NULL)
    GetActiveCell()->SetFocus(true);

  // And we want the cursor start in its visible phase.

  wxTimerEvent dummy(m_timer);
  dummy.SetId(CARET_TIMER_ID);
  OnTimer(dummy);
  event.Skip();
}

void Worksheet::OnKillFocus(wxFocusEvent &event)
{
  m_hasFocus = false;
  if (GetActiveCell() != NULL)
    GetActiveCell()->SetFocus(false);
  event.Skip();
}

void Worksheet::CheckUnixCopy()
{
  if (CanCopy(true))
  {
    wxTheClipboard->UsePrimarySelection(true);
    if (wxTheClipboard->IsUsingPrimarySelection())
    {
      wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
      if (wxTheClipboard->Open())
      {
        wxDataObjectComposite *data = new wxDataObjectComposite;
        // The \0 seems to prevent data corruption on seleting strings while evaluating.
        // The wxTextDataObject is a speculative go at the same bug.
        data->Add(new wxTextDataObject(GetString() + wxT('\0')));
        wxTheClipboard->SetData(data);
        wxTheClipboard->Close();
      }
    }
    wxTheClipboard->UsePrimarySelection(false);
  }
}

//! Is this cell selected?
bool Worksheet::IsSelected(CellType type)
{
  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  if (type == MC_TYPE_IMAGE || type == MC_TYPE_SLIDE)
  {
    if (m_cellPointers.m_selectionStart != m_cellPointers.m_selectionEnd || m_cellPointers.m_selectionStart->GetType() != type)
      return false;
    else
      return true;
  }
  else if (m_cellPointers.m_selectionStart->GetType() != type)
    return false;

  return true;
}

//! Starts playing the animation of a cell generated with the with_slider_* commands
void Worksheet::Animate(bool run)
{
  if (CanAnimate())
  {
    SlideShow *slideShow = dynamic_cast<SlideShow *>(GetSelectionStart());
    slideShow->AnimationRunning(run);
  }
}

bool Worksheet::IsSelectionInWorkingGroup()
{
  if (m_cellPointers.m_selectionStart == NULL)
    return false;

  if (GetWorkingGroup() == NULL)
    return false;

  if (m_cellPointers.m_selectionStart->GetGroup() != GetWorkingGroup())
    return false;

  return true;
}

GroupCell *Worksheet::GetHCaret()
{
  if (m_hCaretActive)
    return m_hCaretPosition;

  if (GetActiveCell())
    return dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());

  if (m_cellPointers.m_selectionStart)
    return dynamic_cast<GroupCell *>(m_cellPointers.m_selectionStart->GetGroup());

  if (MouseSelectionStart() != NULL)
    return dynamic_cast<GroupCell *>(MouseSelectionStart()->GetGroup());

  // A fallback value that is returned if nothing else seems to work
  return m_last;
}

void Worksheet::SetDefaultHCaret()
{
  SetHCaret(m_last);
}

void Worksheet::OnActivate(wxActivateEvent &WXUNUSED(event))
{
  // If the focus changes we might want to refresh the menu.
  RequestRedraw();
}

void Worksheet::SetHCaret(GroupCell *where)
{
  SetSelection(NULL);
  if(m_mainToolBar != NULL)
  {
    m_mainToolBar->UnsetCellStyle();
  }

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  SetActiveCell(NULL, false);
  if (where != NULL)
    wxASSERT_MSG(
            where->GetType() == MC_TYPE_GROUP,
            _("Bug: Trying to move the horizontally-drawn cursor to a place inside a GroupCell."));

  if(m_hCaretPosition != where)
  {
    m_hCaretPosition = where;
    m_hCaretActive = true;

    RequestRedraw();
    if (where != NULL)
      ScheduleScrollToCell(where, false);

    // Tell the cursor to blink, but to be visible right now.
    m_blinkDisplayCaret = true;
    m_hCaretBlinkVisible = true;

    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.Start(blinktime);
  }
  m_hCaretPosition = where;
  m_hCaretActive = true;
}

void Worksheet::ShowHCaret()
{
  if (m_hCaretPosition == NULL)
  {
    if (m_last != NULL)
    {
      SetHCaret(m_last);
    }
    else
      SetHCaret(NULL);
  }

  m_hCaretActive = true;
}

bool Worksheet::CanUndoInsideCell()
{
  if (GetActiveCell() == NULL)
    return false;
  return GetActiveCell()->CanUndo();
}

void Worksheet::UndoInsideCell()
{
  if (GetActiveCell() != NULL)
  {
    GetActiveCell()->Undo();
    GetActiveCell()->GetGroup()->ResetSize();
    GetActiveCell()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

bool Worksheet::CanRedoInsideCell()
{
  if (GetActiveCell() == NULL)
    return false;
  return GetActiveCell()->CanRedo();
}

void Worksheet::RedoInsideCell()
{
  if (GetActiveCell() != NULL)
  {
    GetActiveCell()->Redo();
    GetActiveCell()->GetGroup()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

void Worksheet::SaveValue()
{
  if (GetActiveCell() != NULL)
    GetActiveCell()->SaveValue();
}

void Worksheet::RemoveAllOutput()
{
  // We don't want to remove all output if maxima is currently evaluating.
  if (GetWorkingGroup() != NULL)
    return;

  if(CellsSelected())
  {
    // If the selection is in the output we want to remove the selection.
    if(m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP)
      SetSelection(NULL);
  }

  SetActiveCell(NULL);

  RemoveAllOutput(m_tree);

  Recalculate();
  RequestRedraw();
}

void Worksheet::RemoveAllOutput(GroupCell *tree)
{
  if (tree == NULL)
    tree = m_tree;

  while (tree != NULL)
  {
    // If this function actually does do something we
    // should enable the "save" button.
    m_saved = false;

    tree->RemoveOutput();

    GroupCell *sub = tree->GetHiddenTree();
    if (sub != NULL)
      RemoveAllOutput(sub);
    tree = dynamic_cast<GroupCell *>(tree->m_next);
  }
  m_configuration->AdjustWorksheetSize(true);
}

void Worksheet::OnMouseMiddleUp(wxMouseEvent &event)
{
  m_cellPointers.ResetSearchStart();

  wxTheClipboard->UsePrimarySelection(true);
  if(wxTheClipboard->IsUsingPrimarySelection())
  {
    OnMouseLeftDown(event);
    m_leftDown = false;
    if (m_clickType != CLICK_TYPE_NONE)
      PasteFromClipboard();
    m_clickType = CLICK_TYPE_NONE;
    if(HasCapture())
      ReleaseMouse();
    wxTheClipboard->UsePrimarySelection(false);
  }
}

void Worksheet::CommentSelection()
{
  if (GetActiveCell())
  {
    EditorCell *active = GetActiveCell();
    active->CommentSelection();
    active->ResetSize();
    active->GetGroup()->ResetSize();
    Recalculate(dynamic_cast<GroupCell *>(active->GetGroup()));
  }
}

void Worksheet::OnScrollChanged(wxScrollEvent &WXUNUSED(ev))
{
  // Did we scroll away from the cell that is being currently evaluated?
  // If yes we want to no more follow the evaluation with the scroll and
  // want to enable the button that brings us back.
  ScrolledAwayFromEvaluation();

  // We don't want to start the autosave while the user is scrolling through
  // the document since this will shortly halt the scroll
  m_keyboardInactiveTimer.StartOnce(10000);
}

wxString Worksheet::GetInputAboveCaret()
{
  if (!m_hCaretActive || m_hCaretPosition == NULL)
    return wxEmptyString;

  EditorCell *editor = dynamic_cast<EditorCell *>(m_hCaretPosition->GetEditable());

  if (editor != NULL)
    return editor->ToString();
  return wxEmptyString;
}

wxString Worksheet::GetOutputAboveCaret()
{
  if (!m_hCaretActive || m_hCaretPosition == NULL)
    return wxEmptyString;

  Cell *selectionStart = m_cellPointers.m_selectionStart;
  Cell *selectionEnd = m_cellPointers.m_selectionEnd;
  m_hCaretPosition->SelectOutput(&selectionStart, &selectionEnd);

  wxString output = GetString();

  SetSelection(NULL);

  RequestRedraw();

  return output;
}

bool Worksheet::FindIncremental(wxString str, bool down, bool ignoreCase)
{
  if (SearchStart() != NULL)
  {
    SetActiveCell(SearchStart());
    SearchStart()->CaretToPosition(IndexSearchStartedAt());
  }
  if (str != wxEmptyString)
    return FindNext(str, down, ignoreCase, false);
  else
    return true;
}

bool Worksheet::FindNext(wxString str, bool down, bool ignoreCase, bool warn)
{
  if (m_tree == NULL)
    return false;

  GroupCell *pos;
  int starty;
  if (down)
    starty = 0;
  else
  {
    wxSize canvasSize = GetClientSize();
    starty = canvasSize.y;
  }

  // Default the start of the search at the top or the bottom of the screen
  wxPoint topleft;
  CalcUnscrolledPosition(0, starty, &topleft.x, &topleft.y);
  pos = m_tree;
  while (pos != NULL)
  {
    wxRect rect = pos->GetRect();
    if (rect.GetBottom() > topleft.y)
      break;
    pos = dynamic_cast<GroupCell *>(pos->m_next);
  }

  if (pos == NULL)
  {
    if (down)
      pos = m_tree;
    else
      pos = m_last;
  }

  // If a cursor is active we start the search there instead
  if (GetActiveCell() != NULL)
    pos = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
  else if (m_hCaretActive)
  {
    if (down)
    {
      if (m_hCaretPosition != NULL)
      {
        if (m_hCaretPosition->m_next != NULL)
          pos = dynamic_cast<GroupCell *>(m_hCaretPosition->m_next);
        else
          pos = m_hCaretPosition;
      }
    }
    else
    {
      pos = m_hCaretPosition;
    }
  }

  // If we still don't have a place to start searching we have definitively tried to
  // search in any empty worksheet and know we won't get any result.
  if (pos == NULL)
    return false;

  pos->GetEditable()->SearchStartedHere(pos->GetEditable()->GetCaretPosition());

  // Remember where to go if we need to wrapp the search.
  GroupCell *start = pos;

  bool wrappedSearch = false;

  while ((pos != start) || (!wrappedSearch))
  {
    EditorCell *editor = dynamic_cast<EditorCell *>(pos->GetEditable());

    if (editor != NULL)
    {
      bool found = editor->FindNext(str, down, ignoreCase);

      if (found)
      {
        int start, end;
        editor->GetSelection(&start, &end);
        SetActiveCell(editor);
        editor->SetSelection(start, end);
        ScrollToCaret();
        UpdateTableOfContents();
        RequestRedraw();
        if ((wrappedSearch) && warn)
        {
          wxMessageDialog dialog(m_findDialog,
                                 _("Wrapped search"),
                                 wxEmptyString, wxCENTER | wxOK);
          dialog.ShowModal();
        }
        return true;
      }
    }

    if (down)
    {
      pos = dynamic_cast<GroupCell *>(pos->m_next);
      if (pos == NULL)
      {
        wrappedSearch = true;
        pos = m_tree;
      }
    }
    else
    {
      pos = dynamic_cast<GroupCell *>(pos->m_previous);
      if (pos == NULL)
      {
        wrappedSearch = true;
        pos = m_last;
      }
    }
  }
  return false;
}

bool Worksheet::CaretVisibleIs()
{
  if (m_hCaretActive)
  {
    int y = -1;
    if (m_hCaretPosition)
      y = m_hCaretPosition->GetCurrentY();

    int view_x, view_y;
    int height, width;

    GetViewStart(&view_x, &view_y);
    GetSize(&width, &height);

    view_x *= m_scrollUnit;
    view_y *= m_scrollUnit;

    return ((y >= view_y) && (y <= view_y + height));
  }
  else
  {
    if (GetActiveCell())
    {
      wxPoint point = GetActiveCell()->PositionToPoint(m_configuration->GetDefaultFontSize());
      if (point.y < 1)
      {
        RecalculateForce();
        point = GetActiveCell()->PositionToPoint(m_configuration->GetDefaultFontSize());
      }
      return PointVisibleIs(point);
    }
    else
      return false;
  }

}

void Worksheet::ScrollToCaret()
{
  RecalculateIfNeeded();
  if (m_hCaretActive)
  {
    ScheduleScrollToCell(m_hCaretPosition, false);
  }
  else
  {
    if (GetActiveCell())
    {
      wxPoint point = GetActiveCell()->PositionToPoint(m_configuration->GetDefaultFontSize());
      if (point.y == -1)
      {
        RecalculateForce();
        point = GetActiveCell()->PositionToPoint(m_configuration->GetDefaultFontSize());
      }
      if (QuestionPending())
      {
        point.x = 0;
        point.y = GetActiveCell()->GetGroup()->GetCurrentY();
      }
      else
        ShowPoint(point);
    }
  }
}

void Worksheet::Replace(wxString oldString, wxString newString, bool ignoreCase)
{
  if (GetActiveCell() != NULL)
  {
    if (GetActiveCell()->ReplaceSelection(oldString, newString, false, ignoreCase))
    {
      m_saved = false;
      GroupCell *group = dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup());
      group->ResetInputLabel();
      group->ResetSize();
      GetActiveCell()->ResetSize();
      Recalculate();
      Refresh();
    }
    GetActiveCell()->SearchStartedHere();
  }
}

int Worksheet::ReplaceAll(wxString oldString, wxString newString, bool ignoreCase)
{
  m_cellPointers.ResetSearchStart();

  if (m_tree == NULL)
    return 0;

  int count = 0;

  GroupCell *tmp = m_tree;

  while (tmp != NULL)
  {
    EditorCell *editor = dynamic_cast<EditorCell *>(tmp->GetEditable());

    if (editor != NULL)
    {
      int replaced = editor->ReplaceAll(oldString, newString, ignoreCase);
      if (replaced > 0)
      {
        count += replaced;
        tmp->ResetInputLabel();
        tmp->ResetSize();
      }
    }

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

  if (count > 0)
  {
    m_saved = false;
    Recalculate();
    RequestRedraw();
  }

  return count;
}

bool Worksheet::Autocomplete(AutoComplete::autoCompletionType type)
{
  EditorCell *editor = GetActiveCell();

  if (editor == NULL)
    return false;


  wxString partial;
  if(type != AutoComplete::esccommand)
  {
    editor->SelectWordUnderCaret(false, false, true);
    partial = editor->GetSelectionString();
  }

  if (type == AutoComplete::command)
  {

    // Let's look if we want to complete a unit instead of a command.
    bool inEzUnit = true;
    wxString frontOfSelection = editor->TextInFrontOfSelection();
    int positionOfEzunitStart = frontOfSelection.rfind(wxT('`'));

    if (positionOfEzunitStart != wxNOT_FOUND)
    {
      frontOfSelection = frontOfSelection.Mid(positionOfEzunitStart + 1);
      int numberOfParenthesis = 0;

      for (size_t i = 0; i < frontOfSelection.Length() - 1; i++)
      {
        wxChar ch = frontOfSelection[i];
        if (
                (!wxIsalnum(ch)) &&
                (ch != wxT('(')) &&
                (ch != wxT(')')) &&
                (ch != wxT('*')) &&
                (ch != wxT('/'))
                )
          inEzUnit = false;

        if (ch == wxT('('))
          numberOfParenthesis++;
        if (ch == wxT(')'))
        {
          numberOfParenthesis++;
          if (numberOfParenthesis < 0)
            inEzUnit = false;
        }
      }
    }
    else
      inEzUnit = false;
    if (inEzUnit)
      type = AutoComplete::unit;

    // If we don't have an unit to complete we perhaps want to autocomplete a package name
    // or the name of a demo file
    if(!inEzUnit)
    {
      wxString currentCommand = editor->GetCurrentCommand();
      if((currentCommand == wxT("load")) ||
         (currentCommand == wxT("batchload")) ||
         (currentCommand == wxT("batch")))
      {
        type = AutoComplete::loadfile;
        if(partial == wxEmptyString)
          partial = wxString("\"");
      }

      if(currentCommand == wxT("demo"))
      {
        type = AutoComplete::demofile;
        if(partial == wxEmptyString)
          partial = wxString("\"");
      }

      if((type == AutoComplete::demofile) || (type == AutoComplete::loadfile))
      {
        if(partial[0] != wxT('\"'))
        {
          partial = wxT("\"") + partial;
          // If the editor auto-adds a closing quote this causes auto-completion to fail
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true, false, true);
        }
        if((partial.EndsWith("\"") && (!(partial.EndsWith("\\\"")))))
        {
          partial = partial.Left(partial.Length() - 1);
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true);
        }
      }

      if((type == AutoComplete::command) && (partial[0] == wxT('\"')))
        type = AutoComplete::generalfile;

      if(type == AutoComplete::demofile)
        m_autocomplete->UpdateDemoFiles(partial,
                                       wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if(type == AutoComplete::loadfile)
        m_autocomplete->UpdateLoadFiles(partial,
                                       wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if(type == AutoComplete::generalfile)
        m_autocomplete->UpdateGeneralFiles(partial,
                                          wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));
    }
  }

  if (type == AutoComplete::command)
  {
    // Update the list of words that might not be defined as maxima function or variable
    // but that still appear on the workSheet.
    m_autocomplete->ClearWorksheetWords();
    GroupCell *tmp = m_tree;
    while (tmp != NULL)
    {
      // Don't collect the current word as possible autocompletion.
      if (tmp != GetActiveCell()->GetGroup())
      {
        // Only collect words from Code Cells.
        if ((tmp->GetGroupType() == GC_TYPE_CODE) && (tmp->GetEditable() != NULL))
          m_autocomplete->AddWorksheetWords(tmp->GetEditable()->GetWordList());
      }
      else
      {
        if ((tmp->GetGroupType() == GC_TYPE_CODE) && (tmp->GetEditable() != NULL))
        {
          wxArrayString wordList = tmp->GetEditable()->GetWordList();

          // The current unfinished word is no valid autocompletion, if there is
          // such a thing.
          if (partial.Length() > 0)
          {
            // Don't remove the current word from autocompletion if it never has been
            // added (which happens if autocompletion is called when the cursor is
            // directly followed by the next command without a space or similar inbetween)
            if (wordList.Index(partial) != wxNOT_FOUND)
              wordList.Remove(partial);
          }
          m_autocomplete->AddWorksheetWords(wordList);
        }
      }
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
  }

  m_completions = m_autocomplete->CompleteSymbol(partial, type);
  m_completions.Sort();
  m_autocompleteTemplates = (type == AutoComplete::tmplte);

  /// No completions - clear the selection and return false
  if (m_completions.GetCount() == 0)
  {
    editor->ClearSelection();
    return false;
  }

  /// If there is only one completion, use it
  if ((m_completions.GetCount() == 1) && (type != AutoComplete::esccommand))
  {
    int start, end;
    editor->GetSelection(&start, &end);

    editor->ReplaceSelection(editor->GetSelectionString(), m_completions[0], true, false, true);
    editor->ClearSelection();
    editor->CaretToPosition(start);

    if ((type != AutoComplete::tmplte) || !editor->FindNextTemplate())
      editor->CaretToPosition(start + m_completions[0].Length());

    editor->ResetSize();
    editor->GetGroup()->ResetSize();
    Recalculate(dynamic_cast<GroupCell *>(editor->GetGroup()));

    RequestRedraw();
  }

    /// If there are more than one completions, popup a menu
  else
  {

    // Find the position for the popup menu
    wxPoint pos = editor->PositionToPoint(m_configuration->GetDefaultFontSize());
    wxASSERT((pos.x>=0) && (pos.y >=0));
    #ifndef __WXMSW__
    CalcScrolledPosition(pos.x, pos.y, &pos.x, &pos.y);
    #endif
    // The popup menu appears half a character too high.
    pos.y += m_configuration->Scale_Px(m_configuration->GetFontSize(TS_TEXT));
    m_autocompletePopup = new AutocompletePopup(this,editor,m_autocomplete,type,&m_autocompletePopup);

    // If necessary: Scroll right or down so that the pop-up is visible as a whole.
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    int width;
    int height;
    GetClientSize(&width, &height);
    m_autocompletePopup -> SetPosition(pos);
    m_autocompletePopup -> Create(this);
    m_autocompletePopup -> UpdateResults();
    m_autocompletePopup -> SetFocus();
    wxRect popupRect = m_autocompletePopup -> GetRect();
    wxRect screenRect = wxRect(topleft, topleft+wxPoint(width,height));
    if(screenRect.GetRight() < popupRect.GetRight())
      screenRect.SetLeft(screenRect.GetLeft()+popupRect.GetRight()-screenRect.GetRight());
    if(screenRect.GetBottom() < popupRect.GetBottom())
      screenRect.SetTop(screenRect.GetTop()+popupRect.GetBottom()-screenRect.GetBottom());
    if(screenRect.GetTopLeft() != topleft)
    {
      Scroll(screenRect.GetTopLeft());
      RequestRedraw();
    }
  }
  return true;
}

void Worksheet::OnComplete(wxCommandEvent &event)
{
  if (GetActiveCell() == NULL)
    return;

  EditorCell *editor = dynamic_cast<EditorCell *>(GetActiveCell());
  int caret = editor->GetCaretPosition();

  if (editor->GetSelectionString() != wxEmptyString)
    editor->ReplaceSelection(editor->GetSelectionString(),
                             m_completions[event.GetId() - popid_complete_00], true, false, true);
  else
    editor->InsertText(m_completions[event.GetId() - popid_complete_00]);

  if (m_autocompleteTemplates)
  {
    int sel_start, sel_end;
    editor->GetSelection(&sel_start, &sel_end);
    editor->ClearSelection();

    editor->CaretToPosition(caret);
    if (!editor->FindNextTemplate())
      editor->CaretToPosition(sel_start + m_completions[event.GetId() - popid_complete_00].Length());
  }

  editor->ResetSize();
  editor->GetGroup()->ResetSize();
  Recalculate(dynamic_cast<GroupCell *>(editor->GetGroup()));

  RequestRedraw();
}


void Worksheet::SetActiveCellText(wxString text)
{
  EditorCell *active = dynamic_cast<EditorCell *>(GetActiveCell());
  if (active != NULL)
  {
    GroupCell *parent = dynamic_cast<GroupCell *>(active->GetGroup());
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
      Recalculate(parent, false);
      RequestRedraw();
    }
  }
  else
    OpenHCaret(text);
}

bool Worksheet::InsertText(wxString text)
{
  CloseAutoCompletePopup();

  if (GetActiveCell())
  {
    if (GCContainsCurrentQuestion(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup())))
    {
      m_followEvaluation = true;
      OpenQuestionCaret(text);
    }
    else
    {
      GetActiveCell()->InsertText(text);
      Recalculate(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
      RequestRedraw(dynamic_cast<GroupCell *>(GetActiveCell()->GetGroup()));
    }
  }
  else
    OpenHCaret(text);
  return true;
}

void Worksheet::OpenNextOrCreateCell()
{
  if (m_hCaretPosition && m_hCaretPosition->m_next)
  {
    SetSelection(m_hCaretPosition);
    ActivateNextInput();
  }
  else
    OpenHCaret();
}

void Worksheet::SelectGroupCell(GroupCell *cell)
{
  SetSelection(cell);
  m_hCaretActive = false;
  SetActiveCell(NULL);
  if (cell)
  {
    if (GCContainsCurrentQuestion(cell))
    {
      FollowEvaluation(true);
      OpenQuestionCaret();
    }
    m_hCaretPositionEnd = cell;
    m_hCaretPositionStart = cell;
  }

}

void Worksheet::OnFollow()
{
  if (GetWorkingGroup())
  {
    FollowEvaluation(true);

    if (GCContainsCurrentQuestion(GetWorkingGroup()))
    {
      OpenQuestionCaret();
      ScheduleScrollToCell(GetWorkingGroup(), false);
    }
    else
    {
      if (GetWorkingGroup()->RevealHidden())
      {
        FoldOccurred();
        Recalculate(true);
      }
      SetSelection(GetWorkingGroup());
      SetHCaret(GetWorkingGroup());
      ScheduleScrollToCell(GetWorkingGroup(), false);
    }
  }
}

Worksheet::MathMLDataObject::MathMLDataObject() : wxCustomDataObject(m_mathmlFormat)
{
}

Worksheet::MathMLDataObject::MathMLDataObject(wxString data) : wxCustomDataObject(m_mathmlFormat)
{
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::wxmDataObject::wxmDataObject() : wxCustomDataObject(m_wxmFormat)
{
}

Worksheet::wxmDataObject::wxmDataObject(wxString data) : wxCustomDataObject(m_wxmFormat)
{
  data += wxT('\0');
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::MathMLDataObject2::MathMLDataObject2() : wxCustomDataObject(m_mathmlFormat2)
{
}

Worksheet::MathMLDataObject2::MathMLDataObject2(wxString data) : wxCustomDataObject(m_mathmlFormat2)
{
  data += wxT('\0');
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::RtfDataObject::RtfDataObject() : wxCustomDataObject(m_rtfFormat)
{
}

Worksheet::RtfDataObject::RtfDataObject(wxString data) : wxCustomDataObject(m_rtfFormat)
{
  data += wxT('\0');
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::RtfDataObject2::RtfDataObject2() : wxCustomDataObject(m_rtfFormat2)
{
}

Worksheet::RtfDataObject2::RtfDataObject2(wxString data) : wxCustomDataObject(m_rtfFormat2)
{
  data += wxT('\0');
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

wxString Worksheet::RTFStart()
{
  // The beginning of the RTF document
  wxString document = wxT("{\\rtf1\\ansi\\deff0\n\n");

  // The font table
  document += wxT("{\\fonttbl{\\f0\\froman Times;}}\n\n");

  // Define all colors we want to use
  document += wxT("{\\colortbl;\n");
  for (int i = 1; i < NUMBEROFSTYLES; i++)
  {
    wxColor color = wxColor(m_configuration->GetColor(i));
    if (color.IsOk())
      document += wxString::Format(wxT("\\red%i\\green%i\\blue%i;\n"), color.Red(), color.Green(), color.Blue());
    else
      document += wxString::Format(wxT("\\red%i\\green%i\\blue%i;\n"), 0, 0, 0);
  }
  document += wxT("}\n\n");

  /* Our style sheet:
     Style  Meaning
       0    Ordinary text
       1    Chapter Cell
       2    Section Cell
       3    Subsection Cell
       16   Title Cell
       21   Math Cell
       22   Math Cell with Label
  */
  document += wxT("{\\stylesheet\n");
  document += wxT("{\\s0\\snext0\\widctlpar\\hyphpar0\\kerning1\\li0\\ri0\\lin0\\rin0\\fi0\\f0\\fs24 Normal;}\n");
  document += wxT("{\\s1\\outlinelevel0\\keepn\\b\\f0\\fs40\\sbasedon16\\snext0 Section Cell;}\n");
  document += wxT("{\\s2\\outlinelevel1\\keepn\\b\\f0\\fs36\\sbasedon1\\snext0 Subsection Cell;}\n");
  document += wxT("{\\s3\\outlinelevel2\\keepn\\b\\f0\\fs32\\sbasedon2\\snext0 SubSubsection Cell;}\n");
  document += wxT("{\\s4\\outlinelevel3\\keepn\\b\\f0\\fs30\\sbasedon2\\snext0 Heading5 Cell;}\n");
  document += wxT("{\\s5\\outlinelevel4\\keepn\\b\\f0\\fs28\\sbasedon2\\snext0 Heading6 Cell;}\n");
  document += wxT("{\\s16\\keepn\\b\\f0\\fs56\\snext0 Title Cell;}\n");
  document += wxT("{\\s21\\li1105\\lin1105\\f0\\fs24\\sbasedon0 Math;}\n");
  document += wxT("{\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24\\sbasedon0\\snext21 Math+Label;}\n");
  document += wxT("}\n\n{\n");
  return document;
}

wxString Worksheet::RTFEnd()
{
  wxString document;
  // Close the document
  document += wxT("}\n}");

  return document;
}

void Worksheet::OnMouseCaptureLost(wxMouseCaptureLostEvent &WXUNUSED(event))
{
  m_leftDown = false;
}

#if wxUSE_ACCESSIBILITY
Worksheet::AccessibilityInfo::AccessibilityInfo(wxWindow *parent,
                                                Worksheet *worksheet):
  wxAccessible(worksheet->GetTargetWindow())
{
  m_worksheet = worksheet;
  m_parent = parent;
}

wxAccStatus Worksheet::AccessibilityInfo::GetChildCount (int *childCount)
{
  if(childCount == NULL)
    return wxACC_FAIL;

  GroupCell *cell = m_worksheet->m_tree;
  *childCount = 0;
  while(cell != NULL)
  {
    (*childCount)++;
    cell = dynamic_cast<GroupCell *>(cell->m_next);
  }
  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetChild (int childId, wxAccessible **child)
{
  if(child == NULL)
    return wxACC_FAIL;

  GroupCell *cell = m_worksheet->m_tree;

  *child = NULL;
  if(childId == 0)
  {
    *child = this;
    return wxACC_OK;
  }
  else
  {
    int childCount = 0;
    while((cell != NULL) && (childCount < childId))
    {
      childCount++;
      cell = dynamic_cast<GroupCell *>(cell->m_next);
    }
  }

  if(cell == NULL)
  {
    *child = NULL;
    return wxACC_FAIL;
  }
  else
  {
    *child = cell;
    return wxACC_OK;
  }
}

wxAccStatus Worksheet::AccessibilityInfo::GetDefaultAction (int childId, wxString *actionName)
{
  if(actionName == NULL)
    return wxACC_FAIL;

  if(childId == 0)
  {
    *actionName = _("Type");
    return wxACC_OK;
  }
  else
  {
    wxAccessible *acc;
    GetChild(childId, &acc);
    if(acc != NULL)
      return acc->GetDefaultAction(0, actionName);
    else
      return wxACC_FAIL;
  }
}

wxAccStatus Worksheet::AccessibilityInfo::GetParent (wxAccessible ** parent)
{
  if(parent == NULL)
    return wxACC_FAIL;

  *parent = m_worksheet->GetAccessible();
  if(*parent != NULL)
    return wxACC_OK;
  else
    return wxACC_FAIL;
}


wxAccStatus Worksheet::AccessibilityInfo::GetFocus (int *childId, wxAccessible **child)
{
  if(!m_worksheet->HasFocus())
  {
    if(childId != NULL)
      *childId = 0;
    if(child != NULL)
      *child = NULL;
    return wxACC_FALSE;
  }
  else
  {
    int id = 0;
    Cell *cell = m_worksheet->m_tree;
    while(cell != NULL)
    {
      id++;
      if(cell->GetFocus(&id, &cell) == wxACC_OK)
         {
           if(childId != NULL)
             *childId = id;
           if(child != NULL)
             *child = cell;
           return wxACC_OK;
         }
      cell = dynamic_cast<GroupCell *>(cell->m_next);
    }

    if(childId != NULL)
      *childId = 0;
    if(child != NULL)
      *child = this;

    return wxACC_OK;
  }
}

wxAccStatus Worksheet::AccessibilityInfo::GetLocation(wxRect &rect, int elementId)
{
  if(elementId == 0)
    rect = wxRect(m_worksheet->GetPosition(), m_worksheet->GetPosition()+ m_worksheet->GetClientSize());
  else
  {
    wxAccessible *acc = NULL;
    GetChild(elementId, &acc);
    if(acc != NULL)
    {
      return acc->GetLocation(rect,0);
    }
    else
    {
      return wxACC_FAIL;
    }
  }
  return GetLocation(rect,0);
}

wxAccStatus Worksheet::AccessibilityInfo::HitTest (const wxPoint &pt,
                                                                int *childId, wxAccessible **childObject)
{
  wxRect currentRect;
  GetLocation(currentRect, 0);
  if(!currentRect.Contains(pt))
  {
    if(childId != NULL)
      *childId = 0;
    if(childObject != NULL)
      childObject = NULL;
    return wxACC_FALSE;
  }
  else
  {
	int id = 0;
    GroupCell *cell = m_worksheet->m_tree;

    while(cell != NULL)
    {
      id++;
      cell = dynamic_cast<GroupCell *>(cell->m_next);
      if((cell != NULL) && (cell->HitTest(pt, childId,(Cell **) childObject) == wxACC_OK))
      {
        if(childId != NULL)
          *childId = id;
        if(childObject != NULL)
          *childObject = cell;
        return wxACC_OK;
      }
    }
  }
  if(childId != NULL)
    *childId = 0;
  if(childObject != NULL)
    *childObject = this;
  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetDescription(int childId, wxString *description)
{
  if(description == NULL)
    return wxACC_FAIL;

  if(childId == 0)
  {
    *description = _("The worksheet containing maxima's input and output");
    return wxACC_OK;
  }
  else
  {
    wxAccessible *child;
    if(GetChild (childId, &child) == wxACC_OK)
      return child->GetDescription(childId, description);
    else
    {
      *description = wxEmptyString;
      return wxACC_FAIL;
    }
  }
}

#endif


BEGIN_EVENT_TABLE(Worksheet, wxScrolledCanvas)
                EVT_MENU_RANGE(popid_complete_00, popid_complete_00 + AC_MENU_LENGTH, Worksheet::OnComplete)
                EVT_SIZE(Worksheet::OnSize)
                EVT_PAINT(Worksheet::OnPaint)
                EVT_MOUSE_CAPTURE_LOST(Worksheet::OnMouseCaptureLost)
                EVT_LEFT_UP(Worksheet::OnMouseLeftUp)
                EVT_LEFT_DOWN(Worksheet::OnMouseLeftDown)
                EVT_RIGHT_DOWN(Worksheet::OnMouseRightDown)
                EVT_LEFT_DCLICK(Worksheet::OnDoubleClick)
                EVT_MOTION(Worksheet::OnMouseMotion)
                EVT_ENTER_WINDOW(Worksheet::OnMouseEnter)
                EVT_LEAVE_WINDOW(Worksheet::OnMouseExit)
                EVT_TIMER(wxID_ANY, Worksheet::OnTimer)
                EVT_KEY_DOWN(Worksheet::OnKeyDown)
                EVT_CHAR(Worksheet::OnChar)
                EVT_ERASE_BACKGROUND(Worksheet::OnEraseBackground)
                EVT_KILL_FOCUS(Worksheet::OnKillFocus)
                EVT_SET_FOCUS(Worksheet::OnSetFocus)
                EVT_MIDDLE_UP(Worksheet::OnMouseMiddleUp)
                EVT_SCROLL_CHANGED(Worksheet::OnScrollChanged)
                EVT_MOUSEWHEEL(Worksheet::OnMouseWheel)
END_EVENT_TABLE()

// Define the static variable that contains the format info for placing MathMl
// on the clip board
wxDataFormat Worksheet::m_mathmlFormat;
wxDataFormat Worksheet::m_mathmlFormat2;
wxDataFormat Worksheet::m_rtfFormat;
wxDataFormat Worksheet::m_rtfFormat2;
wxDataFormat Worksheet::m_wxmFormat;
