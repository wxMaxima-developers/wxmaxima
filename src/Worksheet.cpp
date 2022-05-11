// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Worksheet

  Worksheet represents the worksheet.
 */


#include "wxMaxima.h"
#include "CellList.h"
#include "CompositeDataObject.h"
#include "ErrorRedirector.h"
#include "MaxSizeChooser.h"
#include "ResolutionChooser.h"
#include "SVGout.h"
#include "EMFout.h"
#include "WXMformat.h"
#include "Version.h"
#include "levenshtein/levenshtein.h"
#include <wx/richtext/richtextbuffer.h>
#include <wx/tooltip.h>
#include <wx/dcbuffer.h>
#include <wx/wupdlock.h>
#include <wx/event.h>
#include <wx/region.h>
#include "wxMaximaFrame.h"
#include "Worksheet.h"
#include "BitmapOut.h"
#include "AnimationCell.h"
#include "ImgCell.h"
#include "MarkDown.h"
#include "wxm_manual_anchors_xml.h"
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
#include <wx/filefn.h>
#include <stdlib.h>
#include <memory>

//! This class represents the worksheet shown in the middle of the wxMaxima window.
Worksheet::Worksheet(wxWindow *parent, int id, Worksheet* &observer, wxPoint pos, wxSize size) :
  wxScrolled<wxWindow>(
    parent, id, pos, size,
    wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
    | wxSUNKEN_BORDER
#endif
    ),
  m_cellPointers(this),
  m_dc(this),
  m_configuration(&m_configurationTopInstance),
  m_autocomplete(&m_configurationTopInstance),
  m_observer(observer)
{
  m_dontSkipScrollEvent = false;
  m_scrollToCaret = false;
  m_newxPosition = -1;
  m_newyPosition = -1;
  m_autocompletePopup = NULL;
  #ifdef __WXGTK__
  wxString gtk_input_method;
  if(wxGetEnv(wxT("GTK_IM_MODULE"),&gtk_input_method))
  {
    if(gtk_input_method==wxT("xim"))
    {
      wxLogError(_("GTK_IM_MODULE is set to \"xim\". Expect the program to hideously flicker and hotkeys to be broken, see for example https://trac.wxwidgets.org/ticket/18462."));
    }
  }
  #endif
  SetMinClientSize(wxSize(100,100));
  // This is somehow needed for wxAutoBufferedPaintDC
  SetBackgroundStyle(wxBG_STYLE_PAINT);
  GetTargetWindow()->SetBackgroundStyle(wxBG_STYLE_PAINT);
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
  m_configuration->SetContext(m_dc);
  m_configuration->SetWorkSheet(this);
  m_configuration->ReadConfig();
  SetBackgroundColour(m_configuration->DefaultBackgroundColor());
  m_configuration->SetBackgroundBrush(
    *(wxTheBrushList->FindOrCreateBrush(m_configuration->DefaultBackgroundColor(),
                                        wxBRUSHSTYLE_SOLID)));
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
  SetSaved(false);
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
  Connect(SIDEBARKEYEVENT,
          wxCommandEventHandler(Worksheet::OnSidebarKey),
          NULL, this);
  Connect(wxEVT_ERASE_BACKGROUND, wxEraseEventHandler(Worksheet::EraseBackground));
  Connect(
    popid_complete_00, popid_complete_00 + AC_MENU_LENGTH,
    wxEVT_MENU, wxCommandEventHandler(Worksheet::OnComplete));
  Connect(wxEVT_SIZE, wxSizeEventHandler(Worksheet::OnSize));
  Connect(wxEVT_PAINT, wxPaintEventHandler(Worksheet::OnPaint));
  Connect(wxEVT_MOUSE_CAPTURE_LOST, wxMouseCaptureLostEventHandler(Worksheet::OnMouseCaptureLost));
  Connect(wxEVT_LEFT_UP, wxMouseEventHandler(Worksheet::OnMouseLeftUp));
  Connect(wxEVT_LEFT_DOWN, wxMouseEventHandler(Worksheet::OnMouseLeftDown));
  Connect(wxEVT_RIGHT_DOWN, wxMouseEventHandler(Worksheet::OnMouseRightDown));
  Connect(wxEVT_LEFT_DCLICK, wxMouseEventHandler(Worksheet::OnDoubleClick));
  Connect(wxEVT_MIDDLE_UP, wxMouseEventHandler(Worksheet::OnMouseMiddleUp));
  Connect(wxEVT_MOTION, wxMouseEventHandler(Worksheet::OnMouseMotion));
  Connect(wxEVT_ENTER_WINDOW, wxMouseEventHandler(Worksheet::OnMouseEnter));
  Connect(wxEVT_LEAVE_WINDOW, wxMouseEventHandler(Worksheet::OnMouseExit));
  Connect(wxEVT_MOUSEWHEEL, wxMouseEventHandler(Worksheet::OnMouseWheel));
  Connect(wxEVT_TIMER, wxTimerEventHandler(Worksheet::OnTimer));
  Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(Worksheet::OnKeyDown));
  Connect(wxEVT_CHAR, wxKeyEventHandler(Worksheet::OnChar));
  Connect(wxEVT_ERASE_BACKGROUND, wxEraseEventHandler(Worksheet::OnEraseBackground));
  Connect(wxEVT_KILL_FOCUS, wxFocusEventHandler(Worksheet::OnKillFocus));
  Connect(wxEVT_SET_FOCUS, wxFocusEventHandler(Worksheet::OnSetFocus));
  Connect(wxEVT_SCROLL_CHANGED, wxScrollEventHandler(Worksheet::OnScrollChanged));
  Connect(wxEVT_SCROLLWIN_THUMBTRACK, wxScrollWinEventHandler(Worksheet::OnThumbtrack));
  observer = this;
}

void Worksheet::OnSidebarKey(wxCommandEvent &event)
{
  SetFocus();
  if(GetActiveCell())
  {
    GetActiveCell()->InsertText(wxString(wxChar(event.GetId())));
    GroupCell *parent = GetActiveCell()->GetGroup();
    Recalculate(parent);
    RequestRedraw(parent);
  }
  else
    OpenHCaret(wxString(wxChar(event.GetId())));
}

void Worksheet::EraseBackground(wxEraseEvent &WXUNUSED(event)){}

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
    UnsetStatusText();
    if (!m_cellPointers.m_groupCellUnderPointer ||
        (m_pointer_y < m_cellPointers.m_groupCellUnderPointer->GetRect().GetTop()) ||
        (m_pointer_y > m_cellPointers.m_groupCellUnderPointer->GetRect().GetBottom())
      )
    {
      GroupCell *oldGroupCellUnderPointer = m_cellPointers.m_groupCellUnderPointer;

      // find out which group cell lies under the pointer
      for (auto &tmp : OnList(GetTree()))
      {
        auto rect = tmp.GetRect();
        if (m_pointer_y <= rect.GetBottom())
        {
          GetTree()->CellUnderPointer(&tmp);
          break;
        }
      }

      if ((m_configuration->HideBrackets()) && (oldGroupCellUnderPointer != m_cellPointers.m_groupCellUnderPointer))
      {
        if (oldGroupCellUnderPointer)
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
        if (m_cellPointers.m_groupCellUnderPointer)
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

    if (m_cellPointers.m_groupCellUnderPointer)
    {
      // Update the worksheet's ToolTip: wxWidgets doesn't allow us to specify a
      // separate tooltip for every worksheet element, but it does allow us to
      // set the worksheet's tooltip to the appropriate one for the worksheet
      // element the pointer points to.
      if (m_cellPointers.m_groupCellUnderPointer->GetOutputRect().Contains(wxPoint(m_pointer_x, m_pointer_y)))
      {
        m_cellPointers.m_cellUnderPointer = nullptr;
        wxString toolTip = m_cellPointers.m_groupCellUnderPointer->GetToolTip(wxPoint(m_pointer_x, m_pointer_y));

        if (!toolTip.empty())
        {
          if (!GetToolTipText().empty())
          {
            if (toolTip != GetToolTipText())
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

        if(m_cellPointers.m_cellUnderPointer)
        {
          if(
            (m_cellPointers.m_cellUnderPointer->GetType() == MC_TYPE_IMAGE) ||
            (m_cellPointers.m_cellUnderPointer->GetType() == MC_TYPE_SLIDE))
          {
            ImgCellBase*image = dynamic_cast<ImgCellBase *>(m_cellPointers.m_cellUnderPointer.get());
            StatusText(
              wxString::Format(_("%s image, %li×%li, %li ppi"),
                               image->GetExtension().c_str(),
                               (long)image->GetOriginalWidth(),
                               (long)image->GetOriginalWidth(),
                               (long)image->GetPPI())
              );
          }
        }
      }
      else
        UnsetToolTip();
    }
    else
    {
      UnsetToolTip();
      m_cellPointers.m_cellUnderPointer = nullptr;
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
    m_rectToRefresh.Clear();
  }
  else
  {

    wxRegionIterator region(m_rectToRefresh);
    while(region)
    {
      wxRect rect = region.GetRect();
      
      // Don't draw rectangles with zero size or height
      if ((rect.GetWidth() >= 1) && (rect.GetHeight() >= 1))  
      {
        CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
        RefreshRect(rect);
      }
      redrawIssued = true;
      region++;
    }
  }
  m_rectToRefresh.Clear();

  return redrawIssued;
}

void Worksheet::RequestRedraw(GroupCell *start)
{
  m_redrawRequested = true;

  if (start == 0)
    m_redrawStart = GetTree();
  else
  {
    if (m_redrawStart != NULL)
    {
      // No need to waste time avoiding to waste time in a refresh when we don't
      // know our cell's position.
      if ((start->GetCurrentPoint().y < 0) || (m_redrawStart->GetCurrentPoint().y < 0))
      {
        m_redrawStart = GetTree();
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
  TreeUndo_ClearRedoActionList();
  TreeUndo_ClearUndoActionList();

  if(wxConfig::Get() != NULL)
    wxConfig::Get()->Flush();
  if (HasCapture())
    ReleaseMouse();

  m_mainToolBar = NULL;

  ClearDocument();
  m_configuration = NULL;
  m_observer = nullptr;
  if((m_helpfileanchorsThread) && (m_helpfileanchorsThread->joinable()))
    m_helpfileanchorsThread->join();
}

#if wxCHECK_VERSION(3, 1, 2)
#define WORKING_DC_CLEAR 1
#else
#ifndef __WXGTK__
#define WORKING_DC_CLEAR 1
#endif
#endif

#if wxCHECK_VERSION(3, 1, 2)
#if wxCHECK_VERSION(3, 1, 3)
#else
#define DC_ALREADY_SCROLLED 1
#endif
#else
#ifdef __WXGTK3__
#else
#endif
#endif

#define WORKING_AUTO_BUFFER 1

void Worksheet::OnPaint(wxPaintEvent &WXUNUSED(event))
{
  m_configuration->ClearAndEnableRedrawTracing();
  m_configuration->SetBackgroundBrush(
    *(wxTheBrushList->FindOrCreateBrush(m_configuration->DefaultBackgroundColor(),
                                        wxBRUSHSTYLE_SOLID)));
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

  // It is possible that the redraw starts before the idle task attempts
  // to recalculate the worksheet.
  RecalculateIfNeeded();

  // Create a working drawing context that is valid for the time of this redraw
#ifdef WORKING_AUTO_BUFFER
  m_configuration->SetContext(dc);
    
  // Create a graphics context that supports antialiasing, but on MSW
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
    m_configuration->SetContext(m_dc);
    return;
  }
  dcm.SetUserScale(wxWindow::GetContentScaleFactor(),wxWindow::GetContentScaleFactor());
  dcm.SelectObject(m_memory);
  if(!dcm.IsOk())
  {
    m_configuration->SetContext(m_dc);
    return;
  }
  DoPrepareDC(dcm);
  m_configuration->SetContext(dcm);
  // Create a graphics context that supports antialiasing, but on MSW
  // only supports fonts that come in the Right Format.
  wxGCDC antiAliassingDC(dcm);
#endif
  
  // Don't fill the text background with the background color
  m_configuration->GetDC()->SetMapMode(wxMM_TEXT);

  if(antiAliassingDC.IsOk())
    m_configuration->SetAntialiassingDC(antiAliassingDC);

  // Now iterate over all single parts of the region we need to redraw and redraw
  // the worksheet
  wxRegionIterator region(GetUpdateRegion());
  while(region)
  {
    wxRect rect = region.GetRect();

    // Don't draw rectangles with zero size or height
    if ((rect.GetWidth() < 1) || (rect.GetHeight() < 1))
      continue;

    // Set line pen and fill brushes
    SetBackgroundColour(m_configuration->DefaultBackgroundColor());    
    m_configuration->GetDC()->SetBackgroundMode(wxTRANSPARENT);
    m_configuration->GetDC()->SetBackground(m_configuration->GetBackgroundBrush());
    m_configuration->GetDC()->SetBrush(m_configuration->GetBackgroundBrush());
    m_configuration->GetDC()->SetPen(*wxTRANSPARENT_PEN);
    m_configuration->GetDC()->SetLogicalFunction(wxCOPY);
        
    // Tell the configuration where to crop in this redraw
    int xstart, xend, top, bottom;
    CalcUnscrolledPosition(rect.GetLeft(), rect.GetTop(), &xstart, &top);
    CalcUnscrolledPosition(rect.GetRight(), rect.GetBottom(), &xend, &bottom);
    wxRect unscrolledRect;
    unscrolledRect.SetLeft(xstart);
    unscrolledRect.SetRight(xend);
    unscrolledRect.SetTop(top);
    unscrolledRect.SetBottom(bottom);
    m_configuration->SetUpdateRegion(unscrolledRect);

    // Clear the drawing area
    m_configuration->GetDC()->DrawRectangle(unscrolledRect);

    //
    // Draw the cell contents
    //
    if(GetTree())
    {
      wxPoint point;
      point.x = m_configuration->GetIndent();
      point.y = m_configuration->GetBaseIndent() + GetTree()->GetCenterList();
      
      // Draw tree
      m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_DEFAULT), 1, wxPENSTYLE_SOLID)));
      m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_DEFAULT))));
      
      bool atStart = true;
      for (auto &tmp : OnList(GetTree()))
      {
        if (!atStart)
        {
          tmp.UpdateYPosition();
          point = tmp.GetCurrentPoint();
        }
        atStart = false;
        
        wxRect cellRect = tmp.GetRect();
        
        int width;
        int height;
        GetClientSize(&width, &height);
        
        wxPoint upperLeftScreenCorner;
        CalcScrolledPosition(0, 0,
                             &upperLeftScreenCorner.x, &upperLeftScreenCorner.y);
        m_configuration->SetVisibleRegion(wxRect(upperLeftScreenCorner,
                                                 upperLeftScreenCorner + wxPoint(width,height)));
        m_configuration->SetWorksheetPosition(GetPosition());
        // Clear the image cache of all cells above or below the viewport.
        if (cellRect.GetTop() >= bottom || cellRect.GetBottom() <= top)
        {
          // Only actually clear the image cache if there is a screen's height between
          // us and the image's position: Else the chance is too high that we will
          // very soon have to generated a scaled image again.
          if ((cellRect.GetBottom() <= m_lastBottom - 2 * height) || (cellRect.GetTop() >= m_lastTop + 2 * height))
          {
            if (tmp.GetOutput())
              tmp.GetOutput()->ClearCacheList();
          }
        }
        
        tmp.SetCurrentPoint(point);
        if (tmp.DrawThisCell(point))
        {
          tmp.InEvaluationQueue(m_evaluationQueue.IsInQueue(&tmp));
          tmp.LastInEvaluationQueue(m_evaluationQueue.GetCell() == &tmp);
        }
        tmp.Draw(point);
      }
    }
#ifndef WORKING_AUTO_BUFFER
    // Blit the memory image to the window
    dcm.SetDeviceOrigin(0, 0);
    dc.Blit(0, rect.GetTop(), sz.x, rect.GetBottom() - rect.GetTop() + 1, &dcm,
            0, rect.GetTop());
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

    if (GetTree() == NULL)
    {
      m_configuration->SetContext(m_dc);
      return;
    }

    //
    // Draw the selection marks
    //
    if (HasCellsSelected() && m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP)
    {
      m_configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_SELECTION), 1, wxPENSTYLE_SOLID)));
      m_configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_SELECTION))));

      // Draw the marker that tells us which output cells are selected -
      // if output cells are selected, that is.
      for (Cell &tmp : OnDrawList(m_cellPointers.m_selectionStart.get()))
      {
        if (!tmp.IsBrokenIntoLines() && !tmp.IsHidden() && &tmp != GetActiveCell())
          tmp.DrawBoundingBox(dc, false);
        if (&tmp == m_cellPointers.m_selectionEnd)
          break;
      }
    }

    
    m_configuration->SetContext(m_dc);
    m_configuration->UnsetAntialiassingDC();
    m_lastTop = top;
    m_lastBottom = bottom;

    region++;
  }
  
  m_configuration->ReportMultipleRedraws();
}

GroupCell *Worksheet::InsertGroupCells(std::unique_ptr<GroupCell> &&cells, GroupCell *where)
{
  return InsertGroupCells(std::move(cells), where, &treeUndoActions);
}

// InsertGroupCells
// inserts groupcells after position "where" (NULL = top of the document)
// Multiple groupcells can be inserted when cells->m_next != NULL
// Returns the pointer to the last inserted group cell to have fun with
GroupCell *Worksheet::InsertGroupCells(std::unique_ptr<GroupCell> &&cells, GroupCell *where,
                                       UndoActions *undoBuffer)
{
  if (!cells)
    return NULL; // nothing to insert
  bool worksheetSizeHasChanged = true;
  if (where && where->GetNext())
    worksheetSizeHasChanged = false;

  m_configuration->AdjustWorksheetSize(true);
  bool renumbersections = false; // only renumber when true
  
  // TODO What we have here is an iteration through all the cells to see if they
  // fulfill some criterion, and additionally we find the last() cell.
  // When cell list management is refactored, the foldable status should be kept
  // always up-to-date.

  GroupCell *firstOfCellsToInsert = cells.get();
  // Find the last cell in the tree that is to be inserted
  GroupCell *lastOfCellsToInsert = cells.get();
  if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (lastOfCellsToInsert->GetNext())
  {
    if (lastOfCellsToInsert->IsFoldable() || (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
    lastOfCellsToInsert = lastOfCellsToInsert->GetNext();
  }

  if (!m_tree)
  {
    m_tree = std::move(cells);
    m_last = lastOfCellsToInsert;
  }
  else if (!where)
  {
    CellList::SpliceInAfter(lastOfCellsToInsert, std::move(m_tree));
    m_tree = std::move(cells);
  }
  else
  {
    auto *whereNext = where->GetNext();
    CellList::SpliceInAfter(where, std::move(cells), lastOfCellsToInsert);
    // make sure m_last still points to the last cell of the worksheet!!
    if (!whereNext)
      m_last = lastOfCellsToInsert;
    else
      wxASSERT_MSG(m_last, "The pointer to last cell in the document is invalid");
  }

  if (renumbersections)
    NumberSections();

  GroupCell *recalcStart = NULL;
  if(where)
    recalcStart = where->GetNext();
  if(!recalcStart)
    recalcStart = GetTree();

  if(recalcStart)
    Recalculate(recalcStart);
  SetSaved(false); // document has been modified

  if (undoBuffer)
    TreeUndo_MarkCellsAsAdded(firstOfCellsToInsert, lastOfCellsToInsert, undoBuffer);

  if(worksheetSizeHasChanged)
    UpdateMLast();

  RequestRedraw(cells.get());
  AdjustSize();
  return lastOfCellsToInsert;
}

// this goes through GetTree() with m_next, to set the correct m_last
// you can call this after folding, unfolding cells to make sure
// m_last is correct
GroupCell *Worksheet::UpdateMLast()
{
  m_last = GetTree();
  if (m_last)
    m_last = m_last->last();
  if (m_last)
    m_configuration->AdjustWorksheetSize(true);
  return m_last;
}

void Worksheet::ScrollToError()
{
  GroupCell *errorCell = m_cellPointers.m_errorList.LastError();

  if (!errorCell)
    errorCell = GetWorkingGroup(true);

  if (!errorCell)
    return;

  if (errorCell->RevealHidden())
  {
    FoldOccurred();
    Recalculate();
  }

  // Try to scroll to a place from which the full error message is visible
  ScrollToCaret();

  // Set the cursor as close to the error as possible.
  if (errorCell->GetEditable()->ErrorIndexSet())
  {
    ClearSelection();
    m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
    SetActiveCell(errorCell->GetEditable());
    errorCell->GetEditable()->GotoError();
    ScrollToCaret();
  }
  else
    SetHCaret(errorCell);
}

GroupCell *Worksheet::GetWorkingGroup(bool resortToLast) const
{
  GroupCell *tmp = m_cellPointers.GetWorkingGroup(resortToLast);

  if (!resortToLast)
    return tmp;

  // The last group maxima was working on no more exists or has been deleted.
  if (!tmp && m_hCaretActive)
    tmp = m_hCaretPosition;

  // No such cursor? Perhaps there is a vertically drawn one.
  if (!tmp && GetActiveCell())
    tmp = GetActiveCell()->GetGroup();

  // If there is no such cell, neither, we append the line to the end of the
  // worksheet.
  if (!tmp)
    tmp = m_last;

  return tmp;
}

GroupCell *Worksheet::GetInsertGroup() const
{
  GroupCell *tmp = GetWorkingGroup(true);

  if (!tmp && GetActiveCell())
    tmp = GetActiveCell()->GetGroup();

  return tmp;
}

void Worksheet::InsertLine(std::unique_ptr<Cell> &&newCell, bool forceNewLine)
{
  if (!newCell)
    return;

  GroupCell *tmp = GetInsertGroup();
  if (!tmp)
    return;

  newCell->ForceBreakLine(forceNewLine);
  tmp->AppendOutput(std::move(newCell));

  if (!tmp->GetNext())
    UpdateMLast();
  OutputChanged();

  if (FollowEvaluation())
  {
    ClearSelection();
    if (GCContainsCurrentQuestion(tmp))
      OpenQuestionCaret();
    else
      ScrollToCaret();
  }
  Recalculate(tmp);
  RequestRedraw(tmp);
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
    cellToScrollTo = GetHCaret();
  if (!cellToScrollTo)
    cellToScrollTo = GetActiveCell();
  if (!cellToScrollTo)
    cellToScrollTo = GetWorkingGroup(true);
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
      cellToScrollTo = cellToScrollTo->GetNext();
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
  if(m_configuration->GetClientWidth()<1)
    return(false);

  if (!m_recalculateStart || !GetTree())
  {
    m_recalculateStart = {};
    m_configuration->AdjustWorksheetSize(false);
    if(m_configuration->AdjustWorksheetSize())
    {
      AdjustSize();
      m_configuration->AdjustWorksheetSize(false);
    }
    return false;
  }

  if (!GetTree()->Contains(m_recalculateStart))
    m_recalculateStart = GetTree();

  int width;
  int height;
  GetClientSize(&width, &height);

  wxPoint upperLeftScreenCorner;
  CalcScrolledPosition(0, 0,
                       &upperLeftScreenCorner.x, &upperLeftScreenCorner.y);
  m_configuration->SetVisibleRegion(wxRect(upperLeftScreenCorner,
                                           upperLeftScreenCorner + wxPoint(width,height)));
  m_configuration->SetWorksheetPosition(GetPosition());

  for (auto &tmp : OnList(m_recalculateStart))
  {
    tmp.Recalculate();
  }

  m_recalculateStart = {};
  if(m_configuration->AdjustWorksheetSize())
    AdjustSize();
  m_configuration->AdjustWorksheetSize(false);


  return true;
}

void Worksheet::Recalculate(Cell *start)
{
  if(!GetTree())
    return;
  wxASSERT(start);
  if (!start)
    return;

  GroupCell  *group = start->GetGroup();

  if(m_recalculateStart == group)
    return;

  group->MarkNeedsRecalculate();

  if (!m_recalculateStart)
    m_recalculateStart = group;
  else
    // Move m_recalculateStart backwards to start, if start comes before m_recalculateStart.
    for (auto &tmp : OnList(GetTree()))
    {
      if (&tmp == group)
      {
        m_recalculateStart = group;
        return;
      }

      if (&tmp == m_recalculateStart)
        return;
    }
  // If the cells to recalculate neither contain the start nor the tree we should
  // better recalculate all.
  m_recalculateStart = GetTree();
}

/***
 * Resize the control
 */
void Worksheet::OnSize(wxSizeEvent& WXUNUSED(event))
{
  // Inform all cells how wide our display is now
  m_configuration->SetCanvasSize(GetClientSize());

  // Determine if we have a sane thing we can scroll to.
  Cell *CellToScrollTo = {};
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
    CellToScrollTo = GetTree();
    for (; CellToScrollTo; CellToScrollTo = CellToScrollTo->GetNext())
    {
      wxRect rect = CellToScrollTo->GetRect();
      if (rect.GetBottom() > topleft.y)
        break;
    }
  }
  RecalculateForce();

  UpdateConfigurationClientSize();

  GroupCell *prev = {};
  for (auto &tmp : OnList(GetTree()))
  {
    if (!prev)
      ClearSelection();

    tmp.OnSize();

    if (!prev)
      tmp.SetCurrentPoint(m_configuration->GetIndent(),
                          m_configuration->GetBaseIndent() + tmp.GetCenterList());
    else
      tmp.SetCurrentPoint(m_configuration->GetIndent(),
                          prev->GetCurrentPoint().y + prev->GetMaxDrop() + tmp.GetCenterList() +
                            m_configuration->GetGroupSkip());
    prev = &tmp;
  }

  m_configuration->AdjustWorksheetSize(true);
  RequestRedraw();
  if (CellToScrollTo)
    ScheduleScrollToCell(CellToScrollTo, false);
}

void Worksheet::RecalculateForce()
{
  if(GetTree())
    GetTree()->ResetSizeList();
  Recalculate();
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
  ClearSelection();
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
  SetSaved(false);
  UpdateTableOfContents();

  Scroll(0, 0);
}

/***
 * Reset all input prompts to "-->  "
 * Called when Restart Maxima is called from Maxima menu
 */
void Worksheet::ResetInputPrompts()
{
  if (GetTree())
    GetTree()->ResetInputLabelList(); // recursively reset prompts
}

//
// support for numbered sections with hiding
//
void Worksheet::NumberSections()
{
  int s, sub, subsub, h5, h6, i;
  s = sub = subsub = i = h5 = h6 = 0;
  if (GetTree())
    GetTree()->Number(s, sub, subsub, h5, h6, i);
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
  OutputChanged();
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
  if (!which || !which->IsFoldable())
    return {};

  GroupCell *result = which->GetHiddenTree() ? which->Unfold() : which->Fold();

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
  if (!which || !which->IsFoldable())
    return {};

  GroupCell *result = which->GetHiddenTree() ? which->UnfoldAll() : which->FoldAll();

  if (result) // something has folded/unfolded
    FoldOccurred();

  return result;
}

/**
 * Recursively folds the whole document.
 */
void Worksheet::FoldAll()
{
  if (GetTree())
  {
    GetTree()->FoldAll();
    FoldOccurred();
  }
}

/**
 * Recursively unfolds the whole document.
 */
void Worksheet::UnfoldAll()
{
  if (GetTree())
  {
    GetTree()->UnfoldAll();
    FoldOccurred();
  }
}

/***
 * Right mouse - popup-menu
 */
void Worksheet::OnMouseRightDown(wxMouseEvent &event)
{
  m_updateControls = true;
  RecalculateIfNeeded();
  RedrawIfRequested();
  ClearNotification();
  m_cellPointers.ResetSearchStart();

  wxMenu popupMenu;
  int downx, downy;

  // find out if clicked into existing selection, if not, reselect with leftdown
  //
  bool clickInSelection = false;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &downx, &downy);
  if (m_cellPointers.m_selectionStart)
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
      for (Cell &tmp : OnDrawList(m_cellPointers.m_selectionStart.get()))
      {
        auto rect = tmp.GetRect();
        if (rect.Contains(downx, downy))
          clickInSelection = true;

        if (&tmp == m_cellPointers.m_selectionEnd)
          break;
      }
    }
  }
    // SELECTION IN EDITORCELL
  else if (GetActiveCell())
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
  if (!GetActiveCell())
  {
    if (IsSelected(MC_TYPE_IMAGE) || IsSelected(MC_TYPE_SLIDE))
    {
      popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_image, _("Save Image..."), wxEmptyString, wxITEM_NORMAL);
      if (IsSelected(MC_TYPE_SLIDE))
      {
        popupMenu.Append(popid_animation_save, _("Save Animation..."), wxEmptyString, wxITEM_NORMAL);
        popupMenu.Append(popid_copy_animation, _("Copy Animation"),
                          wxEmptyString, wxITEM_NORMAL);
        popupMenu.Append(popid_animation_start, _("Start Animation"), wxEmptyString, wxITEM_NORMAL);
      }
      else
      {
        if (m_cellPointers.m_selectionStart->GetGroup()->GetGroupType() == GC_TYPE_IMAGE)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_maxsizechooser, _("Restrict Maximum size"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_resolutionchooser, _("Set image resolution"), wxEmptyString, wxITEM_NORMAL);

          Cell *cell = m_cellPointers.m_selectionStart->GetGroup()->GetGroup()->GetLabel();
          wxString imgFile = dynamic_cast<ImgCell *>(cell)->GetOrigImageFile();


          // Disable the reload menu item if the original file name is unknown.
          if(imgFile.Length() == 0)
          {
              wxMenuItem* reloadItem = popupMenu.Append(popid_reloadimage, _("Reload Image"), wxEmptyString, wxITEM_NORMAL);
              reloadItem->Enable(false);
          }
          else
          {
            popupMenu.Append(popid_reloadimage,
              wxString::Format(_("Reload Image \"%s\""), imgFile),
              wxEmptyString, wxITEM_NORMAL);
          }

          popupMenu.Append(popid_change_image, _("Change Image..."), wxEmptyString, wxITEM_NORMAL);
        }
      }
      if (m_cellPointers.m_selectionStart && m_cellPointers.m_selectionStart->CanPopOut())
      {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_popup_gnuplot, _("Popout interactively"), wxEmptyString, wxITEM_NORMAL);
      }
    }
    else if (m_cellPointers.m_selectionStart)
    {
      if (IsSelected(MC_TYPE_DEFAULT))
      {
        wxString wordUnderCursor = GetSelectionStart()->ToString();
        if(m_helpfileanchorsThread)
        {
          if(m_helpfileanchorsThread->joinable())
            m_helpfileanchorsThread->join();
          m_helpfileanchorsThread.reset();
        }
        if(m_helpFileAnchors.find(wordUnderCursor) != m_helpFileAnchors.end())
        {
          popupMenu.Append(wxID_HELP, wxString::Format(_("Help on \"%s\""), wordUnderCursor));
          popupMenu.AppendSeparator();
        }
      }
      if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
      {

        if (CanCopy())
        {
          popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_matlab, _("Copy for Octave/Matlab"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_tex, _("Copy as LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_text, _("Copy as plain text"), wxEmptyString, wxITEM_NORMAL);
          if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd)
            popupMenu.Append(popid_copy_mathml, _("Copy as MathML (e.g. to word processor)"), wxEmptyString,
                              wxITEM_NORMAL);
          popupMenu.Append(popid_copy_image, _("Copy as Image"),
                            wxEmptyString, wxITEM_NORMAL);
          if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
             (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu.Append(popid_copy_animation, _("Copy Animation"),
                              wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_svg, _("Copy as SVG"),
                            wxEmptyString, wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
          popupMenu.Append(popid_copy_emf, _("Copy as EMF"),
                            wxEmptyString, wxITEM_NORMAL);
#endif
          popupMenu.Append(popid_copy_rtf, _("Copy as RTF"),
                            wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu.Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }
        popupMenu.AppendSeparator();
        popupMenu.Append(popid_evaluate, _("Evaluate Cell(s)"), wxEmptyString, wxITEM_NORMAL);
        if(m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd)
          popupMenu.Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"), wxEmptyString, wxITEM_NORMAL);

        if (CanMergeSelection())
          popupMenu.Append(popid_merge_cells, _("Merge Cells"), wxEmptyString, wxITEM_NORMAL);

        // Add a "evaluate this <sectioning unit>" context menu entry.
        GroupCell *group;
        if (m_cellPointers.m_selectionEnd)
          group = m_cellPointers.m_selectionEnd.CastAs<GroupCell*>();
        else
          group = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_TITLE)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString, wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SECTION)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Section\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SUBSECTION)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SUBSUBSECTION)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING5)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if (StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING6)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
        }
        if((group->ContainsSavedAnswers())
           || (GCContainsCurrentQuestion(group)))
        {
          popupMenu.AppendSeparator();
          popupMenu.AppendCheckItem(popid_auto_answer, _("Automatically send known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to automatically send them to maxima, if requested"));
          popupMenu.Check(popid_auto_answer, group->AutoAnswer());
          popupMenu.AppendCheckItem(popid_never_autoanswer, _("Never offer known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to offer them as the default answer"));
          popupMenu.Check(popid_never_autoanswer, !m_configuration->OfferKnownAnswers());
        }
        if (group->GetGroupType() == GC_TYPE_IMAGE)
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_maxsizechooser, _("Restrict Maximum size"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_resolutionchooser, _("Set image resolution"), wxEmptyString, wxITEM_NORMAL);
        }
        if (m_cellPointers.m_selectionStart && m_cellPointers.m_selectionStart->CanPopOut())
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_popup_gnuplot, _("Popout interactively"), wxEmptyString, wxITEM_NORMAL);
        }
      }
      else
      {
        if (CanCopy(true))
        {
          popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_matlab, _("Copy for Octave/Matlab"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_tex, _("Copy as LaTeX"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_text, _("Copy as plain text"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_mathml, _("Copy as MathML (e.g. to word processor)"), wxEmptyString,
                           wxITEM_NORMAL);

          popupMenu.Append(popid_copy_image, _("Copy as Image"),
                            wxEmptyString, wxITEM_NORMAL);
          if((GetSelectionStart() != NULL) && (GetSelectionStart() == GetSelectionEnd()) &&
             (GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu.Append(popid_copy_animation, _("Copy Animation"),
                              wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_copy_svg, _("Copy as SVG"),
                            wxEmptyString, wxITEM_NORMAL);
  #if wxUSE_ENH_METAFILE
          popupMenu.Append(popid_copy_emf, _("Copy as EMF"),
                            wxEmptyString, wxITEM_NORMAL);
#endif
          popupMenu.Append(popid_copy_rtf, _("Copy as RTF"),
                            wxEmptyString, wxITEM_NORMAL);
          if (CanDeleteSelection())
            popupMenu.Append(popid_delete, _("Delete Selection"), wxEmptyString, wxITEM_NORMAL);
        }
        if(IsSelected(MC_TYPE_LABEL))
        {
          if(popupMenu.GetMenuItemCount()>0)
            popupMenu.AppendSeparator();
          popupMenu.Append(popid_add_watch_label, _("Add to watchlist"), wxEmptyString, wxITEM_NORMAL);
        }

        if(
          (GetSelectionStart() != NULL) &&
          (GetSelectionStart()->GetType() == MC_TYPE_TEXT) &&
          (dynamic_cast<TextCell *>(GetSelectionStart())->GetStyle() == TS_SPECIAL_CONSTANT)
          )
        {
          if(popupMenu.GetMenuItemCount()>0)
            popupMenu.AppendSeparator();
          popupMenu.AppendCheckItem(
            popid_special_constant_percent, _("Show \"%\" in special constants"),
            wxEmptyString);
          popupMenu.Check(popid_special_constant_percent,
                          m_configuration->CheckKeepPercent());
        }
        if(
          (GetSelectionStart() != NULL) &&
          (GetSelectionStart()->GetType() == MC_TYPE_TEXT) &&
          (dynamic_cast<TextCell *>(GetSelectionStart())->IsOperator())
          )
        {
          if(popupMenu.GetMenuItemCount()>0)
            popupMenu.AppendSeparator();
          popupMenu.AppendCheckItem(
            popid_hideasterisk, _("Hide multiplication dots"),
            wxEmptyString);
          popupMenu.Check(popid_hideasterisk,
                          m_configuration->HidemultiplicationSign());
          popupMenu.AppendCheckItem(
            popid_changeasterisk, _("Show * as multiplication dot"),
            wxEmptyString);
          popupMenu.Check(popid_changeasterisk,
                          m_configuration->GetChangeAsterisk());
        }
        if (
          (GetSelectionStart() != NULL) &&
          (GetSelectionStart() == GetSelectionEnd()) &&
          (GetSelectionStart()->GetStyle() == TS_VARIABLE)
          )
        {
          if(popupMenu.GetMenuItemCount()>0)
            popupMenu.AppendSeparator();
          popupMenu.Append(popid_add_watch, _("Add to watchlist"), wxEmptyString, wxITEM_NORMAL);
        }

        if (IsSelected(MC_TYPE_DEFAULT) || IsSelected(MC_TYPE_LABEL))
        {
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_float, _("To Float"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_solve, _("Solve..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_solve_num, _("Find Root..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_simplify, _("Simplify Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_factor, _("Factor Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_expand, _("Expand Expression"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_subst, _("Substitute..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_integrate, _("Integrate..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_diff, _("Differentiate..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(popid_plot2d, _("Plot 2D..."), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(popid_plot3d, _("Plot 3D..."), wxEmptyString, wxITEM_NORMAL);
        }
      }
      if (
        (GetSelectionStart() == GetSelectionEnd()) &&
        (GetSelectionStart()->GetStyle() == TS_NUMBER)
        )
      {
        popupMenu.AppendSeparator();
        popupMenu.Append(popid_digits_20, _("Show max. 20 digits"));
        popupMenu.Append(popid_digits_50, _("Show max. 50 digits"));
        popupMenu.Append(popid_digits_100, _("Show max. 100 digits"));
        popupMenu.Append(popid_digits_all, _("Always show all digits"));
        popupMenu.Append(popid_digits_all_linebreak, _("Show all + allow linebreaks in long numbers"));
      }

      if (IsSelected(MC_TYPE_LABEL)  ||
          IsSelected(MC_TYPE_PROMPT) ||
          IsSelected(MC_TYPE_MAIN_PROMPT))
      {
        popupMenu.AppendSeparator();
        popupMenu.AppendRadioItem(popid_labels_user, _("Prefer user labels"));
        popupMenu.AppendRadioItem(popid_labels_autogenerated, _("Automatic labels"));
        popupMenu.AppendRadioItem(popid_labels_useronly, _("User labels only"));
        popupMenu.AppendRadioItem(popid_labels_disable, _("Don't show labels"));
        popupMenu.Check(popid_labels_autogenerated,
                         m_configuration->GetLabelChoice() == Configuration::labels_automatic);
        popupMenu.Check(popid_labels_user,
                         m_configuration->GetLabelChoice() == Configuration::labels_prefer_user);
        popupMenu.Check(popid_labels_useronly,
                         m_configuration->GetLabelChoice() == Configuration::labels_useronly);
        popupMenu.Check(popid_labels_disable,
                         m_configuration->GetLabelChoice() == Configuration::labels_none);
        wxMenu *labelWidthMenu = new wxMenu();
        labelWidthMenu->AppendRadioItem(popid_labelwidth3, wxT("3 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth4, wxT("4 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth5, wxT("5 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth6, wxT("6 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth7, wxT("7 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth8, wxT("8 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth9, wxT("9 em"));
        labelWidthMenu->AppendRadioItem(popid_labelwidth10, wxT("10 em"));
        labelWidthMenu->Check(popid_labelwidth3 + m_configuration->LabelWidth() - 3, true);
        popupMenu.Append(popid_labelwidth, _("Label width"), labelWidthMenu);
      }
    }

    else if (m_hCaretActive == true)
    {
      popupMenu.Append(wxID_PASTE, _("Paste"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(wxID_SELECTALL, _("Select All"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.AppendSeparator();
      popupMenu.Append(popid_insert_text, _("Insert Text Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_title, _("Insert Title Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_section, _("Insert Section Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_subsection, _("Insert Subsection Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_subsubsection, _("Insert Subsubsection Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_heading5, _("Insert Heading5 Cell"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(popid_insert_heading6, _("Insert Heading6 Cell"), wxEmptyString, wxITEM_NORMAL);
        popupMenu.AppendSeparator();
        popupMenu.Append(ToolBar::tb_evaltillhere, _("Evaluate Cells Above"), wxEmptyString, wxITEM_NORMAL);
        popupMenu.Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"), wxEmptyString, wxITEM_NORMAL);
    }
  }

    // popup menu in active cell
  else
  {
    popupMenu.Append(wxID_CUT, _("Cut"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.Append(wxID_PASTE, _("Paste"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.AppendSeparator();
    popupMenu.Append(wxID_SELECTALL, _("Select All"), wxEmptyString, wxITEM_NORMAL);

    if (clickInSelection && GetActiveCell()->GetGroup()->GetGroupType() == GC_TYPE_CODE)
      popupMenu.Append(popid_comment_selection, _("Comment Selection"), wxEmptyString, wxITEM_NORMAL);
    wxString selectionString = GetActiveCell()->GetSelectionString();
    if(selectionString.IsEmpty())
      selectionString = GetActiveCell()->GetWordUnderCaret();
    if(!selectionString.IsEmpty() &&
       !selectionString.Contains("\n") &&
       !selectionString.Contains("\r") &&
       !selectionString.Contains(":") &&
       ((selectionString[0] < '0') || (selectionString[0] > '9'))
      )
      popupMenu.Append(popid_add_watch, _("Add to watchlist"), wxEmptyString, wxITEM_NORMAL);

    if (!clickInSelection)
      popupMenu.Append(popid_divide_cell, _("Divide Cell"), wxEmptyString, wxITEM_NORMAL);

    GroupCell *group = NULL;
    if (GetActiveCell())
    {
      wxASSERT(GetActiveCell()->GetGroup());
      group = GetActiveCell()->GetGroup();
    }
    if (m_cellPointers.m_selectionStart)
    {
      if (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
      {
        group = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
      }
    }
    if (group)
    {
      popupMenu.AppendSeparator();
      switch (StartOfSectioningUnit(group)->GetGroupType())
      {
        case GC_TYPE_TITLE:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SECTION:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Section\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSECTION:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSUBSECTION:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING5:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING6:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          break;
        default:
          break;
      }
      switch (group->GetGroupType())
      {
        case GC_TYPE_CODE:
          if((group->GetEditable() != NULL) && (group->GetEditable()->ContainsPoint(wxPoint(downx, downy))))
          {
            wxString wordUnderCursor = group->GetEditable()->GetWordUnderCaret();
            wxArrayString dst[4];
            wxArrayString sameBeginning;
            if(m_helpfileanchorsThread)
            {
              if(m_helpfileanchorsThread->joinable())
                m_helpfileanchorsThread->join();
              m_helpfileanchorsThread.reset();
            }

            if((m_helpFileAnchors.find(wordUnderCursor) != m_helpFileAnchors.end()))
              popupMenu.Append(wxID_HELP, wxString::Format(_("Help on \"%s\""),
                                                           wordUnderCursor));

            HelpFileAnchors::const_iterator it;
            for (it = m_helpFileAnchors.begin(); it != m_helpFileAnchors.end(); ++it)
            {
              wxString cmdName = it->first;
              if(cmdName.EndsWith("_"))
                continue;
              if(cmdName.EndsWith("pkg"))
                continue;
              if(cmdName.StartsWith(wordUnderCursor))
              {
                if (wordUnderCursor != cmdName)
                  sameBeginning.Add(cmdName);
              }
              else
              {
                int dstnce = LevenshteinDistance(wordUnderCursor, cmdName);
                if((dstnce<=4) && (dstnce > 0)) dst[dstnce-1].Add(cmdName);
              }
            }
            m_replacementsForCurrentWord.Clear();
            if(sameBeginning.GetCount() <= 10)
              m_replacementsForCurrentWord = sameBeginning;
            for(int o = 0; o<4; o++)
            {
              if(m_replacementsForCurrentWord.GetCount() + dst[o].GetCount() <= 10)
              {
                for(unsigned int i = 0; i<dst[o].GetCount(); i++)
                  m_replacementsForCurrentWord.Add(dst[o][i]);
              }
              else
                break;
            }
            for(unsigned int i = 0; i<m_replacementsForCurrentWord.GetCount(); i++)
              popupMenu.Append(popid_suggestion1 + i, m_replacementsForCurrentWord[i]);
          }
          popupMenu.AppendSeparator();
          if((group->ContainsSavedAnswers())
             || (GCContainsCurrentQuestion(group)))
          {
            popupMenu.AppendSeparator();
            popupMenu.AppendCheckItem(popid_auto_answer, _("Automatically send known answers"),
                                      _("wxMaxima remembers answers from the last run and is able to automatically send them to maxima, if requested"));
            popupMenu.Check(popid_auto_answer, group->AutoAnswer());
            popupMenu.AppendCheckItem(popid_never_autoanswer, _("Never offer known answers"),
                                      _("wxMaxima remembers answers from the last run and is able to offer them as the default answer"));
            popupMenu.Check(popid_never_autoanswer, !m_configuration->OfferKnownAnswers());
          }
          break;
      case GC_TYPE_TITLE:
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Part"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Part"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SECTION:
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Section"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Section"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSECTION:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Subsection"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Subsection"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_SUBSUBSECTION:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Subsubsection"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Subsubsection"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING5:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 5\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Heading 5"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Heading 5"), wxEmptyString, wxITEM_NORMAL);
          break;
        case GC_TYPE_HEADING6:
          popupMenu.Append(popid_evaluate_section, _("Evaluate Heading 6\tShift+Ctrl+Enter"), wxEmptyString,
                            wxITEM_NORMAL);
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide Heading 6"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide Heading 6"), wxEmptyString, wxITEM_NORMAL);
          break;
        default:
          if (group->GetHiddenTree() != NULL)
            popupMenu.Append(popid_unfold,
                              _("Unhide contents"), wxEmptyString, wxITEM_NORMAL);
          else
            popupMenu.Append(popid_fold,
                              _("Hide contents"), wxEmptyString, wxITEM_NORMAL);
      }
    }

    if(GetActiveCell())
    {
      wxString toolTip = GetActiveCell()->GetLocalToolTip();
      if((toolTip.IsEmpty()) && (group))
        toolTip = group->GetLocalToolTip();

      if (!(toolTip.IsEmpty()))
      {
        if(popupMenu.GetMenuItemCount() > 0)
          popupMenu.AppendSeparator();
        popupMenu.AppendCheckItem(popid_hide_tooltipMarker, _("Hide yellow tooltip marker for this cell"),
                                  _("Don't mark cells that contain tooltips in yellow"));
        popupMenu.Check(popid_hide_tooltipMarker,group->GetSuppressTooltipMarker());
        popupMenu.AppendCheckItem(popid_hide_tooltipMarkerForThisMessage,
                                  _("Hide yellow tooltip marker for this message type"),
                                  _("Don't mark this message text in yellow"));
        popupMenu.Check(popid_hide_tooltipMarkerForThisMessage,
                        m_configuration->HideMarkerForThisMessage(toolTip));
      }
      TextStyle selectionStyle = GetActiveCell()->GetSelectionStyle();
      if (
        (selectionStyle == TS_CODE_VARIABLE) ||
        (selectionStyle == TS_STRING) ||
        (selectionStyle == TS_GREEK_CONSTANT) ||
        (selectionStyle == TS_FUNCTION) ||
        (selectionStyle == TS_USERLABEL)
        )
      {
        wxMenu *facts_sub = new wxMenu;
        if((selectionStyle != TS_FUNCTION) &&
           (selectionStyle != TS_STRING))
        {
          facts_sub->Append(popid_property_real, _("Is a real variable"), _("This symbol has no imaginary part"));
          facts_sub->Append(popid_property_complex, _("Is a complex variable"), _("This symbol might have both real and imaginary part"));
          facts_sub->Append(popid_property_imaginary, _("Is an imaginary variable"), _("This symbol has no real part"));
          facts_sub->Append(popid_property_even, _("Even number"));
          facts_sub->Append(popid_property_odd, _("Odd number"));
          facts_sub->Append(popid_property_integer, _("Is an integer variable"));
          facts_sub->Append(popid_property_noninteger, _("Is no integer variable"));
          facts_sub->Append(popid_property_rational, _("A rational variable"));
          facts_sub->Append(popid_property_irrational, _("A irrational variable"));
          facts_sub->Append(popid_property_greaterThan, _("Greater or less than a value"),
                          _("Calls assume() in order to tell maxima about the variable's range"));
          facts_sub->AppendSeparator();
          facts_sub->Append(popid_property_constant, _("Symbolic Constant"));
          facts_sub->Append(popid_property_nonarray, _("No Array"));
          facts_sub->Append(popid_property_scalar, _("A Scalar"));
          facts_sub->Append(popid_property_nonscalar, _("No Scalar"));
          facts_sub->Append(popid_property_mainvar, _("Likely to be the main variable"));
          facts_sub->AppendSeparator();
          facts_sub->Append(popid_property_bindtest, _("Don't use if unassigned"),
                            _("Throw an error if this symbol is used before assigning it any contents"));
          facts_sub->AppendSeparator();
        }
        if (
          (selectionStyle == TS_CODE_VARIABLE) ||
          (selectionStyle == TS_FUNCTION)
          )
        {
          facts_sub->Append(popid_property_evenfun, _("Is an even function"), _("This function will return even integers"));
          facts_sub->Append(popid_property_oddfun, _("Is an odd function"), _("This function will return odd integers"));
          facts_sub->Append(popid_property_additive, _("Additive function: f(a+b)=f(a)+f(b)"));
          facts_sub->Append(popid_property_multiplicative, _("Multiplicative function: f(a*b)=f(a)*f(b)"));
          facts_sub->Append(popid_property_nary, _("nary: f(x, f(y,z)) = foo(x,y,z)"));
          facts_sub->Append(popid_property_antisymmetric, _("Antisymmetric function"));
          facts_sub->Append(popid_property_commutative, _("Commutative function"));
          facts_sub->Append(popid_property_symmetric, _("Symmetric function"));
          facts_sub->Append(popid_property_increasing, _("Increasing function f(x)>x"));
          facts_sub->Append(popid_property_decreasing, _("Decreasing function f(x)<x"));
          facts_sub->Append(popid_property_integervalued, _("A function returning integers"));
          facts_sub->Append(popid_property_posfun, _("A function returning positive numbers"));
          facts_sub->Append(popid_property_lassociative, _("A left-associative function"));
          facts_sub->Append(popid_property_rassociative, _("A right-associative function"));
          facts_sub->Append(popid_property_linear, _("A linear function"));
          facts_sub->Append(popid_property_outative, _("outative: f(2*x) = 2*f(x)"));
          facts_sub->AppendSeparator();
          facts_sub->Append(popid_property_noun, _("noun: Not evaluated by default"),
                            _("f(x) stays f(x) even if the value of f(x) is computable"));
          facts_sub->Append(popid_property_evfun, _("ev() shall evaluate this automatically"), _("Make an eventual ev() evaluate this"));
          facts_sub->Append(popid_property_evflag, _("Suitable as flag for ev()"), _("Can be specified as flag in ev(exp, flag)"));
        }
        if (
          (selectionStyle == TS_CODE_VARIABLE) ||
          (selectionStyle == TS_STRING)
        )
        {
          
          if(facts_sub->GetMenuItemCount() > 0)
            facts_sub->AppendSeparator();
          facts_sub->Append(popid_property_alphabetic, _("Declare these chars as ordinary letters"));
        }
        popupMenu.Append(wxNewId(), wxString::Format(_("Declare facts about %s"), selectionString.c_str()),
                         facts_sub,
                         _("Inform maxima about facts you know for this symbol"));
      }
    }
  }

  if((m_cellPointers.m_selectionStart))
  {
    wxString toolTip = m_cellPointers.m_selectionStart->GetLocalToolTip();
    GroupCell *group = m_cellPointers.m_selectionStart->GetGroup();
    if(toolTip.IsEmpty())
      toolTip = group->GetLocalToolTip();

    if (!(toolTip.IsEmpty()))
    {
      if(popupMenu.GetMenuItemCount() > 0)
        popupMenu.AppendSeparator();
      popupMenu.AppendCheckItem(popid_hide_tooltipMarker, _("Hide yellow tooltip marker for this cell"),
                                _("Don't mark cells that contain tooltips in yellow"));
      popupMenu.Check(popid_hide_tooltipMarker,group->GetSuppressTooltipMarker());
      popupMenu.AppendCheckItem(popid_hide_tooltipMarkerForThisMessage,
                                _("Hide yellow tooltip marker for this message type"),
                                _("Don't mark this message text in yellow"));
      popupMenu.Check(popid_hide_tooltipMarkerForThisMessage,
                      m_configuration->HideMarkerForThisMessage(toolTip));
    }
  }
  // create menu if we have any items
  if (popupMenu.GetMenuItemCount() > 0)
  {
    m_inPopupMenu = true;
    PopupMenu(&popupMenu);
    m_inPopupMenu = false;
  }
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
      Recalculate(clickedInGC);
    }
    else
    {
      clickedInGC->SwitchHide();
      clickedInGC->ResetSize();
      Recalculate(clickedInGC);
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

  // The user clicked at an ordinary cell
  if ((clickedInGC->GetPrompt()) && (clickedInGC->GetPrompt()->GetRect()).Contains(m_down))
  {
    m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = clickedInGC->GetPrompt();
    m_clickType = CLICK_TYPE_INPUT_LABEL_SELECTION;
  }

  // Let's see if the user clicked at the cell's input label

  // Let's see if the user clicked at the cell's input
  EditorCell *editor = clickedInGC->GetEditable();
  if (editor)
  {
    wxRect rect = editor->GetRect();
    if (
      (m_down.y >= rect.GetTop() && m_down.y <= rect.GetBottom()) &&
      (m_configuration->ShowCodeCells() ||
       editor->GetType() != MC_TYPE_INPUT || !clickedInGC->GetOutput())
      )
    {
      editor->MouseSelectionStartedHere();
      SetActiveCell(editor, false); // do not refresh as we will do so later
      GetActiveCell()->SelectPointText(m_down);
      m_blinkDisplayCaret = true;
      m_clickType = CLICK_TYPE_INPUT_SELECTION;
      if (editor->GetWidth() == -1)
        Recalculate(clickedInGC);
      ScrollToCaret();
      // Here we tend to get unacceptably long delays before the display is
      // refreshed by the idle loop => Trigger the refresh manually.
      ForceRedraw();
      return;
    }
  }

  // what if we tried to select something in output, select it (or if editor, activate it)
  if ((clickedInGC->GetOutputRect()).Contains(m_down))
  {
    wxRect rect2(m_down.x, m_down.y, 1, 1);
    wxPoint mmm(m_down.x + 1, m_down.y + 1);
    auto const sel = clickedInGC->GetCellsInOutputRect(rect2, m_down, mmm);
    m_cellPointers.m_selectionStart = sel.first;
    m_cellPointers.m_selectionEnd = sel.last;
    if (m_cellPointers.m_selectionStart)
    {
      m_clickType = CLICK_TYPE_OUTPUT_SELECTION;
      m_clickInGC = clickedInGC;
    }
  }
}

void Worksheet::OnMouseLeftInGc(wxMouseEvent &event, GroupCell *clickedInGC)
{
  // The click has changed the cell which means the user works here and
  // doesn't want the evaluation mechanism to automatically follow the
  // evaluation any more.
  ScrolledAwayFromEvaluation(true);

  if (m_down.x <= m_configuration->GetIndent())
    OnMouseLeftInGcLeft(event, clickedInGC);
  else
    OnMouseLeftInGcCell(event, clickedInGC);
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
  m_updateControls = true;
  RecalculateIfNeeded();
  RedrawIfRequested();
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

  if (!GetTree())
    return;

  // Handle a shift-click when GroupCells were selected.
  if (m_hCaretPositionStart && event.ShiftDown())
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
  if (GetActiveCell() && event.ShiftDown())
  {

    // We were within an input cell when the selection has started.
    m_clickType = CLICK_TYPE_INPUT_SELECTION;

    // The mouse selection was started in the currently active EditorCell
    GetActiveCell()->MouseSelectionStartedHere();

    // Set a fake starting point for the selection that is inside the cell the selection started in.
    int startingChar = GetActiveCell()->GetCaretPosition();
    if (GetActiveCell()->SelectionActive()) startingChar = GetActiveCell()->GetSelectionStart();
    m_down = wxPoint(GetActiveCell()->PositionToPoint(startingChar));
    GetActiveCell()->SelectNone();
    // Handle the mouse pointer position
    OnMouseMotion(event);

    // Did we shift-click in the currently active cell?

    return;
  }

  // Handle a shift-click when the hCaret is active
  if (m_hCaretPosition && event.ShiftDown())
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
  ClearSelection();
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_hCaretPosition = NULL;
  m_hCaretActive = false;
  SetActiveCell(NULL, false);

  wxRect rect;
  GroupCell *previous = NULL;
  GroupCell *clickedBeforeGC = NULL;
  GroupCell *clickedInGC = NULL;
  for (auto &tmp : OnList(GetTree()))
  { // go through all groupcells
    rect = tmp.GetRect();
    if (m_down.y < rect.GetTop())
    {
      clickedBeforeGC = &tmp;
      break;
    }
    else if (m_down.y <= rect.GetBottom())
    {
      clickedInGC = &tmp;
      break;
    }
    previous = &tmp;
  }

  if (clickedBeforeGC)
  { // we clicked between groupcells, set hCaret
    SetHCaret(previous);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;

    // The click will has changed the position that is in focus so we assume
    // the user wants to work here and doesn't want the evaluation mechanism
    // to automatically follow the evaluation any more.
    ScrolledAwayFromEvaluation(true);
  }
  else if (clickedInGC)
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
  m_clickType_selectionStart = m_clickType;
  RequestRedraw();
  // Re-calculate the table of contents
  UpdateTableOfContents();
}


GroupCell *Worksheet::FirstVisibleGC()
{
  wxPoint point;
  CalcUnscrolledPosition(0, 0, &point.x, &point.y);

  for (auto &tmp : OnList(GetTree()))
  { // go through all groupcells
    wxRect rect = tmp.GetRect();

    if (point.y < rect.GetBottom())
      return &tmp;
  }
  return {};
}

void Worksheet::OnMouseLeftUp(wxMouseEvent &event)
{
  m_cellPointers.ResetSearchStart();
  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  if (GetSelectionStart() && GetSelectionStart() == GetSelectionEnd() &&
      m_leftDownPosition == wxPoint(event.GetX(),event.GetY()) &&
      GetSelectionStart()->GetType() == MC_TYPE_SLIDE)
    dynamic_cast<AnimationCell *>(GetSelectionStart())->AnimationRunning(
      !dynamic_cast<AnimationCell *>(GetSelectionStart())->AnimationRunning());

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
    zoomEvent->SetId((event.GetWheelRotation() > 0) ? wxID_ZOOM_IN : wxID_ZOOM_OUT);
    GetParent()->GetEventHandler()->QueueEvent(zoomEvent);
    return;
  }

  if (!CanAnimate())
  {
    event.Skip();
    return;
  }

  //! Step the slide show.
  int rot = event.GetWheelRotation();

  AnimationCell *tmp = m_cellPointers.m_selectionStart.CastAs<AnimationCell *>();
  tmp->AnimationRunning(false);

  if (rot > 0)
    tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());
  else
    tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() - 1) % tmp->Length());

  wxRect rect = m_cellPointers.m_selectionStart->GetRect();
  RequestRedraw(rect);

  if (m_mainToolBar && m_mainToolBar->m_plotSlider)
  {
#ifdef __WXMSW__
    // On Windows: Set the focus to the slider so it handles further wheel events
    m_mainToolBar->m_plotSlider -> SetFocus();
#endif

    m_mainToolBar->m_plotSlider->SetValue(tmp->GetDisplayedIndex());
  }

#ifdef __WXMSW__
  // On Windows the first scroll event scrolls the canvas. Let's scroll it back again.
  int view_x,view_y;
  GetViewStart(&view_x, &view_y);
  view_y += (rot>0) ? +1 : -1;
  Scroll(view_x, view_y);
#endif
}

void Worksheet::OnMouseMotion(wxMouseEvent &event)
{
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_pointer_x, &m_pointer_y);
  m_mouseMotionWas = true;
  m_updateControls = true;
  if (!GetTree() || !m_leftDown)
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
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = nullptr;

  wxRect rect;

  // find out the group cell the selection begins in
  for (auto &tmp : OnList(GetTree()))
  {
    rect = tmp.GetRect();
    if (ytop <= rect.GetBottom())
    {
      m_cellPointers.m_selectionStart = &tmp;
      break;
    }
  }

  // find out the group cell the selection ends in
  for (auto &tmp : OnList(GetTree()))
  {
    rect = tmp.GetRect();
    if (ybottom < rect.GetTop())
    {
      m_cellPointers.m_selectionEnd = tmp.GetPrevious();
      break;
    }
  }
  if (!m_cellPointers.m_selectionEnd)
    m_cellPointers.m_selectionEnd = m_last;

  if (m_cellPointers.m_selectionStart)
  {
    if (m_cellPointers.m_selectionEnd->GetNext() == m_cellPointers.m_selectionStart)
    {
      SetHCaret(m_cellPointers.m_selectionEnd.CastAs<GroupCell*>());
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
    m_hCaretPositionStart = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
    m_hCaretPositionEnd = m_cellPointers.m_selectionEnd.CastAs<GroupCell*>();
  }
  else
  {
    m_hCaretPositionStart = m_cellPointers.m_selectionEnd.CastAs<GroupCell*>();
    m_hCaretPositionEnd = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
  }
  SetSelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd);
}

void Worksheet::ClickNDrag(wxPoint down, wxPoint up)
{
  Cell *selectionStartOld = m_cellPointers.m_selectionStart,
       *selectionEndOld = m_cellPointers.m_selectionEnd;
  wxRect rect;

  int ytop = wxMin(down.y, up.y);
  int ybottom = wxMax(down.y, up.y);

  if((m_cellPointers.m_cellMouseSelectionStartedIn)
     && (!m_cellPointers.m_cellMouseSelectionStartedIn->GetRect().Inflate(
           m_configuration->GetGroupSkip(),
           m_configuration->GetGroupSkip()).Contains(up)))
  {
    SelectGroupCells(up,down);
    if (GetActiveCell())
    {
      GetActiveCell()->SelectNone();
      SetActiveCell(NULL);
    }
  }
  else
    switch (m_clickType)
    {
    case CLICK_TYPE_NONE:
      return;

    case CLICK_TYPE_INPUT_SELECTION:
      wxASSERT_MSG(m_cellPointers.m_cellMouseSelectionStartedIn,
                   _("Bug: Trying to select inside a cell without having a current cell"));
      if (!m_cellPointers.m_cellMouseSelectionStartedIn)
        return;

      {
        rect = m_cellPointers.m_cellMouseSelectionStartedIn->GetRect();

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
          ClearSelection();
          SetActiveCell(m_cellPointers.m_cellMouseSelectionStartedIn);
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
      ClearSelection();
      rect.x = wxMin(down.x, up.x);
      rect.y = wxMin(down.y, up.y);
      rect.width = wxMax(abs(down.x - up.x), 1);
      rect.height = wxMax(abs(down.y - up.y), 1);

      if (m_clickInGC)
      {
        auto const sel = m_clickInGC->GetCellsInOutputRect(rect, down, up);
        m_cellPointers.m_selectionStart = sel.first;
        m_cellPointers.m_selectionEnd = sel.last;
      }
      break;

    default:
      break;
    } // end switch

  // Refresh only if the selection has changed
  if ((selectionStartOld != m_cellPointers.m_selectionStart)
      || (selectionEndOld != m_cellPointers.m_selectionEnd))
    RequestRedraw();
}

/***
 * Get the string representation of the selection
 */
wxString Worksheet::GetString(bool lb)
{
  if (!m_cellPointers.m_selectionStart)
    return GetActiveCell() ? GetActiveCell()->ToString() : wxString{};

  wxString s;
  for (Cell &tmp : OnDrawList(m_cellPointers.m_selectionStart.get()))
  {
    if (lb && tmp.BreakLineHere() && !s.empty())
      s += wxT('\n');
    s += tmp.ToString();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
  }
  return s;
}

/***
 * Copy selection to clipboard.
 */
bool Worksheet::Copy(bool astext)
{
  if (GetActiveCell())
    return GetActiveCell()->CopyToClipboard();

  if (!m_cellPointers.m_selectionStart)
    return false;

  if (!astext && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
    return CopyCells();

  // If the selection is IMAGE or ANIMATION, copy it to clipboard
  // as image.
  if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
      (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE
       || m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE))
  {
    m_cellPointers.m_selectionStart->CopyToClipboard();
    return true;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxDataObjectComposite *data = new wxDataObjectComposite;

    // Add the wxm code corresponding to the selected output to the clipboard
    wxString s = GetString(true);
    data->Add(new wxmDataObject(s));

    std::unique_ptr<Cell> tmp(CopySelection());

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
      // to the clipboard: For some reason Libreoffice likes RTF more than
      // it likes the MathML - which is standardized.
      wxString rtf;
      rtf = RTFStart() + tmp->ListToRTF() + wxT("\\par\n") + RTFEnd();
      data->Add(new RtfDataObject(rtf));
      data->Add(new RtfDataObject2(rtf), true);
    }

    // Add a string representation of the selected output to the clipboard
    s = tmp->ListToString();
    data->Add(new wxTextDataObject(s));

    if(m_configuration->CopyBitmap())
    {
      // Try to fill bmp with a high-res version of the cells
      BitmapOut output(&m_configuration, std::move(tmp),
                       m_configuration->BitmapScale(),
                       1000000*m_configuration->MaxClipbrdBitmapMegabytes());
      if (output.IsOk())
        data->Add(output.GetDataObject().release());
    }
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    Recalculate();
    return true;
  }
  Recalculate();
  return false;
}

wxString Worksheet::ConvertSelectionToMathML()
{
  if (GetActiveCell())
    return {};

  if (!m_cellPointers.m_selectionStart || !m_cellPointers.m_selectionEnd)
    return {};

  wxString s;
  std::unique_ptr<Cell> tmp(
    CopySelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd, true));

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
  if (GetActiveCell())
	return false;

  if (!m_cellPointers.m_selectionStart)
	return false;

  wxString result;
  bool firstcell = true;
  for (const Cell &tmp : OnList(m_cellPointers.m_selectionStart.get()))
  {
    if (tmp.HasHardLineBreak() && !firstcell)
	  result += wxT("\n");
	result += tmp.ToMatlab();
	if (&tmp == m_cellPointers.m_selectionEnd)
	  break;
	firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxTheClipboard->SetData(new wxTextDataObject(result));
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

bool Worksheet::CopyTeX()
{
  if (GetActiveCell())
    return false;

  if (!m_cellPointers.m_selectionStart)
    return false;

  Cell *const start = m_cellPointers.m_selectionStart;
  bool inMath = false;
  wxString s;

  if (start->GetType() != MC_TYPE_GROUP)
  {
    inMath = m_configuration->WrapLatexMath();
    if (inMath)
      s = wxT("\\[");
  }
  for (const Cell &tmp : OnList(start))
  {
    s += tmp.ToTeX();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
  }
  if (inMath)
    s += wxT("\\]");

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyText()
{
  if (GetActiveCell())
    return false;

  if (!m_cellPointers.m_selectionStart)
    return false;

  wxString result;
  bool firstcell = true;
  for (const Cell &tmp : OnList(m_cellPointers.m_selectionStart.get()))
  {
    if (tmp.HasHardLineBreak() && !firstcell)
      result += wxT("\n");
    result += tmp.ToString();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
    firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    wxTheClipboard->SetData(new wxTextDataObject(result));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyCells()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (!m_cellPointers.m_selectionStart)
    return false;

  if (wxTheClipboard->Open())
  {
#if wxUSE_ENH_METAFILE
    auto *data = new CompositeDataObject;
#else
    wxDataObjectComposite *data = new wxDataObjectComposite;
#endif
    wxString wxm;
    wxString str;
    wxString rtf = RTFStart();

    GroupCell *const end = m_cellPointers.m_selectionEnd->GetGroup();
    bool firstcell = true;
    for (auto &tmp : OnList(m_cellPointers.m_selectionStart->GetGroup()))
    {
      if (!firstcell)
        str += wxT("\n");
      str += tmp.ToString();
      firstcell = false;

      if (m_configuration->CopyRTF())
        rtf += tmp.ToRTF();
      wxm += Format::TreeToWXM(&tmp);

      if (&tmp == end)
      	break;
    }

    rtf += wxT("\\par") + RTFEnd();

    if (m_configuration->CopyRTF())
    {
      data->Add(new RtfDataObject(rtf), true);
      data->Add(new RtfDataObject2(rtf));
    }
    data->Add(new wxTextDataObject(str));
    data->Add(new wxmDataObject(wxm));

    if (m_configuration->CopyBitmap())
    {
      BitmapOut output(&m_configuration, CopySelection(),
                       m_configuration->BitmapScale(),
                       1000000*m_configuration->MaxClipbrdBitmapMegabytes());
      if (output.IsOk())
        data->Add(output.GetDataObject().release());
    }

#if wxUSE_ENH_METAFILE
    if (m_configuration->CopyEMF())
    {
      Emfout emf(&m_configuration, CopySelection());
      if (emf.IsOk())
        data->Add(emf.GetDataObject().release());
    }
#endif
    if (m_configuration->CopySVG())
    {
      Svgout svg(&m_configuration, CopySelection());
      if (svg.IsOk())
        data->Add(svg.GetDataObject().release());
    }

    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    Recalculate();
    return true;
  }

  return false;
}

bool Worksheet::CanDeleteSelection() const
{
  if (!m_cellPointers.HasCellsSelected())
    return false;

  return CanDeleteRegion(
    m_cellPointers.m_selectionStart->GetGroup(),
    m_cellPointers.m_selectionEnd->GetGroup()
    );
}

void Worksheet::DeleteSelection()
{
  DeleteRegion(
    m_cellPointers.m_selectionStart->GetGroup(),
    m_cellPointers.m_selectionEnd->GetGroup()
    );
  TreeUndo_ClearRedoActionList();
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = nullptr;
  RequestRedraw();
}

void Worksheet::DeleteCurrentCell()
{
  GroupCell *cellToDelete = NULL;
  if(m_hCaretActive)
    cellToDelete = m_hCaretPosition;
  if(GetActiveCell())
    cellToDelete = GetActiveCell()->GetGroup();

  if((!cellToDelete) && (GetSelectionStart()) && (GetSelectionEnd()) &&
     (GetSelectionStart()->GetGroup() == GetSelectionEnd()->GetGroup()))
  {
    cellToDelete = GetSelectionStart()->GetGroup();
  }
  if (cellToDelete)
    DeleteRegion(cellToDelete, cellToDelete);
}

bool Worksheet::CanDeleteRegion(GroupCell *start, GroupCell *end) const
{
  if (!start || !end)
    return false;

  // We refuse deletion of a cell we are planning to evaluate
  for (auto &tmp : OnList(start))
  {
    // We refuse deletion of a cell maxima is currently evaluating
    if (&tmp == GetWorkingGroup())
      return false;

    if (&tmp == end)
      return true;
  }

  return true;
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *parentOfStart, GroupCell *end)
{
  TreeUndo_MarkCellsAsAdded(parentOfStart, end, &treeUndoActions);
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end, UndoActions *undoBuffer)
{
  undoBuffer->emplace_front(start, end);
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

void Worksheet::TreeUndo_DiscardAction(UndoActions *actionList)
{
  if (actionList->empty())
    return;

  do
  {
    actionList->pop_back();
  }
  while (!actionList->empty() && actionList->back().m_partOfAtomicAction);
}

void Worksheet::TreeUndo_CellLeft()
{
  // If no cell is active we didn't leave a cell and return from this function.
  if (!GetActiveCell())
    return;

  GroupCell *activeCell = GetActiveCell()->GetGroup();
  if (TreeUndo_ActiveCell) //-V1051
    wxASSERT_MSG(TreeUndo_ActiveCell == activeCell, _("Bug: Cell left but not entered."));

  if (!activeCell->GetEditable())
    return;

  // We only can undo a text change if the text has actually changed.
  if (
    (m_treeUndo_ActiveCellOldText.Length() > 1) &&
    (m_treeUndo_ActiveCellOldText            != activeCell->GetEditable()->GetValue()) &&
    (m_treeUndo_ActiveCellOldText + wxT(";") != activeCell->GetEditable()->GetValue())
    )
  {
    treeUndoActions.emplace_front(activeCell, m_treeUndo_ActiveCellOldText);
    TreeUndo_LimitUndoBuffer();
    TreeUndo_ClearRedoActionList();
  }
}

void Worksheet::TreeUndo_CellEntered()
{
  if (!GetActiveCell() || !GetActiveCell()->GetGroup())
      return;

  TreeUndo_ActiveCell = GetActiveCell()->GetGroup();
  m_treeUndo_ActiveCellOldText = GetActiveCell()->GetValue();
}

void Worksheet::SetCellStyle(GroupCell *group, GroupType style)
{
  if (!group)
    return;

  wxString cellContents;
  if (group->GetEditable())
    cellContents = group->GetEditable()->GetValue();
  auto newGroupCell = std::make_unique<GroupCell>(&m_configuration, style);
  newGroupCell->GetEditable()->SetValue(cellContents);
  GroupCell *prev = group->GetPrevious();
  DeleteRegion(group,group);
  TreeUndo_AppendAction();
  auto *editable = newGroupCell->GetEditable();
  InsertGroupCells(std::move(newGroupCell), prev);
  SetActiveCell(editable, false);
  SetSaved(false);
  Recalculate();
  RequestRedraw();
}

void Worksheet::DeleteRegion(GroupCell *start, GroupCell *end)
{
  DeleteRegion(start, end, &treeUndoActions);
}

void Worksheet::DeleteRegion(GroupCell *start, GroupCell *end, UndoActions *undoBuffer)
{
  m_cellPointers.ResetSearchStart();
  if (!end)
    return;

  // Abort deletion if there is no valid selection or if we cannot
  // delete it.
  if (!CanDeleteRegion(start, end))
    return;

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  //! Set the cursor to a sane place
  SetActiveCell(NULL, false);
  ClearSelection();
  SetHCaret(start->GetPrevious());

  // check if chapters or sections need to be renumbered
  bool renumber = false;
  for (auto &tmp : OnList(start))
  {
    m_evaluationQueue.Remove(&tmp);

    if (tmp.IsFoldable() || (tmp.GetGroupType() == GC_TYPE_IMAGE))
      renumber = true;

    // Don't keep cached versions of scaled images around in the undo buffer.
    if (tmp.GetOutput())
      tmp.GetOutput()->ClearCacheList();

    if (&tmp == end)
      break;
  }

  GroupCell *cellBeforeStart = start->GetPrevious();

  // If the selection ends with the last file of the file m_last has to be
  // set to the last cell that isn't deleted.
  if (end == m_last)
    m_last = cellBeforeStart;

  auto tornOut = CellList::TearOut(start, end);
  if (!tornOut.cellOwner)
  {
    wxASSERT(m_tree.get() == tornOut.cell);
    tornOut.cellOwner = std::move(m_tree);
    m_tree = dynamic_unique_ptr_cast<GroupCell>(std::move(tornOut.tailOwner));
  }

  // Do we have an undo buffer for this action?
  if (undoBuffer)
  {
    // We have an undo buffer => add the deleted cells there
    auto cells = static_unique_ptr_cast<GroupCell>(std::move(tornOut.cellOwner));
    undoBuffer->emplace_front(cellBeforeStart, nullptr, cells.release());
    TreeUndo_LimitUndoBuffer();
  }

  if (renumber)
    NumberSections();
  UpdateTableOfContents();
  Recalculate();
  RequestRedraw();
  SetSaved(false);
}

void Worksheet::SetAnswer(const wxString &answer)
{
  GroupCell *answerCell = GetWorkingGroup();
  if (!answerCell
      || answer.empty()
      || m_lastQuestion.empty()
      )
    return;

  answerCell->SetAnswer(m_lastQuestion, answer);
}

void Worksheet::OpenQuestionCaret(const wxString &txt)
{
  GroupCell *group = GetWorkingGroup(true);
  wxASSERT_MSG(group, _("Bug: Got a question but no cell to answer it in"));
  if (!group)
    return;

  // We are leaving the input part of the current cell in this step.
  TreeUndo_CellLeft();

  // We don't need an undo action for the thing we will do now.
  TreeUndo_ActiveCell = NULL;

  // Make sure that the cell containing the question is visible
  if (group->RevealHidden())
  {
    FoldOccurred();
    Recalculate(group);
  }

  // If we still haven't a cell to put the answer in we now create one.
  if (!m_cellPointers.m_answerCell)
  {
    auto answerCell = std::make_unique<EditorCell>(group, &m_configuration);
    m_cellPointers.m_answerCell = answerCell;
    answerCell->SetType(MC_TYPE_INPUT);
    bool autoEvaluate = false;

    if (!txt.empty())
      answerCell->SetValue(txt);
    else
    {
      auto const &text = group->GetAnswer(m_lastQuestion);
      if (!text.empty())
        autoEvaluate = group->AutoAnswer();
      answerCell->SetValue(text);
    }
    answerCell->CaretToEnd();

    group->AppendOutput(std::move(answerCell));
    Recalculate(group);
    // If we filled in an answer and "AutoAnswer" is true we issue an evaluation event here.
    if(autoEvaluate)
    {
      wxMenuEvent *EvaluateEvent = new wxMenuEvent(wxEVT_MENU, wxMaximaFrame::menu_evaluate);
      GetParent()->GetEventHandler()->QueueEvent(EvaluateEvent);
    }
  }
  // If the user wants to be automatically scrolled to the cell evaluation takes place
  // we scroll to this cell.
  if (FollowEvaluation())
    SetActiveCell(m_cellPointers.m_answerCell, false);

  RequestRedraw();
}

void Worksheet::OpenHCaret(const wxString &txt, GroupType type)
{
  CloseAutoCompletePopup();

  // if we are inside cell maxima is currently evaluating
  // bypass normal behaviour and insert an EditorCell into
  // the output of the working group.
  if (GetWorkingGroup() && m_questionPrompt)
  {
    if ((GetActiveCell() && GetActiveCell()->GetGroup() == GetWorkingGroup()) ||
        (m_hCaretPosition && m_hCaretPosition == GetWorkingGroup()->GetNext()))
    {
      OpenQuestionCaret(txt);
      return;
    }
  }
  // set m_hCaretPosition to a sensible value
  if (GetActiveCell())
  {
    SetHCaret(GetActiveCell()->GetGroup());
  }
  else if (m_cellPointers.m_selectionStart)
    SetHCaret(m_cellPointers.m_selectionStart->GetGroup());

  if (!m_hCaretActive)
  {
    if (!m_last)
      return;
    SetHCaret(m_last);
  }

  // insert a new group cell
  auto group = std::make_unique<GroupCell>(&m_configuration, type, txt);
  // check how much to unfold for this type
  if (m_hCaretPosition)
  {
    while (IsLesserGCType(type, m_hCaretPosition->GetGroupType()))
    {
      GroupCell *result = m_hCaretPosition->Unfold();
      if (!result) // assumes that unfold sets hcaret to the end of unfolded cells
        break; // unfold returns NULL when it cannot unfold
      SetHCaret(result);
    }
  }
  if (type == GC_TYPE_CODE && !m_configuration->ShowCodeCells())
  {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }

  auto *editable = group->GetEditable();
  InsertGroupCells(std::move(group), m_hCaretPosition);
  RequestRedraw();

  // activate editor
  SetActiveCell(editable, false);
  if (GetActiveCell())
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
  m_updateControls = true;
  m_configuration->AdjustWorksheetSize(true);
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

  if (m_autocompletePopup)
  {
    m_autocompletePopup->OnKeyDown(event);
    event.Skip();
    return;
  }

  // Alt+Up and Alt+Down are hotkeys, too.
  if (event.AltDown() && (event.GetKeyCode()==WXK_UP || event.GetKeyCode()==WXK_DOWN))
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
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_CUT);
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
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_COPY);
        GetParent()->ProcessWindowEvent(ev);
      }
      else if (event.ShiftDown())
      {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_PASTE);
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
      if(m_configuration->NumpadEnterEvaluates())
      {
        Evaluate();
        break;
      }
     //fallthrough
    case WXK_RETURN:
      // If Ctrl+Shift are pressed at the same time this is an evaluate event.
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
        bool enterEvaluates = false;
        bool controlOrShift = event.ControlDown() || event.ShiftDown();
        wxConfig::Get()->Read(wxT("enterEvaluates"), &enterEvaluates);
        if (enterEvaluates != controlOrShift)
        { // shift-enter pressed === menu_evaluate event
          Evaluate();
        }
        else
          event.Skip();
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
  return GetWorkingGroup() && cell == GetWorkingGroup() && m_questionPrompt;
}

void Worksheet::QuestionAnswered()
{
  if (m_cellPointers.m_answerCell || m_questionPrompt)
  {
    SetActiveCell(NULL);
    GroupCell *wg = GetWorkingGroup(true);
    if (wg)
    {
      SetHCaret(wg);
      ScrollToCaret();
    }
  }
  m_cellPointers.m_answerCell = nullptr;
  m_questionPrompt = false;
}

void Worksheet::UpdateScrollPos()
{
  if (m_newxPosition > 0)
  {
    m_dontSkipScrollEvent = true;
    SetScrollPos(wxHORIZONTAL, m_newxPosition);
  }
  if (m_newyPosition > 0)
  {
    m_dontSkipScrollEvent = true;
    SetScrollPos(wxVERTICAL, m_newyPosition);
  }
  m_newyPosition = -1;
  m_newxPosition = -1;
}
GroupCell *Worksheet::StartOfSectioningUnit(GroupCell *start)
{
  wxASSERT(start);
  // If the current cell is a sectioning cell we return this cell
  if (IsLesserGCType(GC_TYPE_TEXT, start->GetGroupType()))
    return start;

  GroupCell *end = start;
  while (end && !IsLesserGCType(GC_TYPE_TEXT, end->GetGroupType()))
    end = end->GetPrevious();

  // Return the sectioning cell we found - or the current cell which is the
  // next equivalent to a sectioning cell.
  return end ? end : start;
}

GroupCell *Worksheet::EndOfSectioningUnit(GroupCell *start)
{
  wxASSERT(start);
  GroupCell *sectionbegin = StartOfSectioningUnit(start);
  int endgrouptype = sectionbegin->GetGroupType();

  // Begin with the cell after the start cell - that might contain a section
  // start of any sorts.
  GroupCell *end = start->GetNext();
  if (!end)
    return start;

  // Find the end of the chapter/section/...
  while (end->GetNext() && IsLesserGCType(end->GetGroupType(), endgrouptype))
    end = end->GetNext();

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
    GroupCell *previous = GetActiveCell()->GetGroup()->GetPrevious();
    while (previous && (previous->GetMaxDrop() == 0))
      previous = dynamic_cast<GroupCell *>(previous->GetPrevious());

    if (event.ShiftDown())
    {
      SetSelection(previous, GetActiveCell()->GetGroup());
      m_hCaretPosition = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
      m_hCaretPositionEnd = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
      m_hCaretPositionStart = m_cellPointers.m_selectionEnd.CastAs<GroupCell*>();

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
    GroupCell *start = GetActiveCell()->GetGroup();
    while (start && start->GetNext() && start->GetNext()->GetMaxDrop() == 0)
      start = start->GetNext();

    if (event.ShiftDown())
    {
      GroupCell *end = start;
      if (end->GetNext())
        end = end->GetNext();

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

  // CTRL+"s deactivates on MAC
  if (!GetActiveCell())
    return;

  // an empty cell is removed on backspace/delete
  if ((event.GetKeyCode() == WXK_BACK || event.GetKeyCode() == WXK_DELETE || event.GetKeyCode() == WXK_NUMPAD_DELETE) &&
      GetActiveCell()->GetValue() == wxEmptyString)
  {
    SetSelection(GetActiveCell()->GetGroup());
    DeleteSelection();
    return;
  }

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
      GroupCell *newGroup = GetActiveCell()->GetGroup()->GetPrevious();
      while (newGroup && newGroup->GetMaxDrop() == 0)
        newGroup = newGroup->GetPrevious();
      SetHCaret(newGroup);
      return;
    }
    else
      GetActiveCell()->ProcessEvent(event);
    break;
  case WXK_RIGHT:
    if(GetActiveCell()->CaretAtEnd())
    {
      GroupCell *newGroup = GetActiveCell()->GetGroup();
      while (newGroup && newGroup->GetNext() && newGroup->GetNext()->GetMaxDrop() == 0)
        newGroup = newGroup->GetNext();
      SetHCaret(newGroup);
      return;
    }
    else
      GetActiveCell()->ProcessEvent(event);
    break;
  default:
  {
    GetActiveCell()->ProcessEvent(event);
    GroupCell *parent = GetActiveCell()->GetGroup();
    parent->InputHeightChanged();
    RequestRedraw(parent);
  }
  }

  // Update title and toolbar in order to reflect the "unsaved" state of the worksheet.
  if (IsSaved() && activeCell->GetValue() != oldValue)
  {
    SetSaved(false);
    RequestRedraw();
  }
  // The keypress might have moved the cursor off-screen.
  ScrollToCaret();

  m_blinkDisplayCaret = true;

  if (activeCell->IsDirty())
  {
    SetSaved(false);

    int height = activeCell->GetHeight();
    //   int fontsize = m_configuration->GetDefaultFontSize();
    auto fontsize = m_configuration->GetDefaultFontSize();

    GetActiveCell()->Recalculate(wxMax(fontsize, MC_MIN_SIZE));

    if (height != GetActiveCell()->GetHeight() ||
        GetActiveCell()->GetWidth() + GetActiveCell()->GetCurrentPoint().x >=
        GetClientSize().GetWidth() - m_configuration->GetCellBracketWidth() - m_configuration->GetBaseIndent())
      needRecalculate = true;
  }

  /// If we need to recalculate then refresh the window
  if (needRecalculate)
  {
    GroupCell *group = GetActiveCell()->GetGroup();
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
    if (GetActiveCell()->IsSelectionChanged())
    {
      RequestRedraw(GetActiveCell()->GetGroup());
    }
      /// Otherwise refresh only the active cell
    else
    {
      wxRect rect;
      if (GetActiveCell()->CheckChanges())
      {
        GroupCell *group = GetActiveCell()->GetGroup();
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
    if (IsLesserGCType(GC_TYPE_TEXT, GetActiveCell()->GetGroup()->GetGroupType()))
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
  if (!m_hCaretPositionStart || !m_hCaretPositionEnd)
  {
    if (m_hCaretPosition)
      m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPosition;
    else
      m_hCaretPositionStart = m_hCaretPositionEnd = GetTree();

    if (!m_hCaretPositionStart)
      return;

    if (ccode == WXK_DOWN && m_hCaretPosition && m_hCaretPositionStart->GetNext())
    {
      m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPositionStart->GetNext();
      while (m_hCaretPositionStart && m_hCaretPositionStart->GetMaxDrop() == 0 &&
             m_hCaretPositionStart->GetNext())
        m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPositionStart->GetNext();
    }
  }
  else if (ccode == WXK_UP)
  {
    if (KeyboardSelectionStart() &&
        m_hCaretPositionEnd == KeyboardSelectionStart()->GetGroup()->GetNext())
    {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      ClearSelection();
      KeyboardSelectionStart()->ReturnToSelectionFromBot();
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
    }
    else
    {
      // extend / shorten up selection
      GroupCell *prev = m_hCaretPositionEnd->GetPrevious();
      while (prev && prev->GetMaxDrop() == 0)
        prev = prev->GetPrevious();

      if (prev)
      {
        if (m_hCaretPosition && m_hCaretPosition->GetNext() == m_hCaretPositionEnd)
          m_hCaretPositionStart = prev;
        if (m_hCaretPositionEnd)
          m_hCaretPositionEnd = prev;
      }
      if (m_hCaretPositionEnd)
        ScheduleScrollToCell(m_hCaretPositionEnd, false);
    }
  }
  else
  {
    // We arrive here on WXK_DOWN.
    if (
      KeyboardSelectionStart() &&
      m_hCaretPositionEnd == KeyboardSelectionStart()->GetGroup()->GetPrevious()
      )
    {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      ClearSelection();
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
      KeyboardSelectionStart()->ReturnToSelectionFromTop();
    }
    else
    {
      // extend/shorten down selection
      GroupCell *nxt = m_hCaretPositionEnd->GetNext();
      while (nxt && nxt->GetMaxDrop() == 0)
        nxt = nxt->GetNext();

      if (nxt)
      {
        if (m_hCaretPosition == m_hCaretPositionEnd)
          m_hCaretPositionStart = nxt;
        if (m_hCaretPositionEnd)
          m_hCaretPositionEnd = nxt;
      }
      if (m_hCaretPositionEnd)
        ScheduleScrollToCell(m_hCaretPositionEnd, false);
    }
  }

  if (m_hCaretPositionStart && m_hCaretPositionEnd)
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

void Worksheet::SelectEditable(EditorCell *editor, bool up)
{
  if (editor && (
        m_configuration->ShowCodeCells() || editor->GetType() != MC_TYPE_INPUT))
  {
    SetActiveCell(editor, false);
    m_hCaretActive = false;

    if (up)
      editor->CaretToStart();
    else
      editor->CaretToEnd();

    if (editor->GetWidth() == -1)
      Recalculate(editor->GetGroup());

    ScrollToCaret();
  }
  else
  { // can't get editor... jump over to the next cell..
    if (up)
    {
      if (!m_hCaretPosition)
        SetHCaret(GetTree());
      else
      {
        if (m_hCaretPosition->GetNext())
          SetHCaret(m_hCaretPosition->GetNext());
        else
          SetHCaret(m_last);
      }
    }
    else
      SetHCaret(m_hCaretPosition->GetPrevious());
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

  if (m_cellPointers.m_selectionStart &&
      m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE &&
      ccode == WXK_SPACE)
  {
    m_cellPointers.m_selectionStart.CastAs<AnimationCell*>()->AnimationRunning(
      !m_cellPointers.m_selectionStart.CastAs<AnimationCell*>()->AnimationRunning());
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
      while (CellToScrollTo)
      {
        CellToScrollTo = CellToScrollTo->GetPrevious();

        if (CellToScrollTo && CellToScrollTo->GetRect().GetTop() < (topleft.y - height))
          break;
      }
      // We want to put the cursor in the space above the cell we found.
      if (CellToScrollTo)
        CellToScrollTo = CellToScrollTo->GetPrevious();

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
      GroupCell *CellToScrollTo = GetTree();
      GetClientSize(&width, &height);

      // Make sure we scroll at least one cell
      if (CellToScrollTo)
        CellToScrollTo = CellToScrollTo->GetNext();

      // Now scroll far enough that the bottom of the cell we reach is the last
      // bottom of a cell on the new page.
      while (CellToScrollTo && CellToScrollTo != m_last)
      {
        if (CellToScrollTo->GetRect().GetBottom() > topleft.y + 2 * height)
          break;
        else
          CellToScrollTo = CellToScrollTo->GetNext();
      }
      SetHCaret(CellToScrollTo);
      ScrollToCaret();
      ScrolledAwayFromEvaluation();
      break;
    }
      // These are ignored
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
        if (GetTree())
          ScheduleScrollToCell(GetTree(), true);
        if (event.ShiftDown())
        {
          SetSelection(GetTree(), oldCell);
          m_hCaretPositionStart = oldCell;
          m_hCaretPositionEnd = GetTree();
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
          if (oldCell)
            oldCell = oldCell->GetNext();
          SetSelection(oldCell, m_last);
          m_hCaretPositionStart = oldCell;
          m_hCaretPositionEnd = m_last;
        }
        ScrolledAwayFromEvaluation();
      }
      break;

    case WXK_BACK:
      if (m_hCaretPosition)
      {
        SetSelection(m_hCaretPosition);
        RequestRedraw();
        m_hCaretActive = false;
        return;
      }
      break;

    case WXK_DELETE:
    case WXK_NUMPAD_DELETE:
      if (!m_hCaretPosition )
      {
        if (GetTree())
        {
          SetSelection(GetTree());
          m_hCaretActive = false;
          return;
        }
        ScrolledAwayFromEvaluation();
      }
      else if (m_hCaretPosition->GetNext())
      {
        SetSelection(m_hCaretPosition->GetNext());
        m_hCaretActive = false;
        return;
      }
      break;

    case WXK_UP:
    case WXK_LEFT:
      if (CanAnimate())
      {
        AnimationCell *animation = dynamic_cast<AnimationCell *>(GetSelectionStart());
        animation->AnimationRunning(false);
        StepAnimation(-1);
        break;
      }

      ScrolledAwayFromEvaluation(true);
      if (m_hCaretActive)
      {
        if (m_cellPointers.m_selectionStart)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
            if (tmp->GetPrevious())
            {
              do tmp = tmp->GetPrevious();
              while (
                (tmp->GetPrevious()) && (
                  (tmp->GetGroupType() != GC_TYPE_TITLE) &&
                  (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                  (tmp->GetGroupType() != GC_TYPE_SUBSECTION)
                  )
                );
              if (tmp->GetEditable())
                SetHCaret(tmp);
            }
            else
            {
              if (m_hCaretPosition && m_hCaretPosition->GetEditable())
                SelectEditable(tmp->GetEditable(), false);
            }
          }
          else
            SetHCaret(m_cellPointers.m_selectionStart->GetGroup()->GetPrevious());
        }
        else if (m_hCaretPosition)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_hCaretPosition;
            if (tmp->GetPrevious())
            {
              do tmp = tmp->GetPrevious();
              while (
                (tmp->GetPrevious()) && (
                  (tmp->GetGroupType() != GC_TYPE_TITLE) &&
                  (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                  (tmp->GetGroupType() != GC_TYPE_SUBSECTION)
                  )
                );
              SetHCaret(tmp);
            }
            else if (tmp->GetEditable())
              SelectEditable(tmp->GetEditable(), false);
          }
          else
          {
            if (m_hCaretPosition->GetEditable())
              SelectEditable(m_hCaretPosition->GetEditable(), false);
          }
        }
//      This allows to use WXK_UP in order to move the cursorup from the worksheet to the toolbar.
//      But a down key doesn't go back from the toolbar to the worksheet this
//      is more a nuisance than a feature.
//        else
//          event.Skip();
      }
      else if (m_cellPointers.m_selectionStart)
        SetHCaret(m_cellPointers.m_selectionStart->GetGroup()->GetPrevious());
      else if (!ActivatePrevInput())
        event.Skip();
      break;

    case WXK_DOWN:
    case WXK_RIGHT:
      if (CanAnimate())
      {
        AnimationCell *animation = dynamic_cast<AnimationCell *>(GetSelectionStart());
        animation->AnimationRunning(false);
        StepAnimation(1);
        break;
      }
      ScrolledAwayFromEvaluation(true);
      if (m_hCaretActive)
      {
        if (m_cellPointers.m_selectionEnd)
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_cellPointers.m_selectionEnd.CastAs<GroupCell*>();
            if (tmp->GetNext())
            {
              do tmp = tmp->GetNext();
              while (
                tmp->GetNext() && (
                  (
                    (tmp->GetGroupType() != GC_TYPE_TITLE) &&
                    (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_SUBSECTION)
                    ) ||
                  (tmp->GetNext()->GetMaxDrop() == 0)
                  )
                );
              SetHCaret(tmp);
            }
            else
            {
              SelectEditable(tmp->GetEditable(), false);
            }
          }
          else
            SetHCaret(m_cellPointers.m_selectionEnd.CastAs<GroupCell*>());

        }
        else if (m_hCaretPosition && m_hCaretPosition->GetNext())
        {
          if (event.CmdDown())
          {
            GroupCell *tmp = m_hCaretPosition;
            if (tmp->GetNext())
            {
              do tmp = tmp->GetNext();
              while (
                tmp->GetNext() && (
                  (tmp->GetGroupType() != GC_TYPE_TITLE) &&
                  (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                  (tmp->GetGroupType() != GC_TYPE_SUBSECTION) &&
                  (tmp->GetGroupType() != GC_TYPE_SUBSUBSECTION) &&
                  (tmp->GetGroupType() != GC_TYPE_HEADING5) &&
                  (tmp->GetGroupType() != GC_TYPE_HEADING6)
                  )
                );
              SetHCaret(tmp);
            }
            else
              SelectEditable(tmp->GetEditable(), false);
          }
          else
            SelectEditable(m_hCaretPosition->GetNext()->GetEditable(), true);
        }
        else if (GetTree() && !m_hCaretPosition)
        {
          SelectEditable(GetTree()->GetEditable(), true);
        }

      }
      else if (m_cellPointers.m_selectionEnd)
        SetHCaret(m_cellPointers.m_selectionEnd->GetGroup());
      else if (!ActivateNextInput())
        event.Skip();
      break;

    case WXK_RETURN:
      ScrolledAwayFromEvaluation();
      if (!m_cellPointers.m_selectionStart || !m_cellPointers.m_selectionEnd)
        OpenHCaret(wxEmptyString);
      else
        OpenHCaret(GetString());
      break;

      // ESCAPE is handled by the new cell
    case WXK_ESCAPE:
      OpenHCaret(wxEmptyString);
      Recalculate();
      RecalculateIfNeeded();
      RedrawIfRequested();
      if (GetActiveCell())
        Autocomplete(AutoComplete::esccommand);
      break;

      // keycodes which open hCaret with initial content
    default:
      wxChar tx(event.GetUnicodeKey());
      if ((tx == WXK_NONE) || (!wxIsprint(tx)))
      {
        event.Skip();
        return;
      }
      else
      {
        if(wxIsprint(tx))
          OpenHCaret(tx);
      }
  }

  RequestRedraw();
}

void Worksheet::ClearNotification()
{
  m_notificationMessage.reset();
}

void Worksheet::SetNotification(const wxString &message, int flags)
{
  if(m_windowActive)
    return;

  m_notificationMessage.emplace(wxT("wxMaxima"), message, GetParent(), flags);
  m_notificationMessage->Show();

  // In wxGTK 3.1.0 Leaving the notification message object alive until the message
  // hits its timeout causes a crash (https://trac.wxwidgets.org/ticket/17876).
  // Let's work around this crash by deleting the object as fast as we can.
  // The crash is fixed in version 3.1.1.
#if wxCHECK_VERSION(3, 1, 2)
#else
#ifdef __WXGTK__
  ClearNotification();
#endif
#endif
}
/*****
 * OnChar handles key events. If we have an active cell, sends the
 * event to the active cell, else moves the cursor between groups.
 */
void Worksheet::OnChar(wxKeyEvent &event)
{
  m_configuration->AdjustWorksheetSize(true);
  // Alt+Up and Alt+Down are hotkeys. In order for the main application to realize
  // them they need to be passed to it using the event's Skip() function.
  if(event.AltDown())
    event.Skip();

  if (m_autocompletePopup)
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
    return;
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
      event.Skip();
  }

  // Forward cell creation hotkeys to the class wxMaxima
  if (event.CmdDown() && !event.AltDown())
  {
    if (
            (event.GetKeyCode() == WXK_ESCAPE) ||
            (event.GetKeyCode() == wxT('1')) ||
            (event.GetKeyCode() == wxT('2')) ||
            (event.GetKeyCode() == wxT('3')) ||
            (event.GetKeyCode() == wxT('4')) ||
            (event.GetKeyCode() == wxT('5')) ||
            (event.GetKeyCode() == wxT('+')) ||
            (event.GetKeyCode() == wxT('-')) ||
            (event.GetKeyCode() == wxT('m')) ||
            (event.GetKeyCode() == wxT('\n')) ||
            (event.GetKeyCode() == WXK_RETURN)
            )
    {
      event.Skip();
      return;
    }
  }

  // If the find dialogue is open we use the ESC key as a hotkey that closes
  // the dialogue. If it isn't it is used as part of the shortcuts for
  // entering unicode characters instead.
  if (m_findDialog && event.GetKeyCode() == WXK_ESCAPE)
  {
    m_findDialog->Destroy();
    m_findDialog = NULL;
    return;
  }

  if (GetActiveCell())
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
  int currentHeight = m_configuration->GetIndent();
  *width = m_configuration->GetBaseIndent();

  for (Cell const &tmp : OnList(GetTree()))
  {
    currentHeight += tmp.GetHeightList();
    currentHeight += m_configuration->GetGroupSkip();
    int currentWidth = m_configuration->Scale_Px(m_configuration->GetIndent() + m_configuration->GetDefaultFontSize())
                       + tmp.GetWidth()
                       + m_configuration->Scale_Px(m_configuration->GetIndent() + m_configuration->GetDefaultFontSize());
    *width = wxMax(currentWidth, *width);
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
  if (GetTree())
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
  if (m_virtualWidth_Last != width || m_virtualHeight_Last != virtualHeight)
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
  m_configuration->AdjustWorksheetSize(false);
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
    if (GetTree())
      GetTree()->CellUnderPointer(NULL);
    RequestRedraw();
  }
}

#if wxCHECK_VERSION(3,1,1)
void Worksheet::OnZoom(wxZoomGestureEvent &event)
{
  if (event.IsGestureStart())
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
  if (GetSelectionStart() && GetSelectionStart() == GetSelectionEnd() &&
      GetSelectionStart()->GetType() == MC_TYPE_SLIDE)
  {
    AnimationCell *tmp = m_cellPointers.m_selectionStart.CastAs<AnimationCell*>();
    int pos = tmp->GetDisplayedIndex() + change;

    if (change != 0)
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
      if (m_mainToolBar && m_mainToolBar->m_plotSlider)
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

        if (GetActiveCell())
        {
          rect = GetActiveCell()->GetRect();
          GetActiveCell()->SwitchCaretDisplay();
        }
        else
        {
          m_hCaretBlinkVisible = !m_hCaretBlinkVisible;
          if (!m_hCaretPosition)
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
      if (m_hasFocus)
      {
        int blinktime = wxCaret::GetBlinkTime();
        if (blinktime < 200)
          blinktime = 200;
        m_caretTimer.StartOnce(blinktime);
      }
    }
    break;
  default:
  {
      // Determine if the timer that has expired belongs to a slide show cell.
      Cell *const cell = m_cellPointers.GetCellForTimerId(event.GetId());
      AnimationCell *const animation = dynamic_cast<AnimationCell*>(cell);
      if (animation)
      {
        int pos = animation->GetDisplayedIndex() + 1;

        if (pos >= animation->Length())
          pos = 0;
        animation->SetDisplayedIndex(pos);

        // Refresh the displayed bitmap
        if (!m_configuration->ClipToDrawRegion())
          animation->ReloadTimer();
        else
        {
          wxRect rect = animation->GetRect();
          RequestRedraw(rect);
        }

        if (m_mainToolBar && GetSelectionStart() == animation)
        {
          if (m_mainToolBar->m_plotSlider)
            m_mainToolBar->UpdateSlider(animation);
        }
      }
      break;
    }
  }
}

void Worksheet::RequestRedraw(wxRect rect)
{
  if(!m_rectToRefresh.Union(rect))
    m_rectToRefresh = wxRegion(rect);
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
  m_tree.reset();
  m_last = NULL;
}

std::unique_ptr<GroupCell> Worksheet::CopyTree() const
{
  return GetTree() ? GetTree()->CopyList() : nullptr;
}

bool Worksheet::CopyBitmap()
{
  BitmapOut bitmap(&m_configuration, CopySelection(), 1,
                   1000000*m_configuration->MaxClipbrdBitmapMegabytes());
  bool retval = bitmap.ToClipboard();
  Recalculate();
  return retval;
}

bool Worksheet::CopyAnimation()
{
  if (GetSelectionStart() && GetSelectionStart() == GetSelectionEnd() &&
      GetSelectionStart()->GetType() == MC_TYPE_SLIDE)
    return dynamic_cast<AnimationCell *>(GetSelectionStart())->CopyAnimationToClipboard();
  else
    return false;
}

bool Worksheet::CopySVG()
{
  Svgout svg(&m_configuration, CopySelection());
  bool retval = svg.ToClipboard();
  Recalculate();
  return retval;
}

#if wxUSE_ENH_METAFILE
bool Worksheet::CopyEMF()
{
  Emfout emf(&m_configuration, CopySelection());
  bool retval = emf.ToClipboard();
  Recalculate();
  return retval;
}
#endif

bool Worksheet::CopyRTF()
{
  if(!HasCellsSelected())
    return false;

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (!wxTheClipboard->Open())
    return false;

  wxDataObjectComposite *data = new wxDataObjectComposite;

  wxString rtf = RTFStart();
  GroupCell *end = m_cellPointers.m_selectionEnd->GetGroup();

  for (auto &tmp : OnList(m_cellPointers.m_selectionStart->GetGroup()))
  {
    rtf += tmp.ToRTF();
    if (&tmp == end)
      break;
  }

  rtf += wxT("\\par") + RTFEnd();

  data->Add(new RtfDataObject(rtf), true);
  data->Add(new RtfDataObject2(rtf));

  wxTheClipboard->SetData(data);
  wxTheClipboard->Close();
  return true;
}

wxSize Worksheet::CopyToFile(const wxString &file)
{

  if (m_cellPointers.m_selectionStart &&
      m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
      (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE ||
       m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE))
  {
    return m_cellPointers.m_selectionStart.CastAs<ImgCellBase*>()->ToImageFile(file);
  }
  else
  {
    BitmapOut output(&m_configuration, CopySelection());
    wxSize retval = output.ToFile(file);
    return retval;
  }
}

wxSize Worksheet::CopyToFile(const wxString &file, Cell *start, Cell *end,
                            bool asData, double scale)
{
  auto tmp = CopySelection(start, end, asData);
  BitmapOut output(&m_configuration, std::move(tmp), scale);
  wxSize retval = output.ToFile(file);
  return retval;
}

std::unique_ptr<Cell> Worksheet::CopySelection(bool asData) const
{
  return CopySelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd, asData);
}

void Worksheet::TOCdnd()
{
  if(!m_tableOfContents->DNDStart())
    return;
    // Select the region that is to be moved
    m_cellPointers.m_selectionStart =  m_tableOfContents->DNDStart();
    m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionStart;
    if(m_cellPointers.m_selectionEnd->GetNext())
      m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionEnd->GetNext();
    while((m_cellPointers.m_selectionEnd) &&
          (
            (m_cellPointers.m_selectionEnd->GetNext() != NULL) &&
            (dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetNext())->IsLesserGCType(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd.get())->GetGroupType()))))
      m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionEnd->GetNext();

    // Copy the region we want to move
    CellListBuilder<> copy;
    for (auto &src : OnList(m_cellPointers.m_selectionStart.get()))
    {
      copy.Append(src.Copy(dynamic_cast<GroupCell *>(&src)));
      if(&src == m_cellPointers.m_selectionEnd)
        break;
    }

    // Delete the region we just copied and tell the undo buffer that this
    // is only half of the action we want to perform
    DeleteSelection();
    TreeUndo_AppendAction();

    std::unique_ptr<Cell> copiedCells = std::move(copy);
    std::unique_ptr<GroupCell> copiedGroupCells =
      std::move(unique_cast<Cell, GroupCell>(std::move(copiedCells)));
    InsertGroupCells(std::move(copiedGroupCells), m_tableOfContents->DNDEnd());
    RecalculateForce();
    RequestRedraw();
    UpdateTableOfContents();
    NumberSections();
}

std::unique_ptr<Cell> Worksheet::CopySelection(Cell *start, Cell *end, bool asData) const
{
  CellListBuilder<> copy;

  if (asData)
    for (const Cell &tmp : OnList(start))
    {
      copy.Append(tmp.Copy(tmp.GetGroup()));
      if (&tmp == end)
        break;
    }
  else
    for (const Cell &tmp : OnDrawList(start))
    {
      copy.Append(tmp.Copy(tmp.GetGroup()));
      if (&tmp == end)
        break;
    }

  return std::move(copy);
}

void Worksheet::AddLineToFile(wxTextFile &output, const wxString &s)
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
      output.AddLine(line);
    }
  }
}

//returns the index in (%i...) or (%o...)
int Worksheet::GetCellIndex(Cell *cell) const
{
  if (!cell) return -1;
  wxString strindex = cell->ToString().Trim(); //(%i...)
  long temp;
  if (!strindex.Mid(3, strindex.Len() - 4).ToLong(&temp)) return -1;
  return temp;
}

Worksheet::SimpleMathConfigurationIterator::SimpleMathConfigurationIterator(const wxString &ainput) : pos(0), input(ainput)
{
  if (isValid() && (input[0] == '"' || (input[0] == '/' && input.length() > 1 && input[1] == '*')))
  {
    //skip strings or comments at string start
    pos--;
    ++(*this);
  }
}

void Worksheet::SimpleMathConfigurationIterator::operator++()
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

void Worksheet::CalculateReorderedCellIndices(GroupCell *tree, int &cellIndex, std::vector<int> &cellMap)
{
  for (auto &tmp : OnList(tree))
  {
    if (!tmp.IsHidden() && tmp.GetGroupType() == GC_TYPE_CODE)
    {
      wxString input;
      Cell *prompt = tmp.GetPrompt();
      Cell *cell = tmp.GetEditable();

      if (cell)
         input = cell->ToString();

      if (prompt && cell && input.Len() > 0)
      {
        int outputExpressions = 0;
        int initialHiddenExpressions = 0;
        SimpleMathConfigurationIterator it(input);
        for (; it.isValid(); ++it)
        {
          switch (*it)
          {
            case '$':
              if (initialHiddenExpressions == outputExpressions) initialHiddenExpressions++; //fallthrough
            case ';':
              outputExpressions++;
          }
        }

        long promptIndex = GetCellIndex(prompt);
        long outputIndex = GetCellIndex(tmp.GetLabel()) - initialHiddenExpressions;
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

    if (tmp.GetHiddenTree())
      CalculateReorderedCellIndices(tmp.GetHiddenTree(), cellIndex, cellMap);
  }
}

/***
 * Export content to a HTML file.
 */
bool Worksheet::ExportToHTML(const wxString &file)
{
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;

  // Don't update the worksheet whilst exporting
  wxWindowUpdateLocker noUpdates(this);

  // The path to the image directory as seen from the html directory
  wxString imgDir_rel;
  // The absolute path to the image directory
  wxString imgDir;
  // What happens if we split the filename into several parts.
  wxString path, filename, ext;
  wxConfigBase *config = wxConfig::Get();

  int count = 0;
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
    // prevent optimizing <script src="..."><script> to <script src=..."/>
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
  config->Read(wxT("Style/TextBackground/color"), &colorBg);
  config->Read(wxT("Style/Background/color"), &colorTextBg);

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
;
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
    output << wxT("<p>Please enable JavaScript in order to get a 2d display of the equations embedded in this web page.</p>");
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

  for (auto &tmp : OnList(GetTree()))
  {
    // Handle a code cell
    if (tmp.GetGroupType() == GC_TYPE_CODE)
    {

      // Handle the label
      Cell *out = tmp.GetLabel();

      if (out || (m_configuration->ShowCodeCells()))
        output << wxT("\n\n<!-- Code cell -->\n\n\n");

      // Handle the input
      if (m_configuration->ShowCodeCells())
      {
        Cell *prompt = tmp.GetPrompt();
        output << wxT("<table><tr><td>\n");
        output << wxT("  <span class=\"prompt\">\n");
        output << prompt->ToString();
        output << wxT("\n  </span></td>\n");

        EditorCell *input = tmp.GetEditable();
        if (input)
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
        // Output is a list that can consist of equations, images and animations.
        // We need to handle each of these item types separately => break down the list
        // into chunks of one type.
        Cell *chunkStart = tmp.GetLabel();
        while (chunkStart != NULL)
        {
          Cell *chunkEnd = chunkStart;

          if (
                  (chunkEnd->GetType() != MC_TYPE_SLIDE) &&
                  (chunkEnd->GetType() != MC_TYPE_IMAGE)
                  )
            while (chunkEnd->GetNext() != NULL)
            {
              auto *chunkNext = chunkEnd->GetNext();
              if (
                      (chunkNext->GetType() == MC_TYPE_SLIDE) ||
                      (chunkNext->GetType() == MC_TYPE_IMAGE) ||
                      (chunkNext->GetStyle() == TS_LABEL) ||
                      (chunkNext->GetStyle() == TS_USERLABEL)
                      )
                break;
              chunkEnd = chunkNext;
            }

          // Create a list containing only our chunk.
          auto chunk = CopySelection(chunkStart, chunkEnd);

          // Export the chunk.

          if (chunk->GetType() == MC_TYPE_SLIDE)
          {
            dynamic_cast<AnimationCell *>(&(*chunk))->ToGif(
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
              break;
            }

            case Configuration::svg:
            {
              auto const alttext = EditorCell::EscapeHTMLChars(chunk->ListToString());
              auto const filepath = wxString::Format(wxT("%s/%s_%d.svg"), imgDir, filename, count);
              Svgout svgout(&m_configuration, std::move(chunk), filepath);

              wxString line = wxT("  <img src=\"") +
                filename_encoded + wxT("_htmlimg/") + filename_encoded +
                wxString::Format(wxT("_%d.svg\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\"" ),
                                 count, svgout.GetSize().x) +
                alttext +
                wxT("\" /><br/>\n");

              output << line + "\n";
              break;
            }

            case Configuration::bitmap:
            {
              wxSize size;
              size = CopyToFile(imgDir + wxT("/") + filename + wxString::Format(wxT("_%d.png"), count),
                                &(*chunk),
                                NULL, true, m_configuration->BitmapScale());
              int borderwidth = 0;
              wxString alttext = EditorCell::EscapeHTMLChars(chunk->ListToString());
              borderwidth = chunk->GetImageBorderWidth();

              wxString line = wxT("  <img src=\"") +
                filename_encoded + wxT("_htmlimg/") + filename_encoded +
                wxString::Format(wxT("_%d%s\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\" "),
                                 count, ext.utf8_str(), size.x / m_configuration->BitmapScale() - 2 * borderwidth) +
                alttext +
                wxT("\" /><br/>\n");

              output << line + "\n";
              break;
            }

            default:
            {
              wxString line = chunk->ListToMathML();
              output << wxT("<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\"block\">") << line
                     << wxT("</math>\n");
            }
            }
          }
          else
          {
            wxSize size;
            ext = wxT(".") + dynamic_cast<ImgCellBase *>(&(*chunk))->GetExtension();
            size = dynamic_cast<ImgCellBase *>(&(*chunk))->ToImageFile(
              imgDir + wxT("/") + filename + wxString::Format(wxT("_%d"), count) + ext);
            int borderwidth = 0;
            wxString alttext = EditorCell::EscapeHTMLChars(chunk->ListToString());
            borderwidth = chunk->GetImageBorderWidth();

            wxString line = wxT("  <img src=\"") +
              filename_encoded + wxT("_htmlimg/") + filename_encoded +
              wxString::Format(wxT("_%d%s\" width=\"%i\" style=\"max-width:90%%;\" loading=\"lazy\" alt=\""),
                               count, ext.utf8_str(), size.x - 2 * borderwidth) +
              alttext +
              wxT("\" /><br/>\n");

            output << line + "\n";
          }
          count++;

          chunkStart = chunkEnd->GetNext();
        }
      }
    }
    else // No code cell
    {
      switch (tmp.GetGroupType())
      {
        case GC_TYPE_TEXT:
          output << wxT("\n\n<!-- Text cell -->\n\n\n");
          output << wxT("<div class=\"comment\">\n");
          // A text cell can include block-level HTML elements, e.g. <ul> ... </ul> (converted from Markdown)
          // Therefore do not output <p> ... </p> elements, that would result in invalid HTML.
          output << MarkDown.MarkDown(EditorCell::EscapeHTMLChars(tmp.GetEditable()->ToString())) + "\n";
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SECTION:
          output << wxT("\n\n<!-- Section cell -->\n\n\n");
          output << wxT("<div class=\"section\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() + tmp.GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SUBSECTION:
          output << wxT("\n\n<!-- Subsection cell -->\n\n\n");
          output << wxT("<div class=\"subsect\">\n");
          output << wxT("<p>\n");
          output <<
                  EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() + tmp.GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_SUBSUBSECTION:
          output << wxT("\n\n<!-- Subsubsection cell -->\n\n\n");
          output << wxT("<div class=\"subsubsect\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() + tmp.GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_HEADING5:
          output << wxT("\n\n<!-- Heading5 cell -->\n\n\n");
          output << wxT("<div class=\"heading5\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() + tmp.GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_HEADING6:
          output << wxT("\n\n<!-- Heading6 cell -->\n\n\n");
          output << wxT("<div class=\"heading6\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() + tmp.GetEditable()->ToString()) + "\n";
          output << wxT("</p>\n");
          output << wxT("</div>\n");
          break;
        case GC_TYPE_TITLE:
          output << wxT("\n\n<!-- Title cell -->\n\n\n");
          output << wxT("<div class=\"title\">\n");
          output << wxT("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetEditable()->ToString()) +"\n";
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
          Cell *out = tmp.GetLabel();
          output << wxT("<div class=\"image\">\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) << wxT("\n");
          output << wxT("<br/>\n");
          if (tmp.GetLabel()->GetType() == MC_TYPE_SLIDE)
          {
            dynamic_cast<AnimationCell *>(tmp.GetOutput())->ToGif(imgDir + wxT("/") + filename +
                                                              wxString::Format(wxT("_%d.gif"), count));
            output << wxT("  <img src=\"") + filename_encoded + wxT("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(_("_%d.gif\" alt=\"Animated Diagram\" style=\"max-width:90%%;\" loading=\"lazy\" />"), count)
                   << wxT("\n");
          }
          else
          {
            ImgCellBase *imgCell = dynamic_cast<ImgCellBase *>(out);
            imgCell->ToImageFile(
                    imgDir + wxT("/") + filename + wxString::Format(wxT("_%d."), count) +
                    imgCell->GetExtension());
            output << wxT("  <img src=\"") + filename_encoded + wxT("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(wxT("_%d.%s\" alt=\"Diagram\" style=\"max-width:90%%;\" loading=\"lazy\" />"), count,
                                       imgCell->GetExtension().utf8_str());
          }
          output << wxT("</div>\n");
          count++;
        }
          break;
        case GC_TYPE_CODE:
        case GC_TYPE_INVALID:
          break;
      }
    }
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

  if (m_configuration->ExportContainsWXMX())
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

  // Replace the raw document by the indented one. If that step worked, that is it.
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
  if (GetActiveCell() && GetActiveCell()->GetType() == MC_TYPE_INPUT
      && !m_configuration->ShowCodeCells()
      )
    SetHCaret(GetActiveCell()->GetGroup());
  RecalculateForce();
  ScrollToCaret();
}

/*! Export the file as TeX code
 */
bool Worksheet::ExportToTeX(const wxString &file)
{
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;

  // Don't update the worksheet whilst exporting
  wxWindowUpdateLocker noUpdates(this);

  wxString imgDir;
  wxString path, filename, ext;

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

  output << wxT("\n");
  output << wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");
  output << wxT("\n");

  // Add an eventual preamble requested by the user.
  wxString texPreamble = m_configuration->TexPreamble();
  if (!texPreamble.IsEmpty())
    output << texPreamble << wxT("\n\n");

  output << wxT("\\begin{document}\n");

  //
  // Write contents
  //
  for (auto &tmp : OnList(GetTree()))
  {
    wxString s = tmp.ToTeX(imgDir, filename, &imgCounter);
    output << s << wxT("\n");
  }

  //
  // Close document
  //
  output << wxT("\\end{document}\n");


  bool done = !outfile.GetFile()->Error();
  outfile.Close();

  return done;
}

void Worksheet::LoadSymbols()
{
  if(m_helpfileanchorsThread)
  {
    m_helpfileanchorsThread->join();
    m_helpfileanchorsThread.reset();
  }
  if(m_helpFileAnchors.empty())
  {
    if(!LoadManualAnchorsFromCache())
    {
      if(wxFileExists(GetMaximaHelpFile()))
      {
        if(m_helpfileanchorsThread)
        {
          // Show a busy cursor as long as we finish the old background task
          wxBusyCursor crs;
          m_helpfileanchorsThread->join();
        }
        m_helpfileanchorsThread =
          std::unique_ptr<std::thread>(
            new std::thread(&Worksheet::CompileHelpFileAnchors, this));
      }
      else
        LoadBuiltInManualAnchors();
    }
  }
  m_autocomplete.LoadSymbols();
}

bool Worksheet::LoadBuiltInManualAnchors()
{
  wxLogMessage(_("Using the built-in list of manual anchors."));
  wxMemoryInputStream istream(MANUAL_ANCHORS_XML, MANUAL_ANCHORS_XML_SIZE);
  wxXmlDocument xmlDoc;
  if(!xmlDoc.Load(istream, wxT("UTF-8")))
    return false;
  if(!LoadManualAnchorsFromXML(xmlDoc, false))
    return false;
  return true;
}

bool Worksheet::LoadManualAnchorsFromCache()
{
  SuppressErrorDialogs suppressor;
  wxString anchorsFile = Dirstructure::Get()->AnchorsCacheFile();
  if(!wxFileExists(anchorsFile))
  {
    wxLogMessage(_("No file with the subjects the manual contained in the last wxMaxima run."));
    return false;
  }
  wxXmlDocument xmlDocument(anchorsFile);
  if(!xmlDocument.IsOk())
  {
    wxLogMessage(_("The cache for the subjects the manual contains cannot be read."));
    wxRemoveFile(anchorsFile);
    return false;
  }

  if(LoadManualAnchorsFromXML(xmlDocument))
  {
    wxLogMessage(wxString::Format(_("Read the entries the maxima manual offers from %s"), Dirstructure::Get()->AnchorsCacheFile().utf8_str()));
    return true;
  }
  return !m_helpFileAnchors.empty();
}


void Worksheet::CompileHelpFileAnchors()
{
  SuppressErrorDialogs suppressor;

  wxString MaximaHelpFile = GetMaximaHelpFile();

  if(m_helpFileAnchors.empty() && (!(MaximaHelpFile.IsEmpty())))
  {
    m_helpFileAnchors["wxbarsplot"] = "barsplot";
    m_helpFileAnchors["wxboxplot"] = "boxplot";
    m_helpFileAnchors["wxhistogram"] = "histogram";
    m_helpFileAnchors["wxpiechart"] = "piechart";
    m_helpFileAnchors["wxscatterplot"] = "scatterplot";
    m_helpFileAnchors["wxstarplot"] = "starplot";
    m_helpFileAnchors["wxdrawdf"] = "drawdf";
    m_helpFileAnchors["wxdraw"] = "draw";
    m_helpFileAnchors["wxdraw2d"] = "draw2d";
    m_helpFileAnchors["wxdraw3d"] = "draw3d";
    m_helpFileAnchors["with_slider_draw"] = "draw";
    m_helpFileAnchors["with_slider_draw2d"] = "draw2d";
    m_helpFileAnchors["with_slider_draw3d"] = "draw3d";

    int foundAnchors = 0;
    wxLogMessage(_("Compiling the list of anchors the maxima manual provides"));
    wxRegEx idExtractor(".*<span id=\\\"([a-zAZ0-9_-]*)\\\"");
    wxRegEx idExtractor_oldManual(".*<a name=\\\"([a-zAZ0-9_-]*)\\\"");
    wxString escapeChars = "<=>[]`%?;\\$%&+-*/.!\'@#:^_";
    if(wxFileExists(MaximaHelpFile))
    {
      wxFileInputStream input(MaximaHelpFile);
      if(input.IsOk())
      {
        wxTextInputStream text(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
        while(input.IsOk() && !input.Eof())
        {
          wxString line = text.ReadLine();
          wxStringTokenizer tokens(line, wxT(">"));
          while(tokens.HasMoreTokens())
          {
            wxString token = tokens.GetNextToken();
            wxString oldToken(token);
            wxString id;
            if(idExtractor.Replace(&token, "\\1")>0)
              id = token;
            else
            {
              if(idExtractor_oldManual.Replace(&token, "\\1")>0)
                id = token;
            }
            if(!id.IsEmpty())
            {
              // In anchors a space is represented by a hyphen
              token.Replace("-", " ");
              // Some other chars including the minus are represented by "_00xx"
              // where xx is being the ascii code of the char.
              for(wxString::const_iterator it = escapeChars.begin(); it != escapeChars.end(); ++it)
                token.Replace(wxString::Format("_00%x",(char)*it), *it);
              // What the g_t means I don't know. But we don't need it
              if(token.StartsWith("g_t"))
                token = token.Right(token.Length()-3);
              //! Tokens that end with "-1" aren't too useful, normally.
              if((!token.EndsWith("-1")) && (!token.Contains(" ")))              {
                m_helpFileAnchors[token] = id;
                foundAnchors++;
              }
            }
          }
        }
      }
    }
    if(m_helpFileAnchors["%solve"].IsEmpty())
      m_helpFileAnchors["%solve"] = m_helpFileAnchors["to_poly_solve"];

    if((m_helpFileAnchors.find("find_root_error") == m_helpFileAnchors.end()) &&
       (m_helpFileAnchors.find("find_root") != m_helpFileAnchors.end()))
      m_helpFileAnchors["find_root_error"] = m_helpFileAnchors["find_root"];

    wxLogMessage(wxString::Format(_("Found %i anchors."), foundAnchors));
    if(foundAnchors > 50)
      SaveManualAnchorsToCache();
    else
      LoadBuiltInManualAnchors();
  }
}

void Worksheet::SaveManualAnchorsToCache()
{
  long num = m_helpFileAnchors.size();
  if(num <= 50)
  {
    wxLogMessage(
      wxString::Format(
        _("Found only %li keywords in maxima's manual. Not caching them to disc."),
        num));
    return;
  }
  wxXmlAttribute *maximaVersion = new wxXmlAttribute(
    wxT("maxima_version"),
    m_maximaVersion);
  wxXmlNode *topNode = new wxXmlNode(
    NULL,
    wxXML_DOCUMENT_NODE, wxEmptyString,
    wxEmptyString
    );
  wxXmlNode *headNode = new wxXmlNode(
    topNode,
    wxXML_ELEMENT_NODE, wxT("maxima_toc"),
    wxEmptyString,
    maximaVersion
    );

  Worksheet::HelpFileAnchors::const_iterator it;
  for (it = m_helpFileAnchors.begin();
       it != m_helpFileAnchors.end();
       ++it)
  {
    wxXmlNode *manualEntry =
      new wxXmlNode(
        headNode,
          wxXML_ELEMENT_NODE,
        "entry");
    wxXmlNode *anchorNode = new wxXmlNode(
      manualEntry,
      wxXML_ELEMENT_NODE,
      "anchor");
    new wxXmlNode(
      anchorNode,
      wxXML_TEXT_NODE,
      wxEmptyString,
      it->second);
    wxXmlNode *keyNode = new wxXmlNode(
      manualEntry,
      wxXML_ELEMENT_NODE,
        "key");
    new wxXmlNode(
        keyNode,
        wxXML_TEXT_NODE,
        wxEmptyString,
        it->first);
  }
  wxXmlDocument xmlDoc;
  xmlDoc.SetDocumentNode(topNode);
  wxXmlNode *commentNode = new wxXmlNode(
    NULL,
    wxXML_COMMENT_NODE,
    wxEmptyString,
    _("This file is generated by wxMaxima\n"
      "It caches the list of subjects maxima's manual offers and is automatically\n"
      "overwritten if maxima's version changes or the file cannot be read"));

  xmlDoc.AppendToProlog(commentNode);

  wxString saveName = Dirstructure::AnchorsCacheFile();
  wxLogMessage(wxString::Format(_("Trying to cache the list of subjects the manual contains in the file %s."),
                                saveName.utf8_str()));
  xmlDoc.Save(saveName);
}

bool Worksheet::LoadManualAnchorsFromXML(wxXmlDocument xmlDocument, bool checkManualVersion)
{
  wxXmlNode *headNode = xmlDocument.GetDocumentNode();
  if(!headNode)
  {
    wxLogMessage(_("The cache for the subjects the manual contains has no head node."));
    return false;
  }
  headNode = headNode->GetChildren();
  while((headNode) && (headNode->GetName() != wxT("maxima_toc")))
    headNode = headNode->GetNext();
  if(!headNode)
  {
    wxLogMessage(_("Anchors file has no top node."));
    return false;
  }
  if(checkManualVersion && ((headNode->GetAttribute(wxT("maxima_version")) != m_maximaVersion)))
  {
    wxLogMessage(_("The cache for the subjects the manual contains is from a different Maxima version."));
    return false;
  }
  wxXmlNode *entry = headNode->GetChildren();
  if(entry == NULL)
  {
    wxLogMessage(_("No entries in the caches for the subjects the manual contains."));
    return false;
  }
  while(entry)
  {
    if(entry->GetName() == wxT("entry"))
    {
      wxString key;
      wxString anchor;
      wxXmlNode *node = entry->GetChildren();
      while(node)
      {
        if((node->GetName() == wxT("anchor")) && (node->GetChildren()))
          anchor = node->GetChildren()->GetContent();
        if((node->GetName() == wxT("key")) && (node->GetChildren()))
          key = node->GetChildren()->GetContent();
        node = node->GetNext();
      }
      if((!key.IsEmpty()) && (!anchor.IsEmpty()))
        m_helpFileAnchors[key] = anchor;
    }
    entry = entry->GetNext();
  }
  return !m_helpFileAnchors.empty();
}

wxString Worksheet::GetMaximaHelpFile()
{
  // One may set the help file location in the wxMaxima configuration (on Unix: ~/.wxMaxima), e.g.
  // helpFile=/usr/local/share/maxima/5.44.0/doc/html/maxima_singlepage.html
  // Use that file, if the configuration option is used.
  wxString headerFile;
  wxConfig::Get()->Read(wxT("helpFile"), &headerFile);
  if (headerFile.Length() && wxFileExists(headerFile)) {
    wxLogMessage(_("Using Maxima help file from wxMaxima configuration file (helpFile=...))"));
    return headerFile;
  }
#ifdef __CYGWIN__
  // Cygwin uses /c/something instead of c:/something and passes this path to the
  // web browser - which doesn't support cygwin paths => convert the path to a
  // native windows pathname if needed.
  if(headerFile.Length()>1 && headerFile[1]==wxT('/'))
  {
    headerFile[1]=headerFile[2];
    headerFile[2]=wxT(':');
  }
#endif // __CYGWIN__
  wxPathList helpfilepaths;
  helpfilepaths.Add(m_maximaDocDir);
  helpfilepaths.Add(m_maximaDocDir+"/html");
  helpfilepaths.Add(m_maximaDocDir+"/../html");
  helpfilepaths.Add(m_configuration->MaximaShareDir() + "/../doc/html");
  helpfilepaths.Add(m_configuration->MaximaShareDir() + "/doc/html");
  wxString helpfile_location = helpfilepaths.FindAbsoluteValidPath("maxima_singlepage.html");
  wxFileName helpfile_cleanup(helpfile_location);
  helpfile_cleanup.Normalize(wxPATH_NORM_ENV_VARS | wxPATH_NORM_DOTS | wxPATH_NORM_TILDE | wxPATH_NORM_ABSOLUTE | wxPATH_NORM_LONG | wxPATH_NORM_SHORTCUT);
  if (helpfile_cleanup.IsFileReadable()) {
    return helpfile_cleanup.GetFullPath();
  } else {
    return wxEmptyString;
  }
}

wxString Worksheet::UnicodeToMaxima(wxString s)
{
  s.Replace(wxT("\u2052"), "-"); // commercial minus sign
  s.Replace(wxT("\uFE63"), "-"); // unicode small minus sign
  s.Replace(wxT("\uFF0D"), "-"); // unicode big minus sign
  s.Replace(wxT("\uFF0B"), "+"); // unicode big plus
  s.Replace(wxT("\uFB29"), "+"); // hebrew alternate plus

  wxString retval;

  for (auto const &tok : MaximaTokenizer(s, m_configuration).PopTokens())
  {
    auto &tokenString = tok.GetText();
    switch(tok.GetStyle())
    {
    case TS_DEFAULT:
    case TS_CODE_OPERATOR:
    case TS_CODE_VARIABLE:
    case TS_CODE_FUNCTION:
      if(tokenString == wxT("\u221A")) {retval += wxT(" sqrt ");continue;}
      if(tokenString == wxT("\u222B")) {retval += wxT(" integrate ");continue;}
      if(tokenString == wxT("\u2211")) {retval += wxT(" sum ");continue;}
      if(tokenString == wxT("\u220F")) {retval += wxT(" product ");continue;}
      if(tokenString == wxT("\u2148")) {retval += wxT(" %i ");continue;}
      if(tokenString == wxT("\u2147")) {retval += wxT(" %e ");continue;}
      if(tokenString == wxT("\u22C0")) {retval += wxT(" and ");continue;}
      if(tokenString == wxT("\u22C1")) {retval += wxT(" or ");continue;}
      if(tokenString == wxT("\u22BB")) {retval += wxT(" xor ");continue;}
      if(tokenString == wxT("\u22BC")) {retval += wxT(" nand ");continue;}
      if(tokenString == wxT("\u22BD")) {retval += wxT(" nor ");continue;}
      if(tokenString == wxT("\u21D2")) {retval += wxT(" implies ");continue;}
      if(tokenString == wxT("\u21D4")) {retval += wxT(" equiv ");continue;}
      if(tokenString == wxT("\u00AC")) {retval += wxT(" not ");continue;}
      if(tokenString == wxT("\u03C0")) {retval += wxT(" %pi ");continue;}
      // Only executed if none of the conditions that can be found above fires
      retval += tokenString;
      break;
    default:
      if(tokenString == wxT("\u221E")) {retval += wxT(" inf ");continue;}
      retval += tokenString;
    }
  }

  retval.Replace(wxT("\u00B2"), "^2");
  retval.Replace(wxT("\u00B3"), "^3");
  retval.Replace(wxT("\u00BD"), "(1/2)");
  retval.Replace(wxT("\u2205"), "[]"); // An empty list
  retval.Replace(wxT("\u2212"), "-");
  retval.Replace(wxT("\u2260"), "#");  // The "not equal" sign
  retval.Replace(wxT("\u2264"), "<=");
  retval.Replace(wxT("\u2265"), ">=");
  retval.Replace(wxT("\u00B7"), "*");  // An unicode multiplication sign
  retval.Replace(wxT("\u2052"), "-");  // commercial minus sign
  retval.Replace(wxT("\uFE63"), "-");  // unicode small minus sign
  retval.Replace(wxT("\uFF0D"), "-");  // unicode big minus sign
  retval.Replace(wxT("\uFF0B"), "+");  // unicode big plus
  retval.Replace(wxT("\uFB29"), "+");  // hebrew alternate plus
  return retval;
}

void Worksheet::ExportToMAC(wxTextFile &output, GroupCell *tree, bool wxm, const std::vector<int> &cellMap,
                           bool fixReorderedIndices)
{
  // Show a busy cursor as long as we open a file.
  wxBusyCursor crs;

  // Don't update the worksheet whilst exporting
  wxWindowUpdateLocker noUpdates(this);

  //
  // Write contents
  //
  for (auto &tmp : OnList(tree))
  {
    AddLineToFile(output, Format::TreeToWXM(&tmp));

    if (wxm && tmp.GetGroupType() == GC_TYPE_CODE)
    {
      EditorCell *txt = tmp.GetEditable();
      if (txt && fixReorderedIndices)
      {
        wxString input = txt->ToString(true);

        for (SimpleMathConfigurationIterator it = SimpleMathConfigurationIterator(input);
             it.pos + 1 < it.input.length(); ++it
             )
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
  }
}

bool Worksheet::ExportToMAC(const wxString &file)
{
  bool wasSaved = m_saved;

  // Show a busy cursor as long as we export or save.
  wxBusyCursor crs;
  // Don't update the worksheet whilst exporting
  wxWindowUpdateLocker noUpdates(this);

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
    AddLineToFile(backupfile, Format::WXMFirstLine);
    wxString version(wxT(GITVERSION));
    AddLineToFile(backupfile, wxT("/* [ Created with wxMaxima version ") + version + wxT(" ] */"));
  }

  bool fixReorderedIndices = m_configuration->FixReorderedIndices();
  std::vector<int> cellMap;
  if (fixReorderedIndices)
  {
    int cellIndex = 1;
    CalculateReorderedCellIndices(GetTree(), cellIndex, cellMap);
  }
  ExportToMAC(backupfile, GetTree(), wxm, cellMap, fixReorderedIndices);

  AddLineToFile(backupfile, wxEmptyString);
  if (wxm)
  {
    AddLineToFile(backupfile, wxT("/* Old versions of Maxima abort on loading files that end in a comment. */"));
    AddLineToFile(backupfile, wxT("\"Created with wxMaxima " GITVERSION "\"$"));
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
    SetSaved(true);
  else
    SetSaved(wasSaved);
  return true;
}

/*
  Save the data as wxmx file

  First saves the data to a backup file ending in .wxmx~ so if anything goes
  horribly wrong in this stepp all that is lost is the data that was input
  since the last save. Then the original .wxmx file is replaced in a
  (hopefully) atomic operation.
*/
bool Worksheet::ExportToWXMX(const wxString &file, bool markAsSaved)
{
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;
  // Don't update the worksheet whilst exporting
  wxWindowUpdateLocker noUpdates(this);
  wxLogMessage(_("Starting to save the worksheet as .wxmx"));
  // delete temp file if it already exists
  wxString backupfile = file + wxT("~");
  if (wxFileExists(backupfile))
  {
    if (!wxRemoveFile(backupfile))
      return false;
  }
  {
    wxFFileOutputStream out(backupfile);
    if (!out.IsOk())
      return false;
    {
      wxZipOutputStream zip(out);
      if (!zip.IsOk())
        return false;
      {
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
          "viewed using a text editor just save the xml's text as \"content.xml\"\n"
          "and try to open it using a recent version of wxMaxima.\n"
          "If it is valid XML (the XML header is intact, all opened tags are closed again,\n"
          "the text is saved with the text encoding \"UTF8 without BOM\" and the few\n"
          "special characters XML requires this for are properly escaped)\n"
          "chances are high that wxMaxima will be able to recover all code and text\n"
          "from the XML file.\n\n"
          );
        zip.CloseEntry();

        // next zip entry is "content.xml", xml of GetTree()

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
            cursorCell = GetActiveCell()->GetGroup();
        }

        if (cursorCell == NULL)
          ActiveCellNumber = 0;

        // We want to save the information that the cursor is in the nth cell.
        // Count the cells until then.

        bool found = false;
        if (GetTree() && ActiveCellNumber > 0)
          for (auto &tmp : OnList(GetTree()))
          {
            if (&tmp == cursorCell)
            {
              found = true;
              break;
            }
            ActiveCellNumber++;
          }

        // Paranoia: What happens if we didn't find the cursor?
        if (!GetTree() || !found)
          ActiveCellNumber = -1;

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
            xmlText += wxString::Format(" variables_%li=\"%s\"", i, Cell::XMLescape(variables[i]).utf8_str());
        }

        xmlText << ">\n";

        // Reset image counter
        m_cellPointers.WXMXResetCounter();

        if (GetTree())
          xmlText += GetTree()->ListToXML();

        // Delete all but one control character from the string: there should be
        // no way for them to enter this string, anyway. But sometimes they still
        // do...
        for (wxString::const_iterator it = xmlText.begin(); it != xmlText.end(); ++it)
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

        {
          // Prepare reading the files we have stored in memory
          std::unique_ptr<wxFileSystem> fsystem(new wxFileSystem);
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
                wxLogMessage(_("Produced invalid XML. The erroneous XML data has therefore not been saved but has been put on the clipboard in order to allow to debug it."));
                wxTheClipboard->Close();
              }

              // Remove all files from our internal filesystem
              wxString memFsName = fsystem->FindFirst("*", wxFILE);
              while(memFsName != wxEmptyString)
              {
                wxString name = memFsName.Right(memFsName.Length()-7);
                wxMemoryFSHandler::RemoveFile(name);
                memFsName = fsystem->FindNext();
              }
              return false;
            }
          }

          // wxWidgets could pretty-print the XML document now. But as no-one will
          // look at it, anyway, there might be no good reason to do so.
          if (GetTree() != NULL)output << xmlText;

          // Move all files we have stored in memory during saving to zip file
          wxString memFsName = fsystem->FindFirst("*", wxFILE);
          while(memFsName != wxEmptyString)
          {
            wxString name = memFsName.Right(memFsName.Length()-7);
            if(name != wxT("dummyfile"))
            {
              zip.CloseEntry();

              wxFSFile *fsfile;
              fsfile = fsystem->OpenFile(memFsName);

              if (fsfile)
              {
                // The data for gnuplot is likely to change in its entirety if it
                // ever changes => We can store it in a compressed form.
                if(name.EndsWith(wxT(".data")))
                  zip.SetLevel(9);
                else
                  zip.SetLevel(0);

                zip.PutNextEntry(name);
                std::unique_ptr<wxInputStream> imagefile(fsfile->GetStream());

                while (!(imagefile->Eof()))
                  imagefile->Read(zip);
              }
            }
            wxMemoryFSHandler::RemoveFile(name);
            memFsName = fsystem->FindNext();
          }
        }
      }
      if(!zip.Close())
        return false;
    }
    if (!out.Close())
      return false;
  }
  // If all data is saved now we can overwrite the actual save file.
  // We will try to do so a few times if we suspect a MSW virus scanner or similar
  // temporarily hindering us from doing so.

  // The following line is paranoia as closing (and thus writing) the file has
  // succeeded.
  if(!wxFileExists(backupfile))
    return false;

  // Now we try to open the file in order to see if saving hasn't failed
  // without returning an error - which can apparently happen on MSW.
  wxString wxmxURI = wxURI(wxT("file://") + backupfile).BuildURI();
  wxmxURI.Replace("#", "%23");
#ifdef  __WXMSW__
  // Fixes a missing "///" after the "file:". This works because we always get absolute
  // file names.
  wxRegEx uriCorector1("^file:([a-zA-Z]):");
  wxRegEx uriCorector2("^file:([a-zA-Z][a-zA-Z]):");
  uriCorector1.ReplaceFirst(&wxmxURI,wxT("file:///\\1:"));
  uriCorector2.ReplaceFirst(&wxmxURI,wxT("file:///\\1:"));
#endif
  // The URI of the wxm code contained within the .wxmx file
  wxString filename = wxmxURI + wxT("#zip:content.xml");

  // Open the file we have saved yet just in order to see if we
  // actually managed to save it correctly.
  {
    wxFileSystem fs;
    wxFSFile *fsfile;
    fsfile = fs.OpenFile(filename);

    // Did we succeed in opening the file?
    if (!fsfile)
    {
      wxLogMessage(_(wxT("Saving succeeded, but the file could not be read again \u21D2 Not replacing the old saved file.")));
      return false;
    }
    wxDELETE(fsfile);
  }

  {
    bool done;
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
    if(!done)
    {
      wxSleep(1);
      if (!wxRenameFile(backupfile, file, true))
        return false;
    }
    if (markAsSaved)
      SetSaved(true);

    wxLogMessage(_("wxmx file saved"));
  }
  return true;
}

bool Worksheet::CanEdit()
{
  if (!m_cellPointers.m_selectionStart || m_cellPointers.m_selectionEnd != m_cellPointers.m_selectionStart)
    return false;

  if (!m_cellPointers.m_selectionStart->IsEditable())
    return false;

  if (!m_cellPointers.m_selectionStart->GetPrevious())
    return false;

  if (m_cellPointers.m_selectionStart->GetPrevious()->GetType() != MC_TYPE_MAIN_PROMPT)
    return false;

  return true;
}

void Worksheet::OnDoubleClick(wxMouseEvent &WXUNUSED(event))
{
  m_updateControls = true;
  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  if (GetActiveCell())
    GetActiveCell()->SelectWordUnderCaret();
  else if (m_cellPointers.m_selectionStart)
  {
    // FIXME This code path can never get activated, because
    // OnMouseLeftDown clears the selection.
    GroupCell *parent = m_cellPointers.m_selectionStart->GetGroup();
    auto sel = parent->GetCellsInOutput();
    m_cellPointers.m_selectionStart = sel.first;
    m_cellPointers.m_selectionEnd = sel.last;
  }

  RequestRedraw();
// Re-calculate the table of contents
  UpdateTableOfContents();
}

bool Worksheet::ActivateInput(int direction)
{
  auto const advance =
      (direction >= 0) ? +[](GroupCell *cell) { return cell->GetNext(); }
                       : +[](GroupCell *cell) { return cell->GetPrevious(); };

  GroupCell *tmp = {};
  if (m_cellPointers.m_selectionStart)
    tmp = m_cellPointers.m_selectionStart->GetGroup();
  else if (GetActiveCell())
  {
    tmp = GetActiveCell()->GetGroup();
    SetActiveCell(nullptr);
  }
  if (!tmp)
    return false;

  tmp = advance(tmp);
  if (!tmp)
    return false;

  while (tmp)
  {
    auto *const succ = advance(tmp);
    if (succ && succ->GetMaxDrop() == 0)
      tmp = succ;
    else
      break;
  }

  for (; tmp; tmp = advance(tmp))
  {
    auto *const input = tmp->GetEditable();
    if (input)
    {
      SetActiveCell(input, false);
      if (direction >= 0)
        GetActiveCell()->CaretToStart();
      else
        GetActiveCell()->CaretToEnd();
      RequestRedraw();
      return true;
    }
  }

  return false;
}

/////////////////////////////////////////////////////////////
// methods related to evaluation queue
//
void Worksheet::AddDocumentToEvaluationQueue()
{
  FollowEvaluation(true);
  for (auto &tmp : OnList(GetTree()))
    AddToEvaluationQueue(&tmp);

  SetHCaret(m_last);
}

void Worksheet::AddToEvaluationQueue(GroupCell *cell)
{
  if (cell->GetGroupType() == GC_TYPE_CODE)
  {
    // Gray out the output of the cell in order to mark it as "not current".
    if (cell->GetEditable())
    {
      cell->GetEditable()->ContainsChanges(true);
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
  for (auto &tmp : OnList(GetTree()))
  {
    AddToEvaluationQueue(&tmp);
    m_evaluationQueue.AddHiddenTreeToQueue(&tmp);
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
  GroupCell *start = {};
  if (HasCellsSelected())
    start = m_cellPointers.m_selectionStart->GetGroup();

  if (GetActiveCell())
    start = GetActiveCell()->GetGroup()->GetPrevious();

  if (!start)
    start = GetHCaret();

  if (start)
    start = start->GetNext();

  if (!start)
    return;

  AddSelectionToEvaluationQueue(start, m_last);
}

void Worksheet::AddSelectionToEvaluationQueue()
{
  AddSelectionToEvaluationQueue(m_cellPointers.m_selectionStart.CastAs<GroupCell*>(),
                                m_cellPointers.m_selectionEnd.CastAs<GroupCell*>());
}

void Worksheet::AddSelectionToEvaluationQueue(GroupCell *start, GroupCell *end)
{
  FollowEvaluation(true);
  if (!start || !end)
    return;
  wxASSERT(start->GetType() == MC_TYPE_GROUP);
  for (auto &tmp : OnList(start))
  {
    AddToEvaluationQueue(&tmp);
    if (&tmp == end)
      break;
  }
  SetHCaret(end);
}

void Worksheet::AddDocumentTillHereToEvaluationQueue()
{
  FollowEvaluation(true);
  GroupCell *stop = m_hCaretActive ? m_hCaretPosition : nullptr;
  if (!stop) {
    if (!GetActiveCell())
      return;
    stop = GetActiveCell()->GetGroup();
    if (stop->GetPrevious())
      stop = stop->GetPrevious();
  }

  if (!stop)
    return;

  for (auto &tmp : OnList(GetTree()))
  {
    AddToEvaluationQueue(&tmp);
    if (&tmp == stop)
      break;
  }
}

void Worksheet::AddCellToEvaluationQueue(GroupCell *gc)
{
  AddToEvaluationQueue(gc);
  SetHCaret(gc);
}

//////// end of EvaluationQueue related stuff ////////////////
void Worksheet::ScrolledAwayFromEvaluation(bool ScrolledAway)
{
  if (ScrolledAway != m_scrolledAwayFromEvaluation)
  {
    m_scrolledAwayFromEvaluation = ScrolledAway;
    if (FollowEvaluation() && ScrolledAway)
    {
      FollowEvaluation(false);
      if (m_mainToolBar && GetActiveCell())
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

  if (!cell)
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
  long undoLimit = m_configuration->UndoLimit();

  if (undoLimit == 0)
    return;

  while ((long) treeUndoActions.size() > undoLimit)
    TreeUndo_DiscardAction(&treeUndoActions);
}

bool Worksheet::CanTreeUndo() const
{
  if (treeUndoActions.empty())
    return false;
  else
  {
    // If the next undo action will delete cells we have to look if we are allowed
    // to do this.
    if (treeUndoActions.front().m_newCellsEnd)
      return CanDeleteRegion(
              treeUndoActions.front().m_start,
              treeUndoActions.front().m_newCellsEnd
      );
    else return true;
  }
}

bool Worksheet::CanTreeRedo() const
{
  if (treeRedoActions.empty())
  {
    return false;
  }
  else
  {
    // If the next redo action will delete cells we have to look if we are allowed
    // to do this.
    if (treeRedoActions.front().m_newCellsEnd)
      return CanDeleteRegion(
              treeRedoActions.front().m_start,
              treeRedoActions.front().m_newCellsEnd
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

bool Worksheet::CanMergeSelection() const
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

bool Worksheet::TreeUndoCellDeletion(UndoActions *sourcelist, UndoActions *undoForThisOperation)
{
  TreeUndoAction &action = sourcelist->front();
  GroupCell *newCursorPos = action.m_oldCells.get();
  if (newCursorPos)
    newCursorPos = newCursorPos->last();

  InsertGroupCells(std::move(action.m_oldCells), action.m_start, undoForThisOperation);
  SetHCaret(newCursorPos);
  return true;
}

bool Worksheet::TreeUndoCellAddition(UndoActions *sourcelist, UndoActions *undoForThisOperation)
{
  const TreeUndoAction &action = sourcelist->front();
  wxASSERT_MSG(action.m_start,
               _("Bug: Got a request to delete the cell above the beginning of the worksheet."));

  // We make the cell we want to end the deletion with visible.
  if (action.m_newCellsEnd->RevealHidden())
    FoldOccurred();

  wxASSERT_MSG(CanDeleteRegion(action.m_start, action.m_newCellsEnd),
               _("Got a request to undo an action that involves a delete which isn't possible at this moment."));

  // Set the cursor to a sane position.
  if (action.m_newCellsEnd->GetNext())
    SetHCaret(action.m_newCellsEnd->GetNext());
  else
    SetHCaret(action.m_start->GetPrevious());

  // Actually delete the cells we want to remove.
  DeleteRegion(action.m_start, action.m_newCellsEnd, undoForThisOperation);
  return true;
}

bool Worksheet::TreeUndoTextChange(UndoActions *sourcelist, UndoActions *undoForThisOperation)
{
  const TreeUndoAction &action = sourcelist->front();

  wxASSERT_MSG(action.m_start,
               _("Bug: Got a request to change the contents of the cell above the beginning of the worksheet."));


  if (!GetTree()->Contains(action.m_start))
  {
    wxASSERT_MSG(GetTree()->Contains(action.m_start), _("Bug: Undo request for cell outside worksheet."));
    return false;
  }

  if (action.m_start)
  {

    // If this action actually does do nothing - we have not done anything
    // and want to make another attempt on undoing things.
    if (
      (action.m_oldText == action.m_start->GetEditable()->GetValue()) ||
      (action.m_oldText + wxT(";") == action.m_start->GetEditable()->GetValue())
      )
    {
      sourcelist->pop_front();
      return TreeUndo(sourcelist, undoForThisOperation);
    }

    // Document the old state of this cell so the next action can be undone.
    undoForThisOperation->emplace_front(action.m_start, action.m_start->GetEditable()->GetValue());

    // Revert the old cell state
    action.m_start->GetEditable()->SetValue(action.m_oldText);

    // Make sure that the cell we have to work on is in the visible part of the tree.
    if (action.m_start->RevealHidden())
      FoldOccurred();

    SetHCaret(action.m_start);

    Recalculate();
    RequestRedraw();

    wxASSERT_MSG(!action.m_newCellsEnd,
                 _("Bug: Got a request to first change the contents of a cell and to then undelete it."));
    wxASSERT_MSG(!action.m_oldCells, _("Bug: Undo action with both cell contents change and cell addition."));
    return true;
  }
  return false;
}

bool Worksheet::TreeUndo(UndoActions *sourcelist, UndoActions *undoForThisOperation)
{
  if (sourcelist->empty())
    return false;

  SetSaved(false);

  // Seems like saving the current value of the currently active cell
  // in the tree undo buffer makes the behavior of TreeUndo feel
  // more predictable to the user.
  if (GetActiveCell())
    TreeUndo_CellLeft();

  if(sourcelist->empty())
    return false;

  const TreeUndoAction &action = sourcelist->front();

  if (action.m_start)
  {
    // Make sure that the cell we work on is in the visible part of the tree.
    if (action.m_start->RevealHidden())
      FoldOccurred();
  }

  bool actionContinues;
  do{
    const TreeUndoAction &actn = sourcelist->front();
    if (actn.m_newCellsEnd)
      TreeUndoCellAddition(sourcelist, undoForThisOperation);
    else
    {
      if (actn.m_oldCells)
        TreeUndoCellDeletion(sourcelist, undoForThisOperation);
      else
        TreeUndoTextChange(sourcelist, undoForThisOperation);
    }
    TreeUndo_AppendAction(undoForThisOperation);
    sourcelist->pop_front();
    actionContinues = !sourcelist->empty() && sourcelist->front().m_partOfAtomicAction;
  } while (actionContinues);
  if (!undoForThisOperation->empty())
    undoForThisOperation->front().m_partOfAtomicAction = false;
  Recalculate();
  RequestRedraw();
  return true;
}

/*! Mark an editor cell as the active one

 */
void Worksheet::SetActiveCell(EditorCell *cell, bool callRefresh)
{
  if (GetActiveCell() == cell)
    return;

  if (GetActiveCell())
    TreeUndo_CellLeft();

  if (m_mainToolBar)
  {
    if (!cell)
      m_mainToolBar -> UnsetCellStyle();
    else
      m_mainToolBar -> SetCellStyle(cell->GetGroup()->GetGroupType());
  }

  bool scrollneeded = GetActiveCell() && GetActiveCell() != cell;

  if (cell)
  {
    m_cellPointers.m_selectionStart = nullptr;
    m_cellPointers.m_selectionEnd = nullptr;
    cell->ActivateCursor();
    if (!m_redrawRequested) m_caretTimer.Stop();
  }
  else if (GetActiveCell())
    GetActiveCell()->DeactivateCursor();

  TreeUndo_CellEntered();

  if (cell)
  {
    m_blinkDisplayCaret = true;

    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.StartOnce(blinktime);
    m_hCaretActive = false; // we have activated a cell .. disable caret
    m_hCaretPosition = NULL;
  }

  if (callRefresh) // = true default
    RequestRedraw();

  if (cell && !m_configuration->ShowCodeCells() && GetActiveCell()
      && GetActiveCell()->GetType() == MC_TYPE_INPUT)
  {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }
  if (scrollneeded && cell)
    ScrollToCaret();
}


void Worksheet::SetSelection(Cell *start, Cell *end)
{
  if (m_cellPointers.m_selectionStart != start || m_cellPointers.m_selectionEnd != end)
    RequestRedraw();
  m_cellPointers.m_selectionStart = start;
  m_cellPointers.m_selectionEnd = end;

  if (!m_cellPointers.m_selectionStart)
  {
    m_hCaretPositionStart = NULL;
    m_hCaretPositionEnd = NULL;
  }

  if (m_mainToolBar)
  {
    if (!start && !end)
    {
      if (!GetActiveCell())
        m_mainToolBar->UnsetCellStyle();
    }
    else
    {
      if (start != end && !GetActiveCell())
        m_mainToolBar -> UnsetCellStyle();
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
  if (GetActiveCell())
  {
    GetActiveCell()->CutToClipboard();
    GroupCell *group = GetActiveCell()->GetGroup();
    Recalculate(group);
    RequestRedraw();
    return true;
  }
  else if (m_cellPointers.m_selectionStart && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
  {
    if (CopyCells())
    {
      GroupCell *group = NULL;
      if(GetActiveCell())
        group = GetActiveCell()->GetGroup();
      if(group && (group->GetPrevious()))
        group = group->GetPrevious();
      if(group)
        Recalculate(group);
      else
        Recalculate();
      DeleteSelection();
      return true;
    }
    else
      return false;
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
  wxClipboard *clip = wxClipboard::Get();
  if ((wxTheClipboard->IsSupported(wxDF_TEXT))
    || (wxTheClipboard->IsSupported(wxDF_UNICODETEXT))
    || (wxTheClipboard->IsSupported(m_wxmFormat)))
  {
    wxString inputs;

    {
      // Opening assert dialogues in this context might cause gtk to end up in
      // an endless wait before the dialogue's buttons can be displayed.
      SuppressErrorDialogs suppressor;

      if (wxTheClipboard->IsSupported(m_wxmFormat))
      {
        wxmDataObject data;
        wxTheClipboard->GetData(data);
        inputs = wxString::FromUTF8((char *) data.GetData(), data.GetDataSize());
        if(!inputs.StartsWith(wxT("/* [wxMaxima: ")))
          wxLogMessage(_(".wxm clipboard data with unusual header"));
        else
          wxLogMessage(_("Read .wxm data from clipboard"));
      }
      else
      {
        wxTextDataObject data;
        wxTheClipboard->GetData(data);
        inputs = data.GetText();
      }
    }

    if (inputs.StartsWith(wxT("/* [wxMaxima: ")))
    {

      // Convert the text from the clipboard into an array of lines
      wxStringTokenizer lines(inputs, wxT("\n"), wxTOKEN_RET_EMPTY);
      wxArrayString lines_array;
      while (lines.HasMoreTokens())
        lines_array.Add(lines.GetNextToken());

      // Load the array like we would do with a .wxm file
      auto contents = Format::TreeFromWXM(lines_array, &m_configuration);

      // Add the result of the last operation to the worksheet.
      if (contents)
      {
        // ! Tell the rest of this function that we have found cells
        cells = true;

        // Search for the last cell we want to paste
        GroupCell *end = contents->last();

        // Now paste the cells
        if (!GetTree())
        {
          // Empty work sheet => We paste cells as the new cells
          m_tree = std::move(contents);
          m_last = end;
        }
        else
        {
          bool hasHSelection = m_cellPointers.m_selectionStart && (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP);
          auto *target = GetActiveCell() ? GetActiveCell()->GetGroup() : nullptr;
          if (!target)
            target = GetHCaret();
          if (hasHSelection)
          {
            DeleteSelection();
            TreeUndo_AppendAction();
          }
          InsertGroupCells(std::move(contents), target);
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
    GroupCell *group = GetActiveCell()->GetGroup();

    if (group)
    {
      wxBitmapDataObject bitmap;
      wxTheClipboard->GetData(bitmap);
      auto ic = std::make_unique<ImgCell>(group, &m_configuration, bitmap.GetBitmap());
      group->AppendOutput(std::move(ic));
      Recalculate(group);
    }
  }

  // Clipboard does not have the cell structure.
  if (!cells)
  {
    if (GetActiveCell())
    {
      GetActiveCell()->PasteFromClipboard();
      GetActiveCell()->ResetSize();
      GetActiveCell()->GetGroup()->ResetSize();
      Recalculate(GetActiveCell()->GetGroup());
      RequestRedraw();
    }
    else
    {
      if (m_hCaretActive && (wxTheClipboard->IsSupported(wxDF_TEXT)
        || wxTheClipboard->IsSupported(wxDF_UNICODETEXT)))
      {
        wxTextDataObject obj;
        wxTheClipboard->GetData(obj);
        wxString txt = obj.GetText();

        OpenHCaret(txt);
        RequestRedraw();
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
  if (!GetActiveCell() && GetTree())
  {
    SetSelection(GetTree(), m_last);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    m_hCaretActive = false;
  }
  else if (GetActiveCell())
  {
    if (!GetActiveCell()->AllSelected())
      GetActiveCell()->SelectAll();
    else
    {
      SetActiveCell(NULL);
      SetSelection(GetTree(), m_last);
      m_clickType = CLICK_TYPE_GROUP_SELECTION;
      m_hCaretActive = false;
    }
  }
  ScrolledAwayFromEvaluation();
  RequestRedraw();
}

void Worksheet::DivideCell()
{
  if (!GetActiveCell())
    return;

  GroupCell *parent = GetActiveCell()->GetGroup();
  if (parent->GetEditable() != GetActiveCell())
    return;

  GroupType gctype = parent->GetGroupType();
  if (gctype == GC_TYPE_IMAGE)
    return;

  if (GetActiveCell()->CaretAtStart() || GetActiveCell()->CaretAtEnd())
    return;

  parent->RemoveOutput();
  wxString newcellstring = GetActiveCell()->DivideAtCaret();
  parent->InputHeightChanged();

  SetHCaret(parent);
  OpenHCaret(newcellstring, gctype);
  if (GetActiveCell())
    GetActiveCell()->CaretToStart();
  ScrolledAwayFromEvaluation();
  Recalculate(parent);
}

void Worksheet::MergeCells()
{
  if(!m_cellPointers.m_selectionStart)
    return;

  wxString newcell;

  for (auto &tmp : OnList(m_cellPointers.m_selectionStart.CastAs<GroupCell*>()))
  {
    if (newcell.Length() > 0)
      newcell += wxT("\n");
    newcell += tmp.GetEditable()->GetValue();

    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
  }

  GroupCell *selStart = m_cellPointers.m_selectionStart.CastAs<GroupCell*>();
  EditorCell *editor = selStart->GetEditable();
  editor->SetValue(newcell);

  m_cellPointers.m_selectionStart = selStart->GetNext();
  DeleteSelection();
  editor->GetGroup()->ResetInputLabel();
  editor->GetGroup()->RemoveOutput();
  editor->ResetSize();
  editor->GetGroup()->ResetSize();
  Recalculate(editor->GetGroup());
  SetActiveCell(editor, true);
  ScrolledAwayFromEvaluation();
}

void Worksheet::OnSetFocus(wxFocusEvent &event)
{
  return;
  m_hasFocus = true;
  // We want the cursor to blink and to start doing so soon
  m_caretTimer.StartOnce(1);
  if (GetActiveCell())
    GetActiveCell()->SetFocus(true);

  event.Skip();
}

void Worksheet::OnKillFocus(wxFocusEvent &event)
{
  return;
  m_hasFocus = false;
  if (GetActiveCell())
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
        wxString data = GetString();
        wxLogMessage(wxString::Format(_("Middle-click clipboard data: %s"),data.c_str()));
        wxTheClipboard->SetData(new wxTextDataObject(data));
        wxTheClipboard->Close();
      }
    }
    wxTheClipboard->UsePrimarySelection(false);
  }
}

//! Is this cell selected?
bool Worksheet::IsSelected(CellType type)
{
  return
    m_cellPointers.m_selectionStart
    && m_cellPointers.m_selectionStart->GetType() == type
    && ((type != MC_TYPE_IMAGE && type != MC_TYPE_SLIDE) ||
        m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd);
}

//! Starts playing the animation of a cell generated with the with_slider_* commands
void Worksheet::Animate(bool run)
{
  if (CanAnimate())
  {
    AnimationCell *animation = dynamic_cast<AnimationCell *>(GetSelectionStart());
    animation->AnimationRunning(run);
  }
}

bool Worksheet::IsSelectionInWorkingGroup()
{
  return
    m_cellPointers.m_selectionStart
    && GetWorkingGroup()
    && m_cellPointers.m_selectionStart->GetGroup() == GetWorkingGroup();
}

GroupCell *Worksheet::GetHCaret()
{
  if (m_hCaretActive)
    return m_hCaretPosition;

  if (GetActiveCell())
    return GetActiveCell()->GetGroup();

  if (m_cellPointers.m_selectionStart)
    return m_cellPointers.m_selectionStart->GetGroup();

  if (MouseSelectionStart())
    return MouseSelectionStart()->GetGroup();

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
  ClearSelection();
  if (m_mainToolBar)
  {
    m_mainToolBar->UnsetCellStyle();
  }

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  SetActiveCell(NULL, false);
  if (where)
    wxASSERT_MSG(
      where->GetType() == MC_TYPE_GROUP,
      _("Bug: Trying to move the horizontally-drawn cursor to a place inside a GroupCell."));

  m_hCaretActive = true;
  if (m_hCaretPosition != where)
  {
    m_hCaretPosition = where;

    RequestRedraw();
    if (where)
      ScheduleScrollToCell(where, false);

    // Tell the cursor to blink, but to be visible right now.
    m_blinkDisplayCaret = true;
    m_hCaretBlinkVisible = true;

    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.Start(blinktime);
  }
}

void Worksheet::ShowHCaret()
{
  if (!m_hCaretPosition)
    SetHCaret(m_last);

  m_hCaretActive = true;
}

bool Worksheet::CanUndoInsideCell() const
{
  return GetActiveCell() && GetActiveCell()->CanUndo();
}

void Worksheet::UndoInsideCell()
{
  if (GetActiveCell())
  {
    GetActiveCell()->Undo();
    GetActiveCell()->GetGroup()->ResetSize();
    GetActiveCell()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

bool Worksheet::CanRedoInsideCell() const
{
  return GetActiveCell() && GetActiveCell()->CanRedo();
}

void Worksheet::RedoInsideCell()
{
  if (GetActiveCell())
  {
    GetActiveCell()->Redo();
    GetActiveCell()->GetGroup()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

void Worksheet::SaveValue()
{
  if (GetActiveCell())
    GetActiveCell()->SaveValue();
}

void Worksheet::RemoveAllOutput()
{
  // We don't want to remove all output if maxima is currently evaluating.
  if (GetWorkingGroup())
    return;

  if (HasCellsSelected())
  {
    // If the selection is in the output we want to remove the selection.
    if (m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP)
      ClearSelection();
  }

  SetActiveCell(NULL);

  RemoveAllOutput(GetTree());
  OutputChanged();

  Recalculate();
  RequestRedraw();
}

void Worksheet::RemoveAllOutput(GroupCell *cell)
{
  if (!cell)
    cell = GetTree();

  for (auto &tmp : OnList(cell))
  {
    // If this function actually does do something we
    // should enable the "save" button.
    OutputChanged();

    tmp.RemoveOutput();

    GroupCell *sub = tmp.GetHiddenTree();
    if (sub)
      RemoveAllOutput(sub);
  }
  m_configuration->AdjustWorksheetSize(true);
  Recalculate();
}

void Worksheet::OnMouseMiddleUp(wxMouseEvent &event)
{
  if (m_inPopupMenu)
    // Pasting with an active popup menu makes no sense,
    // and additionally the ReleaseMouse() will fail an assertion if the
    // PopupMenu event loop is active.
    return;
  m_cellPointers.ResetSearchStart();

  wxTheClipboard->UsePrimarySelection(true);
  if (wxTheClipboard->IsUsingPrimarySelection())
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
    Recalculate(active->GetGroup());
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

void Worksheet::OnThumbtrack(wxScrollWinEvent &ev)
{
  // We don't want to start the autosave while the user is scrolling through
  // the document since this will shortly halt the scroll
  m_keyboardInactiveTimer.StartOnce(10000);
  if (CanAnimate())
   {
     //! Step the slide show.
     auto *tmp = m_cellPointers.m_selectionStart.CastAs<AnimationCell*>();
     tmp->AnimationRunning(false);

     if (ev.GetEventType() == wxEVT_SCROLLWIN_LINEUP)
       tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() + 1) % tmp->Length());
     else
       tmp->SetDisplayedIndex((tmp->GetDisplayedIndex() - 1) % tmp->Length());

     wxRect rect = m_cellPointers.m_selectionStart->GetRect();
     RequestRedraw(rect);
   }
  else
  {
    ScrolledAwayFromEvaluation();

    if (ev.GetOrientation() == wxHORIZONTAL)
      m_newxPosition = ev.GetPosition();
    else
      m_newyPosition = ev.GetPosition();

    if (m_dontSkipScrollEvent)
    {
      RequestRedraw();
      ev.Skip();
    }
  }
}

wxString Worksheet::GetInputAboveCaret()
{
  if (!m_hCaretActive || !m_hCaretPosition)
    return {};

  EditorCell *editor = m_hCaretPosition->GetEditable();
  return editor ? editor->ToString() : wxString{};
}

wxString Worksheet::GetOutputAboveCaret()
{
  if (!m_hCaretActive || !m_hCaretPosition)
    return {};

  auto const sel = m_hCaretPosition->GetCellsInOutput();
  m_cellPointers.m_selectionStart = sel.first;
  m_cellPointers.m_selectionEnd = sel.last;

  wxString output = GetString();

  ClearSelection();
  RequestRedraw();

  return output;
}

bool Worksheet::FindIncremental(const wxString &str, bool down, bool ignoreCase)
{
  if (SearchStart())
  {
    SetActiveCell(SearchStart());
    SearchStart()->CaretToPosition(IndexSearchStartedAt());
  }

  return (!str.empty()) ? FindNext(str, down, ignoreCase, false) : true;
}

bool Worksheet::FindNext(const wxString &str, bool down, bool ignoreCase, bool warn)
{
  if (!GetTree())
    return false;

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
  GroupCell *pos = GetTree();
  for (; pos; pos = pos->GetNext())
  {
    wxRect rect = pos->GetRect();
    if (rect.GetBottom() > topleft.y)
      break;
  }

  if (!pos)
    pos = down ? GetTree() : m_last;

  // If a cursor is active we start the search there instead
  if (GetActiveCell())
    pos = GetActiveCell()->GetGroup();
  else if (m_hCaretActive)
  {
    pos = (down && m_hCaretPosition && m_hCaretPosition->GetNext())
            ? m_hCaretPosition->GetNext()
            : m_hCaretPosition;
  }

  // If we still don't have a place to start searching we have definitively tried to
  // search in any empty worksheet and know we won't get any result.
  if (!pos)
    return false;

  pos->GetEditable()->SearchStartedHere(pos->GetEditable()->GetCaretPosition());

  // Remember where to go if we need to wrap the search.
  GroupCell *start = pos;

  bool wrappedSearch = false;
  while (pos != start || !wrappedSearch)
  {
    EditorCell *editor = pos->GetEditable();

    if (editor)
    {
      bool found = editor->FindNext(str, down, ignoreCase);

      if (found)
      {
        int strt, end;
        editor->GetSelection(&strt, &end);
        SetActiveCell(editor);
        editor->SetSelection(strt, end);
        ScrollToCaret();
        UpdateTableOfContents();
        RequestRedraw();
        if ((wrappedSearch) && warn)
        {
          LoggingMessageDialog dialog(m_findDialog,
                                 _("Wrapped search"),
                                 wxEmptyString, wxCENTER | wxOK);
          dialog.ShowModal();
        }
        return true;
      }
    }

    if (down)
    {
      pos = pos->GetNext();
      if (!pos)
      {
        wrappedSearch = true;
        pos = GetTree();
      }
    }
    else
    {
      pos = pos->GetPrevious();
      if (!pos)
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

    view_y *= m_scrollUnit;
    return ((y >= view_y) && (y <= view_y + height));
  }
  else
  {
    if (GetActiveCell())
    {
      wxPoint point = GetActiveCell()->PositionToPoint();
      if (point.y < 1)
      {
        RecalculateForce();
        point = GetActiveCell()->PositionToPoint();
      }
      return PointVisibleIs(point);
    }
    else
      return false;
  }
}

bool Worksheet::ScrollToCaretIfNeeded()
{
  if(!m_scrollToCaret)
    return false;

  m_scrollToCaret = false;

  RecalculateIfNeeded();
  if (m_hCaretActive)
  {
    ScheduleScrollToCell(m_hCaretPosition, false);
  }
  else
  {
    if (GetActiveCell())
    {
      wxPoint point = GetActiveCell()->PositionToPoint();

      // Carets in output cells [maxima questions] get assigned a position
      // only when they are drawn --- or if we update the position manually.
      if(point.y<0)
      {
        GetActiveCell()->GetGroup()->UpdateOutputPositions();
        wxASSERT(GetActiveCell()->GetGroup()->GetCurrentPoint().x >= 0);
        wxASSERT(GetActiveCell()->GetGroup()->GetCurrentPoint().y >= 0);
        point = GetActiveCell()->PositionToPoint();
      }
      wxASSERT(point.y >= 0);
      ShowPoint(point);
    }
  }
  return true;
}

void Worksheet::Replace(const wxString &oldString, const wxString &newString, bool ignoreCase)
{
  if (!GetActiveCell())
    return;

  if (GetActiveCell()->ReplaceSelection(oldString, newString, false, ignoreCase))
  {
    SetSaved(false);
    GroupCell *group = GetActiveCell()->GetGroup();
    group->ResetInputLabel();
    group->ResetSize();
    GetActiveCell()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
  GetActiveCell()->SearchStartedHere();
}

int Worksheet::ReplaceAll(const wxString &oldString, const wxString &newString, bool ignoreCase)
{
  m_cellPointers.ResetSearchStart();

  if (!GetTree())
    return 0;

  int count = 0;
  for (auto &tmp : OnList(GetTree()))
  {
    EditorCell *editor = tmp.GetEditable();
    if (editor)
    {
      SetActiveCell(editor);
      int replaced = editor->ReplaceAll(oldString, newString, ignoreCase);
      if (replaced > 0)
      {
        count += replaced;
        tmp.ResetInputLabel();
        tmp.ResetSize();
      }
    }
  }

  if (count > 0)
  {
    SetSaved(false);
    Recalculate();
    RequestRedraw();
  }

  return count;
}

bool Worksheet::Autocomplete(AutoComplete::autoCompletionType type)
{
  EditorCell *editor = GetActiveCell();

  if (!editor)
    return false;

  wxString partial;
  if (type != AutoComplete::esccommand)
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

      for (wxString::const_iterator it = frontOfSelection.begin();
           it != frontOfSelection.end(); ++it)
      {
        wxChar ch = *it;
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

    // If we don't have a unit to complete we perhaps want to autocomplete a package name
    // or the name of a demo file
    if(!inEzUnit)
    {
      wxString currentCommand = editor->GetCurrentCommand();
      if ((currentCommand == wxT("load")) ||
          (currentCommand == wxT("batchload")) ||
          (currentCommand == wxT("batch")))
      {
        type = AutoComplete::loadfile;
        if (partial.empty())
          partial = wxString("\"");
      }

      if (currentCommand == wxT("demo"))
      {
        type = AutoComplete::demofile;
        if (partial.empty())
          partial = wxString("\"");
      }

      if ((type == AutoComplete::demofile) || (type == AutoComplete::loadfile))
      {
        if (partial[0] != wxT('\"'))
        {
          partial = wxT("\"") + partial;
          // If the editor auto-adds a closing quote this causes auto-completion to fail
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true, false, true);
        }
        if ((partial.EndsWith("\"") && (!(partial.EndsWith("\\\"")))))
        {
          partial = partial.Left(partial.Length() - 1);
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true);
        }
      }

      if ((type == AutoComplete::command) && (partial[0] == wxT('\"')))
        type = AutoComplete::generalfile;

      if (type == AutoComplete::demofile)
        m_autocomplete.UpdateDemoFiles(partial,
                                       wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if (type == AutoComplete::loadfile)
        m_autocomplete.UpdateLoadFiles(partial,
                                       wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if (type == AutoComplete::generalfile)
        m_autocomplete.UpdateGeneralFiles(partial,
                                          wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));
    }
  }

  if (type == AutoComplete::command)
  {
    // Update the list of words that might not be defined as maxima function or variable
    // but that still appear on the workSheet.
    m_autocomplete.ClearWorksheetWords();
    for (auto &tmp : OnList(GetTree()))
    {
      // Don't collect the current word as possible autocompletion.
      if (&tmp != GetActiveCell()->GetGroup())
      {
        // Only collect words from Code Cells.
        if ((tmp.GetGroupType() == GC_TYPE_CODE) && tmp.GetEditable())
          m_autocomplete.AddWorksheetWords(tmp.GetEditable()->GetWordList());
      }
      else
      {
        if ((tmp.GetGroupType() == GC_TYPE_CODE) && tmp.GetEditable())
        {
          auto const &wordList = tmp.GetEditable()->GetWordList();

          // The current unfinished word is no valid autocompletion, if there is
          // such a thing.
          if (!partial.empty())
          {
            // Don't remove the current word from autocompletion if it never has been
            // added (which happens if autocompletion is called when the cursor is
            // directly followed by the next command without a space or similar inbetween)
            auto partialAt = std::find(wordList.begin(), wordList.end(), partial);
            m_autocomplete.AddWorksheetWords(wordList.begin(), partialAt);
            if (partialAt != wordList.end())
              m_autocomplete.AddWorksheetWords(std::next(partialAt), wordList.end());
          }
          else
            m_autocomplete.AddWorksheetWords(wordList);
        }
      }
    }
  }

  m_completions = m_autocomplete.CompleteSymbol(partial, type);
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

    if (type != AutoComplete::tmplte || !editor->FindNextTemplate())
      editor->CaretToPosition(start + m_completions[0].Length());

    editor->ResetSize();
    editor->GetGroup()->ResetSize();
    Recalculate(editor->GetGroup());

    RequestRedraw();
  }

    /// If there are more than one completions, popup a menu
  else
  {

    // Find the position for the popup menu
    RecalculateIfNeeded();
    wxPoint pos = editor->PositionToPoint();
    // There might be no current point yet in this EditorCell.
    if((pos.x < 0) || (pos.y < 0))
      pos = editor->GetGroup()->GetCurrentPoint();
    wxASSERT((pos.x>=0) && (pos.y >=0));
    CalcScrolledPosition(pos.x, pos.y, &pos.x, &pos.y);
    // The popup menu appears half a character too high.
    pos.y += m_configuration->Scale_Px(m_configuration->GetFontSize(TS_TEXT)).Get();
    wxASSERT(!m_autocompletePopup);
    m_autocompletePopup = new AutocompletePopup(this,editor,&m_autocomplete,type,&m_autocompletePopup);

    // If necessary: Scroll right or down so that the pop-up is visible as a whole.
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    int width;
    int height;
    GetClientSize(&width, &height);
    if((pos.x >= 0) && (pos.y >= 0))
       m_autocompletePopup -> SetPosition(pos);
    m_autocompletePopup -> Create(this);
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
  if (!GetActiveCell())
    return;

  EditorCell *editor = GetActiveCell();
  int caret = editor->GetCaretPosition();

  if (!editor->GetSelectionString().empty())
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
  Recalculate(editor->GetGroup());

  RequestRedraw();
}

bool Worksheet::SectioningMoveIn()
{
  if(!m_tableOfContents->RightClickedOn()->SectioningCanMoveIn())
    return false;

  std::list<GroupCell *> sections;
  GroupType type = m_tableOfContents->RightClickedOn()->GetGroupType();
  for (auto &tmp : OnList(m_tableOfContents->RightClickedOn()))
  {
    if((&tmp != m_tableOfContents->RightClickedOn()) && (!tmp.IsLesserGCType(type)))
      break;
    if(tmp.IsHeading())
      sections.push_back(&tmp);
  }
  
  for (auto &tmp : sections)
  {
    switch (tmp->GetGroupType())
    {
    case GC_TYPE_HEADING6:
      break;
    case GC_TYPE_HEADING5:
      SetCellStyle(tmp,GC_TYPE_HEADING6);
      break;
    case GC_TYPE_SUBSUBSECTION:
      SetCellStyle(tmp,GC_TYPE_HEADING5);
      break;
    case GC_TYPE_SUBSECTION:
      SetCellStyle(tmp,GC_TYPE_SUBSUBSECTION);
      break;
    case GC_TYPE_SECTION:
      SetCellStyle(tmp,GC_TYPE_SUBSECTION);
      break;
    case GC_TYPE_TITLE:
      SetCellStyle(tmp,GC_TYPE_SECTION);
      break;
    default:
      wxASSERT_MSG(false,
                   _("Bug: Encountered a heading I don't know how to move in one sectioning unit"));
      return false;
    }
  }
  return true;
}

bool Worksheet::SectioningMoveOut()
{
  if(!m_tableOfContents->RightClickedOn()->SectioningCanMoveOut())
    return false;

  GroupType type = m_tableOfContents->RightClickedOn()->GetGroupType();
  std::list<GroupCell *> sections;
  for (auto &tmp : OnList(m_tableOfContents->RightClickedOn()))
  {
    if((&tmp != m_tableOfContents->RightClickedOn()) && (!tmp.IsLesserGCType(type)))
      break;
    if(tmp.IsHeading())
      sections.push_back(&tmp);
  }
  for (auto &tmp: sections)
  {
    switch (tmp->GetGroupType())
    {
    case GC_TYPE_HEADING6:
      SetCellStyle(tmp,GC_TYPE_HEADING5);
      break;
    case GC_TYPE_HEADING5:
      SetCellStyle(tmp,GC_TYPE_SUBSUBSECTION);
      break;
    case GC_TYPE_SUBSUBSECTION:
      SetCellStyle(tmp,GC_TYPE_SUBSECTION);
      break;
    case GC_TYPE_SUBSECTION:
      SetCellStyle(tmp,GC_TYPE_SECTION);
      break;
    case GC_TYPE_SECTION:
      SetCellStyle(tmp,GC_TYPE_TITLE);
      break;
    case GC_TYPE_TITLE:
      wxASSERT_MSG(false,
                   _("Bug: Trying to move a title out by one sectioning unit"));        
      break;
    default:
      wxASSERT_MSG(false,
                   _("Bug: Encountered a heading I don't know how to move out one sectioning unit"));
      return false;
    }
  }
  return true;
}


void Worksheet::SetActiveCellText(const wxString &text)
{
  EditorCell *active = GetActiveCell();
  if (active)
  {
    GroupCell *parent = active->GetGroup();
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
      Recalculate(parent);
      RequestRedraw();
    }
  }
  else
    OpenHCaret(text);
}

bool Worksheet::InsertText(const wxString &text)
{
  CloseAutoCompletePopup();

  if (GetActiveCell())
  {
    if (GCContainsCurrentQuestion(GetActiveCell()->GetGroup()))
    {
      m_followEvaluation = true;
      OpenQuestionCaret(text);
    }
    else
    {
      GetActiveCell()->InsertText(text);
      Recalculate(GetActiveCell()->GetGroup());
      RequestRedraw(GetActiveCell()->GetGroup());
    }
  }
  else
    OpenHCaret(text);
  return true;
}

void Worksheet::OpenNextOrCreateCell()
{
  if (m_hCaretPosition && m_hCaretPosition->GetNext())
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
  if (!GetWorkingGroup())
    return;

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
      Recalculate();
    }
    SetSelection(GetWorkingGroup());
    SetHCaret(GetWorkingGroup());
    ScheduleScrollToCell(GetWorkingGroup(), false);
  }
}

Worksheet::MathMLDataObject::MathMLDataObject() : wxCustomDataObject(m_mathmlFormat)
{
}

Worksheet::MathMLDataObject::MathMLDataObject(const wxString &data) :
    wxCustomDataObject(m_mathmlFormat),
    m_databuf(data.utf8_str())

{
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::wxmDataObject::wxmDataObject() : wxCustomDataObject(m_wxmFormat)
{
}

Worksheet::wxmDataObject::wxmDataObject(wxString data) : wxCustomDataObject(m_wxmFormat)
{
  m_databuf = data.utf8_str();
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::MathMLDataObject2::MathMLDataObject2() : wxCustomDataObject(m_mathmlFormat2)
{
}

Worksheet::MathMLDataObject2::MathMLDataObject2(wxString data) :
    wxCustomDataObject(m_mathmlFormat2)
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

wxString Worksheet::RTFStart() const
{
  // The beginning of the RTF document
  wxString document = wxT("{\\rtf1\\ansi\\deff0\n\n");

  // The font table
  document += wxT("{\\fonttbl{\\f0\\froman Times;}}\n\n");

  // Define all colors we want to use
  document += wxT("{\\colortbl;\n");
  for (int i = 1; i < NUMBEROFSTYLES; i++)
  {
    wxColor color = wxColor(m_configuration->GetColor((TextStyle) i));
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

wxString Worksheet::RTFEnd() const
{
  // Close the document

  return wxT("}\n}");
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
  if (!childCount)
    return wxACC_FAIL;

  *childCount = 0;
  for (auto &cell : OnList(m_worksheet->GetTree()))
    (*childCount)++;

  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetChild (int childId, wxAccessible **child)
{
  if (!child)
    return wxACC_FAIL;

  if (childId == 0)
  {
    *child = this;
    return wxACC_OK;
  }

  int childCount = 0;
  GroupCell *cell = m_worksheet->GetTree();
  while (cell && childCount < childId)
  {
    childCount++;
    cell = cell->GetNext();
  }

  *child = cell->GetAccessible();
  return child ? wxACC_OK : wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetDefaultAction (int childId, wxString *actionName)
{
  if (!actionName)
    return wxACC_FAIL;

  if (childId == 0)
  {
    *actionName = _("Type");
    return wxACC_OK;
  }

  wxAccessible *acc;
  GetChild(childId, &acc);
  return acc ? acc->GetDefaultAction(0, actionName) : wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetParent (wxAccessible ** parent)
{
  if (!parent)
    return wxACC_FAIL;

  *parent = m_worksheet->GetAccessible();
  return *parent ? wxACC_OK : wxACC_FAIL;
}


// wxAccStatus Worksheet::AccessibilityInfo::GetFocus (int *childId, wxAccessible **child)
// {
//   if(!m_worksheet->HasFocus())
//   {
//     if(childId != NULL)
//       *childId = 0;
//     if(child != NULL)
//       *child = NULL;
//     return wxACC_FALSE;
//   }
//   else
//   {
//     int id = 0;
//     Cell *cell = m_worksheet->GetTree();
//     while(cell != NULL)
//     {
//       id++;
//       if(cell->GetFocus(&id, &cell) == wxACC_OK)
//          {
//            if(childId != NULL)
//              *childId = id;
//            if(child != NULL)
//              *child = cell;
//            return wxACC_OK;
//          }
//       cell = cell->GetNext();
//     }

//     if(childId != NULL)
//       *childId = 0;
//     if(child != NULL)
//       *child = this;

//     return wxACC_OK;
//   }
// }

wxAccStatus Worksheet::AccessibilityInfo::GetLocation(wxRect &rect, int elementId)
{
  if (elementId == 0)
  {
    rect = wxRect(m_worksheet->GetPosition(), m_worksheet->GetPosition()+ m_worksheet->GetClientSize());
    return GetLocation(rect, 0);
  }

  wxAccessible *acc = NULL;
  GetChild(elementId, &acc);
  return acc ? acc->GetLocation(rect, 0) : wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::HitTest(const wxPoint &pt,
                                                  int *childId, wxAccessible **childObject)
{
  wxRect currentRect;
  GetLocation(currentRect, 0);
  if (!currentRect.Contains(pt))
  {
    if (childId)
      *childId = 0;
    if (childObject)
      *childObject = NULL;
    return wxACC_FALSE;
  }

  int id = 0;
  for (Cell *cell = m_worksheet->GetTree(); cell; )
  {
    id ++;
    cell = cell->GetNext();
    Cell *childCell = nullptr;
    if (cell && cell->HitTest(pt, childId, &childCell) == wxACC_OK)
    {
      if (childId)
        *childId = id;
      if (childObject)
        *childObject = cell->GetAccessible();
      return wxACC_OK;
    }
  }

  if (childId)
    *childId = 0;
  if (childObject)
    *childObject = this;
  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetDescription(int childId, wxString *description)
{
  if (!description)
    return wxACC_FAIL;

  if (childId == 0)
  {
    *description = _("The worksheet containing maxima's input and output");
    return wxACC_OK;
  }

  wxAccessible *child;
  if (GetChild (childId, &child) == wxACC_OK)
    return child->GetDescription(childId, description);

  *description = wxString{};
  return wxACC_FAIL;
}

#endif

// Define the static variable that contains the format info for placing MathMl
// on the clip board
wxDataFormat Worksheet::m_mathmlFormat;
wxDataFormat Worksheet::m_mathmlFormat2;
wxDataFormat Worksheet::m_rtfFormat;
wxDataFormat Worksheet::m_rtfFormat2;
wxDataFormat Worksheet::m_wxmFormat;

CellPointers *Cell::GetCellPointers() const
{ return &GetWorksheet()->GetCellPointers(); }
