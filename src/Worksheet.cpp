// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include "Worksheet.h"
#include "BTextCtrl.h"
#include "cells/AnimationCell.h"
#include "graphical_io/BitmapOut.h"
#include "cells/CellList.h"
#include "CompositeDataObject.h"
#include "graphical_io/EMFout.h"
#include "cells/ImgCell.h"
#include "MarkDown.h"
#include "dialogs/MaxSizeChooser.h"
#include "dialogs/ResolutionChooser.h"
#include "graphical_io/SVGout.h"
#include "Version.h"
#include "Compat.h"
#include "WorksheetContextMenu.h"
#include "WorksheetExport.h"
#include "WXMformat.h"
#include "levenshtein/levenshtein.h"
#include "wxMaxima.h"
#include "wxMaximaFrame.h"
#include "ArtProvider.h"
#include <algorithm>
#include <memory>
#include <vector>
#include <utility>
#include <stdlib.h>
#include <wx/caret.h>
#include <wx/clipbrd.h>
#include <wx/config.h>
#include <wx/dcbuffer.h>
#include <wx/dcgraph.h>
#include <wx/event.h>
#include <wx/fileconf.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/mstream.h>
#include <wx/region.h>
#include <wx/richtext/richtextbuffer.h>
#include <wx/settings.h>
#include <wx/stopwatch.h>
#include <wx/tokenzr.h>
#include <wx/tooltip.h>
#include <wx/txtstrm.h>
#include <wx/uri.h>
#include <wx/wfstream.h>
#include <wx/wupdlock.h>
#include <wx/xml/xml.h>
#include <wx/zipstrm.h>
#include <cmath>
#if wxCHECK_VERSION(3, 2, 0)
#include <wx/bmpbndl.h>
#endif

//! This class represents the worksheet shown in the middle of the wxMaxima
//! window.
Worksheet::Worksheet(wxWindow *parent, int id,
                     Configuration *config, wxPoint pos, wxSize size, bool reactToEvents)
: wxScrolled<wxWindow>(parent, id, pos, size,
                       wxVSCROLL | wxHSCROLL | wxWANTS_CHARS
#if defined __WXMSW__
                       | wxBORDER_SUNKEN
#endif
                       ),
  m_unsavedDocuments(wxS("unsaved")),
  m_cellPointers(this), m_dc(this), m_configuration(config),
  m_autocomplete(config),
  m_maximaManual(m_configuration) {
  m_autocompletePopup = NULL;
#ifdef __WXGTK__
  wxString gtk_input_method;
  if (wxGetEnv(wxS("GTK_IM_MODULE"), &gtk_input_method)) {
    if (gtk_input_method == wxS("xim")) {
      wxLogError(_("GTK_IM_MODULE is set to \"xim\". Expect the program to "
                   "hideously flicker and hotkeys to be broken, see for "
                   "example https://github.com/wxWidgets/wxWidgets/issues/18462."));
    }
  }
#endif
  SetMinClientSize(wxSize(100, 100));
  // This is somehow needed for wxAutoBufferedPaintDC
  SetBackgroundStyle(wxBG_STYLE_PAINT);
  GetTargetWindow()->SetBackgroundStyle(wxBG_STYLE_PAINT);

#if wxUSE_ACCESSIBILITY
  m_accessibilityInfo = NULL;
#endif
#if wxCHECK_VERSION(3, 1, 1)
  EnableTouchEvents(wxTOUCH_ZOOM_GESTURE);
#endif
  m_configuration->SetWorkSheet(this);
  m_configuration->ReadConfig();
  SetBackgroundColour(m_configuration->DefaultBackgroundColor());

  m_configuration->SetBackgroundBrush(*(wxTheBrushList->FindOrCreateBrush(
                                                                          m_configuration->DefaultBackgroundColor(), wxBRUSHSTYLE_SOLID)));
  m_autocompletePopup = NULL;
  m_wxmFormat = wxDataFormat(wxS("text/x-wxmaxima-batch"));
  m_mathmlFormat = wxDataFormat(wxS("MathML"));
  m_mathmlFormat2 = wxDataFormat(wxS("application/mathml-presentation+xml"));
  m_rtfFormat = wxDataFormat(wxS("application/rtf"));
  m_rtfFormat2 = wxDataFormat(wxS("text/rtf"));
  m_treeUndo.ForgetActiveCell();
  m_clickInGC = NULL;
  m_last = nullptr;
  m_timer.SetOwner(this, TIMER_ID);
  m_caretTimer.SetOwner(this, CARET_TIMER_ID);
  m_displayTimeoutTimer.SetOwner(this, DISPLAY_TIMEOUT_ID);

  SetSaved(false);
  AdjustSize();
  m_autocompleteTemplates = true;
  int blinktime = wxCaret::GetBlinkTime();
  if (blinktime < 200)
    blinktime = 200;
  m_caretTimer.Start(blinktime);
  DisableKeyboardScrolling();

  // hack to workaround problems in RtL locales,
  // https://bugzilla.redhat.com/show_bug.cgi?id=455863
  SetLayoutDirection(wxLayout_LeftToRight);

  // If the following option is missing a size change might cause the scrollbar
  // to be shown causing a size change causing a relayout causing the scrollbar
  // to disappear causing a size change... ...which might be an endless loop.
  ShowScrollbars(wxSHOW_SB_ALWAYS, wxSHOW_SB_ALWAYS);
  ClearDocument();
#if wxUSE_ACCESSIBILITY
  m_accessibilityInfo = new AccessibilityInfo(GetTargetWindow(), this);
  // Actually hand this accessible to the window, so screen readers use it
  // instead of the default wxWindowAccessible (which only exposes a generic
  // "panel" with the raw scrollbars and a null thumb). The window takes
  // ownership and deletes it on destruction.
  GetTargetWindow()->SetAccessible(m_accessibilityInfo);
#endif
#if wxCHECK_VERSION(3, 1, 1)
  //  Disabled, as it resets the zoom to 1:1 on right-click (GTK) or closes the
  //  right-click dialogue (wxMSW)
  //  Connect(wxEVT_GESTURE_ZOOM, wxZoomGestureEventHandler(Worksheet::OnZoom),
  //        NULL, this);
#endif
  Bind(SIDEBARKEYEVENT, &Worksheet::OnSidebarKey, this);
  Bind(wxEVT_ERASE_BACKGROUND, &Worksheet::EraseBackground, this);
  Bind(wxEVT_MENU, &Worksheet::OnComplete, this,
       EventIDs::popid_autocomplete_keyword1,
       EventIDs::popid_autocomplete_keyword1 + EventIDs::NumberOfAutocompleteKeywords - 1);
  Bind(wxEVT_SIZE, &Worksheet::OnSize, this);
  Bind(wxEVT_PAINT, &Worksheet::OnPaint, this);
  Bind(wxEVT_MOUSE_CAPTURE_LOST, &Worksheet::OnMouseCaptureLost, this);
  if(reactToEvents)
    {
      Bind(wxEVT_LEFT_UP, &Worksheet::OnMouseLeftUp, this);
      Bind(wxEVT_LEFT_DOWN, &Worksheet::OnMouseLeftDown, this);
      Bind(wxEVT_RIGHT_DOWN, &Worksheet::OnMouseRightDown, this);
      Bind(wxEVT_LEFT_DCLICK, &Worksheet::OnDoubleClick, this);
      Bind(wxEVT_MIDDLE_UP, &Worksheet::OnMouseMiddleUp, this);
      Bind(wxEVT_KEY_DOWN, &Worksheet::OnKeyDown, this);
      Bind(wxEVT_CHAR, &Worksheet::OnChar, this);
    }
  Bind(wxEVT_MOTION, &Worksheet::OnMouseMotion, this);
  Bind(wxEVT_ENTER_WINDOW, &Worksheet::OnMouseEnter, this);
  Bind(wxEVT_LEAVE_WINDOW, &Worksheet::OnMouseExit, this);
  Bind(wxEVT_MOUSEWHEEL, &Worksheet::OnMouseWheel, this);
  Bind(wxEVT_TIMER, &Worksheet::OnTimer, this);
  Bind(wxEVT_ERASE_BACKGROUND, &Worksheet::OnEraseBackground, this);
  Bind(wxEVT_KILL_FOCUS, &Worksheet::OnKillFocus, this);
  Bind(wxEVT_SET_FOCUS, &Worksheet::OnSetFocus, this);
  Bind(wxEVT_SCROLL_CHANGED, &Worksheet::OnScrollChanged, this);
  Bind(wxEVT_SCROLLWIN_LINEUP, &Worksheet::OnScrollEvent, this);
  Bind(wxEVT_SCROLLWIN_LINEDOWN, &Worksheet::OnScrollEvent, this);
  Bind(wxEVT_SCROLLWIN_PAGEUP, &Worksheet::OnScrollEvent, this);
  Bind(wxEVT_SCROLLWIN_PAGEDOWN, &Worksheet::OnScrollEvent, this);
  Bind(wxEVT_SCROLLWIN_THUMBRELEASE, &Worksheet::OnScrollEvent, this);
  Bind(wxEVT_SCROLLWIN_THUMBTRACK, &Worksheet::OnScrollEvent, this);
  CallAfter([this] {m_configuration->SetCanvasSize(GetClientSize());});
}

void Worksheet::OnSidebarKey(wxCommandEvent &event) {
  if (BTextCtrl::LastActive() == NULL) {
    SetFocus();
    // Send the char button to the active cell or a new cell on the worksheet
    if (GetActiveCell()) {
      GetActiveCell()->InsertText(wxString(wxChar(event.GetId())));
      GroupCell *parent = GetActiveCell()->GetGroup();
      Recalculate(parent);
      RequestRedraw(parent);
    } else
      OpenHCaret(wxString(wxChar(event.GetId())));
  } else {
    // Send the char to the last active text control
    wxTextCtrl *textCtrl = BTextCtrl::LastActive();
    long pos = textCtrl->GetInsertionPoint();
    textCtrl->WriteText(wxString(wxChar(event.GetId())));
    textCtrl->SetInsertionPoint(pos + 1);
    CallAfter(&Worksheet::FocusTextControl);
  }
}

void Worksheet::FocusTextControl() {
  wxTextCtrl *textCtrl = BTextCtrl::LastActive();
  if (textCtrl) {
    wxLogMessage(_("Forwarding the keyboard focus to a text control"));
    long pos = textCtrl->GetInsertionPoint();
    textCtrl->SetFocus();
    textCtrl->SetInsertionPoint(pos);
  }
}

void Worksheet::EraseBackground(wxEraseEvent &WXUNUSED(event)) {}

wxSize Worksheet::DoGetBestClientSize() const {
  wxSize size(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) * .6,
              wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) * .6);
  if (size.x < 800)
    size.x = 800;
  if (size.y < 600)
    size.y = 600;
  return size;
}

bool Worksheet::RedrawIfRequested() {
  m_displayTimeoutTimer.Start(1000);
  bool redrawIssued = false;
  RecalculateIfNeeded();

  if (m_mouseMotionWas) {
    UnsetStatusText();
    if (!m_cellPointers.m_groupCellUnderPointer ||
        (m_pointer_y <
         m_cellPointers.m_groupCellUnderPointer->GetRect().GetTop()) ||
        (m_pointer_y >
         m_cellPointers.m_groupCellUnderPointer->GetRect().GetBottom())) {
      const GroupCell *oldGroupCellUnderPointer =
        m_cellPointers.m_groupCellUnderPointer;

      // Find out which group cell lies under the pointer. If the pointer is in
      // the gap *between* two cells (below one cell's bottom and above the next
      // cell's top) it lies in neither, so the result stays null and no bracket
      // is shown. (Previously the search snapped to the nearest cell in the
      // walk direction, which made the result flip between the cell above and
      // the cell below on every mouse event while hovering in such a gap.)
      GroupCell *startSearch = m_cellPointers.m_groupCellUnderPointer;
      if (!startSearch) startSearch = GetTree();

      GroupCell *found = nullptr;
      if (startSearch && m_pointer_y > startSearch->GetRect().GetBottom()) {
          // Walk downwards.
          for (auto &tmp : OnList(startSearch)) {
              if (m_pointer_y < tmp.GetRect().GetTop())
                  break; // reached a cell that starts below the pointer: it's in a gap
              if (m_pointer_y <= tmp.GetRect().GetBottom()) {
                  found = &tmp;
                  break;
              }
          }
      } else if (startSearch && m_pointer_y < startSearch->GetRect().GetTop()) {
          // Walk upwards.
          for (Cell *tmp = startSearch; tmp; tmp = tmp->GetPrevious()) {
              if (m_pointer_y > tmp->GetRect().GetBottom())
                  break; // reached a cell that ends above the pointer: it's in a gap
              if (m_pointer_y >= tmp->GetRect().GetTop()) {
                  found = dynamic_cast<GroupCell *>(tmp);
                  break;
              }
          }
      } else if (!m_cellPointers.m_groupCellUnderPointer) {
          found = startSearch; // pointer is within startSearch's vertical extent
      }
      // GetTree() is null on an empty worksheet (e.g. right after startup);
      // only then is there no cell to point at and nothing to update.
      if (GetTree())
          GetTree()->CellUnderPointer(found);

      // Make the right brackets autohide
      if ((m_configuration->HideBrackets()) &&
          (oldGroupCellUnderPointer !=
           m_cellPointers.m_groupCellUnderPointer)) {
        if (oldGroupCellUnderPointer) {
          RequestRedraw(
                        wxRect(0, oldGroupCellUnderPointer->GetRect().GetTop(),
                               m_configuration->GetIndent(),
                               oldGroupCellUnderPointer->GetRect().GetHeight()));
        }
        if (m_cellPointers.m_groupCellUnderPointer) {
          RequestRedraw(wxRect(
                               0, m_cellPointers.m_groupCellUnderPointer->GetRect().GetTop(),
                               m_configuration->GetIndent(),
                               m_cellPointers.m_groupCellUnderPointer->GetRect().GetHeight()));
        }
      }
    }

    if (m_cellPointers.m_groupCellUnderPointer) {
      // Update the worksheet's ToolTip: wxWidgets doesn't allow us to specify a
      // separate tooltip for every worksheet element, but it does allow us to
      // set the worksheet's tooltip to the appropriate one for the worksheet
      // element the pointer points to.
      if (m_cellPointers.m_groupCellUnderPointer->GetOutputRect().Contains(
                                                                           wxPoint(m_pointer_x, m_pointer_y))) {
        m_cellPointers.m_cellUnderPointer = nullptr;
        wxString toolTip = m_cellPointers.m_groupCellUnderPointer->GetToolTip(
                                                                              wxPoint(m_pointer_x, m_pointer_y));

        if (!toolTip.empty()) {
          if (!GetToolTipText().empty()) {
            if (toolTip != GetToolTipText()) {
              // Disabling and re-enabling tooltips resets the tooltip poput
              // delay timer. We need to do that since we don't have a individual
              // per-cell popout delay timer in the cell the cursor currently is in,
              // but only the per-worksheet one.
              wxToolTip::Enable(false);
              wxToolTip::Enable(true);
              SetToolTip(toolTip);
            }
          } else
            SetToolTip(toolTip);
        } else
          UnsetToolTip();

        if (m_cellPointers.m_cellUnderPointer) {
          const ImgCellBase * const image = GetSelectedImgCellBase();
          if (image != NULL) {
            StatusText(wxString::Format(
                                        _("%s image, %li×%li, %li ppi"),
                                        image->GetExtension().ToUTF8().data(),
                                        static_cast<long>(image->GetOriginalWidth()),
                                        static_cast<long>(image->GetOriginalWidth()),
                                        static_cast<long>(image->GetPPI())));
          }
        }
      } else
        UnsetToolTip();
    } else {
      UnsetToolTip();
      m_cellPointers.m_cellUnderPointer = nullptr;
    }
    m_mouseMotionWas = false;
    redrawIssued = true;
  }

  if (m_fullRedrawRequested) {
    // A redraw of the whole worksheet beginning with a specific cell was requested

    // TODO: Only redraw the region that actually needs refreshing: Currently we
    // refresh the whole screen even if m_redrawStart is set.
    Refresh();
    m_fullRedrawRequested = false;
    m_redrawStart = NULL;
    redrawIssued = true;
  } else {
    // Ignore regions that we marked for redrawing, but that are outside the
    // current window
    m_regionToRefresh.Intersect(m_configuration->GetVisibleRegion());

    // Only try to draw a region if said region still exists
    if(m_regionToRefresh.IsOk())
      {
        // A redraw of a worksheet region was requested
        wxRegionIterator region(m_regionToRefresh);
        while (region.HaveRects()) {
          wxRect rect = region.GetRect();

          // Don't draw rectangles with zero size or height
          if ((rect.GetWidth() >= 1) && (rect.GetHeight() >= 1)) {
            CalcScrolledPosition(rect.x, rect.y, &rect.x, &rect.y);
            RefreshRect(rect);
          }
          redrawIssued = true;
          region++;
        }
      }
  }
  m_regionToRefresh.Clear();

  return redrawIssued;
}

void Worksheet::RequestRedraw(GroupCell *start) {
  m_fullRedrawRequested = true;

  if (start == NULL)
    m_redrawStart = GetTree();
  else {
    if (m_redrawStart) {
      // No need to waste time avoiding to waste time in a refresh when we don't
      // know our cell's position.
      if ((start->GetCurrentPoint().y < 0) ||
          (m_redrawStart->GetCurrentPoint().y < 0)) {
        m_redrawStart = GetTree();
      } else if (start->GetCurrentPoint().y <
                 m_redrawStart->GetCurrentPoint().y)
        m_redrawStart = start;
    } else
      m_redrawStart = start;
  }

  // Make sure there is a timeout for the redraw
  if (!m_caretTimer.IsRunning()) {
    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.Start(blinktime);
  }
}

Worksheet::~Worksheet() {
  TreeUndo_ClearRedoActionList();
  TreeUndo_ClearUndoActionList();

  if (wxConfig::Get() != NULL)
    wxConfig::Get()->Flush();
  if (HasCapture())
    ReleaseMouse();

  m_mainToolBar = NULL;

  ClearDocument();
  m_configuration = NULL;
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
#endif

void Worksheet::OnPaint(wxPaintEvent &WXUNUSED(event)) {
  m_configuration->ClearAndEnableRedrawTracing();
  m_configuration->SetBackgroundBrush(*(wxTheBrushList->FindOrCreateBrush(
                                                                          m_configuration->DefaultBackgroundColor(), wxBRUSHSTYLE_SOLID)));
  wxAutoBufferedPaintDC dc(this);
  if (!dc.IsOk())
    return;

  // Some drawing contents
#ifndef DC_ALREADY_SCROLLED
  PrepareDC(dc);
#endif

  // Don't attempt to draw in a window of the size 0.
  if ((GetClientSize().x < 1) || (GetClientSize().y < 1))
    return;

  // It is possible that the redraw starts before the idle task attempts
  // to recalculate the worksheet.
  RecalculateIfNeeded();

  // Create a graphics context that supports antialiasing, but on MSW
  // only supports fonts that come in the Right Format.
  wxGCDC antiAliassingDC(dc);

  // Don't fill the text background with the background color
  // No need to do the same for the antialiassing DC: We won't use that
  // one for drawing text as on MS Windows it doesn't support all fonts
  dc.SetMapMode(wxMM_TEXT);

  // Now iterate over all single parts of the region we need to redraw and
  // redraw the worksheet
  wxRect totalRect = GetUpdateRegion().GetBox();
  if ((totalRect.GetWidth() >= 1) && (totalRect.GetHeight() >= 1)) {
    // Set line pen and fill brushes
    SetBackgroundColour(m_configuration->DefaultBackgroundColor());
    PrepareDrawGC(dc);
    PrepareDrawGC(antiAliassingDC);

    // Tell the configuration where to crop in this region
    int xstart, xend, top, bottom;
    CalcUnscrolledPosition(totalRect.GetLeft(), totalRect.GetTop(), &xstart, &top);
    CalcUnscrolledPosition(totalRect.GetRight(), totalRect.GetBottom(), &xend, &bottom);
    wxRect unscrolledRect;
    unscrolledRect.SetLeft(xstart);
    unscrolledRect.SetRight(xend);
    unscrolledRect.SetTop(top);
    unscrolledRect.SetBottom(bottom);
    m_configuration->SetUpdateRegion(unscrolledRect);

    int width;
    int height;
    GetClientSize(&width, &height);

    wxPoint upperLeftScreenCorner;
    CalcUnscrolledPosition(0, 0, &upperLeftScreenCorner.x,
                           &upperLeftScreenCorner.y);
    wxRect visibleRegion = wxRect(upperLeftScreenCorner,
                                  upperLeftScreenCorner + wxPoint(width, height));

    m_configuration->SetVisibleRegion(visibleRegion);
    m_configuration->SetWorksheetPosition(GetPosition());

    // Clear the drawing area (Clear() doesn't work on some wx3.0 installs)
    dc.DrawRectangle(unscrolledRect);

    //
    // Draw the cell contents
    //
    if (GetTree()) {
      dc.SetPen(*(wxThePenList->FindOrCreatePen(
                                                m_configuration->GetColor(TS_MATH), 1, wxPENSTYLE_SOLID)));
      dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(
                                                      m_configuration->GetColor(TS_MATH))));
      for (auto &cell : OnList(GetTree())) {
        if (cell.GetCurrentPoint().y + (cell.GetHeight() - cell.GetCenter()) < top) continue;
        if (cell.GetCurrentPoint().y - cell.GetCenter() > bottom) break;

        wxRect cellRect = cell.GetRect();

        // Clear the image cache of all cells above or below the viewport.
        if (cellRect.GetTop() >= bottom || cellRect.GetBottom() <= top) {
          // Only actually clear the image cache if there is a screen's height
          // between us and the image's position: Else the chance is too high
          // that we will very soon have to generated a scaled image again.
          if ((cellRect.GetBottom() <= m_lastBottom - 2 * height) ||
              (cellRect.GetTop() >= m_lastTop + 2 * height)) {
            if (cell.GetOutput())
              cell.GetOutput()->ClearCacheList();
          }
        }
        //      m_drawThreads.push_back(std::jthread(&Worksheet::DrawGroupCell_UsingBitmap,
        //                                  this,
        //                                  &dc, &cell, unscrolledRect));
        DrawGroupCell(dc, antiAliassingDC, cell);
      }
    }

    {
      std::lock_guard<std::mutex> guard(m_drawDCLock);

      //
      // Draw the horizontal caret
      //
      if ((m_hCaretActive) && (!m_hCaretPositionStart) &&
          (m_hCaretBlinkVisible) && (m_hasFocus) && (m_hCaretPosition)) {
        dc.SetPen(*(wxThePenList->FindOrCreatePen(
                                                  m_configuration->GetColor(TS_CURSOR), 1, wxPENSTYLE_SOLID)));
        dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(
                                                        m_configuration->GetColor(TS_CURSOR), wxBRUSHSTYLE_SOLID)));
        wxRect currentGCRect = m_hCaretPosition->GetRect();
        int caretY = (static_cast<int>(m_configuration->GetGroupSkip())) / 2 +
          currentGCRect.GetBottom() + 1;
        dc.DrawRectangle(
                         xstart + m_configuration->GetBaseIndent(),
                         caretY - m_configuration->GetCursorWidth() / 2, MC_HCARET_WIDTH,
                         m_configuration->GetCursorWidth());
      }

      if ((m_hCaretActive) && (!m_hCaretPositionStart) && (m_hasFocus) &&
          (!m_hCaretPosition)) {
        if (!m_hCaretBlinkVisible) {
          dc.SetBrush(
                      m_configuration->GetBackgroundBrush());
          dc.SetPen(*wxThePenList->FindOrCreatePen(
                                                   GetBackgroundColour(), m_configuration->Scale_Px(1)));
        } else {
          dc.SetPen(*(wxThePenList->FindOrCreatePen(
                                                    m_configuration->GetColor(TS_CURSOR), m_configuration->Scale_Px(1),
                                                    wxPENSTYLE_SOLID)));
          dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(
                                                          m_configuration->GetColor(TS_CURSOR), wxBRUSHSTYLE_SOLID)));
        }

        wxRect cursor =
          wxRect(xstart + m_configuration->GetCellBracketWidth(),
                 (m_configuration->GetBaseIndent() -
                  m_configuration->GetCursorWidth()) /
                 2,
                 MC_HCARET_WIDTH, m_configuration->GetCursorWidth());
        dc.DrawRectangle(cursor);
      }
    }

    m_lastTop = top;
    m_lastBottom = bottom;
  }

  m_configuration->ReportMultipleRedraws();
}

void Worksheet::PrepareDrawGC(wxDC &dc) const
{
  dc.SetMapMode(wxMM_TEXT);
  dc.SetBackgroundMode(wxTRANSPARENT);
  dc.SetBackground(m_configuration->GetBackgroundBrush());
  dc.SetBrush(m_configuration->GetBackgroundBrush());
  // Transparent pen so the area-clearing DrawRectangle()s fill only (no perimeter
  // stroke). The old default was *wxWHITE_PEN, whose 1px border was invisible only
  // on a white background -- on a dark background it showed as a 1px ring, and on
  // DCs where the pen over-strokes by a pixel it would even clear one pixel too
  // many. Filling without a border avoids both.
  dc.SetPen(*wxTRANSPARENT_PEN);
  dc.SetLogicalFunction(wxCOPY);
}

void Worksheet::DrawGroupCell_UsingBitmap(wxDC *dc, GroupCell *cell)
{
  // Determine which rectangle we need to draw, effectively:
  // The part of the GroupCell that is in the region to be drawn.
  wxRect drawRect = cell->GetRect();
  if(drawRect.GetHeight() < 1)
    return;
  if(drawRect.GetWidth() < 1)
    return;
  wxSize sz(drawRect.GetWidth(), drawRect.GetHeight());

  // Create a bitmap of the size of our drawRect
#ifdef __WXMAC__
  wxBitmap bmp =
    wxBitmap(sz * wxWindow::GetContentScaleFactor(), wxBITMAP_SCREEN_DEPTH,
             wxWindow::GetContentScaleFactor());
#else
  wxBitmap bmp =
    wxBitmap(sz * wxWindow::GetContentScaleFactor(), wxBITMAP_SCREEN_DEPTH);
#endif
  wxASSERT(bmp.IsOk());
  {
    // Create a DrawContext that draws on a bitmap the size of our drawRect
    std::unique_lock<std::mutex> lock(m_drawDCLock);
    wxMemoryDC dcm(bmp);
    lock.unlock();
    dcm.SetUserScale(wxWindow::GetContentScaleFactor(),
                     wxWindow::GetContentScaleFactor());
    dcm.SetDeviceOrigin(-drawRect.GetLeft(), -drawRect.GetTop());
    //dcm.SetLogicalOrigin(-drawRect.GetLeft(), -drawRect.GetTop());
    PrepareDrawGC(dcm);
    wxASSERT(dcm.IsOk());
    // Create an antialiassing DrawContext that draws on dcm
    wxGCDC antiAliassingDC(dcm);
    PrepareDrawGC(antiAliassingDC);
    wxASSERT(antiAliassingDC.IsOk());

    // Clear the drawing area. Clear() doesn't work in some wx3.0 installs
    dcm.DrawRectangle(drawRect);

    // Fill the bitmap with contents
    DrawGroupCell(dcm, antiAliassingDC, *cell);

    // This lock does guard the destruction of the memory DC, as well as the blit
    lock.lock();
    // Blit the bitmap onto the destination dc
    dc->Blit(drawRect.GetLeft(), drawRect.GetTop(), drawRect.GetWidth(), drawRect.GetHeight(),
             &dcm, drawRect.GetLeft(), drawRect.GetTop());
  }
}

void Worksheet::DrawGroupCell(wxDC &dc, wxDC &adc, GroupCell &cell)
{
  const bool drawContents = cell.DrawThisCell();
  // The bracket lives in the indentation to the left of the cell's contents, so
  // it has to be (re)drawn even when only that strip - and not the contents -
  // lies in the update region. This is what lets a bracket appearing/vanishing
  // under the mouse repaint just the narrow bracket column instead of the whole
  // cell. (cell.Draw() draws the bracket and then gates its contents on
  // DrawThisCell() itself, so passing through here with only the bracket in the
  // update region paints the bracket and skips the contents.)
  const bool drawBracket =
      m_configuration->ShowBrackets() &&
      m_configuration->InUpdateRegion(cell.GetBracketRect());

  if (!drawContents && !drawBracket)
    return;

  // DrawBracket() reflects these flags, so set them whenever we draw the cell.
  cell.InEvaluationQueue(m_evaluationQueue.IsInQueue(&cell));
  cell.LastInEvaluationQueue(m_evaluationQueue.GetCell() == &cell);

  if (drawContents) {
    //
    // Draw the selection marks for an eventual selection inside this cell
    //
    if (HasCellsSelected() &&
        (m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP) &&
        (&cell == m_cellPointers.m_selectionStart->GetGroup())) {
      // Draw the marker that tells us which output cells are selected -
      // if output cells are selected, that is.
      for (Cell &c : OnDrawList(m_cellPointers.m_selectionStart.get())) {
        if (!c.IsBrokenIntoLines() && !c.IsHidden() &&
            &c != GetActiveCell())
          {
            dc.SetPen(*(wxThePenList->FindOrCreatePen(m_configuration->GetColor(TS_SELECTION),
                                                      1, wxPENSTYLE_SOLID)));
            dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(m_configuration->GetColor(TS_SELECTION))));
            c.DrawBoundingBox(dc, false);
            dc.SetBrush(m_configuration->GetBackgroundBrush());
            dc.SetPen(*wxTRANSPARENT_PEN);
          }
        if (&c == m_cellPointers.m_selectionEnd)
          break;
      }
    }
  }
  cell.Draw(&dc, &adc);
}

GroupCell *Worksheet::InsertGroupCells(std::unique_ptr<GroupCell> &&cells,
                                       GroupCell *where) {
  return InsertGroupCells(std::move(cells), where, &m_treeUndo.UndoStack());
}

// InsertGroupCells
// inserts groupcells after position "where" (NULL = top of the document)
// Multiple groupcells can be inserted when cells->m_next != NULL
// Returns the pointer to the last inserted group cell to have fun with
GroupCell *Worksheet::InsertGroupCells(std::unique_ptr<GroupCell> &&cells,
                                       GroupCell *where,
                                       UndoActions *undoBuffer) {
  if (!cells)
    return NULL; // nothing to insert

  m_configuration->SetAdjustWorksheetSizeNeeded(true);
  bool renumbersections = false; // only renumber when true

  // TODO What we have here is an iteration through all the cells to see if they
  // fulfill some criterion, and additionally we find the last() cell.
  // When cell list management is refactored, the foldable status should be kept
  // always up-to-date.

  GroupCell *firstOfCellsToInsert = cells.get();
  // Find the last cell in the tree that is to be inserted
  GroupCell *lastOfCellsToInsert = cells.get();
  if (lastOfCellsToInsert->IsFoldable() ||
      (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
    renumbersections = true;
  while (lastOfCellsToInsert->GetNext()) {
    if (lastOfCellsToInsert->IsFoldable() ||
        (lastOfCellsToInsert->GetGroupType() == GC_TYPE_IMAGE))
      renumbersections = true;
    lastOfCellsToInsert = lastOfCellsToInsert->GetNext();
  }

  if (!m_tree) {
    m_tree = std::move(cells);
    m_last = lastOfCellsToInsert;
  } else if (!where) {
    CellList::SpliceInAfter(lastOfCellsToInsert, std::move(m_tree));
    RequestRedraw(cells.get());
    m_tree = std::move(cells);
    m_last = nullptr; // finding new last is easier via GetLastCellInWorksheet
  } else {
    if (where == m_last)
        m_last = lastOfCellsToInsert;
    else
        m_last = nullptr;
    CellList::SpliceInAfter(where, std::move(cells), lastOfCellsToInsert);
    // make sure m_last still points to the last cell of the worksheet!!
  }

  if (renumbersections)
    NumberSections();

  GroupCell *recalcStart = NULL;
  if (where)
    recalcStart = where->GetNext();
  if (!recalcStart)
    recalcStart = GetTree();

  if (recalcStart)
    Recalculate(recalcStart);
  SetSaved(false); // document has been modified

  if (undoBuffer)
    TreeUndo_MarkCellsAsAdded(firstOfCellsToInsert, lastOfCellsToInsert,
                              undoBuffer);
  // AdjustSize() is intentionally NOT called here. Cell positions are
  // stale at this point (recalculation has only been scheduled, not
  // executed). Calling AdjustSize() with stale positions would compute a
  // virtualHeight that is too small, causing wxWidgets to clamp the scroll
  // position and jump the view to the top. The flag
  // SetAdjustWorksheetSizeNeeded(true) (set above) ensures that
  // RecalculateIfNeeded() will call AdjustSize() after positions are correct.
  return lastOfCellsToInsert;
}

void Worksheet::ScrollToError() {
  GroupCell *errorCell = m_cellPointers.m_errorList.LastError();

  if (!errorCell)
    errorCell = GetWorkingGroup(true);

  if (!errorCell)
    return;

  if (errorCell->RevealHidden()) {
    FoldOccurred();
    Recalculate();
  }

  // Try to scroll to a place from which the full error message is visible
  ScrollToCaret();

  // Set the cursor as close to the error as possible.
  if (errorCell->GetEditable()->ErrorIndexSet()) {
    ClearSelection();
    m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
    SetActiveCell(errorCell->GetEditable());
    errorCell->GetEditable()->GotoError();
    ScrollToCaret();
  } else
    SetHCaret(errorCell);
}

GroupCell *Worksheet::GetWorkingGroup(bool resortToLast) const {
  GroupCell *cell = m_cellPointers.GetWorkingGroup(resortToLast);

  if (!resortToLast)
    return cell;

  // The last group maxima was working on no more exists or has been deleted.
  if (!cell && m_hCaretActive)
    cell = m_hCaretPosition;

  // No such cursor? Perhaps there is a vertically drawn one.
  if (!cell && GetActiveCell())
    cell = GetActiveCell()->GetGroup();

  // If there is no such cell, neither, we append the line to the end of the
  // worksheet.
  if (!cell)
    cell = GetLastCellInWorksheet();

  return cell;
}

GroupCell *Worksheet::GetInsertGroup() const {
  GroupCell *cell = GetWorkingGroup(true);

  if (!cell && GetActiveCell())
    cell = GetActiveCell()->GetGroup();

  return cell;
}

void Worksheet::InsertLine(std::unique_ptr<Cell> &&newCell, bool forceNewLine) {
  if (!newCell)
    return;

  GroupCell *cell = GetInsertGroup();
  if (!cell)
    return;

  newCell->ForceBreakLine(forceNewLine);
  cell->AppendOutput(std::move(newCell));
  m_configuration->SetAdjustWorksheetSizeNeeded(true);

  cell->OutputHeightChanged();
  AdjustSize();
  OutputChanged();
  RequestRedraw(cell);
  RedrawIfRequested();

  if (FollowEvaluation()) {
    ClearSelection();
    if (GCContainsCurrentQuestion(cell))
      OpenQuestionCaret();
    else
      ScrollToCaret();
  }
}

void Worksheet::SetZoomFactor(double newzoom) {
  // Restrict zoom factors to tenths: If we allow arbitrary floats we sometimes
  // recalculate on requests to change the zoom factor by amounts that are too small to
  // make any effect.
  newzoom = round(newzoom * 10) / 10.0;

  if (newzoom < m_configuration->GetMinZoomFactor())
    newzoom = m_configuration->GetMinZoomFactor();
  if (newzoom > m_configuration->GetMaxZoomFactor())
    newzoom = m_configuration->GetMaxZoomFactor();

  // If the zoom factor hasn't changed return. We don't test for equality with
  // zero since in this case that might probably work. But testing floats for
  // equality is a bad habit.
  if (fabs(m_configuration->GetZoomFactor() - newzoom) < .00005)
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
  if (!cellToScrollTo) {
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    cellToScrollTo = GetTree();
    while (cellToScrollTo != NULL) {
      wxRect rect = cellToScrollTo->GetRect();
      if (rect.GetBottom() > topleft.y)
        break;
      cellToScrollTo = cellToScrollTo->GetNext();
    }
  }
  if(GetTree())
    {
      GetTree()->FontsChangedList();
      GetTree()->ResetSizeList();
      GetTree()->ClearCacheList();
      Recalculate();
      RequestRedraw();
      ScheduleScrollToCell(cellToScrollTo);
    }
}

bool Worksheet::RecalculateIfNeeded(bool timeout) {
  if (m_configuration->GetCanvasSize().x < 1)
    return (false);
  if (m_configuration->GetCanvasSize().y < 1)
    return (false);

  if (!m_recalculateStart || !GetTree()) {
    m_recalculateStart = {};
    return false;
  }

  if (!GetTree()->Contains(m_recalculateStart))
    m_recalculateStart = GetTree();

  m_configuration->SetWorksheetPosition(GetPosition());

  if (m_recalculateStart == GetTree())
    m_maxWidth_Cached = -1;

  if(timeout)
    {
      wxStopWatch stopwatch;
      bool recalculated = false;
      bool stopwatchStarted = false;
      bool propagationNeeded = true;
      int cellsVisited = 0;
      for (auto &cell : OnList(m_recalculateStart.get())) {
        cellsVisited++;
        GroupCell *group = static_cast<GroupCell *>(&cell);
        bool neededRecalc = group->NeedsRecalculation();
        bool movedThisTime = false;
        bool sizeChanged = false;

        if (neededRecalc) {
          int oldHeight = group->GetHeight();
          sizeChanged = group->Recalculate();
          if (group->GetHeight() != oldHeight)
            sizeChanged = true;
          movedThisTime = true;
          recalculated |= sizeChanged;

          int currentWidth =
            m_configuration->Scale_Px(m_configuration->GetIndent() +
                                      m_configuration->GetDefaultFontSize()) +
            cell.GetWidth() +
            m_configuration->Scale_Px(m_configuration->GetIndent() +
                                      m_configuration->GetDefaultFontSize());
          m_maxWidth_Cached = std::max(m_maxWidth_Cached, currentWidth);
        } else if (propagationNeeded) {
          movedThisTime = group->Reposition();
        } else {
          // Don't stop here: a cell further down the worksheet may still be
          // dirty. Cells can be marked for recalculation non-contiguously (e.g.
          // by folding/hiding or by asynchronous Maxima output landing in
          // specific cells), so a clean, non-propagating cell does not imply
          // everything below it is clean. Keep scanning; the m_recalculateStart
          // bookkeeping below advances the resume point past this clean cell.
        }
        propagationNeeded = movedThisTime || sizeChanged;

        if((cell.GetRect().GetTop() > m_configuration->GetVisibleRegion().GetBottom()) &&
           recalculated)
          {
            if(!stopwatchStarted)
              stopwatch.Start();
          }
        if(cell.GetNext() != NULL)
          m_recalculateStart = cell.GetNext();
        else
          {
            wxLogMessage(_("Recalculation hit the end of the worksheet => Updating its size (Visited %d cells in %ld ms)"), cellsVisited, stopwatch.Time());
            m_recalculateStart = {};
            AdjustSize();
          }
        if(stopwatch.Time() > 50)
          break;
      }
    }
  else
    {
      wxStopWatch stopwatch;
      bool propagationNeeded = true;
      int cellsVisited = 0;
      for (auto &cell : OnList(m_recalculateStart.get())) {
        cellsVisited++;
        GroupCell *group = static_cast<GroupCell *>(&cell);
        bool neededRecalc = group->NeedsRecalculation();
        bool movedThisTime = false;
        bool sizeChanged = false;

        if (neededRecalc) {
          int oldHeight = group->GetHeight();
          sizeChanged = group->Recalculate();
          if (group->GetHeight() != oldHeight)
            sizeChanged = true;
          movedThisTime = true;
          if (sizeChanged)
            m_configuration->SetAdjustWorksheetSizeNeeded(true);

          int currentWidth =
            m_configuration->Scale_Px(m_configuration->GetIndent() +
                                      m_configuration->GetDefaultFontSize()) +
            cell.GetWidth() +
            m_configuration->Scale_Px(m_configuration->GetIndent() +
                                      m_configuration->GetDefaultFontSize());
          m_maxWidth_Cached = std::max(m_maxWidth_Cached, currentWidth);
        } else if (propagationNeeded) {
          movedThisTime = group->Reposition();
        } else {
          // Don't stop here: a cell further down the worksheet may still be
          // dirty. Cells can be marked for recalculation non-contiguously (e.g.
          // by folding/hiding or by asynchronous Maxima output landing in
          // specific cells), so a clean, non-propagating cell does not imply
          // everything below it is clean. Keep scanning to the end.
        }
        propagationNeeded = movedThisTime || sizeChanged;

        if(cell.GetNext() == NULL)
          {
            wxLogMessage(_("Recalculated the whole worksheet at once => Updating its size (Visited %d cells in %ld ms)"), cellsVisited, stopwatch.Time());
          }
      }
    }
  if (m_configuration->GetAdjustWorksheetSizeNeeded())
    AdjustSize();

  m_recalculateStart = {};
  return true;
}

void Worksheet::Recalculate(Cell *start) {
  if (!GetTree())
    return;
  wxASSERT(start);
  if (!start)
    return;

  GroupCell *group = start->GetGroup();
  if (!group)
    return;

  if (m_recalculateStart == group)
    return;

  group->MarkNeedsRecalculate();

  if (!m_recalculateStart)
    m_recalculateStart = group;
  else {
    // Move m_recalculateStart backwards to start, if start comes before
    // m_recalculateStart.
    for (Cell *walk = group; walk; walk = walk->GetPrevious()) {
      if (walk == m_recalculateStart)
        return;
    }
    m_recalculateStart = group;
  }
}

/***
 * Resize the control
 */
void Worksheet::OnSize(wxSizeEvent &event) {
  event.Skip();
  // Inform all cells how wide our display is now
  UpdateConfigurationClientSize();

  // Determine if we have a sane thing we can scroll to.
  Cell *CellToScrollTo = {};
  if (CaretVisibleIs()) {
    CellToScrollTo = m_hCaretPosition;
    if (!CellToScrollTo)
      CellToScrollTo = GetActiveCell();
  }
  if (!CellToScrollTo)
    CellToScrollTo = GetWorkingGroup(true);

  if (!CellToScrollTo) {
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    CellToScrollTo = GetTree();
    for (; CellToScrollTo; CellToScrollTo = CellToScrollTo->GetNext()) {
      wxRect rect = CellToScrollTo->GetRect();
      if (rect.GetBottom() > topleft.y)
        break;
    }
  }
  Recalculate();

  m_configuration->SetAdjustWorksheetSizeNeeded(true);
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
void Worksheet::ClearDocument() {
  CloseAutoCompletePopup();
  ClearSelection();
  SetActiveCell(NULL);
  m_clickType = CLICK_TYPE_NONE;
  m_clickInGC = NULL;
  m_hCaretActive = false;
  SetHCaret(NULL); // horizontal caret at the top of document
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  m_recalculateStart = NULL;
  m_evaluationQueue.Clear();
  TreeUndo_ClearBuffers();

  m_blinkDisplayCaret = true;
  SetSaved(false);
  UpdateTableOfContents();
  DestroyTree();

  Scroll(0, 0);
}

/***
 * Reset all input prompts to "-->  "
 * Called when Restart Maxima is called from Maxima menu
 */
void Worksheet::ResetInputPrompts() {
  if (GetTree())
    GetTree()->ResetInputLabelList(); // recursively reset prompts
}

//
// support for numbered sections with hiding
//
void Worksheet::NumberSections() const {
  int s, sub, subsub, h5, h6, i;
  s = sub = subsub = i = h5 = h6 = 0;
  if (GetTree())
    GetTree()->Number(s, sub, subsub, h5, h6, i);
}

bool Worksheet::IsLesserGCType(int type, int comparedTo) {
  switch (type) {
  case GC_TYPE_CODE:
  case GC_TYPE_TEXT:
  case GC_TYPE_IMAGE:
    if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
        (comparedTo == GC_TYPE_SUBSECTION) ||
        (comparedTo == GC_TYPE_SUBSUBSECTION) ||
        (comparedTo == GC_TYPE_HEADING5) || (comparedTo == GC_TYPE_HEADING6))
      return true;
    else
      return false;
  case GC_TYPE_HEADING6:
    if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
        (comparedTo == GC_TYPE_SUBSECTION) ||
        (comparedTo == GC_TYPE_HEADING5) ||
        (comparedTo == GC_TYPE_SUBSUBSECTION))
      return true;
    else
      return false;
  case GC_TYPE_HEADING5:
    if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
        (comparedTo == GC_TYPE_SUBSECTION) ||
        (comparedTo == GC_TYPE_SUBSUBSECTION))
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
void Worksheet::FoldOccurred() {
  OutputChanged();
}

/**
 * Toggles the status of the fold for the given GroupCell.
 * If the cell is folded, it will be unfolded; otherwise it will be folded.
 *
 * @param which   The GroupCell to fold or unfold.
 * @return        A pointer to a GroupCell if the action succeeded;
 *                NULL otherwise.
 */
GroupCell *Worksheet::ToggleFold(GroupCell *which) {
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
GroupCell *Worksheet::ToggleFoldAll(GroupCell *which) {
  if (!which || !which->IsFoldable())
    return {};

  GroupCell *result =
    which->GetHiddenTree() ? which->UnfoldAll() : which->FoldAll();

  if (result) // something has folded/unfolded
    FoldOccurred();

  return result;
}

/**
 * Recursively folds the whole document.
 */
void Worksheet::FoldAll() {
  if (GetTree()) {
    GetTree()->FoldAll();
    FoldOccurred();
  }
}

/**
 * Recursively unfolds the whole document.
 */
void Worksheet::UnfoldAll() {
  if (GetTree()) {
    GetTree()->UnfoldAll();
    FoldOccurred();
  }
}

/***
 * Right mouse - popup-menu
 */
void Worksheet::OnMouseRightDown(wxMouseEvent &event) {
  event.Skip();
  BTextCtrl::ForgetLastActive();
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
  if (m_cellPointers.m_selectionStart) {
    // SELECTION OF GROUPCELLS
    if (m_cellPointers.m_selectionStart->GetType() ==
        MC_TYPE_GROUP) { // a selection of groups
      if (downx <= m_configuration->GetCellBracketWidth() + 3) {
        wxRect rectStart = m_cellPointers.m_selectionStart->GetRect();
        wxRect rectEnd = m_cellPointers.m_selectionEnd->GetRect();
        if (((downy >= rectStart.GetTop()) && (downy <= rectEnd.GetBottom())) ||
            ((downy >= rectEnd.GetTop()) && (downy <= rectStart.GetBottom())))
          clickInSelection = true;
      }
    }
    // SELECTION OF OUTPUT
    else {
      for (const Cell &cell : OnDrawList(m_cellPointers.m_selectionStart.get())) {
        auto rect = cell.GetRect();
        if (rect.Contains(downx, downy))
          clickInSelection = true;

        if (&cell == m_cellPointers.m_selectionEnd)
          break;
      }
    }
  }
  // SELECTION IN EDITORCELL
  else if (GetActiveCell()) {
    if (GetActiveCell()->IsPointInSelection(wxPoint(downx, downy)))
      clickInSelection = true;
  }

  // emulate a left click to set the cursor
  if (!clickInSelection) {
    wxMouseEvent dummy = event;
    dummy.SetShiftDown(false);
    OnMouseLeftDown(dummy);
    m_leftDown = false;
    m_clickType = CLICK_TYPE_NONE;
    if (HasCapture())
      ReleaseMouse();
  }

  PopulateWorksheetContextMenu(*this, popupMenu, downx, downy,
                               clickInSelection);

  // create menu if we have any items
  if (popupMenu.GetMenuItemCount() > 0) {
    m_inPopupMenu = true;
    // Popping up the menu causes us to loose the mouse capture
    // which causes an assert if we captured the mouse and didn't
    // request to un-capture it.
    if(HasCapture())
      ReleaseMouse();
    PopupMenu(&popupMenu);
    m_inPopupMenu = false;
  }
}

/***
 * We have a mouse click to the left of a GroupCel.
 */
void Worksheet::OnMouseLeftInGcLeft(wxMouseEvent &event,
                                    GroupCell *clickedInGC) {
  if ((clickedInGC->HideRect())
      .Contains(m_down)) // did we hit the hide rectancle
    {
      if (clickedInGC->IsFoldable()) {
        if (event.ShiftDown())
          ToggleFoldAll(clickedInGC);
        else
          ToggleFold(clickedInGC);
        Recalculate(clickedInGC);
      } else {
        clickedInGC->SwitchHide();
        clickedInGC->ResetSize();
        Recalculate(clickedInGC);
        m_clickType = CLICK_TYPE_NONE; // ignore drag-select
      }
    } else {
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    SetSelection(clickedInGC);
  }
}

/***
 * We have a mouse click in the GroupCell.
 */
void Worksheet::OnMouseLeftInGcCell(wxMouseEvent &WXUNUSED(event),
                                    GroupCell *clickedInGC) {
  if (GCContainsCurrentQuestion(clickedInGC)) {
    // The user clicked at the cell maxima has asked a question in.
    FollowEvaluation(true);
    OpenQuestionCaret();
    return;
  }

  // The user clicked at an ordinary cell
  if ((clickedInGC->GetPrompt()) &&
      (clickedInGC->GetPrompt()->GetRect()).Contains(m_down)) {
    m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd =
      clickedInGC->GetPrompt();
    m_clickType = CLICK_TYPE_INPUT_LABEL_SELECTION;
  }

  // Let's see if the user clicked at the cell's input label

  // Let's see if the user clicked at the cell's input
  EditorCell *editor = clickedInGC->GetEditable();
  if (editor) {
    wxRect rect = editor->GetRect();
    if ((m_down.y >= rect.GetTop() && m_down.y <= rect.GetBottom()) &&
        (m_configuration->ShowCodeCells() ||
         editor->GetType() != MC_TYPE_INPUT || !clickedInGC->GetOutput())) {
      editor->MouseSelectionStartedHere();
      SetActiveCell(editor);
      editor->SelectPointText(m_down);
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

  // what if we tried to select something in output, select it (or if editor,
  // activate it)
  if ((clickedInGC->GetOutputRect()).Contains(m_down)) {
    wxRect rect2(m_down.x, m_down.y, 1, 1);
    wxPoint mmm(m_down.x + 1, m_down.y + 1);
    auto const [first, last] = clickedInGC->GetCellsInOutputRect(rect2, m_down, mmm);
    m_cellPointers.m_selectionStart = first;
    m_cellPointers.m_selectionEnd = last;
    if (m_cellPointers.m_selectionStart) {
      m_clickType = CLICK_TYPE_OUTPUT_SELECTION;
      m_clickInGC = clickedInGC;
    }
  }
}

void Worksheet::OnMouseLeftInGc(wxMouseEvent &event, GroupCell *clickedInGC) {
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
 * m_clickType is used in ClickNDrag when click-draging to determine what kind
 * of selection behaviour we want.
 *
 * - check in which GroupCell it falls
 * - if it falls between groupCells activate caret and
 * CLICK_TYPE_GROUP_SELECTION
 * - if it falls within a groupcell investigate where did it fall (input or
 * output)
 */
void Worksheet::OnMouseLeftDown(wxMouseEvent &event) {
  event.Skip();
  BTextCtrl::ForgetLastActive();
  m_updateControls = true;
  RecalculateIfNeeded();
  RedrawIfRequested();
  CloseAutoCompletePopup();
  m_leftDownPosition = wxPoint(event.GetX(), event.GetY());
  ClearNotification();

  // During drag-and-drop We want to track the mouse position.
  if (event.LeftDown()) {
    if (!HasCapture())
      CaptureMouse();
    m_leftDown = true;
  }

  m_cellPointers.ResetSearchStart();

  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

  if (!GetTree())
    return;

  // Handle a shift-click when GroupCells were selected.
  if (m_hCaretPositionStart && event.ShiftDown()) {
    // We were selecting group cells when the shift-click happened.
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    // Set a fake starting point for the selection that is inside the cell the
    // selection started in.
    m_down = wxPoint(m_hCaretPositionStart->GetRect().GetLeft(),
                     m_hCaretPositionStart->GetRect().GetTop());
    // Handle the mouse pointer position
    OnMouseMotion(event);
    return;
  }

  // Handle a shift-click when the text editor is active.
  if (GetActiveCell() && event.ShiftDown()) {
    // We were within an input cell when the selection has started.
    m_clickType = CLICK_TYPE_INPUT_SELECTION;

    // The mouse selection was started in the currently active EditorCell
    GetActiveCell()->MouseSelectionStartedHere();

    // Set a fake starting point for the selection that is inside the cell the
    // selection started in.
    int startingChar = GetActiveCell()->GetCaretPosition();
    if (GetActiveCell()->SelectionActive())
      startingChar = GetActiveCell()->GetSelectionStart();
    m_down = wxPoint(GetActiveCell()->PositionToPoint(startingChar));
    GetActiveCell()->SelectNone();
    // Handle the mouse pointer position
    OnMouseMotion(event);

    // Did we shift-click in the currently active cell?

    return;
  }

  // Handle a shift-click when the hCaret is active
  if (m_hCaretPosition && event.ShiftDown()) {
    // We were selecting group cells when the shift-click happened.
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    // Set a fake starting point for the selection that is inside the cell the
    // selection started in.
    m_down = wxPoint(m_hCaretPosition->GetRect().GetLeft(),
                     m_hCaretPosition->GetRect().GetBottom() + 1);
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
  SetActiveCell(NULL);

  wxRect rect;
  GroupCell *previous = NULL;
  const GroupCell *clickedBeforeGC = NULL;
  GroupCell *clickedInGC = NULL;
  for (GroupCell &cell : OnList(GetTree())) { // go through all groupcells
    rect = cell.GetRect();
    if (m_down.y < rect.GetTop()) {
      clickedBeforeGC = &cell;
      break;
    } else if (m_down.y <= rect.GetBottom()) {
      clickedInGC = &cell;
      break;
    }
    previous = &cell;
  }

  if (clickedBeforeGC) { // we clicked between groupcells, set hCaret
    SetHCaret(previous);
    m_clickType = CLICK_TYPE_GROUP_SELECTION;

    // The click will has changed the position that is in focus so we assume
    // the user wants to work here and doesn't want the evaluation mechanism
    // to automatically follow the evaluation any more.
    ScrolledAwayFromEvaluation(true);
  } else if (clickedInGC) {
    ScrolledAwayFromEvaluation(true);
    OnMouseLeftInGc(event, clickedInGC);
  }

  else { // we clicked below last groupcell (both clickedInGC and
    // clickedBeforeGC == NULL)
    // set hCaret (or activate last cell?)
    SetHCaret(GetLastCellInWorksheet());
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    ScrolledAwayFromEvaluation(true);
  }
  RequestRedraw(clickedInGC);
  // Re-calculate the table of contents
  UpdateTableOfContents();
}

GroupCell *Worksheet::FirstVisibleGC() {
  wxPoint point;
  CalcUnscrolledPosition(0, 0, &point.x, &point.y);

  for (auto &cell : OnList(GetTree())) { // go through all groupcells
    wxRect rect = cell.GetRect();

    if (point.y < rect.GetBottom())
      return &cell;
  }
  return {};
}

void Worksheet::OnMouseLeftUp(wxMouseEvent &event) {
  event.Skip();
  m_cellPointers.ResetSearchStart();
  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  AnimationCell * const animCell = GetSelectedAnimation();
  if (animCell != NULL)
    animCell->ToggleAnimationRunning();

  m_leftDown = false;
  m_mouseDrag = false;
  m_clickInGC =
    NULL; // pointer to NULL to prevent crashes if the cell is deleted
  m_clickType = CLICK_TYPE_NONE;
  CheckUnixCopy();
  SetFocus();
  m_cellPointers.ResetMouseSelectionStart();
}

void Worksheet::OnMouseWheel(wxMouseEvent &event) {
  if (event.GetModifiers() & wxMOD_CONTROL) {
    wxCommandEvent *zoomEvent = new wxCommandEvent;
    zoomEvent->SetEventType(wxEVT_MENU);
    zoomEvent->SetId((event.GetWheelRotation() > 0) ? wxID_ZOOM_IN
                     : wxID_ZOOM_OUT);
    GetParent()->GetEventHandler()->QueueEvent(zoomEvent);
    return;
  }
  if(CanAnimate())
    {
      auto *animation = m_cellPointers.m_selectionStart.CastAs<AnimationCell *>();
      animation->AnimationRunning(false);
      auto displayedIndex = animation->GetDisplayedIndex();
      if (event.GetWheelRotation() > 0)
        {
          if(displayedIndex < animation->Length() - 1)
            animation->SetDisplayedIndex(displayedIndex + 1);
        }
      else
        {
          if(displayedIndex > 0)
            animation->SetDisplayedIndex(displayedIndex - 1);
        }
      wxRect rect = animation->GetRect();
      RequestRedraw(rect);
    }
  else
    {
      event.Skip();
      // The wheel scrolls the document, but (unlike the scrollbar) does not emit
      // a wxEVT_SCROLLWIN_*/wxEVT_SCROLL_CHANGED event, so without this the
      // "follow evaluation" button would only light up minutes later, when some
      // unrelated scroll event finally fired. Run the same check the scrollbar
      // handlers do so scrolling the running evaluation out of view enables the
      // button right away.
      CallAfter(&Worksheet::CheckIfActiveCellScrolledOut);
    }
}

void Worksheet::OnMouseMotion(wxMouseEvent &event) {
  event.Skip();
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_pointer_x,
                         &m_pointer_y);
  m_mouseMotionWas = true;
  m_updateControls = true;
  if (!GetTree() || !m_leftDown)
    return;

  m_mouseDrag = true;
  m_up.x = m_pointer_x;
  m_up.y = m_pointer_y;
  if (m_mouseOutside) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
  ClickNDrag(m_down, m_up);
}

void Worksheet::SelectGroupCells(wxPoint down, wxPoint up) {
  // Calculate the rectangle that has been selected
  int ytop = std::min(down.y, up.y);
  int ybottom = std::max(down.y, up.y);
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = nullptr;

  wxRect rect;

  // find out the group cell the selection begins in
  GroupCell *startSearch = GetTree();
  for (auto &cell : OnList(startSearch)) {
    if (ytop <= cell.GetRect().GetBottom()) {
      m_cellPointers.m_selectionStart = &cell;
      break;
    }
  }

  // find out the group cell the selection ends in
  GroupCell *endSearch = m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
  if (!endSearch) endSearch = GetTree();
  for (auto &cell : OnList(endSearch)) {
    rect = cell.GetRect();
    if (ybottom < rect.GetTop()) {
      m_cellPointers.m_selectionEnd = cell.GetPrevious();
      break;
    }
  }
  if (m_cellPointers.m_selectionStart && !m_cellPointers.m_selectionEnd)
    m_cellPointers.m_selectionEnd = GetLastCellInWorksheet();

  if (m_cellPointers.m_selectionStart) {
    if (m_cellPointers.m_selectionEnd->GetNext() ==
        m_cellPointers.m_selectionStart) {
      SetHCaret(m_cellPointers.m_selectionEnd.CastAs<GroupCell *>());
    } else {
      m_hCaretActive = false;
      m_hCaretPosition = NULL;
    }
  } else {
    m_hCaretActive = true;
    m_hCaretPosition = GetLastCellInWorksheet();
  }

  if (down.y > up.y) {
    m_hCaretPositionStart =
      m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
    m_hCaretPositionEnd = m_cellPointers.m_selectionEnd.CastAs<GroupCell *>();
  } else {
    m_hCaretPositionStart = m_cellPointers.m_selectionEnd.CastAs<GroupCell *>();
    m_hCaretPositionEnd = m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
  }
  SetSelection(m_cellPointers.m_selectionStart, m_cellPointers.m_selectionEnd);
}

GroupCell *Worksheet::GetLastCellInWorksheet() const
{
  if (m_last) return m_last;
  GroupCell *last = GetTree();
  if (last)
    last = last->last();
  m_last = last;
  return m_last;
}

void Worksheet::ClickNDrag(wxPoint down, wxPoint up) {
  const Cell *selectionStartOld = m_cellPointers.m_selectionStart,
    *selectionEndOld = m_cellPointers.m_selectionEnd;
  wxRect rect;

  switch (m_clickType) {
  case CLICK_TYPE_NONE:
    return;

  case CLICK_TYPE_INPUT_SELECTION:
    if (!m_cellPointers.m_cellMouseSelectionStartedIn)
      return;

    {
      rect = m_cellPointers.m_cellMouseSelectionStartedIn->GetRect();

      // Let's see if we are still inside the cell we started selecting in.
      if (!rect.Inflate(m_configuration->GetGroupSkip(),
                        m_configuration->GetGroupSkip()).Contains(up)) {
        // If we have just started selecting GroupCells we have to unselect
        // the already-selected text in the cell we have started selecting in.
        if (GetActiveCell()) {
          GetActiveCell()->SelectNone();
          SetActiveCell(NULL);
        }

        // we have left the cell we started to select in =>
        // select all group cells between start and end of the selection.
        SelectGroupCells(up, down);
      } else {
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
      m_cellPointers.m_selectionString.Clear();
      ClearSelection();
      rect.x = std::min(down.x, up.x);
      rect.y = std::min(down.y, up.y);
      rect.width = std::max(abs(down.x - up.x), 1);
      rect.height = std::max(abs(down.y - up.y), 1);

      if (m_clickInGC) {
        auto const [first, last] = m_clickInGC->GetCellsInOutputRect(rect, down, up);
        m_cellPointers.m_selectionStart = first;
        m_cellPointers.m_selectionEnd = last;
        
        Cell *cell = first;
        while(cell)
          {
            m_cellPointers.m_selectionString.Append(cell->ToString());
            if(cell == last)
              break;
            cell = cell->GetNext();
          }
      }
      break;

    default:
      break;
    } // end switch

  // Refresh only if the selection has changed
  if ((selectionStartOld != m_cellPointers.m_selectionStart) ||
      (selectionEndOld != m_cellPointers.m_selectionEnd))
    RequestRedraw();
}

/***
 * Get the string representation of the selection
 */
wxString Worksheet::GetString(bool lb) const {
  if (!m_cellPointers.m_selectionStart)
    return GetActiveCell() ? GetActiveCell()->ToString() : wxString{};

  wxString s;
  for (const Cell &cell : OnDrawList(m_cellPointers.m_selectionStart.get())) {
    if (lb && cell.BreakLineHere() && !s.empty())
      s += wxS('\n');
    s += cell.ToString();
    if (&cell == m_cellPointers.m_selectionEnd)
      break;
  }
  return s;
}

/***
 * Copy selection to clipboard.
 */
bool Worksheet::Copy(bool astext) const {
  if (GetActiveCell())
    return GetActiveCell()->CopyToClipboard();

  if (!m_cellPointers.m_selectionStart)
    return false;

  if (!astext && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP)
    return CopyCells();

  // If the selection is IMAGE or ANIMATION, copy it to clipboard
  // as image.
  if (m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
      (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE ||
       m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE)) {
    m_cellPointers.m_selectionStart->CopyToClipboard();
    return true;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    wxDataObjectComposite *data = new wxDataObjectComposite;

    // Add the wxm code corresponding to the selected output to the clipboard
    wxString s = GetString(true);
    data->Add(new wxmDataObject(s));

    std::unique_ptr<Cell> cell(CopySelection());

    if (m_configuration->CopyMathML()) {
      // Add a mathML representation of the data to the clipboard
      s = ConvertSelectionToMathML();
      if (s != wxEmptyString) {
        // We mark the MathML version of the data on the clipboard as
        // "preferred" as if an application supports MathML neither bitmaps nor
        // plain text makes much sense.
        data->Add(new MathMLDataObject(s), true);
        data->Add(new MathMLDataObject2(s), true);
        if (m_configuration->CopyMathMLHTML())
          data->Add(new wxHTMLDataObject(s), true);
        // wxMathML is a HTML5 flavour, as well.
        // See
        // https://github.com/fred-wang/Mathzilla/blob/master/mathml-copy/lib/copy-mathml.js#L21
        //
        // Unfortunately MS Word and Libreoffice Writer don't like this idea so
        // I have disabled the following line of code again:
        //
        // data->Add(new wxHTMLDataObject(s));
      }
    }

    if (m_configuration->CopyRTF()) {
      // Add a RTF representation of the currently selected text
      // to the clipboard: For some reason Libreoffice likes RTF more than
      // it likes the MathML - which is standardized.
      wxString rtf;
      rtf = RTFStart() + cell->ListToRTF() + wxS("\\par\n") + RTFEnd();
      data->Add(new RtfDataObject(rtf));
      data->Add(new RtfDataObject2(rtf), true);
    }

    // Add a string representation of the selected output to the clipboard
    s = cell->ListToString();
    data->Add(new wxTextDataObject(s));

    if (m_configuration->CopyBitmap()) {
      // Try to fill bmp with a high-res version of the cells
      BitmapOut output(&m_configuration, std::move(cell),
                       m_configuration->BitmapScale(),
                       1000000 * m_configuration->MaxClipbrdBitmapMegabytes());
      if (output.IsOk())
        data->Add(output.GetDataObject().release());
    }
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

wxString Worksheet::ConvertSelectionToMathML() const {
  if (GetActiveCell())
    return {};

  if (!m_cellPointers.m_selectionStart || !m_cellPointers.m_selectionEnd)
    return {};

  wxString s;
  std::unique_ptr<Cell> tmp(CopySelection(m_cellPointers.m_selectionStart,
                                          m_cellPointers.m_selectionEnd, true));

  s = wxString(wxS("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n")) +
    wxS("<semantics>") + tmp->ListToMathML(true) +
    wxS("</semantics>") + wxS("</math>");

  // We might add indentation as additional eye candy to all but extremely long
  // xml data chunks.
  if (s.Length() < 1000000) {
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

    // If we failed to load the document the word processor will most probably
    // fail, too. But we can still put it into the clipboard for debugging
    // purposes.
    if (doc.IsOk()) {
      wxMemoryOutputStream ostream;
      doc.Save(ostream);
      s = wxString::FromUTF8(
                             reinterpret_cast<char *>(ostream.GetOutputStreamBuffer()->GetBufferStart()),
                             ostream.GetOutputStreamBuffer()->GetBufferSize());

      // Now the string has a header we want to get rid of again.
      auto pos = s.Find("\n");
      wxASSERT(pos >= 0);
      if(pos >= 0)
        s = s.SubString(static_cast<std::size_t>(pos) + 1, s.Length());
    }
  }
  return s;
}

bool Worksheet::CopyMathML() const {
  wxString s = ConvertSelectionToMathML();

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));

  if (wxTheClipboard->Open()) {
    wxDataObjectComposite *data = new wxDataObjectComposite;
    // The default clipboard slot for MathML
    data->Add(new MathMLDataObject(s), true);
    data->Add(new MathMLDataObject2(s), true);
    // A fallback for communicating with non-mathML-aware programs
    data->Add(new wxTextDataObject(s));
    // wxMathML is a HTML5 flavour, as well.
    // See
    // https://github.com/fred-wang/Mathzilla/blob/master/mathml-copy/lib/copy-mathml.js#L21
    //
    // The \0 tries to work around a strange bug in wxWidgets that sometimes
    // makes string endings disappear
    data->Add(new wxHTMLDataObject(s + wxS('\0')));
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

bool Worksheet::CopyMatlab() const {
  if (GetActiveCell())
    return false;

  if (!m_cellPointers.m_selectionStart)
    return false;

  wxString result;
  bool firstcell = true;
  for (const Cell &tmp : OnList(m_cellPointers.m_selectionStart.get())) {
    if (tmp.HasHardLineBreak() && !firstcell)
      result += wxS("\n");
    result += tmp.ToMatlab();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
    firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(result));
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

bool Worksheet::CopyTeX() const {
  if (GetActiveCell())
    return false;

  if (!m_cellPointers.m_selectionStart)
    return false;

  Cell *const start = m_cellPointers.m_selectionStart;
  bool inMath = false;
  wxString s;

  if (start->GetType() != MC_TYPE_GROUP) {
    inMath = m_configuration->WrapLatexMath();
    if (inMath)
      s = wxS("\\[");
  }
  for (const Cell &tmp : OnList(start)) {
    s += tmp.ToTeX();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
  }
  if (inMath)
    s += wxS("\\]");

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyText() const {
  if (GetActiveCell())
    return false;

  if (!m_cellPointers.m_selectionStart)
    return false;

  wxString result;
  bool firstcell = true;
  for (const Cell &tmp : OnList(m_cellPointers.m_selectionStart.get())) {
    if (tmp.HasHardLineBreak() && !firstcell)
      result += wxS("\n");
    result += tmp.ToString();
    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
    firstcell = false;
  }

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(result));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CopyCells() const {
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (!m_cellPointers.m_selectionStart)
    return false;

  if (wxTheClipboard->Open()) {
#if wxUSE_ENH_METAFILE
    auto *data = new CompositeDataObject;
#else
    wxDataObjectComposite *data = new wxDataObjectComposite;
#endif
    wxString wxm;
    wxString str;
    wxString rtf = RTFStart();

    const GroupCell *const end = m_cellPointers.m_selectionEnd->GetGroup();
    bool firstcell = true;
    for (auto &tmp : OnList(m_cellPointers.m_selectionStart->GetGroup())) {
      if (!firstcell)
        str += wxS("\n");
      str += tmp.ToString();
      firstcell = false;

      if (m_configuration->CopyRTF())
        rtf += tmp.ToRTF();
      wxm += Format::TreeToWXM(&tmp);

      if (&tmp == end)
        break;
    }

    rtf += wxS("\\par") + RTFEnd();

    if (m_configuration->CopyRTF()) {
      data->Add(new RtfDataObject(rtf), true);
      data->Add(new RtfDataObject2(rtf));
    }
    data->Add(new wxTextDataObject(str));
    data->Add(new wxmDataObject(wxm));

    if (m_configuration->CopyBitmap()) {
      std::unique_ptr<BitmapOut> output(new BitmapOut(&m_configuration, CopySelection(),
                                                      m_configuration->BitmapScale(),
                                                      1000000 * m_configuration->MaxClipbrdBitmapMegabytes()));
      if (output->IsOk())
        data->Add(output->GetDataObject().release());
    }

#if wxUSE_ENH_METAFILE
    if (m_configuration->CopyEMF()) {
      std::unique_ptr<Emfout> emf(new Emfout(&m_configuration, CopySelection()));
      if (emf->IsOk())
        data->Add(emf->GetDataObject().release());
    }
#endif
    if (m_configuration->CopySVG()) {
      std::unique_ptr<Svgout> svg(new Svgout(&m_configuration, CopySelection()));
      if (svg->IsOk())
        data->Add(svg->GetDataObject().release());
    }

    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool Worksheet::CanDeleteSelection() const {
  if (!m_cellPointers.HasCellsSelected())
    return false;

  return CanDeleteRegion(m_cellPointers.m_selectionStart->GetGroup(),
                         m_cellPointers.m_selectionEnd->GetGroup());
}

void Worksheet::DeleteSelection() {
  DeleteRegion(m_cellPointers.m_selectionStart->GetGroup(),
               m_cellPointers.m_selectionEnd->GetGroup());
  TreeUndo_ClearRedoActionList();
  m_cellPointers.m_selectionStart = m_cellPointers.m_selectionEnd = nullptr;
  RequestRedraw();
}

void Worksheet::DeleteCurrentCell() {
  GroupCell *cellToDelete = NULL;
  if (m_hCaretActive)
    cellToDelete = m_hCaretPosition;
  if (GetActiveCell())
    cellToDelete = GetActiveCell()->GetGroup();

  if ((!cellToDelete) && (GetSelectionStart()) && (GetSelectionEnd()) &&
      (GetSelectionStart()->GetGroup() == GetSelectionEnd()->GetGroup())) {
    cellToDelete = GetSelectionStart()->GetGroup();
  }
  if (cellToDelete)
    DeleteRegion(cellToDelete, cellToDelete);
}

bool Worksheet::CanDeleteRegion(GroupCell *start, const GroupCell *end) const {
  if (!start || !end)
    return false;

  // We refuse deletion of a cell we are planning to evaluate
  for (const GroupCell &tmp : OnList(start)) {
    // We refuse deletion of a cell maxima is currently evaluating
    if (&tmp == GetWorkingGroup())
      return false;

    if (&tmp == end)
      return true;
  }

  return true;
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *parentOfStart,
                                          GroupCell *end) {
  TreeUndo_MarkCellsAsAdded(parentOfStart, end, &m_treeUndo.UndoStack());
}

void Worksheet::TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end,
                                          UndoActions *undoBuffer) {
  undoBuffer->emplace_front(start, end);
  TreeUndo_LimitUndoBuffer();
}

void Worksheet::TreeUndo_ClearBuffers() {
  m_treeUndo.ClearBuffers();
}

void Worksheet::TreeUndo_CellLeft() {
  // If no cell is active we didn't leave a cell and return from this function.
  if (!GetActiveCell())
    return;

  GroupCell *activeCell = GetActiveCell()->GetGroup();
  if (!activeCell)
    return;

  if (!activeCell->GetEditable())
    return;

  m_treeUndo.CellLeft(activeCell, activeCell->GetEditable()->GetValue(),
                      m_configuration->UndoLimit());
}

void Worksheet::TreeUndo_CellEntered() {
  if (!GetActiveCell() || !GetActiveCell()->GetGroup())
    return;

  m_treeUndo.CellEntered(GetActiveCell()->GetGroup(), GetActiveCell()->GetValue(),
                         static_cast<long long>(GetActiveCell()->SelectionStart()),
                         static_cast<long long>(GetActiveCell()->SelectionEnd()));
}

void Worksheet::SetCellStyle(GroupCell *group, GroupType style) {
  if (!group)
    return;

  wxString cellContents;
  if (group->GetEditable())
    cellContents = group->GetEditable()->GetValue();
  auto newGroupCell = std::make_unique<GroupCell>(m_configuration, style);
  newGroupCell->GetEditable()->SetValue(cellContents);
  GroupCell *prev = group->GetPrevious();
  DeleteRegion(group, group);
  TreeUndo_AppendAction();
  auto *editable = newGroupCell->GetEditable();
  InsertGroupCells(std::move(newGroupCell), prev);
  SetActiveCell(editable);
  SetSaved(false);
  Recalculate();
  RequestRedraw();
}

void Worksheet::DeleteRegion(GroupCell *start, GroupCell *end) {
  DeleteRegion(start, end, &m_treeUndo.UndoStack());
}

void Worksheet::DeleteRegion(GroupCell *start, GroupCell *end,
                             UndoActions *undoBuffer) {
  m_cellPointers.ResetSearchStart();
  if (!end)
    return;

  // Abort deletion if there is no valid selection or if we cannot
  // delete it.
  if (!CanDeleteRegion(start, end))
    return;

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  //! Set the cursor to a sane place
  SetActiveCell(NULL);
  ClearSelection();
  SetHCaret(start->GetPrevious());

  // check if chapters or sections need to be renumbered
  bool renumber = false;
  for (auto &tmp : OnList(start)) {
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

  auto tornOut = CellList::TearOut(start, end);
  m_last = nullptr;
  if (!tornOut.cellOwner) {
    wxASSERT(m_tree.get() == tornOut.cell);
    tornOut.cellOwner = std::move(m_tree);
    m_tree = dynamic_unique_ptr_cast<GroupCell>(std::move(tornOut.tailOwner));
  }

  // Do we have an undo buffer for this action?
  if (undoBuffer) {
    // We have an undo buffer => add the deleted cells there
    auto cells =
      static_unique_ptr_cast<GroupCell>(std::move(tornOut.cellOwner));
    undoBuffer->emplace_front(cellBeforeStart, nullptr, cells.release());
    TreeUndo_LimitUndoBuffer();
  }

  if (renumber)
    NumberSections();
  UpdateTableOfContents();
  Recalculate();
  if(GetTree())
    GetTree()->UpdateYPositionList();
  RequestRedraw();
  SetSaved(false);
}

void Worksheet::SetAnswer(const wxString &answer) {
  GroupCell *answerCell = GetWorkingGroup();
  if (!answerCell || answer.empty() || m_lastQuestion.empty())
    return;

  answerCell->SetAnswer(m_lastQuestion, answer);
}

bool Worksheet::OpenQuestionCaret(const wxString &txt) {
  GroupCell *group = GetWorkingGroup(true);
  wxASSERT_MSG(group, _("Bug: Got a question but no cell to answer it in"));
  if (!group)
    return false;

  // We are leaving the input part of the current cell in this step.
  TreeUndo_CellLeft();

  // We don't need an undo action for the thing we will do now.
  m_treeUndo.ForgetActiveCell();

  // Make sure that the cell containing the question is visible
  if (group->RevealHidden()) {
    FoldOccurred();
    Recalculate(group);
  }

  bool autoEvaluate = false;
  // If we still haven't a cell to put the answer in we now create one.
  if (!m_cellPointers.m_answerCell) {
    auto answerCell = std::make_unique<EditorCell>(group, m_configuration);
    m_cellPointers.m_answerCell = answerCell;
    answerCell->SetType(MC_TYPE_INPUT);

    if (!txt.empty())
      answerCell->SetValue(txt);
    else {
      auto const &text = group->GetAnswer(m_lastQuestion);
      if (!text.empty())
        autoEvaluate = group->AutoAnswer();
      answerCell->SetValue(text);
    }
    answerCell->CaretToEnd();

    group->AppendOutput(std::move(answerCell));
    m_configuration->SetAdjustWorksheetSizeNeeded(true);
    Recalculate(group);
  }

  // If we filled in an answer and "AutoAnswer" is true we issue an evaluation
  // event here.
  if (autoEvaluate) {
    wxMenuEvent *EvaluateEvent =
      new wxMenuEvent(wxEVT_MENU, EventIDs::menu_evaluate);
    GetParent()->GetEventHandler()->QueueEvent(EvaluateEvent);
  }

  // If the user wants to be automatically scrolled to the cell evaluation takes
  // place we scroll to this cell.
  if (FollowEvaluation() || autoEvaluate)
    SetActiveCell(m_cellPointers.m_answerCell);

  RequestRedraw();
  return autoEvaluate;
}

void Worksheet::OpenHCaret(const wxString &txt, GroupType type) {
  CloseAutoCompletePopup();

  // if we are inside cell maxima is currently evaluating
  // bypass normal behaviour and insert an EditorCell into
  // the output of the working group.
  if (GetWorkingGroup() && m_questionPrompt) {
    if ((GetActiveCell() && GetActiveCell()->GetGroup() == GetWorkingGroup()) ||
        (m_hCaretPosition &&
         m_hCaretPosition == GetWorkingGroup()->GetNext())) {
      OpenQuestionCaret(txt);
      return;
    }
  }
  // set m_hCaretPosition to a sensible value
  if (GetActiveCell()) {
    SetHCaret(GetActiveCell()->GetGroup());
  } else if (m_cellPointers.m_selectionStart)
    SetHCaret(m_cellPointers.m_selectionStart->GetGroup());

  if (!m_hCaretActive) {
    if (!GetLastCellInWorksheet())
      return;
    SetHCaret(GetLastCellInWorksheet());
  }

  // insert a new group cell
  auto group = std::make_unique<GroupCell>(m_configuration, type, txt);
  // check how much to unfold for this type
  if (m_hCaretPosition) {
    while (IsLesserGCType(type, m_hCaretPosition->GetGroupType())) {
      GroupCell *result = m_hCaretPosition->Unfold();
      if (!result) // assumes that unfold sets hcaret to the end of unfolded
        // cells
        break;     // unfold returns NULL when it cannot unfold
      SetHCaret(result);
    }
  }
  if (type == GC_TYPE_CODE && !m_configuration->ShowCodeCells()) {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }

  auto *editable = group->GetEditable();
  InsertGroupCells(std::move(group), m_hCaretPosition);
  // activate the editor
  SetActiveCell(editable);
  Recalculate(editable->GetGroup());
  RequestRedraw();
  // If we just have started typing inside a new cell we don't want the screen
  // to scroll away.
  ScrolledAwayFromEvaluation();

  // Here we tend to get unacceptably long delays before the display is
  // refreshed by the idle loop => Trigger the refresh manually.
  ForceRedraw();
}

void Worksheet::Evaluate() {
  wxMenuEvent *EvaluateEvent =
    new wxMenuEvent(wxEVT_MENU, EventIDs::menu_evaluate);
  GetParent()->GetEventHandler()->QueueEvent(EvaluateEvent);
}

/***
 * Support for copying and deleting with keyboard
 */
void Worksheet::OnKeyDown(wxKeyEvent &event) {
  BTextCtrl::ForgetLastActive();
  m_updateControls = true;
  m_configuration->SetAdjustWorksheetSizeNeeded(true);
  ClearNotification();
  // Track the activity of the keyboard. Setting the keyboard
  // to inactive again is done in wxMaxima.cpp
  m_keyboardInactiveTimer.StartOnce(10000);

  // If Alt and Ctrl are down at the same time we are almost entirely sure that
  // this is a hotkey we need to pass to the main application. One exception is
  // curly brackets in - I think it was france.
  if (event.ControlDown() && event.AltDown()) {
    if ((event.GetUnicodeKey() == wxS('{')) ||
        (event.GetUnicodeKey() == wxS('}'))) {
      event.Skip();
      return;
    }
  }

  if (m_autocompletePopup) {
    m_autocompletePopup->OnKeyDown(event);
    event.Skip();
    return;
  }

  // Alt+Up and Alt+Down are hotkeys, too.
  if (event.AltDown() &&
      (event.GetKeyCode() == WXK_UP || event.GetKeyCode() == WXK_DOWN)) {
    event.Skip();
    return;
  }

  // Handling of the keys this class has to handle
  switch (event.GetKeyCode()) {
  case WXK_DELETE:
  case WXK_NUMPAD_DELETE:
    if (event.ShiftDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_CUT);
      GetParent()->ProcessWindowEvent(ev);
    } else if (CanDeleteSelection()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, EventIDs::popid_delete);
      GetParent()->ProcessWindowEvent(ev);
    } else
      event.Skip();
    break;

  case WXK_INSERT:
    if (event.ControlDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_COPY);
      GetParent()->ProcessWindowEvent(ev);
    } else if (event.ShiftDown()) {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, wxID_PASTE);
      GetParent()->ProcessWindowEvent(ev);
    } else
      event.Skip();
    break;

  case WXK_BACK:
    if ((event.ControlDown()) && (event.ShiftDown()))
      DeleteCurrentCell();
    else {
      if (CanDeleteSelection()) {
        wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, EventIDs::popid_delete);
        GetParent()->ProcessWindowEvent(ev);
      } else
        event.Skip();
    }
    break;

  case WXK_NUMPAD_ENTER:
    if (m_configuration->NumpadEnterEvaluates()) {
      Evaluate();
      break;
    }
    // fallthrough
  case WXK_RETURN:
    // If Ctrl+Shift are pressed at the same time this is an evaluate event.
    if (event.ControlDown() && event.ShiftDown()) {
      // Queue an evaluate event for the window containing this worksheet.
      wxCommandEvent *evaluateEvent = new wxCommandEvent;
      evaluateEvent->SetEventType(wxEVT_MENU);
      evaluateEvent->SetId(EventIDs::popid_evaluate_section);
      GetParent()->GetEventHandler()->QueueEvent(evaluateEvent);
    } else {
      bool enterEvaluates = false;
      bool controlOrShift = event.ControlDown() || event.ShiftDown();
      wxConfig::Get()->Read(wxS("enterEvaluates"), &enterEvaluates);
      if (enterEvaluates !=
          controlOrShift) { // shift-enter pressed === EventIDs::menu_evaluate event
        Evaluate();
      } else
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

bool Worksheet::GCContainsCurrentQuestion(const GroupCell *cell) {
  return GetWorkingGroup() && cell == GetWorkingGroup() && m_questionPrompt;
}

void Worksheet::QuestionAnswered() {
  if (m_cellPointers.m_answerCell || m_questionPrompt) {
    SetActiveCell(NULL);
    GroupCell *wg = GetWorkingGroup(true);
    if (wg) {
      SetHCaret(wg);
      ScrollToCaret();
    }
  }
  m_cellPointers.m_answerCell = nullptr;
  m_questionPrompt = false;
}

void Worksheet::UpdateScrollPos() {
  if (m_newxPosition > 0) {
    SetScrollPos(wxHORIZONTAL, m_newxPosition);
  }
  if (m_newyPosition > 0) {
    SetScrollPos(wxVERTICAL, m_newyPosition);
  }
  m_newyPosition = -1;
  m_newxPosition = -1;
}

GroupCell *Worksheet::StartOfSectioningUnit(GroupCell *start) {
  wxASSERT(start);
  if(start == NULL)
    return start;
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

GroupCell *Worksheet::EndOfSectioningUnit(GroupCell *start) {
  wxASSERT(start);
  const GroupCell *sectionbegin = StartOfSectioningUnit(start);

  // Begin with the cell after the start cell - that might contain a section
  // start of any sorts.
  GroupCell *end = start;
  if((end) && (end->GetNext()))
    end = end->GetNext();

  // Find the end of the chapter/section/...
  if(end)
    {
      int endgrouptype = sectionbegin->GetGroupType();
      while (end->GetNext() && IsLesserGCType(end->GetGroupType(), endgrouptype))
        end = end->GetNext();
    }
  return end;
}

void Worksheet::UpdateConfigurationClientSize() {
  m_configuration->SetCanvasSize(GetClientSize());
}

/****
 * OnCharInActive is called when we have a wxKeyEvent and
 * an EditorCell is active.
 *
 * OnCharInActive sends the event to the active EditorCell
 * and then updates the window.
 */
void Worksheet::OnCharInActive(wxKeyEvent &event) {
  bool needRecalculate = false;
  // Set once the IsDirty() path below has issued a full redraw of the active
  // group, so the per-cell refresh further down can skip a redundant second
  // repaint (see the note there).
  bool fullyRedrawn = false;

  if ((event.GetKeyCode() == WXK_UP || event.GetKeyCode() == WXK_PAGEUP) &&
      GetActiveCell()->CaretAtStart()) {
    // Get the first previous cell that isn't hidden
    GroupCell *previous = GetActiveCell()->GetGroup()->GetPrevious();
    while (previous && (previous->GetMaxDrop() == 0))
      previous = dynamic_cast<GroupCell *>(previous->GetPrevious());

    if (event.ShiftDown()) {
      SetSelection(previous, GetActiveCell()->GetGroup());
      m_hCaretPosition = m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
      m_hCaretPositionEnd =
        m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
      m_hCaretPositionStart =
        m_cellPointers.m_selectionEnd.CastAs<GroupCell *>();

      GetActiveCell()->KeyboardSelectionStartedHere();
      GetActiveCell()->SelectNone();
      SetActiveCell(NULL);
      RequestRedraw(m_hCaretPosition);
    } else {
      if (GCContainsCurrentQuestion(previous)) {
        // The user moved into the cell maxima has asked a question in.
        FollowEvaluation(true);
        OpenQuestionCaret();
        return;
      } else
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

  if ((event.GetKeyCode() == WXK_DOWN || event.GetKeyCode() == WXK_PAGEDOWN) &&
      GetActiveCell()->CaretAtEnd()) {
    // Get the first next cell that isn't hidden
    GroupCell *start = GetActiveCell()->GetGroup();
    while (start && start->GetNext() && start->GetNext()->GetMaxDrop() == 0)
      start = start->GetNext();

    if (event.ShiftDown()) {
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
    } else {
      if (GCContainsCurrentQuestion(start)) {
        // The user moved into the cell maxima has asked a question in.
        FollowEvaluation(true);
        OpenQuestionCaret();
        return;
      } else
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
  if ((event.GetKeyCode() == WXK_BACK || event.GetKeyCode() == WXK_DELETE ||
       event.GetKeyCode() == WXK_NUMPAD_DELETE) &&
      GetActiveCell()->GetValue() == wxEmptyString) {
    SetSelection(GetActiveCell()->GetGroup());
    DeleteSelection();
    return;
  }

  ///
  /// send event to active cell
  ///
  const EditorCell *activeCell = GetActiveCell();
  wxString oldValue = activeCell->GetValue();
  int oldHeight = activeCell->GetHeight();

  switch (event.GetKeyCode()) {
  case WXK_LEFT:
    if (GetActiveCell()->CaretAtStart()) {
      GroupCell *newGroup = GetActiveCell()->GetGroup()->GetPrevious();
      while (newGroup && newGroup->GetMaxDrop() == 0)
        newGroup = newGroup->GetPrevious();
      SetHCaret(newGroup);
      return;
    } else
      GetActiveCell()->ProcessEvent(event);
    break;
  case WXK_RIGHT:
    if (GetActiveCell()->CaretAtEnd()) {
      GroupCell *newGroup = GetActiveCell()->GetGroup();
      while (newGroup && newGroup->GetNext() &&
             newGroup->GetNext()->GetMaxDrop() == 0)
        newGroup = newGroup->GetNext();
      SetHCaret(newGroup);
      return;
    } else
      GetActiveCell()->ProcessEvent(event);
    break;
  default: {
    GetActiveCell()->ProcessEvent(event);
  }
  }

  // Update title and toolbar in order to reflect the "unsaved" state of the
  // worksheet.
  // cppcheck-suppress knownConditionTrueFalse
  if (IsSaved() && activeCell->GetValue() != oldValue) {
    SetSaved(false);
    RequestRedraw();
  }
  // The keypress might have moved the cursor off-screen.
  ScrollToCaret();

  m_blinkDisplayCaret = true;

  if (activeCell->IsDirty()) {
    SetSaved(false);

    //   int fontsize = m_configuration->GetDefaultFontSize();
    auto fontsize = m_configuration->GetDefaultFontSize();

    GetActiveCell()->Recalculate(std::max(fontsize, MC_MIN_SIZE));

    if (oldHeight != GetActiveCell()->GetHeight()) {
      GetActiveCell()->GetGroup()->InputHeightChanged();
      AdjustSize();
      ScrollToCaret();
    }

    RequestRedraw(GetActiveCell()->GetGroup());
    RedrawIfRequested();
    fullyRedrawn = true;

    if (GetActiveCell()->GetWidth() + GetActiveCell()->GetCurrentPoint().x >=
        GetClientSize().GetWidth() - m_configuration->GetCellBracketWidth() -
            m_configuration->GetBaseIndent())
      needRecalculate = true;
  }

  /// If we need to recalculate then refresh the window
  if (needRecalculate) {
    GroupCell *group = GetActiveCell()->GetGroup();
    group->ResetSize_Recursively();
    if (GetActiveCell()->CheckChanges() &&
        (group->GetGroupType() == GC_TYPE_CODE) &&
        (GetActiveCell() == group->GetEditable()))
      group->ResetInputLabel();
    //    Recalculate(group, false);
    RequestRedraw(group);
    RedrawIfRequested();
  } else {
    if (GetActiveCell()->IsSelectionChanged()) {
      RequestRedraw(GetActiveCell()->GetGroup());
    }
    /// Otherwise refresh only the active cell
    else {
      wxRect rect;
      bool labelReset = false;
      if (GetActiveCell()->CheckChanges()) {
        GroupCell *group = GetActiveCell()->GetGroup();
        if ((group->GetGroupType() == GC_TYPE_CODE) &&
            (GetActiveCell() == group->GetEditable())) {
          group->ResetInputLabel();
          labelReset = true;
        }
        rect = group->GetRect();
        rect.width = GetVirtualSize().x;
      } else {
        rect = GetActiveCell()->GetRect();
        rect.width = GetVirtualSize().x;
      }
      rect.x -= m_configuration->GetCursorWidth() / 2;
      // A content keypress already triggered a full redraw of the group in the
      // IsDirty() path above. Repeating this partial row repaint then produced a
      // second, separate paint frame -- and when it was deferred (no immediate
      // RedrawIfRequested) it fired on a later event such as the caret-blink
      // timer, clearing and re-drawing the row out of step with the keypress.
      // That detached frame is what made short Text/Title cells' text blink in
      // sync with the cursor. So skip this repaint when the group was already
      // fully redrawn, unless a code cell's input label was just reset here
      // (which changed after that redraw and must be repainted).
      if (!fullyRedrawn || labelReset) {
        RequestRedraw(rect);
        RedrawIfRequested();
      }
    }
  }

  if (GetActiveCell()) {
    if (IsLesserGCType(GC_TYPE_TEXT,
                       GetActiveCell()->GetGroup()->GetGroupType()))
      UpdateTableOfContents();
  }
}

void Worksheet::SelectWithChar(int ccode) {
  ScrolledAwayFromEvaluation();
  // start making a selection
  // m_hCaretPositionStart is the first group selected
  // m_hCaretPositionEnd is tle last group selected
  // we always move m_hCaretPosition
  if (!m_hCaretPositionStart || !m_hCaretPositionEnd) {
    if (m_hCaretPosition)
      m_hCaretPositionStart = m_hCaretPositionEnd = m_hCaretPosition;
    else
      m_hCaretPositionStart = m_hCaretPositionEnd = GetTree();

    if (!m_hCaretPositionStart)
      return;

    if (ccode == WXK_DOWN && m_hCaretPosition &&
        m_hCaretPositionStart->GetNext()) {
      m_hCaretPositionStart = m_hCaretPositionEnd =
        m_hCaretPositionStart->GetNext();
      while (m_hCaretPositionStart &&
             m_hCaretPositionStart->GetMaxDrop() == 0 &&
             m_hCaretPositionStart->GetNext())
        m_hCaretPositionStart = m_hCaretPositionEnd =
          m_hCaretPositionStart->GetNext();
    }
  } else if (ccode == WXK_UP) {
    if (KeyboardSelectionStart() &&
        m_hCaretPositionEnd ==
        KeyboardSelectionStart()->GetGroup()->GetNext()) {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      ClearSelection();
      KeyboardSelectionStart()->ReturnToSelectionFromBot();
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
    } else {
      // extend / shorten up selection
      GroupCell *prev = m_hCaretPositionEnd->GetPrevious();
      while (prev && prev->GetMaxDrop() == 0)
        prev = prev->GetPrevious();

      if (prev) {
        if ((m_hCaretPosition) &&
            m_hCaretPosition->GetNext() == m_hCaretPositionEnd.get())
          m_hCaretPositionStart = prev;
        m_hCaretPositionEnd = prev;
      }
      if (m_hCaretPositionEnd)
        ScheduleScrollToCell(m_hCaretPositionEnd.get(), false);
    }
  } else {
    // We arrive here on WXK_DOWN.
    if (KeyboardSelectionStart() &&
        m_hCaretPositionEnd ==
        KeyboardSelectionStart()->GetGroup()->GetPrevious()) {
      // We are in the cell the selection started in
      SetActiveCell(KeyboardSelectionStart());
      ClearSelection();
      m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
      KeyboardSelectionStart()->ReturnToSelectionFromTop();
    } else {
      // extend/shorten down selection
      GroupCell *nxt = m_hCaretPositionEnd->GetNext();
      while (nxt && nxt->GetMaxDrop() == 0)
        nxt = nxt->GetNext();

      if (nxt) {
        if (m_hCaretPosition == m_hCaretPositionEnd)
          m_hCaretPositionStart = nxt;
        m_hCaretPositionEnd = nxt;
      }
      if (m_hCaretPositionEnd)
        ScheduleScrollToCell(m_hCaretPositionEnd, false);
    }
  }

  if (m_hCaretPositionStart && m_hCaretPositionEnd) {
    // m_hCaretPositionStart can be above or below m_hCaretPositionEnd
    if (m_hCaretPositionStart->GetCurrentY() <
        m_hCaretPositionEnd->GetCurrentY()) {
      SetSelection(m_hCaretPositionStart, m_hCaretPositionEnd);
    } else {
      SetSelection(m_hCaretPositionEnd, m_hCaretPositionStart);
    }
  }
  RequestRedraw();
}

void Worksheet::SelectEditable(EditorCell *editor, bool up) {
  if (editor && (m_configuration->ShowCodeCells() ||
                 editor->GetType() != MC_TYPE_INPUT)) {
    SetActiveCell(editor);
    m_hCaretActive = false;

    if (up)
      editor->CaretToStart();
    else
      editor->CaretToEnd();

    if (editor->GetWidth() == -1)
      Recalculate(editor->GetGroup());

    ScrollToCaret();
  } else { // can't get editor... jump over to the next cell..
    if (up) {
      if (!m_hCaretPosition)
        SetHCaret(GetTree());
      else {
        if (m_hCaretPosition->GetNext())
          SetHCaret(m_hCaretPosition->GetNext());
        else
          SetHCaret(GetLastCellInWorksheet());
      }
    } else
      SetHCaret(m_hCaretPosition->GetPrevious());
  }
  RequestRedraw();
}

void Worksheet::OnCharNoActive(wxKeyEvent &event) {
  int ccode = event.GetKeyCode();

  // If Shift is down we are selecting with WXK_UP and WXK_DOWN
  if (event.ShiftDown() && (ccode == WXK_UP || ccode == WXK_DOWN)) {
    SelectWithChar(ccode);
    return;
  }

  m_cellPointers.ResetKeyboardSelectionStart();

  if (m_cellPointers.m_selectionStart &&
      m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE &&
      ccode == WXK_SPACE) {
    m_cellPointers.m_selectionStart.CastAs<AnimationCell *>()->AnimationRunning(
                                                                                !m_cellPointers.m_selectionStart.CastAs<AnimationCell *>()
                                                                                ->AnimationRunning());
    return;
  }

  // Remove selection with shift+WXK_UP/WXK_DOWN
  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;

  switch (ccode) {
  case WXK_PAGEUP:
    {
      wxPoint topleft;
      int width;
      int height;
      CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
      GroupCell *CellToScrollTo = GetLastCellInWorksheet();

      GetClientSize(&width, &height);

      // Scroll as many cells as are needed to make sure that
      // the new cell is the upmost cell that begins on the new page.
      while (CellToScrollTo) {
        CellToScrollTo = CellToScrollTo->GetPrevious();

        if (CellToScrollTo &&
            CellToScrollTo->GetRect().GetTop() < (topleft.y - height))
          break;
      }
      // We want to put the cursor in the space above the cell we found.
      if (CellToScrollTo)
        CellToScrollTo = CellToScrollTo->GetPrevious();

      ScrolledAwayFromEvaluation();
      SetHCaret(CellToScrollTo);
      break;
    }
  case WXK_PAGEDOWN: {
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
    while (CellToScrollTo && CellToScrollTo != GetLastCellInWorksheet()) {
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
    // Returning to the beginning of the worksheet on pressing POS1 isn't what
    // one would expect from an ordinary editor so we ignore the key if it is.
    // pressed alone. But on pressing Ctrl+POS1 one would expect to end up at
    // the beginning of the document...

    if (event.CmdDown()) {
      GroupCell *oldCell = GetHCaret();
      SetHCaret(NULL);
      if (GetTree())
        ScheduleScrollToCell(GetTree(), true);
      if (event.ShiftDown()) {
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

    if (event.CmdDown()) {
      GroupCell *oldCell = GetHCaret();
      SetHCaret(GetLastCellInWorksheet());
      if (event.ShiftDown()) {
        if (oldCell)
          oldCell = oldCell->GetNext();
        SetSelection(oldCell, GetLastCellInWorksheet());
        m_hCaretPositionStart = oldCell;
        m_hCaretPositionEnd = GetLastCellInWorksheet();
      }
      ScrolledAwayFromEvaluation();
    }
    break;

  case WXK_BACK:
    if (m_hCaretPosition) {
      SetSelection(m_hCaretPosition);
      RequestRedraw();
      m_hCaretActive = false;
      return;
    }
    break;

  case WXK_DELETE:
  case WXK_NUMPAD_DELETE:
    if (!m_hCaretPosition) {
      if (GetTree()) {
        SetSelection(GetTree());
        m_hCaretActive = false;
        return;
      }
      ScrolledAwayFromEvaluation();
    } else if (m_hCaretPosition->GetNext()) {
      SetSelection(m_hCaretPosition->GetNext());
      m_hCaretActive = false;
      return;
    }
    break;

  case WXK_UP:
  case WXK_LEFT:
    {
      AnimationCell * const animCell = GetSelectedAnimation();
      if (animCell != NULL) {
        animCell->AnimationRunning(false);
        StepAnimation(-1);
        break;
      }
    }

    ScrolledAwayFromEvaluation(true);
    if (m_hCaretActive) {
      if (m_cellPointers.m_selectionStart) {
        if (event.CmdDown()) {
          GroupCell *tmp =
            m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
          if (tmp->GetPrevious()) {
            do
              {
                tmp = tmp->GetPrevious();
              }
            while ((tmp->GetPrevious()) &&
                   ((tmp->GetGroupType() != GC_TYPE_TITLE) &&
                    (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_SUBSECTION)));

            if (tmp->GetEditable())
              SetHCaret(tmp);
          } else {
            if (m_hCaretPosition && m_hCaretPosition->GetEditable())
              SelectEditable(tmp->GetEditable(), false);
          }
        } else
          SetHCaret(m_cellPointers.m_selectionStart->GetGroup()->GetPrevious());
      } else if (m_hCaretPosition) {
        if (event.CmdDown()) {
          GroupCell *tmp = m_hCaretPosition;
          if (tmp->GetPrevious()) {
            do
              {
                tmp = tmp->GetPrevious();
              }
            while ((tmp->GetPrevious()) &&
                   ((tmp->GetGroupType() != GC_TYPE_TITLE) &&
                    (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_SUBSECTION)));
            SetHCaret(tmp);
          } else if (tmp->GetEditable())
            SelectEditable(tmp->GetEditable(), false);
        } else {
          if (m_hCaretPosition->GetEditable())
            SelectEditable(m_hCaretPosition->GetEditable(), false);
        }
      }
      //      This allows to use WXK_UP in order to move the cursorup from the
      //      worksheet to the toolbar. But a down key doesn't go back from the
      //      toolbar to the worksheet this is more a nuisance than a feature.
      //        else
      //          event.Skip();
    } else if (m_cellPointers.m_selectionStart)
      SetHCaret(m_cellPointers.m_selectionStart->GetGroup()->GetPrevious());
    else if (!ActivatePrevInput())
      event.Skip();
    break;

  case WXK_DOWN:
  case WXK_RIGHT:
    {
      AnimationCell * const animation = GetSelectedAnimation();
      if (animation != NULL) {
        animation->AnimationRunning(false);
        StepAnimation(1);
        break;
      }
    }
    ScrolledAwayFromEvaluation(true);
    if (m_hCaretActive) {
      if (m_cellPointers.m_selectionEnd) {
        if (event.CmdDown()) {
          GroupCell *tmp = m_cellPointers.m_selectionEnd.CastAs<GroupCell *>();
          if (tmp->GetNext()) {
            do
              {
                tmp = tmp->GetNext();
              }
            while (tmp->GetNext() &&
                   (((tmp->GetGroupType() != GC_TYPE_TITLE) &&
                     (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                     (tmp->GetGroupType() != GC_TYPE_SUBSECTION)) ||
                    (tmp->GetNext()->GetMaxDrop() == 0)));
            SetHCaret(tmp);
          } else {
            SelectEditable(tmp->GetEditable(), false);
          }
        } else
          SetHCaret(m_cellPointers.m_selectionEnd.CastAs<GroupCell *>());

      } else if (m_hCaretPosition && m_hCaretPosition->GetNext()) {
        if (event.CmdDown()) {
          GroupCell *tmp = m_hCaretPosition;
          if (tmp->GetNext()) {
            do
              {
                tmp = tmp->GetNext();
              }
            while (tmp->GetNext() &&
                   ((tmp->GetGroupType() != GC_TYPE_TITLE) &&
                    (tmp->GetGroupType() != GC_TYPE_SECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_SUBSECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_SUBSUBSECTION) &&
                    (tmp->GetGroupType() != GC_TYPE_HEADING5) &&
                    (tmp->GetGroupType() != GC_TYPE_HEADING6)));
            SetHCaret(tmp);
          } else
            SelectEditable(tmp->GetEditable(), false);
        } else
          SelectEditable(m_hCaretPosition->GetNext()->GetEditable(), true);
      } else if (GetTree() && !m_hCaretPosition) {
        SelectEditable(GetTree()->GetEditable(), true);
      }

    } else if (m_cellPointers.m_selectionEnd)
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
    if ((tx == WXK_NONE) || (!wxIsprint(tx))) {
      event.Skip();
      return;
    } else {
      if (wxIsprint(tx))
        OpenHCaret(tx);
    }
  }

  RequestRedraw();
}

void Worksheet::ClearNotification() { m_notificationMessage.reset(); }

void Worksheet::SetNotification(const wxString &message, int flags) {
  if (m_windowActive)
    return;

  m_notificationMessage.emplace(wxS("wxMaxima"), message, GetParent(), flags);
  m_notificationMessage->Show();

  // In wxGTK 3.1.0 Leaving the notification message object alive until the
  // message hits its timeout causes a crash
  // (https://github.com/wxWidgets/wxWidgets/issues/17876). Let's work around this crash by
  // deleting the object as fast as we can. The crash is fixed in version 3.1.1.
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
void Worksheet::OnChar(wxKeyEvent &event) {
  /* Doing an unconditional event.Skip() here means that on any key that
     looks like navigation (up, down,...) the worksheet looses focus */
  UpdateControlsNeeded(true);
  BTextCtrl::ForgetLastActive();
  m_configuration->SetAdjustWorksheetSizeNeeded(true);
  // Alt+Up and Alt+Down are hotkeys. In order for the main application to
  // realize them they need to be passed to it using the event's Skip()
  // function.
  if (event.AltDown())
    event.Skip();

  if (m_autocompletePopup) {
    // We don't want autocompletion to be able to trigger another
    // autocompletion: On keypress Autocompletion processes the keypress and
    // issues another autocompletion that otherwise would result in an endless
    // loop.
    if (!((event.GetKeyCode() == WXK_TAB) && (event.AltDown())) &&
        !((event.GetKeyCode() == 'k') && (event.AltDown())) &&
        !((event.GetKeyCode() == 'K') && (event.AltDown())))
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
  if (event.CmdDown() && !event.AltDown()) {
    if (!(event.GetKeyCode() == WXK_LEFT) &&
        !(event.GetKeyCode() == WXK_RIGHT) && !(event.GetKeyCode() == WXK_UP) &&
        !(event.GetKeyCode() == WXK_DOWN) &&
        !(event.GetKeyCode() == WXK_BACK) &&
        !(event.GetKeyCode() == WXK_NUMPAD_DELETE) &&
        !(event.GetKeyCode() == WXK_DELETE) &&
        !(event.GetKeyCode() == WXK_HOME) && !(event.GetKeyCode() == WXK_END))
      event.Skip();
  }

  // Forward cell creation hotkeys to the class wxMaxima
  if (event.CmdDown() && !event.AltDown()) {
    if ((event.GetKeyCode() == WXK_ESCAPE) ||
        (event.GetKeyCode() == wxS('1')) || (event.GetKeyCode() == wxS('2')) ||
        (event.GetKeyCode() == wxS('3')) || (event.GetKeyCode() == wxS('4')) ||
        (event.GetKeyCode() == wxS('5')) || (event.GetKeyCode() == wxS('+')) ||
        (event.GetKeyCode() == wxS('-')) || (event.GetKeyCode() == wxS('m')) ||
        (event.GetKeyCode() == wxS('\n')) ||
        (event.GetKeyCode() == WXK_RETURN)) {
      event.Skip();
      return;
    }
  }

  // If the find dialogue is open we use the ESC key as a hotkey that closes
  // the dialogue. If it isn't it is used as part of the shortcuts for
  // entering unicode characters instead.
  if (m_findDialog && event.GetKeyCode() == WXK_ESCAPE) {
    m_findDialog->Destroy();
    m_findDialog = NULL;
    return;
  }

  if (GetActiveCell()) {
    if (event.GetKeyCode() != WXK_ESCAPE)
      OnCharInActive(event);
    else
      Autocomplete(AutoComplete::esccommand);
  } else
    OnCharNoActive(event);
}

/***
 * Get maximum x and y in the tree.
 */
void Worksheet::GetMaxPoint(int *width, int *height) {
  int currentHeight = m_configuration->GetIndent();
  *width = m_configuration->GetBaseIndent();

  if (m_maxWidth_Cached >= 0) {
    *width = std::max(*width, m_maxWidth_Cached);
  } else {
    for (Cell const &tmp : OnList(GetTree())) {
      int currentWidth =
          m_configuration->Scale_Px(m_configuration->GetIndent() +
                                    m_configuration->GetDefaultFontSize()) +
          tmp.GetWidth() +
          m_configuration->Scale_Px(m_configuration->GetIndent() +
                                    m_configuration->GetDefaultFontSize());
      *width = std::max(currentWidth, *width);
    }
    m_maxWidth_Cached = *width;
  }
  GroupCell *lastCell = GetLastCellInWorksheet();
  if (lastCell) {
    Cell *walk = lastCell;
    int extraHeight = 0;
    while (walk && (!walk->HasStaleSize() || walk->GetCurrentPoint().y < 0)) {
      if (walk->HasStaleSize()) {
        extraHeight += walk->GetMaxDrop() + m_configuration->GetGroupSkip();
      } else {
        extraHeight += m_configuration->GetGroupSkip() + 20;
      }
      walk = walk->GetPrevious();
    }
    if (walk) {
      *height = walk->GetCurrentPoint().y + walk->GetMaxDrop() + extraHeight;
    } else {
      *height = m_configuration->GetBaseIndent() + extraHeight;
    }
  } else {
    *height = currentHeight;
  }
}

/***
 * Adjust the virtual size and scrollbars.
 */
void Worksheet::AdjustSize() {
  int width = 40, height = 40;
  int virtualHeight = 40;
  int clientWidth, clientHeight;
  GetClientSize(&clientWidth, &clientHeight);
  if (GetTree()) {
    width = m_configuration->GetBaseIndent();
    height = width;

    GetMaxPoint(&width, &height);
    // when window is scrolled all the way down, document occupies top 1/8 of
    // clientHeight
    height += clientHeight - clientHeight / 8;
    virtualHeight = std::max(clientHeight + 10,
                          height); // ensure we always have VSCROLL active

    // Safety net: never shrink the virtual height below what the current
    // scroll position requires. If GetMaxPoint() is called with stale cell
    // positions (e.g. during partial recalculation), it may underestimate
    // the worksheet height. Shrinking the virtual size would cause
    // wxWidgets to clamp the scroll position, making the view jump.
    int scrollX, scrollY;
    GetViewStart(&scrollX, &scrollY);
    int currentScrollPixelY = scrollY * m_scrollUnit;
    virtualHeight = std::max(virtualHeight,
                             currentScrollPixelY + clientHeight + 10);

    // Don't set m_scrollUnit too high for big windows on hi-res screens:
    // Allow scrolling by a tenth of a line doesn't make too much sense,
    // but will make scrolling feel sluggish.
    height = GetClientSize().y;
  }
  if ((m_virtualWidth_Last != width || m_virtualHeight_Last != virtualHeight) &&
      virtualHeight > 0)
    {
    m_virtualWidth_Last = width;
    m_virtualHeight_Last = virtualHeight;
    SetVirtualSize(width, virtualHeight);
    m_scrollUnit = height / 30;
    // Ensure a sane scroll unit even for the fringe case of a very small
    // screen.
    if (m_scrollUnit < 10)
      m_scrollUnit = 10;

    SetScrollRate(m_scrollUnit, m_scrollUnit);
  }
  m_configuration->SetAdjustWorksheetSizeNeeded(false);
}

/***
 * Support for selecting cells outside display
 */
void Worksheet::OnMouseExit(wxMouseEvent &event) {
  event.Skip();
  m_mouseOutside = true;
  if (m_leftDown) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
    m_timer.Start(200, true);
  }

  // If only the bracket of the cell under the mouse pointer is shown perhaps it
  // is logical to stop displaying it if the mouse pointer is outside the
  // window. If this isn't the case I'm not against the following block being
  // deleted.
  if (m_configuration->HideBrackets()) {
    if (GetTree())
      GetTree()->CellUnderPointer(NULL);
    RequestRedraw();
  }
}

#if wxCHECK_VERSION(3, 1, 1)
void Worksheet::OnZoom(wxZoomGestureEvent &event) {
  if (event.IsGestureStart())
    m_zoomAtGestureStart = m_configuration->GetZoomFactor();

  SetZoomFactor(m_zoomAtGestureStart * pow(event.GetZoomFactor(), .2));
}
#endif

void Worksheet::OnMouseEnter(wxMouseEvent &WXUNUSED(event)) {
  m_mouseOutside = false;
}

void Worksheet::StepAnimation(int change) {
  if (GetSelectionStart() && GetSelectionStart() == GetSelectionEnd() &&
      GetSelectionStart()->GetType() == MC_TYPE_SLIDE) {
    AnimationCell *tmp =
      m_cellPointers.m_selectionStart.CastAs<AnimationCell *>();
    int pos = tmp->GetDisplayedIndex() + change;

    if (change != 0) {
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

void Worksheet::OnTimer(wxTimerEvent &event) {
  switch (event.GetId()) {
  case DISPLAY_TIMEOUT_ID: {
    RedrawIfRequested();
    break;
  }
  case TIMER_ID: {
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
  } break;
  case CARET_TIMER_ID: {
    int virtualsize_x;
    int virtualsize_y;
    GetVirtualSize(&virtualsize_x, &virtualsize_y);

    if (m_blinkDisplayCaret) {
      wxRect rect;

      if (GetActiveCell()) {
        rect = GetActiveCell()->GetRect();
        GetActiveCell()->SwitchCaretDisplay();
      } else {
        m_hCaretBlinkVisible = !m_hCaretBlinkVisible;
        if (!m_hCaretPosition) {
          rect.SetTop(0);
          rect.SetBottom(m_configuration->GetGroupSkip());
        } else {
          rect = m_hCaretPosition->GetRect();
          int caretY =
            static_cast<int>(m_configuration->GetGroupSkip()) / 2 + rect.GetBottom() + 1;
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
    if (m_hasFocus) {
      int blinktime = wxCaret::GetBlinkTime();
      if (blinktime < 200)
        blinktime = 200;
      m_caretTimer.StartOnce(blinktime);
    }
  } break;
  default: {
    // Determine if the timer that has expired belongs to a slide show cell.
    Cell *const cell = m_cellPointers.GetCellForTimerId(event.GetId());
    AnimationCell *const animation = dynamic_cast<AnimationCell * const>(cell);
    if (animation) {
      int pos = animation->GetDisplayedIndex() + 1;

      if (pos >= animation->Length())
        pos = 0;
      animation->SetDisplayedIndex(pos);

      // Refresh the displayed bitmap
      if (!m_configuration->ClipToDrawRegion())
        animation->ReloadTimer();
      else {
        wxRect rect = animation->GetRect();
        RequestRedraw(rect);
      }

      if (m_mainToolBar && GetSelectionStart() == animation) {
        m_mainToolBar->UpdateSlider(animation);
      }
    }
    break;
  }
  }
}

void Worksheet::RequestRedraw(wxRect rect) {
  // If a cell has been wider the last time it was drawn we need to clear the screen to the right end of the viewport
  if(rect.GetRight() > m_configuration->GetIndent() + m_configuration->GetCellBracketWidth())
    rect.SetRight(std::max(rect.GetRight(), m_configuration->GetVisibleRegion().GetRight()));
  if (!m_regionToRefresh.Union(rect))
    m_regionToRefresh = wxRegion(rect);
}

/***
 * Destroy the tree
 */
void Worksheet::DestroyTree() {
  m_hCaretActive = false;
  SetHCaret(NULL);
  TreeUndo_ClearUndoActionList();
  TreeUndo_ClearRedoActionList();
  m_tree.reset();
  m_last = NULL;
}

std::unique_ptr<GroupCell> Worksheet::CopyTree() const {
  return GetTree() ? GetTree()->CopyList() : nullptr;
}

bool Worksheet::CopyBitmap() const {
  BitmapOut bitmap(&m_configuration, CopySelection(), 1,
                   1000000 * m_configuration->MaxClipbrdBitmapMegabytes());
  bool retval = bitmap.ToClipboard();
  return retval;
}

bool Worksheet::CopyAnimation() const {
  AnimationCell * const animation = GetSelectedAnimation();
  if (animation != NULL)
    return animation->CopyAnimationToClipboard();
  else
    return false;
}

bool Worksheet::CopySVG() const {
  Svgout svg(&m_configuration, CopySelection());
  bool retval = svg.ToClipboard();
  return retval;
}

#if wxUSE_ENH_METAFILE
bool Worksheet::CopyEMF() const {
  Emfout emf(&m_configuration, CopySelection());
  bool retval = emf.ToClipboard();
  return retval;
}
#endif

bool Worksheet::CopyRTF() const {
  if (!HasCellsSelected())
    return false;

  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (!wxTheClipboard->Open())
    return false;

  wxDataObjectComposite *data = new wxDataObjectComposite;

  wxString rtf = RTFStart();
  const GroupCell *end = m_cellPointers.m_selectionEnd->GetGroup();

  for (auto &tmp : OnList(m_cellPointers.m_selectionStart->GetGroup())) {
    rtf += tmp.ToRTF();
    if (&tmp == end)
      break;
  }

  rtf += wxS("\\par") + RTFEnd();

  data->Add(new RtfDataObject(rtf), true);
  data->Add(new RtfDataObject2(rtf));

  wxTheClipboard->SetData(data);
  wxTheClipboard->Close();
  return true;
}

wxSize Worksheet::CopyToFile(const wxString &file) const {
  if (m_cellPointers.m_selectionStart &&
      m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
      (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_IMAGE ||
       m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE)) {
    return m_cellPointers.m_selectionStart.CastAs<ImgCellBase *>()->ToImageFile(
                                                                                file);
  } else {
    BitmapOut output(&m_configuration, CopySelection());
    wxSize retval = output.ToFile(file);
    return retval;
  }
}

wxSize Worksheet::CopyToFile(const wxString &file, Cell *start, Cell *end,
                             bool asData, double scale) const {
  return WorksheetExport::CopyToFile(file, start, end, asData, scale,
                                     &m_configuration);
}

std::unique_ptr<Cell> Worksheet::CopySelection(bool asData) const {
  return CopySelection(m_cellPointers.m_selectionStart,
                       m_cellPointers.m_selectionEnd, asData);
}

void Worksheet::TOCdnd(GroupCell *dndStart, GroupCell *dndEnd) {
  if((m_cellPointers.m_selectionStart == nullptr) || (m_cellPointers.m_selectionEnd == nullptr))
    return;
  if (!dndStart)
    return;

  // We only update the table of contents when there is time => no guarantee
  // that the cell that was clicked at actually still is part of the tree.
  if (!m_tree || !m_tree->Contains(dndStart))
    return;
  if (dndEnd && !m_tree->Contains(dndEnd))
    return;

  // Select the region that is to be moved
  m_cellPointers.m_selectionStart = dndStart;
  m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionStart;
  if (m_cellPointers.m_selectionEnd->GetNext())
    m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionEnd->GetNext();
  while ((m_cellPointers.m_selectionEnd) &&
         ((m_cellPointers.m_selectionEnd->GetNext() != NULL) &&
          (dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetNext()) &&
           dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd.get()) &&
           (dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd->GetNext())
            ->IsLesserGCType(dynamic_cast<GroupCell *>(m_cellPointers.m_selectionEnd.get())
                             ->GetGroupType())))))
    m_cellPointers.m_selectionEnd = m_cellPointers.m_selectionEnd->GetNext();

  // Copy the region we want to move
  CellListBuilder<> copy;
  for (auto &src : OnList(m_cellPointers.m_selectionStart.get())) {
    copy.Append(src.Copy(dynamic_cast<GroupCell *>(&src)));
    if (&src == m_cellPointers.m_selectionEnd)
      break;
  }

  // Delete the region we just copied and tell the undo buffer that this
  // is only half of the action we want to perform
  DeleteSelection();
  TreeUndo_AppendAction();

  std::unique_ptr<Cell> copiedCells = std::move(copy);
  std::unique_ptr<GroupCell> copiedGroupCells =
    unique_cast<Cell, GroupCell>(std::move(copiedCells));
  InsertGroupCells(std::move(copiedGroupCells), dndEnd);
  Recalculate();
  RequestRedraw();
  UpdateTableOfContents();
  NumberSections();
}

std::unique_ptr<Cell> Worksheet::CopySelection(Cell *start, Cell *end,
                                               bool asData) const {
  return WorksheetExport::CopySelection(start, end, asData);
}

/***
 * Export content to a HTML file.
 */
bool Worksheet::ExportToHTML(const wxString &file) {
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;
  bool ok = WorksheetExport::ExportToHTML(GetTree(), m_configuration, file,
                                          &GetCellPointers(), GetHCaret());
  // The exporter toggles the clip region; re-layout now that it is restored.
  Recalculate();
  return ok;
}

void Worksheet::CodeCellVisibilityChanged() {
  // Move the cursor out of the currently active cell if we are about to
  // hide it
  if (GetActiveCell() && GetActiveCell()->GetType() == MC_TYPE_INPUT &&
      !m_configuration->ShowCodeCells())
    SetHCaret(GetActiveCell()->GetGroup());
  Recalculate();
  ScrollToCaret();
}

/*! Export the file as TeX code
 */
bool Worksheet::ExportToTeX(const wxString &file) {
  // Show a busy cursor as long as we export.
  wxBusyCursor crs;
  return WorksheetExport::ExportToTeX(GetTree(), m_configuration, file);
}

void Worksheet::LoadSymbols() { m_autocomplete.LoadSymbols(); }

const wxString Worksheet::UnicodeToMaxima(wxString s) {
  s.Replace(wxS("\u2052"), "-"); // commercial minus sign
  s.Replace(wxS("\uFE63"), "-"); // unicode small minus sign
  s.Replace(wxS("\uFF0D"), "-"); // unicode big minus sign
  s.Replace(wxS("\uFF0B"), "+"); // unicode big plus
  s.Replace(wxS("\uFB29"), "+"); // hebrew alternate plus

  wxString retval;

  for (auto const &tok : MaximaTokenizer(s, m_configuration).PopTokens()) {
    auto const &tokenString = tok.GetText();
    switch (tok.GetTextStyle()) {
    case TS_CODE_DEFAULT:
    case TS_CODE_OPERATOR:
    case TS_CODE_VARIABLE:
    case TS_CODE_FUNCTION:
      if (tokenString == wxS("\u221A")) {
        retval += wxS(" sqrt ");
        continue;
      }
      if (tokenString == wxS("\u222B")) {
        retval += wxS(" integrate ");
        continue;
      }
      if (tokenString == wxS("\u2211")) {
        retval += wxS(" sum ");
        continue;
      }
      if (tokenString == wxS("\u220F")) {
        retval += wxS(" product ");
        continue;
      }
      if (tokenString == wxS("\u2148")) {
        retval += wxS(" %i ");
        continue;
      }
      if (tokenString == wxS("\u2147")) {
        retval += wxS(" %e ");
        continue;
      }
      if (tokenString == wxS("\u22C0")) {
        retval += wxS(" and ");
        continue;
      }
      if (tokenString == wxS("\u22C1")) {
        retval += wxS(" or ");
        continue;
      }
      if (tokenString == wxS("\u22BB")) {
        retval += wxS(" xor ");
        continue;
      }
      if (tokenString == wxS("\u22BC")) {
        retval += wxS(" nand ");
        continue;
      }
      if (tokenString == wxS("\u22BD")) {
        retval += wxS(" nor ");
        continue;
      }
      if (tokenString == wxS("\u21D2")) {
        retval += wxS(" implies ");
        continue;
      }
      if (tokenString == wxS("\u21D4")) {
        retval += wxS(" equiv ");
        continue;
      }
      if (tokenString == wxS("\u00AC")) {
        retval += wxS(" not ");
        continue;
      }
      if (tokenString == wxS("\u03C0")) {
        retval += wxS(" %pi ");
        continue;
      }
      // Only executed if none of the conditions that can be found above fires
      retval += tokenString;
      break;
    default:
      if (tokenString == wxS("\u221E")) {
        retval += wxS(" inf ");
        continue;
      }
      retval += tokenString;
    }
  }

  retval.Replace(wxS("\u00B2"), "^2");
  retval.Replace(wxS("\u00B3"), "^3");
  retval.Replace(wxS("\u00BC"), "(1/4)");
  retval.Replace(wxS("\u00BD"), "(1/2)");
  retval.Replace(wxS("\u00BE"), "(3/4)");
  retval.Replace(wxS("\u2150"), "(1/7)");
  retval.Replace(wxS("\u2151"), "(1/9)");
  retval.Replace(wxS("\u2152"), "(1/10)");
  retval.Replace(wxS("\u2153"), "(1/3)");
  retval.Replace(wxS("\u2154"), "(2/3)");
  retval.Replace(wxS("\u2155"), "(1/5)");
  retval.Replace(wxS("\u2156"), "(2/5)");
  retval.Replace(wxS("\u2157"), "(3/5)");
  retval.Replace(wxS("\u2158"), "(4/5)");
  retval.Replace(wxS("\u2159"), "(1/6)");
  retval.Replace(wxS("\u215A"), "(5/6)");
  retval.Replace(wxS("\u215B"), "(1/8)");
  retval.Replace(wxS("\u215C"), "(3/8)");
  retval.Replace(wxS("\u215D"), "(5/8)");
  retval.Replace(wxS("\u215E"), "(7/8)");
  retval.Replace(wxS("\u215F"), "1/"); // not a exact mathematical expression
  retval.Replace(wxS("\u2189"), "(0/3)"); // used in baseball??
  retval.Replace(wxS("\u2205"), "[]"); // An empty list
  retval.Replace(wxS("\u2212"), "-");
  retval.Replace(wxS("\u2260"), "#"); // The "not equal" sign
  retval.Replace(wxS("\u2264"), "<=");
  retval.Replace(wxS("\u2265"), ">=");
  retval.Replace(wxS("\u00B7"), "*"); // An unicode multiplication sign
  retval.Replace(wxS("\u2052"), "-"); // commercial minus sign
  retval.Replace(wxS("\uFE63"), "-"); // unicode small minus sign
  retval.Replace(wxS("\uFF0D"), "-"); // unicode big minus sign
  retval.Replace(wxS("\uFF0B"), "+"); // unicode big plus
  retval.Replace(wxS("\uFB29"), "+"); // hebrew alternate plus
  return retval;
}

bool Worksheet::ExportToMAC(const wxString &file) {
  bool wasSaved = m_saved;

  // Show a busy cursor as long as we export or save.
  wxBusyCursor crs;
  // Don't update the worksheet whilst exporting
  //  wxWindowUpdateLocker noUpdates(this);

  bool wxm;

  if (file.Right(4) == wxS(".wxm"))
    wxm = true;
  else
    wxm = false;

  wxTextFile backupfile(file + wxS("~"));
  if (backupfile.Exists()) {
    if (!backupfile.Open())
      return false;
    backupfile.Clear();
  } else if (!backupfile.Create())
    return false;

  if (wxm) {
    WorksheetExport::AddLineToFile(backupfile, Format::WXMFirstLine);
    WorksheetExport::AddLineToFile(backupfile, wxS("/* [ Created with wxMaxima version " WXMAXIMA_VERSION " ] */"));
  }

  bool fixReorderedIndices = m_configuration->FixReorderedIndices();
  std::vector<int> cellMap;
  if (fixReorderedIndices) {
    int cellIndex = 1;
    WorksheetExport::CalculateReorderedCellIndices(GetTree(), cellIndex, cellMap);
  }
  WorksheetExport::ExportToMAC(backupfile, GetTree(), wxm, cellMap, fixReorderedIndices);

  WorksheetExport::AddLineToFile(backupfile, wxEmptyString);
  if (wxm) {
    WorksheetExport::AddLineToFile(backupfile, wxS("/* Old versions of Maxima abort on loading "
                                  "files that end in a comment. */"));
    WorksheetExport::AddLineToFile(backupfile, wxS("\"Created with wxMaxima " WXMAXIMA_VERSION "\"$"));
  }

  // Try to save the file.
  bool done = backupfile.Write(wxTextFileType_None);
  // Even if that failed we should perhaps still issue a Close() .
  if (!backupfile.Close())
    return false;
  if (!done)
    return false;

  {
    // If we succeeded in saving the backup file we now can overwrite the Real
    // Thing.
    done = wxRenameFile(file + wxS("~"), file, true);
    if (!done) {
      wxSleep(1);
      wxRenameFile(file + wxS("~"), file, true);
    }
  }
  if (!done) {
    wxSleep(1);
    if (!wxRenameFile(file + wxS("~"), file, true))
      {
        m_unsavedDocuments.AddDocument(file + wxS("~"));
        return false;
      }
  }

  if (wxm)
    SetSaved(true);
  else
    SetSaved(wasSaved);
  return true;
}

bool Worksheet::CanEdit() {
  if (!m_cellPointers.m_selectionStart ||
      m_cellPointers.m_selectionEnd != m_cellPointers.m_selectionStart)
    return false;

  if (!m_cellPointers.m_selectionStart->IsEditable())
    return false;

  if (!m_cellPointers.m_selectionStart->GetPrevious())
    return false;

  if (m_cellPointers.m_selectionStart->GetPrevious()->GetType() !=
      MC_TYPE_MAIN_PROMPT)
    return false;

  return true;
}

void Worksheet::OnDoubleClick(wxMouseEvent &event) {
  event.Skip();
  BTextCtrl::ForgetLastActive();
  m_updateControls = true;
  // No more track the mouse when it is outside the worksheet
  if (HasCapture())
    ReleaseMouse();

  if (GetActiveCell())
    GetActiveCell()->SelectWordUnderCaret();
  else if (m_cellPointers.m_selectionStart) {
    // FIXME This code path can never get activated, because
    // OnMouseLeftDown clears the selection.
    const GroupCell *parent = m_cellPointers.m_selectionStart->GetGroup();
    auto const [first, last] = parent->GetCellsInOutput();
    m_cellPointers.m_selectionStart = first;
    m_cellPointers.m_selectionEnd = last;
  }

  RequestRedraw();
  // Re-calculate the table of contents
  UpdateTableOfContents();
}

bool Worksheet::ActivateInput(int direction) {
  auto const advance =
    (direction >= 0) ? +[](const GroupCell *cell) { return cell->GetNext(); }
    : +[](const GroupCell *cell) { return cell->GetPrevious(); };

  GroupCell *tmp = {};
  if (m_cellPointers.m_selectionStart)
    tmp = m_cellPointers.m_selectionStart->GetGroup();
  else if (GetActiveCell()) {
    tmp = GetActiveCell()->GetGroup();
    SetActiveCell(nullptr);
  }
  if (!tmp)
    return false;

  tmp = advance(tmp);
  if (!tmp)
    return false;

  while (tmp) {
    auto *const succ = advance(tmp);
    if (succ && succ->GetMaxDrop() == 0)
      tmp = succ;
    else
      break;
  }

  for (; tmp; tmp = advance(tmp)) {
    auto *const input = tmp->GetEditable();
    if (input) {
      SetActiveCell(input);
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
void Worksheet::AddDocumentToEvaluationQueue() {
  FollowEvaluation(true);
  for (auto &tmp : OnList(GetTree()))
    AddToEvaluationQueue(&tmp);

  SetHCaret(GetLastCellInWorksheet());
}

void Worksheet::AddToEvaluationQueue(GroupCell *cell) {
  if ((cell->GetGroupType() == GC_TYPE_CODE) && (!cell->IsHidden())) {
    // Gray out the output of the cell in order to mark it as "not current".
    if (cell->GetEditable()) {
      cell->GetEditable()->ContainsChanges(true);
      // ...and add it to the evaluation queue
      m_evaluationQueue.AddToQueue(cell);
    }
  }
}

/**
 * Add the entire document, including hidden cells, to the evaluation queue.
 */
void Worksheet::AddEntireDocumentToEvaluationQueue() {
  FollowEvaluation(true);
  for (auto &tmp : OnList(GetTree())) {
    AddToEvaluationQueue(&tmp);
    m_evaluationQueue.AddHiddenTreeToQueue(&tmp);
  }
  SetHCaret(GetLastCellInWorksheet());
}

void Worksheet::AddSectionToEvaluationQueue(GroupCell *start) {
  // Find the begin of the current section
  start = StartOfSectioningUnit(start);

  // Find the end of the current section
  GroupCell *end = EndOfSectioningUnit(start);
  AddSelectionToEvaluationQueue(start, end);
}

void Worksheet::AddRestToEvaluationQueue() {
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

  AddSelectionToEvaluationQueue(start, GetLastCellInWorksheet());
}

void Worksheet::AddSelectionToEvaluationQueue() {
  AddSelectionToEvaluationQueue(
                                m_cellPointers.m_selectionStart.CastAs<GroupCell *>(),
                                m_cellPointers.m_selectionEnd.CastAs<GroupCell *>());
}

void Worksheet::AddSelectionToEvaluationQueue(GroupCell *start,
                                              GroupCell *end) {
  FollowEvaluation(true);
  if (!start || !end)
    return;
  wxASSERT(start->GetType() == MC_TYPE_GROUP);
  for (auto &tmp : OnList(start)) {
    AddToEvaluationQueue(&tmp);
    if (&tmp == end)
      break;
  }
  SetHCaret(end);
}

void Worksheet::AddDocumentTillHereToEvaluationQueue() {
  FollowEvaluation(true);
  const GroupCell *stop = m_hCaretPosition.get();
  if(!m_hCaretActive) stop = nullptr;
  if (!stop) {
    if (!GetActiveCell())
      return;
    stop = GetActiveCell()->GetGroup();
    if (stop->GetPrevious())
      stop = stop->GetPrevious();
  }

  if (!stop)
    return;

  for (auto &tmp : OnList(GetTree())) {
    AddToEvaluationQueue(&tmp);
    if (&tmp == stop)
      break;
  }
}

void Worksheet::AddCellToEvaluationQueue(GroupCell *gc) {
  AddToEvaluationQueue(gc);
  SetHCaret(gc);
}

//////// end of EvaluationQueue related stuff ////////////////
void Worksheet::ScrolledAwayFromEvaluation(bool ScrolledAway) {
  if (ScrolledAway != m_scrolledAwayFromEvaluation) {
    m_scrolledAwayFromEvaluation = ScrolledAway;
    if (FollowEvaluation() && ScrolledAway) {
      FollowEvaluation(false);
      if (m_mainToolBar && GetActiveCell())
        m_mainToolBar->EnableTool(ToolBar::tb_follow, true);
    } else {
      if (m_mainToolBar)
        m_mainToolBar->EnableTool(ToolBar::tb_follow, false);
    }
  }
}

void Worksheet::FollowEvaluation(bool followEvaluation) {
  m_followEvaluation = followEvaluation;
  if (followEvaluation)
    ScrolledAwayFromEvaluation(false);
}

Cell *Worksheet::FindCellByUUID(const wxString &uuid) {
  if (!GetTree())
    return nullptr;

  struct FindUUID {
    Cell *operator()(Cell *cell, const wxString &uuid) {
      if (cell->GetUUID() == uuid)
        return cell;
      for (Cell &inner : OnInner(cell)) {
        Cell *found = (*this)(&inner, uuid);
        if (found)
          return found;
      }
      if (cell->GetNext())
        return (*this)(cell->GetNext(), uuid);
      return nullptr;
    }
  } finder;

  return finder(GetTree(), uuid);
}

void Worksheet::ScrollToCellIfNeeded() {
  if (!m_cellPointers.m_scrollToCell)
    return;
  m_cellPointers.m_scrollToCell = false;

  RecalculateIfNeeded();

  const Cell *cell = m_cellPointers.CellToScrollTo();

  if (!cell) {
    int view_x, view_y;
    GetViewStart(&view_x, &view_y);
    Scroll(view_x, 0);
    return;
  }

  if (cell == GetActiveCell()) {
    ScrollToCaret();
    return;
  }

  int cellY = cell->GetCurrentY();

  if (cellY < 0) {
    Recalculate();
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

  if (m_scrollToTopOfCell) {
    // Scroll upwards if the top of the thing we want to scroll to is less than
    // 1/2 scroll unit away from the top of the page
    if (cellTop - m_scrollUnit < view_y)
      Scroll(-1, std::max(cellTop / m_scrollUnit - 1, 0));

    // Scroll downwards if the top of the thing we want to scroll to is less
    // than 1/2 scroll unit away from the bottom of the page
    if (cellTop + m_scrollUnit > view_y + height)
      Scroll(-1, std::max(cellTop / m_scrollUnit - 1, 0));
  } else {
    // Scroll downwards if the bottom of the thing we want to scroll to is less
    // than 1/2 scroll unit away from the bottom of the page
    if (cellBottom + m_scrollUnit > view_y + height)
      Scroll(-1, std::max(cellBottom / m_scrollUnit - 1, 0));

    // Scroll upwards if the bottom of the thing we want to scroll to is less
    // than 1/2 scroll unit away from the top of the page
    if (cellBottom - m_scrollUnit < view_y)
      Scroll(-1, std::max(cellBottom / m_scrollUnit - 1, 0));
  }
  RequestRedraw();
}

bool Worksheet::CanUndo() const {
  return CanTreeUndo() || CanUndoInsideCell();
}

bool Worksheet::CanRedo() const {
  return CanTreeRedo() || CanRedoInsideCell();
}

void Worksheet::Undo() {
  if (CanUndoInsideCell()) {
    wxLogMessage(_("Issuing the active cell's undo function"));
    UndoInsideCell();
    Recalculate();
  } else {
    if (CanTreeUndo()) {
      wxLogMessage(_("Issuing the worksheet's undo function"));
      TreeUndo();
      UpdateTableOfContents();
    }
  }
}

void Worksheet::TreeUndo_LimitUndoBuffer() {
  m_treeUndo.LimitUndoBuffer(m_configuration->UndoLimit());
}

bool Worksheet::CanTreeUndo() const {
  const UndoActions &undoActions = m_treeUndo.UndoStack();
  if (undoActions.empty())
    return false;
  else {
    // If the next undo action will delete cells we have to look if we are
    // allowed to do this.
    if (undoActions.front().m_newCellsEnd)
      return CanDeleteRegion(undoActions.front().m_start,
                             undoActions.front().m_newCellsEnd);
    else
      return true;
  }
}

bool Worksheet::CanTreeRedo() const {
  const UndoActions &redoActions = m_treeUndo.RedoStack();
  if (redoActions.empty()) {
    return false;
  } else {
    // If the next redo action will delete cells we have to look if we are
    // allowed to do this.
    if (redoActions.front().m_newCellsEnd)
      return CanDeleteRegion(redoActions.front().m_start,
                             redoActions.front().m_newCellsEnd);
    else
      return true;
  }
}

void Worksheet::Redo() {
  if (CanRedoInsideCell()) {
    RedoInsideCell();
  } else {
    if (CanTreeRedo()) {
      TreeRedo();
      UpdateTableOfContents();
    }
  }
}

bool Worksheet::CanMergeSelection() const {
  // We cannot merge cells if not at least two cells are selected
  if (GetSelectionStart() == GetSelectionEnd())
    return false;

  // We cannot merge cells if we cannot delete the cells that are
  // removed during the merge.
  if (!CanDeleteSelection())
    return false;

  return true;
}

bool Worksheet::TreeUndoCellDeletion(UndoActions *sourcelist,
                                     UndoActions *undoForThisOperation) {
  TreeUndoAction &action = sourcelist->front();
  GroupCell *newCursorPos = action.m_oldCells.get();
  if (newCursorPos)
    newCursorPos = newCursorPos->last();

  InsertGroupCells(std::move(action.m_oldCells), action.m_start,
                   undoForThisOperation);
  SetHCaret(newCursorPos);
  return true;
}

bool Worksheet::TreeUndoCellAddition(UndoActions *sourcelist,
                                     UndoActions *undoForThisOperation) {
  const TreeUndoAction &action = sourcelist->front();
  wxASSERT_MSG(action.m_start, _("Bug: Got a request to delete the cell above "
                                 "the beginning of the worksheet."));

  // We make the cell we want to end the deletion with visible.
  if ((action.m_newCellsEnd) && (action.m_newCellsEnd->RevealHidden()))
    FoldOccurred();

  wxASSERT_MSG(CanDeleteRegion(action.m_start, action.m_newCellsEnd),
               _("Got a request to undo an action that involves a delete which "
                 "isn't possible at this moment."));

  // Set the cursor to a sane position.
  if ((action.m_newCellsEnd) && (action.m_newCellsEnd->GetNext()))
    {
      SetHCaret(action.m_newCellsEnd->GetNext());
    }
  else
    {
      if((action.m_start) && (action.m_start->GetPrevious()))
        SetHCaret(action.m_start->GetPrevious());
    }

  // Actually delete the cells we want to remove.
  DeleteRegion(action.m_start, action.m_newCellsEnd, undoForThisOperation);
  return true;
}

bool Worksheet::TreeUndoTextChange(UndoActions *sourcelist,
                                   UndoActions *undoForThisOperation) {
  const TreeUndoAction &action = sourcelist->front();

  // m_start is a CellPtr, so it auto-nulls if the cell whose text this action
  // changed was destroyed after the action was recorded. A destroyed (or
  // otherwise no-longer-present) cell cannot have its text change undone, so we
  // drop the action rather than dereference a stale pointer.
  if (!action.m_start || !GetTree()->Contains(action.m_start)) {
    wxLogMessage(
      wxS("Skipping the undo of a text change: the cell it refers to is no "
          "longer part of the worksheet."));
    return false;
  }

  if (action.m_start) {
    // If this action actually does do nothing - we have not done anything
    // and want to make another attempt on undoing things.
    if ((action.m_oldText == action.m_start->GetEditable()->GetValue()) ||
        (action.m_oldText + wxS(";") ==
         action.m_start->GetEditable()->GetValue())) {
      sourcelist->pop_front();
      return TreeUndo(sourcelist, undoForThisOperation);
    }

    // Document the current state of this cell (including cursor/selection)
    // so the next action can be undone (i.e. this undo can itself be redone).
    {
      EditorCell *ed = action.m_start->GetEditable();
      undoForThisOperation->emplace_front(
        action.m_start, ed->GetValue(),
        static_cast<long long>(ed->SelectionStart()),
        static_cast<long long>(ed->SelectionEnd()));
    }

    // Revert the old cell state
    action.m_start->GetEditable()->SetValue(action.m_oldText);

    // Restore the cursor/selection that was recorded when this undo entry was saved.
    if (action.m_oldSelStart >= 0) {
      EditorCell *ed = action.m_start->GetEditable();
      long long selEnd = (action.m_oldSelEnd >= 0) ? action.m_oldSelEnd : action.m_oldSelStart;
      ed->SetSelection(static_cast<size_t>(action.m_oldSelStart),
                       static_cast<size_t>(selEnd));
    }

    // Make sure that the cell we have to work on is in the visible part of the
    // tree.
    if (action.m_start->RevealHidden())
      FoldOccurred();

    SetHCaret(action.m_start);

    Recalculate();
    RequestRedraw();

    wxASSERT_MSG(!action.m_newCellsEnd,
                 _("Bug: Got a request to first change the contents of a cell "
                   "and to then undelete it."));
    wxASSERT_MSG(!action.m_oldCells, _("Bug: Undo action with both cell "
                                       "contents change and cell addition."));
    return true;
  }
  return false;
}

bool Worksheet::TreeUndo(UndoActions *sourcelist,
                         UndoActions *undoForThisOperation) {
  if (sourcelist->empty())
    return false;

  SetSaved(false);

  // Seems like saving the current value of the currently active cell
  // in the tree undo buffer makes the behavior of TreeUndo feel
  // more predictable to the user.
  if (GetActiveCell())
    TreeUndo_CellLeft();

  const TreeUndoAction &action = sourcelist->front();

  if (action.m_start) {
    // Make sure that the cell we work on is in the visible part of the tree.
    if (action.m_start->RevealHidden())
      FoldOccurred();
  }

  bool actionContinues;
  do {
    const TreeUndoAction &actn = sourcelist->front();
    if (actn.m_newCellsEnd)
      TreeUndoCellAddition(sourcelist, undoForThisOperation);
    else {
      if (actn.m_oldCells)
        TreeUndoCellDeletion(sourcelist, undoForThisOperation);
      else
        TreeUndoTextChange(sourcelist, undoForThisOperation);
    }
    TreeUndoManager::AppendAction(undoForThisOperation);
    sourcelist->pop_front();
    actionContinues =
      !sourcelist->empty() && sourcelist->front().m_partOfAtomicAction;
  } while (actionContinues);
  if (!undoForThisOperation->empty())
    undoForThisOperation->front().m_partOfAtomicAction = false;
  Recalculate();
  RequestRedraw();
  return true;
}

/*! Mark an editor cell as the active one

 */
void Worksheet::SetActiveCell(EditorCell *cell) {
  if (GetActiveCell() == cell)
    return;

  if (GetActiveCell())
    TreeUndo_CellLeft();

  if (m_mainToolBar) {
    if (!cell || !cell->GetGroup())
      m_mainToolBar->UnsetCellStyle();
    else
      m_mainToolBar->SetCellStyle(cell->GetGroup()->GetGroupType());
  }

  bool scrollneeded = GetActiveCell() && GetActiveCell() != cell;

  if (cell) {
    m_cellPointers.m_selectionStart = nullptr;
    m_cellPointers.m_selectionEnd = nullptr;
    if(cell->ActivateCursor())
      Recalculate(cell->GetGroup());
  } else if (GetActiveCell())
    GetActiveCell()->DeactivateCursor();

#if wxUSE_ACCESSIBILITY
  if (m_accessibilityInfo != NULL) {
    int childId = GetAccessibilityId(cell);
    // Notify on GetTargetWindow() because that's where the AccessibilityInfo is attached
    wxAccessible::NotifyEvent(wxACC_EVENT_OBJECT_FOCUS, GetTargetWindow(), wxOBJID_CLIENT, childId);
  }
#endif

  TreeUndo_CellEntered();

  if (cell) {
    m_blinkDisplayCaret = true;

    int blinktime = wxCaret::GetBlinkTime();
    if (blinktime < 200)
      blinktime = 200;
    m_caretTimer.StartOnce(blinktime);
    m_hCaretActive = false; // we have activated a cell .. disable caret
    m_hCaretPosition = NULL;
  }

  RequestRedraw();

  if (cell && !m_configuration->ShowCodeCells() && GetActiveCell() &&
      GetActiveCell()->GetType() == MC_TYPE_INPUT) {
    m_configuration->ShowCodeCells(true);
    CodeCellVisibilityChanged();
  }
  if (scrollneeded && cell)
    ScrollToCaret();
}

void Worksheet::SetSelection(Cell *start, Cell *end) {
  if (m_cellPointers.m_selectionStart != start ||
      m_cellPointers.m_selectionEnd != end)
    RequestRedraw();
  m_cellPointers.m_selectionStart = start;
  m_cellPointers.m_selectionEnd = end;

  if (!m_cellPointers.m_selectionStart) {
    m_hCaretPositionStart = NULL;
    m_hCaretPositionEnd = NULL;
  }

  if (m_mainToolBar) {
    if (!start && !end) {
      if (!GetActiveCell())
        m_mainToolBar->UnsetCellStyle();
    } else {
      if (start != end && !GetActiveCell())
        m_mainToolBar->UnsetCellStyle();
      else
        {
          if(start && start->GetGroup())
            m_mainToolBar->SetCellStyle(start->GetGroup()->GetGroupType());
        }
    }
  }
}

bool Worksheet::PointVisibleIs(wxPoint point) {
  int view_x, view_y;
  int height, width;

  CalcUnscrolledPosition(0, 0, &view_x, &view_y);

  GetSize(&width, &height);

  if ((point.y < view_y) ||
      (point.y >
       view_y + height - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20))
    return false;

  if ((point.x < view_x) ||
      (point.x >
       view_x + width - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20))
    return false;

  return true;
}

void Worksheet::ShowPoint(wxPoint point) {
  wxASSERT((point.x >= 0) && (point.y >= 0));
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

  const Configuration *configuration = m_configuration;
  int fontsize_px =
    configuration->GetZoomFactor() * configuration->GetDefaultFontSize();
  if ((point.y - fontsize_px < view_y) ||
      (point.y + fontsize_px > view_y + height - 20)) {
    sc = true;
    scrollToY = point.y - height / 2;
  } else
    scrollToY = view_y;

  if ((point.x - fontsize_px < view_x) || (point.x + 2 > view_x + width - 20)) {
    sc = true;
    scrollToX = point.x - width / 2;
  } else
    scrollToX = view_x;

  if (sc) {
    Scroll(scrollToX / m_scrollUnit, scrollToY / m_scrollUnit);
  }
}

bool Worksheet::CutToClipboard() {
  if (GetActiveCell()) {
    GetActiveCell()->CutToClipboard();
    GroupCell *group = GetActiveCell()->GetGroup();
    Recalculate(group);
    RequestRedraw();
    return true;
  } else if (m_cellPointers.m_selectionStart &&
             m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP) {
    if (CopyCells()) {
      GroupCell *group = NULL;
      if (GetActiveCell())
        group = GetActiveCell()->GetGroup();
      if (group && (group->GetPrevious()))
        group = group->GetPrevious();
      if (group)
        Recalculate(group);
      else
        Recalculate();
      DeleteSelection();
      return true;
    } else
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
void Worksheet::PasteFromClipboard() {
  bool cells = false;

  // Check for cell structure
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (!wxTheClipboard->Open())
    return;

  // Check if the clipboard contains text.
  if ((wxTheClipboard->IsSupported(wxDF_TEXT)) ||
      (wxTheClipboard->IsSupported(wxDF_UNICODETEXT)) ||
      (wxTheClipboard->IsSupported(m_wxmFormat))) {
    wxString inputs;

    {
      if (wxTheClipboard->IsSupported(m_wxmFormat)) {
        wxmDataObject data;
        wxTheClipboard->GetData(data);
        inputs = wxString::FromUTF8(reinterpret_cast<char *>(data.GetData()), data.GetDataSize());
        if (!inputs.StartsWith(wxS("/* [wxMaxima: ")))
          wxLogMessage(_(".wxm clipboard data with unusual header"));
        else
          wxLogMessage(_("Read .wxm data from clipboard"));
      } else {
        wxTextDataObject data;
        wxTheClipboard->GetData(data);
        inputs = data.GetText();
      }
    }

    // Normalize external line endings (Windows "\r\n" / classic-macOS lone '\r')
    // to '\n' before parsing: the line tokenizer below splits on '\n' only, and
    // '\r' is wxMaxima's internal soft word-wrap break, so it must not leak into
    // the pasted cell text.
    inputs.Replace(wxS("\r\n"), wxS("\n"));
    inputs.Replace(wxS("\r"), wxS("\n"));

    if (inputs.StartsWith(wxS("/* [wxMaxima: "))) {
      // Convert the text from the clipboard into an array of lines
      wxStringTokenizer lines(inputs, wxS("\n"), wxTOKEN_RET_EMPTY);
      std::vector <wxString> lines_array;
      while (lines.HasMoreTokens())
        lines_array.push_back(lines.GetNextToken());

      // Load the array like we would do with a .wxm file
      auto contents = Format::TreeFromWXM(lines_array, m_configuration);

      // Add the result of the last operation to the worksheet.
      if (contents) {
        contents->SetConfigurationList(m_configuration);
        // ! Tell the rest of this function that we have found cells
        cells = true;

        // Search for the last cell we want to paste
        GroupCell *end = contents->last();

        // Now paste the cells
        if (!GetTree()) {
          // Empty work sheet => We paste cells as the new cells
          m_tree = std::move(contents);
        } else {
          bool hasHSelection =
            m_cellPointers.m_selectionStart &&
            (m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP);
          auto *target =
            GetActiveCell() ? GetActiveCell()->GetGroup() : nullptr;
          if (!target)
            target = GetHCaret();
          if (hasHSelection) {
            DeleteSelection();
            TreeUndo_AppendAction();
          }
          InsertGroupCells(std::move(contents), target);
        }
        NumberSections();
        Recalculate();
        RequestRedraw();
        RedrawIfRequested();
        SetHCaret(end);
      }
    }
  }
  // Check if the clipboard contains an image.
  else if (wxTheClipboard->IsSupported(wxDF_BITMAP)) {
    OpenHCaret(wxEmptyString, GC_TYPE_IMAGE);
    GroupCell *group = GetActiveCell()->GetGroup();

    if (group) {
      wxBitmapDataObject bitmap;
      wxTheClipboard->GetData(bitmap);
      auto ic =
        std::make_unique<ImgCell>(group, m_configuration, bitmap.GetBitmap());
      group->AppendOutput(std::move(ic));
      m_configuration->SetAdjustWorksheetSizeNeeded(true);
      Recalculate(group);
    }
  }

  // Clipboard does not have the cell structure.
  if (!cells) {
    if (GetActiveCell()) {
      GetActiveCell()->PasteFromClipboard();
      GetActiveCell()->ResetSize();
      GetActiveCell()->GetGroup()->ResetSize();
      Recalculate(GetActiveCell()->GetGroup());
      RequestRedraw();
    } else {
      if (m_hCaretActive && (wxTheClipboard->IsSupported(wxDF_TEXT) ||
                             wxTheClipboard->IsSupported(wxDF_UNICODETEXT))) {
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

  UpdateTableOfContents();
  ScrolledAwayFromEvaluation();
}

void Worksheet::SelectAll() {
  if (!GetActiveCell() && GetTree()) {
    SetSelection(GetTree(), GetLastCellInWorksheet());
    m_clickType = CLICK_TYPE_GROUP_SELECTION;
    m_hCaretActive = false;
  } else if (GetActiveCell()) {
    if (!GetActiveCell()->AllSelected())
      GetActiveCell()->SelectAll();
    else {
      SetActiveCell(NULL);
      SetSelection(GetTree(), GetLastCellInWorksheet());
      m_clickType = CLICK_TYPE_GROUP_SELECTION;
      m_hCaretActive = false;
    }
  }
  ScrolledAwayFromEvaluation();
  RequestRedraw();
}

void Worksheet::DivideCell() {
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

void Worksheet::MergeCells() {
  if (!m_cellPointers.m_selectionStart)
    return;

  wxString newcell;

  for (auto &tmp :
         OnList(m_cellPointers.m_selectionStart.CastAs<GroupCell *>())) {
    if (newcell.Length() > 0)
      newcell += wxS("\n");
    newcell += tmp.GetEditable()->GetValue();

    if (&tmp == m_cellPointers.m_selectionEnd)
      break;
  }

  GroupCell *selStart = m_cellPointers.m_selectionStart.CastAs<GroupCell *>();
  EditorCell *editor = selStart->GetEditable();
  editor->SetValue(newcell);

  m_cellPointers.m_selectionStart = selStart->GetNext();
  DeleteSelection();
  editor->GetGroup()->ResetInputLabel();
  editor->GetGroup()->RemoveOutput();
  editor->ResetSize();
  editor->GetGroup()->ResetSize();
  Recalculate(editor->GetGroup());
  SetActiveCell(editor);
  ScrolledAwayFromEvaluation();
}

void Worksheet::OnSetFocus(wxFocusEvent &event) {
  wxLogMessage(_("Worksheet got the mouse focus"));
  m_hasFocus = true;

  // We want the cursor to blink and to start doing so soon
  m_caretTimer.StartOnce(1);
  if (GetActiveCell())
    GetActiveCell()->SetFocus(true);

  event.Skip();
}

void Worksheet::OnKillFocus(wxFocusEvent &event) {
  m_hasFocus = false;
  if (GetActiveCell())
    GetActiveCell()->SetFocus(false);
  event.Skip();
}

void Worksheet::CheckUnixCopy() {
  if (CanCopy()) {
    wxTheClipboard->UsePrimarySelection(true);
    if (wxTheClipboard->IsUsingPrimarySelection()) {
      wxASSERT_MSG(!wxTheClipboard->IsOpened(),
                   _("Bug: The clipboard is already opened"));
      if (wxTheClipboard->Open()) {
        wxString data = GetString();
        wxLogMessage(_("Middle-click clipboard data: %s"),
                     static_cast<const char*>(data.mb_str()));
        wxTheClipboard->SetData(new wxTextDataObject(data));
        wxTheClipboard->Close();
      }
    }
    wxTheClipboard->UsePrimarySelection(false);
  }
}

//! Is this cell selected?
bool Worksheet::IsSelected(CellType type) {
  return m_cellPointers.m_selectionStart &&
    m_cellPointers.m_selectionStart->GetType() == type &&
    ((type != MC_TYPE_IMAGE && type != MC_TYPE_SLIDE) ||
     m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd);
}

//! Starts playing the animation of a cell generated with the with_slider_*
//! commands
void Worksheet::Animate(bool run) const {
  AnimationCell * const animation = GetSelectedAnimation();
  if(animation != NULL)
    animation->AnimationRunning(run);
}

bool Worksheet::IsSelectionInWorkingGroup() {
  return m_cellPointers.m_selectionStart && GetWorkingGroup() &&
    m_cellPointers.m_selectionStart->GetGroup() == GetWorkingGroup();
}

GroupCell *Worksheet::GetHCaret() {
  if (m_hCaretActive)
    return m_hCaretPosition;

  if (GetActiveCell())
    return GetActiveCell()->GetGroup();

  if (m_cellPointers.m_selectionStart)
    return m_cellPointers.m_selectionStart->GetGroup();

  if (MouseSelectionStart())
    return MouseSelectionStart()->GetGroup();

  // A fallback value that is returned if nothing else seems to work
  return GetLastCellInWorksheet();
}

void Worksheet::SetDefaultHCaret() { SetHCaret(GetLastCellInWorksheet()); }

void Worksheet::OnActivate(wxActivateEvent &event) {
  // If the focus changes we might want to refresh the menu.
  RequestRedraw();
  if (event.GetActive())
    wxLogMessage(_("Worksheet got activated"));
  event.Skip();
}

void Worksheet::SetHCaret(GroupCell *where) {
  ClearSelection();
  if (m_mainToolBar) {
    m_mainToolBar->UnsetCellStyle();
  }

  m_hCaretPositionStart = m_hCaretPositionEnd = NULL;
  SetActiveCell(NULL);
  if (where)
    wxASSERT_MSG(where->GetType() == MC_TYPE_GROUP,
                 _("Bug: Trying to move the horizontally-drawn cursor to a "
                   "place inside a GroupCell."));

  m_hCaretActive = true;
  if (m_hCaretPosition != where) {
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

void Worksheet::ShowHCaret() {
  if (!m_hCaretPosition)
    SetHCaret(GetLastCellInWorksheet());

  m_hCaretActive = true;
}

bool Worksheet::CanUndoInsideCell() const {
  return GetActiveCell() && GetActiveCell()->CanUndo();
}

void Worksheet::UndoInsideCell() {
  if (GetActiveCell()) {
    GetActiveCell()->Undo();
    GetActiveCell()->GetGroup()->ResetSize();
    GetActiveCell()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

bool Worksheet::CanRedoInsideCell() const {
  return GetActiveCell() && GetActiveCell()->CanRedo();
}

void Worksheet::RedoInsideCell() {
  if (GetActiveCell()) {
    GetActiveCell()->Redo();
    GetActiveCell()->GetGroup()->ResetSize();
    Recalculate();
    RequestRedraw();
  }
}

void Worksheet::SaveValue() {
  if (GetActiveCell())
    GetActiveCell()->SaveValue();
}

void Worksheet::RemoveAllOutput() {
  if (HasCellsSelected()) {
    // If the selection is in the output we want to remove the selection.
    if (m_cellPointers.m_selectionStart->GetType() != MC_TYPE_GROUP)
      ClearSelection();
  }

  RemoveAllOutput(GetTree());
  OutputChanged();

  Recalculate();
  RequestRedraw();
}

void Worksheet::RemoveAllOutput(GroupCell *cell) {
  if (!cell)
    cell = GetTree();

  for (auto &tmp : OnList(cell)) {
    // If this function actually does do something we
    // should enable the "save" button.
    if(GetWorkingGroup() != &tmp)
      tmp.RemoveOutput();

    GroupCell *sub = tmp.GetHiddenTree();
    if (sub)
      RemoveAllOutput(sub);
  }
  m_configuration->SetAdjustWorksheetSizeNeeded(true);
  OutputChanged();
  Recalculate();
}

void Worksheet::OnMouseMiddleUp(wxMouseEvent &event) {
  if (m_inPopupMenu)
    // Pasting with an active popup menu makes no sense,
    // and additionally the ReleaseMouse() will fail an assertion if the
    // PopupMenu event loop is active.
    return;
  m_cellPointers.ResetSearchStart();

  wxTheClipboard->UsePrimarySelection(true);
  if (wxTheClipboard->IsUsingPrimarySelection()) {
    OnMouseLeftDown(event);
    m_leftDown = false;
    if (m_clickType != CLICK_TYPE_NONE)
      PasteFromClipboard();
    m_clickType = CLICK_TYPE_NONE;
    if (HasCapture())
      ReleaseMouse();
    wxTheClipboard->UsePrimarySelection(false);
  }
  event.Skip();
}

void Worksheet::CommentSelection() {
  if (GetActiveCell()) {
    EditorCell *active = GetActiveCell();
    active->CommentSelection();
    active->ResetSize();
    active->GetGroup()->ResetSize();
    Recalculate(active->GetGroup());
  }
}

void Worksheet::OnScrollChanged(wxScrollEvent &ev) {
  // Did we scroll away from the cell that is being currently evaluated?
  // If yes we want to no more follow the evaluation with the scroll and
  // want to enable the button that brings us back.
  CallAfter(&Worksheet::CheckIfActiveCellScrolledOut);

  // We don't want to start the autosave while the user is scrolling through
  // the document since this will shortly halt the scroll
  m_keyboardInactiveTimer.StartOnce(10000);
  ev.Skip();
}

void Worksheet::OnScrollEvent(wxScrollWinEvent &ev) {
  m_keyboardInactiveTimer.StartOnce(10000);
  // If we don't Skip() that event we effectively veto it.
  if (!CanAnimate()) {
    ev.Skip();
    CallAfter(&Worksheet::CheckIfActiveCellScrolledOut);
  }
}

void Worksheet::CheckIfActiveCellScrolledOut() {
  if (GetActiveCell()) {
    int width;
    int height;
    GetClientSize(&width, &height);

    wxPoint upperLeftScreenCorner;
    CalcUnscrolledPosition(0, 0, &upperLeftScreenCorner.x,
                           &upperLeftScreenCorner.y);
    wxRect visibleRegion = wxRect(upperLeftScreenCorner,
                                  upperLeftScreenCorner + wxPoint(width, height));

    if (!visibleRegion.Intersects(GetActiveCell()->GetRect())) {
      ScrolledAwayFromEvaluation(true);
    }
  }
}

wxString Worksheet::GetInputAboveCaret() {
  if (!m_hCaretActive || !m_hCaretPosition)
    return {};

  const EditorCell *editor = m_hCaretPosition->GetEditable();
  return editor ? editor->ToString() : wxString{};
}

wxString Worksheet::GetOutputAboveCaret() {
  if (!m_hCaretActive || !m_hCaretPosition)
    return {};

  auto const [first, last] = m_hCaretPosition->GetCellsInOutput();
  m_cellPointers.m_selectionStart = first;
  m_cellPointers.m_selectionEnd = last;

  wxString output = GetString();

  ClearSelection();
  RequestRedraw();

  return output;
}

bool Worksheet::FindIncremental(const wxString &str, bool down,
                                bool ignoreCase, bool searchInInput,
                                bool searchInOutput) {
  if (SearchStart()) {
    SetActiveCell(SearchStart());
    SearchStart()->CaretToPosition(IndexSearchStartedAt());
  }

  return (!str.empty()) ? FindNext(str, down, ignoreCase, searchInInput, searchInOutput, false) : true;
}


bool Worksheet::FindIncremental_RegEx(const wxString &str, bool down,
                                      bool searchInInput, bool searchInOutput) {
  if (SearchStart()) {
    SetActiveCell(SearchStart());
    SearchStart()->CaretToPosition(IndexSearchStartedAt());
  }

  return (!str.empty()) ? FindNext_Regex(str, down, searchInInput, searchInOutput, false) : true;
}

bool Worksheet::FindNext(const wxString &str, bool down, bool ignoreCase,
                         bool searchInInput, bool searchInOutput,
                         bool warn) {
  if (!GetTree())
    return false;

  int starty;
  if (down)
    starty = 0;
  else {
    wxSize canvasSize = GetClientSize();
    starty = canvasSize.y;
  }

  // Default the start of the search at the top or the bottom of the screen
  wxPoint topleft;
  CalcUnscrolledPosition(0, starty, &topleft.x, &topleft.y);
  GroupCell *pos = GetTree();
  for (; pos; pos = pos->GetNext()) {
    wxRect rect = pos->GetRect();
    if (rect.GetBottom() > topleft.y)
      break;
  }

  if (!pos)
    pos = down ? GetTree() : GetLastCellInWorksheet();

  // If a cursor is active we start the search there instead
  if (GetActiveCell())
    pos = GetActiveCell()->GetGroup();
  else if (GetSelectionStart())
    pos = GetSelectionStart()->GetGroup();
  else if (m_hCaretActive) {
    pos = (down && m_hCaretPosition && m_hCaretPosition->GetNext())
      ? m_hCaretPosition->GetNext()
      : m_hCaretPosition;
    if(pos == NULL)
      pos = GetTree();
  }

  // If we still don't have a place to start searching we have definitively
  // tried to search in any empty worksheet and know we won't get any result.
  if (!pos)
    return false;

  // If the search string is a UUID, try to find the cell with that UUID
  if (str.Length() >= 32) {
    Cell *uuidCell = FindCellByUUID(str);
    if (uuidCell) {
      GroupCell *group = uuidCell->GetGroup();
      if (group) {
        if (auto *editor = dynamic_cast<EditorCell *>(uuidCell)) {
          SetActiveCell(editor);
          ScrollToCaret();
        } else {
          SetSelection(uuidCell);
          ScheduleScrollToCell(uuidCell);
        }
        UpdateTableOfContents();
        RequestRedraw();
        return true;
      }
    }
  }

  // Remember where to go if we need to wrap the search.
  const GroupCell *startGroup = pos;

  bool wrappedSearch = false;
  bool startInInitial = true;
  bool done = false;
  while (!done) {
    bool foundInGroup = false;

    // Search order for 'down': Prompt -> Editor -> Output
    // For 'up': Output -> Editor -> Prompt

    if (down) {
      // Search order for 'down': Prompt -> Editor -> Output

      // 1. Prompt
      bool skipPrompt = !searchInInput;
      if (startInInitial) {
        if (GetActiveCell() || (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() != pos->GetPrompt()))
          skipPrompt = true;
      }
      if (!skipPrompt && pos->GetPrompt()) {
        wxString text = pos->GetPrompt()->ToString();
        wxString s = str;
        if (ignoreCase) {
          text.MakeLower();
          s.MakeLower();
        }
        if (text.Contains(s)) {
          SetActiveCell(NULL);
          SetSelection(pos->GetPrompt());
          foundInGroup = true;
        }
      }

      // 2. Editor
      if (!foundInGroup) {
        bool skipEditor = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && !GetActiveCell() && GetSelectionStart() != pos->GetPrompt())
            skipEditor = true;
        }
        if (!skipEditor && pos->GetEditable()) {
          if (pos->GetEditable()->FindNext(str, down, ignoreCase)) {
            SetActiveCell(pos->GetEditable());
            foundInGroup = true;
          }
        }
      }

      // 3. Output
      if (!foundInGroup && searchInOutput && pos->GetLabel()) {
        bool outputStarted = true;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() != pos->GetPrompt() && !GetActiveCell())
            outputStarted = false;
        }

        for (const Cell &cell : OnDrawList(pos->GetLabel())) {
          if (!outputStarted) {
            if (&cell == GetSelectionStart())
              outputStarted = true;
            continue;
          }
          wxString text = cell.ToString();
          wxString s = str;
          if (ignoreCase) {
            text.MakeLower();
            s.MakeLower();
          }
          if (text.Contains(s)) {
            SetActiveCell(NULL);
            SetSelection(const_cast<Cell *>(&cell));
            foundInGroup = true;
            break;
          }
        }
      }
    } else {
      // Search order for 'up': Output -> Editor -> Prompt

      // 1. Output
      if (searchInOutput && pos->GetLabel()) {
        bool skipOutput = false;
        if (startInInitial) {
          if (GetActiveCell() || (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt()))
            skipOutput = true;
        }

        if (!skipOutput) {
          std::vector<Cell *> outputCells;
          for (const Cell &cell : OnDrawList(pos->GetLabel())) {
            outputCells.push_back(const_cast<Cell *>(&cell));
          }

          int startIndex = outputCells.size() - 1;
          if (startInInitial && GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && !GetActiveCell()) {
            for (int i = 0; i < (int)outputCells.size(); ++i) {
              if (outputCells[i] == GetSelectionStart()) {
                startIndex = i - 1;
                break;
              }
            }
          }

          for (int i = startIndex; i >= 0; --i) {
            Cell *cell = outputCells[i];
            wxString text = cell->ToString();
            wxString s = str;
            if (ignoreCase) {
              text.MakeLower();
              s.MakeLower();
            }
            if (text.Contains(s)) {
              SetActiveCell(NULL);
              SetSelection(cell);
              foundInGroup = true;
              break;
            }
          }
        }
      }

      // 2. Editor
      if (!foundInGroup) {
        bool skipEditor = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt())
            skipEditor = true;
        }
        if (!skipEditor && pos->GetEditable()) {
          if (pos->GetEditable()->FindNext(str, down, ignoreCase)) {
            SetActiveCell(pos->GetEditable());
            foundInGroup = true;
          }
        }
      }

      // 3. Prompt
      if (!foundInGroup && pos->GetPrompt()) {
        bool skipPrompt = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt())
            skipPrompt = true;
        }
        if (!skipPrompt) {
          wxString text = pos->GetPrompt()->ToString();
          wxString s = str;
          if (ignoreCase) {
            text.MakeLower();
            s.MakeLower();
          }
          if (text.Contains(s)) {
            SetActiveCell(NULL);
            SetSelection(pos->GetPrompt());
            foundInGroup = true;
          }
        }
      }
    }

    if (foundInGroup) {
      if (GetActiveCell())
        ScrollToCaret();
      else if (GetSelectionStart())
        ScheduleScrollToCell(GetSelectionStart(), false);

      UpdateTableOfContents();
      RequestRedraw();
      if ((wrappedSearch) && warn) {
        LoggingMessageDialog dialog(m_findDialog, _("Wrapped search"),
                                    wxEmptyString, wxCENTER | wxOK);
        dialog.ShowModal();
      }
      return true;
    }

    GroupCell *nextPos;
    if (down) {
      nextPos = pos->GetNext();
      if (!nextPos && !wrappedSearch) {
        wrappedSearch = true;
        nextPos = GetTree();
      }
    } else {
      nextPos = pos->GetPrevious();
      if (!nextPos && !wrappedSearch) {
        wrappedSearch = true;
        nextPos = GetLastCellInWorksheet();
      }
    }

    if (!nextPos || (nextPos == startGroup && wrappedSearch))
      done = true;
    
    pos = nextPos;
    startInInitial = false;
  }
  return false;
}

bool Worksheet::FindNext_Regex(const wxString &str, const bool &down,
                               bool searchInInput, bool searchInOutput,
                               bool warn) {
  if (!GetTree())
    return false;

  int starty;
  if (down)
    starty = 0;
  else {
    wxSize canvasSize = GetClientSize();
    starty = canvasSize.y;
  }

  // Default the start of the search at the top or the bottom of the screen
  wxPoint topleft;
  CalcUnscrolledPosition(0, starty, &topleft.x, &topleft.y);
  GroupCell *pos = GetTree();
  for (; pos; pos = pos->GetNext()) {
    wxRect rect = pos->GetRect();
    if (rect.GetBottom() > topleft.y)
      break;
  }

  if (!pos)
    pos = down ? GetTree() : GetLastCellInWorksheet();

  // If a cursor is active we start the search there instead
  if (GetActiveCell())
    pos = GetActiveCell()->GetGroup();
  else if (GetSelectionStart())
    pos = GetSelectionStart()->GetGroup();
  else if (m_hCaretActive) {
    pos = (down && m_hCaretPosition && m_hCaretPosition->GetNext())
      ? m_hCaretPosition->GetNext()
      : m_hCaretPosition;
    if(pos == NULL)
      pos = GetTree();
  }

  // If we still don't have a place to start searching we have definitively
  // tried to search in any empty worksheet and know we won't get any result.
  if (!pos)
    return false;

  if (pos->GetEditable())
    pos->GetEditable()->SearchStartedHere(pos->GetEditable()->GetCaretPosition());

  wxRegEx re(str);
  if (!re.IsValid())
    return false;

  // Remember where to go if we need to wrap the search.
  const GroupCell *startGroup = pos;

  bool wrappedSearch = false;
  bool startInInitial = true;
  bool done = false;
  while (!done) {
    bool foundInGroup = false;

    if (down) {
      // 1. Prompt
      bool skipPrompt = !searchInInput;
      if (startInInitial) {
        if (GetActiveCell() || (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() != pos->GetPrompt()))
          skipPrompt = true;
      }
      if (!skipPrompt && pos->GetPrompt()) {
        wxString text = pos->GetPrompt()->ToString();
        if (re.Matches(text)) {
          SetActiveCell(NULL);
          SetSelection(pos->GetPrompt());
          foundInGroup = true;
        }
      }

      // 2. Editor
      if (!foundInGroup) {
        bool skipEditor = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && !GetActiveCell() && GetSelectionStart() != pos->GetPrompt())
            skipEditor = true;
        }
        if (!skipEditor && pos->GetEditable()) {
          if (pos->GetEditable()->FindNext_RegEx(str, down)) {
            SetActiveCell(pos->GetEditable());
            foundInGroup = true;
          }
        }
      }

      // 3. Output
      if (!foundInGroup && searchInOutput && pos->GetLabel()) {
        bool outputStarted = true;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() != pos->GetPrompt() && !GetActiveCell())
            outputStarted = false;
        }

        for (const Cell &cell : OnDrawList(pos->GetLabel())) {
          if (!outputStarted) {
            if (&cell == GetSelectionStart())
              outputStarted = true;
            continue;
          }
          wxString text = cell.ToString();
          if (re.Matches(text)) {
            SetActiveCell(NULL);
            SetSelection(const_cast<Cell *>(&cell));
            foundInGroup = true;
            break;
          }
        }
      }
    } else {
      // Search order for 'up': Output -> Editor -> Prompt

      // 1. Output
      if (searchInOutput && pos->GetLabel()) {
        bool skipOutput = false;
        if (startInInitial) {
          if (GetActiveCell() || (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt()))
            skipOutput = true;
        }

        if (!skipOutput) {
          std::vector<Cell *> outputCells;
          for (const Cell &cell : OnDrawList(pos->GetLabel())) {
            outputCells.push_back(const_cast<Cell *>(&cell));
          }

          int startIndex = outputCells.size() - 1;
          if (startInInitial && GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && !GetActiveCell()) {
            for (int i = 0; i < (int)outputCells.size(); ++i) {
              if (outputCells[i] == GetSelectionStart()) {
                startIndex = i - 1;
                break;
              }
            }
          }

          for (int i = startIndex; i >= 0; --i) {
            Cell *cell = outputCells[i];
            wxString text = cell->ToString();
            if (re.Matches(text)) {
              SetActiveCell(NULL);
              SetSelection(cell);
              foundInGroup = true;
              break;
            }
          }
        }
      }

      // 2. Editor
      if (!foundInGroup) {
        bool skipEditor = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt())
            skipEditor = true;
        }
        if (!skipEditor && pos->GetEditable()) {
          if (pos->GetEditable()->FindNext_RegEx(str, down)) {
            SetActiveCell(pos->GetEditable());
            foundInGroup = true;
          }
        }
      }

      // 3. Prompt
      if (!foundInGroup && pos->GetPrompt()) {
        bool skipPrompt = !searchInInput;
        if (startInInitial) {
          if (GetSelectionStart() && GetSelectionStart()->GetGroup() == pos && GetSelectionStart() == pos->GetPrompt())
            skipPrompt = true;
        }
        if (!skipPrompt) {
          wxString text = pos->GetPrompt()->ToString();
          if (re.Matches(text)) {
            SetActiveCell(NULL);
            SetSelection(pos->GetPrompt());
            foundInGroup = true;
          }
        }
      }
    }

    if (foundInGroup) {
      if (GetActiveCell())
        ScrollToCaret();
      else if (GetSelectionStart())
        ScheduleScrollToCell(GetSelectionStart(), false);

      UpdateTableOfContents();
      RequestRedraw();
      if ((wrappedSearch) && warn) {
        LoggingMessageDialog dialog(m_findDialog, _("Wrapped search"),
                                    wxEmptyString, wxCENTER | wxOK);
        dialog.ShowModal();
      }
      return true;
    }

    GroupCell *nextPos;
    if (down) {
      nextPos = pos->GetNext();
      if (!nextPos && !wrappedSearch) {
        wrappedSearch = true;
        nextPos = GetTree();
      }
    } else {
      nextPos = pos->GetPrevious();
      if (!nextPos && !wrappedSearch) {
        wrappedSearch = true;
        nextPos = GetLastCellInWorksheet();
      }
    }

    if (!nextPos || (nextPos == startGroup && wrappedSearch))
      done = true;
    
    pos = nextPos;
    startInInitial = false;
  }
  return false;
}

bool Worksheet::CaretVisibleIs() {
  if (m_hCaretActive) {
    int y = -1;
    if (m_hCaretPosition)
      y = m_hCaretPosition->GetCurrentY();

    int view_x, view_y;
    int height, width;

    GetViewStart(&view_x, &view_y);
    GetSize(&width, &height);

    view_y *= m_scrollUnit;
    return ((y >= view_y) && (y <= view_y + height));
  } else {
    if (GetActiveCell()) {
      wxPoint point = GetActiveCell()->PositionToPoint();
      if (point.y < 1) {
        Recalculate();
        point = GetActiveCell()->PositionToPoint();
      }
      return PointVisibleIs(point);
    } else
      return false;
  }
}

bool Worksheet::ScrollToCaretIfNeeded() {
  if (!m_scrollToCaret)
    return false;

  m_scrollToCaret = false;

  RecalculateIfNeeded();
  if (m_hCaretActive) {
    ScheduleScrollToCell(m_hCaretPosition, false);
  } else {
    if (GetActiveCell()) {
      wxPoint point = GetActiveCell()->PositionToPoint();

      // Carets in output cells [maxima questions] get assigned a position
      // only when they are drawn --- or if we update the position manually.
      if (point.y < 0) {
        GetActiveCell()->GetGroup()->UpdateOutputPositions();
        wxASSERT(GetActiveCell()->GetGroup()->GetCurrentPoint().x >= 0);
        wxASSERT(GetActiveCell()->GetGroup()->GetCurrentPoint().y >= 0);
        point = GetActiveCell()->PositionToPoint();
      }
      wxASSERT(point.y >= 0);
      ShowPoint(point);
    } else if (GetSelectionStart()) {
      ScheduleScrollToCell(GetSelectionStart(), false);
    }
  }
  return true;
}

void Worksheet::Replace(const wxString &oldString, const wxString &newString,
                        bool ignoreCase) {
  if (!GetActiveCell())
    return;

  if (GetActiveCell()->ReplaceSelection(oldString, newString, false,
                                        ignoreCase)) {
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

void Worksheet::Replace_RegEx(const wxString &oldString, const wxString &newString) {
  if (!GetActiveCell())
    return;

  if (GetActiveCell()->ReplaceSelection_RegEx(oldString, newString)) {
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

int Worksheet::ReplaceAll(const wxString &oldString, const wxString &newString,
                          bool ignoreCase, bool searchInInput, bool WXUNUSED(searchInOutput)) {
  m_cellPointers.ResetSearchStart();

  if (!GetTree() || !searchInInput)
    return 0;

  int count = 0;
  for (auto &tmp : OnList(GetTree())) {
    EditorCell *editor = tmp.GetEditable();
    if (editor) {
      SetActiveCell(editor);
      int replaced = editor->ReplaceAll(oldString, newString, ignoreCase);
      if (replaced > 0) {
        count += replaced;
        tmp.ResetInputLabel();
        tmp.ResetSize();
      }
    }
  }

  if (count > 0) {
    SetSaved(false);
    Recalculate();
    RequestRedraw();
  }

  return count;
}

int Worksheet::ReplaceAll_RegEx(const wxString &oldString, const wxString &newString,
                                bool searchInInput, bool WXUNUSED(searchInOutput)) {
  m_cellPointers.ResetSearchStart();

  if (!GetTree() || !searchInInput)
    return 0;

  int count = 0;
  for (auto &tmp : OnList(GetTree())) {
    EditorCell *editor = tmp.GetEditable();
    if (editor) {
      SetActiveCell(editor);
      int replaced = editor->ReplaceAll_RegEx(oldString, newString);
      if (replaced > 0) {
        count += replaced;
        tmp.ResetInputLabel();
        tmp.ResetSize();
      }
    }
  }

  if (count > 0) {
    SetSaved(false);
    Recalculate();
    RequestRedraw();
  }

  return count;
}

bool Worksheet::Autocomplete(AutoComplete::autoCompletionType type) {
  EditorCell *editor = GetActiveCell();

  if (!editor)
    return false;

  wxString partial;
  if (type != AutoComplete::esccommand) {
    editor->SelectWordUnderCaret(false, false, true);
    partial = editor->GetSelectionString();

    // If the word we are autocompleting is a placeholder for a file name
    // we want to autocomplete file names, instead.
    if ((type == AutoComplete::command) &&
        ((partial == wxS("file_name")) || (partial == wxS("filename")) ||
         (partial == wxS("file")) || (partial == wxS("S")) ||
         (partial == wxS("imagename")) || (partial == wxS("path")) ||
         (partial == wxS("compiled_filename")) ||
         (partial == wxS("lisp_filename")))) {
      size_t left = editor->SelectionLeft();
      size_t right = editor->SelectionRight();
      wxString text = editor->GetValue();
      if ((left > 0) && (right < text.Length()) && (text[left - 1] == wxS('<')) &&
          (text[right] == wxS('>'))) {
        editor->SetSelection(left - 1, right + 1);
        partial = wxS("\"");
        type = AutoComplete::generalfile;
      }
    }
  }

  if (type == AutoComplete::command) {
    // Let's look if we want to complete a unit instead of a command.
    bool inEzUnit = true;
    wxString frontOfSelection = editor->TextInFrontOfSelection();
    int positionOfEzunitStart = frontOfSelection.rfind(wxS('`'));

    if (positionOfEzunitStart != wxNOT_FOUND) {
      frontOfSelection = frontOfSelection.Mid(static_cast<std::size_t>(positionOfEzunitStart) + 1);
      int numberOfParenthesis = 0;

      for (wxString::const_iterator it = frontOfSelection.begin();
           it != frontOfSelection.end(); ++it) {
        wxChar ch = *it;
        if ((!wxIsalnum(ch)) && (ch != wxS('(')) && (ch != wxS(')')) &&
            (ch != wxS('*')) && (ch != wxS('/')))
          inEzUnit = false;

        if (ch == wxS('('))
          numberOfParenthesis++;
        if (ch == wxS(')')) {
          numberOfParenthesis++;
          if (numberOfParenthesis < 0)
            inEzUnit = false;
        }
      }
    } else
      inEzUnit = false;
    if (inEzUnit)
      type = AutoComplete::unit;

    // If we don't have a unit to complete we perhaps want to autocomplete a
    // package name or the name of a demo file
    if (!inEzUnit) {
      wxString currentCommand = editor->GetCurrentCommand();
      if ((currentCommand == wxS("load")) ||
          (currentCommand == wxS("batchload")) ||
          (currentCommand == wxS("batch")) ||
          (currentCommand == wxS("opena")) ||
          (currentCommand == wxS("opena_binary")) ||
          (currentCommand == wxS("openr")) ||
          (currentCommand == wxS("openr_binary")) ||
          (currentCommand == wxS("openw")) ||
          (currentCommand == wxS("openw_binary")) ||
          (currentCommand == wxS("show_image")) ||
          (currentCommand == wxS("read_array")) ||
          (currentCommand == wxS("read_list")) ||
          (currentCommand == wxS("read_matrix")) ||
          (currentCommand == wxS("read_binary_array")) ||
          (currentCommand == wxS("read_binary_list")) ||
          (currentCommand == wxS("read_binary_matrix")) ||
          (currentCommand == wxS("read_hashed_array")) ||
          (currentCommand == wxS("read_nested_list")) ||
          (currentCommand == wxS("write_data")) ||
          (currentCommand == wxS("write_binary_data"))) {
        type = AutoComplete::generalfile;
        if ((currentCommand == wxS("load")) ||
            (currentCommand == wxS("batchload")) ||
            (currentCommand == wxS("batch")))
          type = AutoComplete::loadfile;

        if (partial.empty())
          partial = wxString("\"");
      }

      if (currentCommand == wxS("demo")) {
        type = AutoComplete::demofile;
        if (partial.empty())
          partial = wxString("\"");
      }

      if ((type == AutoComplete::demofile) ||
          (type == AutoComplete::loadfile)) {
        if (partial.at(0) != wxS('\"')) {
          partial = wxS("\"") + partial;
          // If the editor auto-adds a closing quote this causes auto-completion
          // to fail
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true,
                                   false, true);
        }
        if ((partial.EndsWith("\"") && (!(partial.EndsWith("\\\""))))) {
          partial = partial.Left(partial.Length() - 1);
          editor->ReplaceSelection(editor->GetSelectionString(), partial, true);
        }
      }

      if ((type == AutoComplete::command) && (partial.at(0) == wxS('\"')))
        type = AutoComplete::generalfile;

      if (type == AutoComplete::demofile)
        m_autocomplete.UpdateDemoFiles(
                                       partial, wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if (type == AutoComplete::loadfile)
        m_autocomplete.UpdateLoadFiles(
                                       partial, wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));

      if (type == AutoComplete::generalfile)
        m_autocomplete.UpdateGeneralFiles(
                                          partial, wxFileName(m_currentFile).GetPath(wxPATH_GET_VOLUME));
    }
  }

  if (type == AutoComplete::command) {
    // Update the list of words that might not be defined as maxima function or
    // variable but that still appear on the workSheet.
    m_autocomplete.ClearWorksheetWords();
    for (auto &tmp : OnList(GetTree())) {
      // Don't collect the current word as possible autocompletion.
      if (&tmp != GetActiveCell()->GetGroup()) {
        // Only collect words from Code Cells.
        if ((tmp.GetGroupType() == GC_TYPE_CODE) && tmp.GetEditable())
          m_autocomplete.AddWorksheetWords(tmp.GetEditable()->GetWordList());
      } else {
        if ((tmp.GetGroupType() == GC_TYPE_CODE) && tmp.GetEditable()) {
          auto const &wordList = tmp.GetEditable()->GetWordList();

          // The current unfinished word is no valid autocompletion, if there is
          // such a thing.
          if (!partial.empty()) {
            // Don't remove the current word from autocompletion if it never has
            // been added (which happens if autocompletion is called when the
            // cursor is directly followed by the next command without a space
            // or similar inbetween)
            auto partialAt =
              std::find(wordList.begin(), wordList.end(), partial);
            m_autocomplete.AddWorksheetWords(wordList.begin(), partialAt);
            if (partialAt != wordList.end())
              m_autocomplete.AddWorksheetWords(std::next(partialAt),
                                               wordList.end());
          } else
            m_autocomplete.AddWorksheetWords(wordList);
        }
      }
    }
  }

  m_completions = m_autocomplete.CompleteSymbol(partial, type);
  std::sort(m_completions.begin(), m_completions.end());
  m_autocompleteTemplates = (type == AutoComplete::tmplte);

  /// No completions - clear the selection and return false
  if (m_completions.size() == 0) {
    editor->ClearSelection();
    return false;
  }

  /// If there is only one completion, use it
  if ((m_completions.size() == 1) && (type != AutoComplete::esccommand)) {
    std::size_t start, end;
    editor->GetSelection(&start, &end);

    editor->ReplaceSelection(editor->GetSelectionString(), m_completions.at(0),
                             true, false, true);
    editor->ClearSelection();
    editor->CaretToPosition(start);

    if (type != AutoComplete::tmplte || !editor->FindNextTemplate())
      editor->CaretToPosition(start + m_completions.at(0).Length());

    editor->ResetSize();
    editor->GetGroup()->ResetSize();
    Recalculate(editor->GetGroup());

    RequestRedraw();
  }

  /// If there are more than one completions, popup a menu
  else {
    // Find the position for the popup menu
    RecalculateIfNeeded();
    wxPoint pos = editor->PositionToPoint();
    // There might be no current point yet in this EditorCell.
    if ((pos.x < 0) || (pos.y < 0))
      pos = editor->GetGroup()->GetCurrentPoint();
    wxASSERT((pos.x >= 0) && (pos.y >= 0));
    CalcScrolledPosition(pos.x, pos.y, &pos.x, &pos.y);
    // The popup menu appears half a character too high.
    pos.y +=
      m_configuration->Scale_Px(m_configuration->GetFontSize(TS_TEXT)).Get();
    wxASSERT(!m_autocompletePopup);
    m_autocompletePopup = new AutocompletePopup(this, editor, &m_autocomplete,
                                                type, &m_autocompletePopup);

    // If necessary: Scroll right or down so that the pop-up is visible as a
    // whole.
    wxPoint topleft;
    CalcUnscrolledPosition(0, 0, &topleft.x, &topleft.y);
    int width;
    int height;
    GetClientSize(&width, &height);
    if ((pos.x >= 0) && (pos.y >= 0))
      m_autocompletePopup->SetPosition(pos);
    m_autocompletePopup->Create(this);
    m_autocompletePopup->SetFocus();
    wxRect popupRect = m_autocompletePopup->GetRect();
    wxRect screenRect = wxRect(topleft, topleft + wxPoint(width, height));
    if (screenRect.GetRight() < popupRect.GetRight())
      screenRect.SetLeft(screenRect.GetLeft() + popupRect.GetRight() -
                         screenRect.GetRight());
    if (screenRect.GetBottom() < popupRect.GetBottom())
      screenRect.SetTop(screenRect.GetTop() + popupRect.GetBottom() -
                        screenRect.GetBottom());
    if (screenRect.GetTopLeft() != topleft) {
      Scroll(screenRect.GetTopLeft());
      RequestRedraw();
    }
  }
  return true;
}

void Worksheet::OnComplete(wxCommandEvent &event) {
  if (!GetActiveCell())
    return;

  EditorCell *editor = GetActiveCell();
  int caret = editor->GetCaretPosition();

  int item = event.GetId() - EventIDs::popid_autocomplete_keyword1;
  wxASSERT(item >= 0);
  wxASSERT(item < EventIDs::NumberOfAutocompleteKeywords);

  if (!editor->GetSelectionString().empty())
    editor->ReplaceSelection(editor->GetSelectionString(),
                             m_completions.at(item),
                             true, false, true);
  else
    editor->InsertText(m_completions.at(item));

  if (m_autocompleteTemplates) {
    std::size_t sel_start, sel_end;
    editor->GetSelection(&sel_start, &sel_end);
    editor->ClearSelection();

    editor->CaretToPosition(caret);
    if (!editor->FindNextTemplate())
      editor->CaretToPosition(
                              sel_start +
                              m_completions.at(item).Length());
  }

  editor->ResetSize();
  editor->GetGroup()->ResetSize();
  Recalculate(editor->GetGroup());

  RequestRedraw();
}

bool Worksheet::SectioningMoveIn(GroupCell *parent) {
  if (!parent)
    return false;

  std::list<GroupCell *> sections;
  GroupType type = parent->GetGroupType();
  for (auto &tmp : OnList(parent)) {
    if ((&tmp != parent) &&
        (!tmp.IsLesserGCType(type)))
      break;
    if (tmp.IsHeading())
      sections.push_back(&tmp);
  }

  for (auto &tmp : sections) {
    switch (tmp->GetGroupType()) {
    case GC_TYPE_HEADING6:
      break;
    case GC_TYPE_HEADING5:
      SetCellStyle(tmp, GC_TYPE_HEADING6);
      break;
    case GC_TYPE_SUBSUBSECTION:
      SetCellStyle(tmp, GC_TYPE_HEADING5);
      break;
    case GC_TYPE_SUBSECTION:
      SetCellStyle(tmp, GC_TYPE_SUBSUBSECTION);
      break;
    case GC_TYPE_SECTION:
      SetCellStyle(tmp, GC_TYPE_SUBSECTION);
      break;
    case GC_TYPE_TITLE:
      SetCellStyle(tmp, GC_TYPE_SECTION);
      break;
    default:
      wxASSERT_MSG(false, _("Bug: Encountered a heading I don't know how to "
                            "move in one sectioning unit"));
      return false;
    }
  }
  return true;
}

bool Worksheet::SectioningMoveOut(GroupCell *parent) {
  // parent may be null: it comes from the TOC's "right-clicked" cell, which can
  // be destroyed before this menu command runs (matches SectioningMoveIn).
  if (!parent)
    return false;
  if (!parent->SectioningCanMoveOut())
    return false;

  GroupType type = parent->GetGroupType();
  std::list<GroupCell *> sections;
  for (auto &tmp : OnList(parent)) {
    if ((&tmp != parent) &&
        (!tmp.IsLesserGCType(type)))
      break;
    if (tmp.IsHeading())
      sections.push_back(&tmp);
  }
  for (auto &tmp : sections) {
    switch (tmp->GetGroupType()) {
    case GC_TYPE_HEADING6:
      SetCellStyle(tmp, GC_TYPE_HEADING5);
      break;
    case GC_TYPE_HEADING5:
      SetCellStyle(tmp, GC_TYPE_SUBSUBSECTION);
      break;
    case GC_TYPE_SUBSUBSECTION:
      SetCellStyle(tmp, GC_TYPE_SUBSECTION);
      break;
    case GC_TYPE_SUBSECTION:
      SetCellStyle(tmp, GC_TYPE_SECTION);
      break;
    case GC_TYPE_SECTION:
      SetCellStyle(tmp, GC_TYPE_TITLE);
      break;
    case GC_TYPE_TITLE:
      wxASSERT_MSG(false,
                   _("Bug: Trying to move a title out by one sectioning unit"));
      break;
    default:
      wxASSERT_MSG(false, _("Bug: Encountered a heading I don't know how to "
                            "move out one sectioning unit"));
      return false;
    }
  }
  return true;
}

void Worksheet::SetActiveCellText(const wxString &text) {
  EditorCell *active = GetActiveCell();
  if (active) {
    GroupCell *parent = active->GetGroup();
    if (parent->GetGroupType() == GC_TYPE_CODE && parent->IsMainInput(active)) {
      active->SaveValue();
      active->SetValue(text);
      active->ResetSize();
      active->ResetSize_Recursively();
      parent->ResetSize();
      parent->ResetSize_Recursively();
      parent->ResetInputLabel();
      Recalculate(parent);
      RequestRedraw(parent);
    }
  } else
    OpenHCaret(text);
}

bool Worksheet::InsertText(const wxString &text) {
  CloseAutoCompletePopup();

  if (GetActiveCell()) {
    if (GCContainsCurrentQuestion(GetActiveCell()->GetGroup())) {
      m_followEvaluation = true;
      OpenQuestionCaret(text);
    } else {
      GetActiveCell()->InsertText(text);
      Recalculate(GetActiveCell()->GetGroup());
      RequestRedraw(GetActiveCell()->GetGroup());
    }
  } else
    OpenHCaret(text);
  return true;
}

void Worksheet::OpenNextOrCreateCell() {
  if (m_hCaretPosition && m_hCaretPosition->GetNext()) {
    SetSelection(m_hCaretPosition);
    ActivateNextInput();
  } else
    OpenHCaret();
}

void Worksheet::SelectGroupCell(GroupCell *cell) {
  SetSelection(cell);
  m_hCaretActive = false;
  SetActiveCell(NULL);
  if (cell) {
    if (GCContainsCurrentQuestion(cell)) {
      FollowEvaluation(true);
      OpenQuestionCaret();
    }
    m_hCaretPositionEnd = cell;
    m_hCaretPositionStart = cell;
  }
}

void Worksheet::OnFollow() {
  if (!GetWorkingGroup())
    return;

  FollowEvaluation(true);
  if (GCContainsCurrentQuestion(GetWorkingGroup())) {
    OpenQuestionCaret();
    ScheduleScrollToCell(GetWorkingGroup(), false);
  } else {
    if (GetWorkingGroup()->RevealHidden()) {
      FoldOccurred();
      Recalculate();
    }
    SetSelection(GetWorkingGroup());
    SetHCaret(GetWorkingGroup());
    ScheduleScrollToCell(GetWorkingGroup(), false);
  }
}

Worksheet::MathMLDataObject::MathMLDataObject()
  : wxCustomDataObject(m_mathmlFormat) {}

Worksheet::MathMLDataObject::MathMLDataObject(const wxString &data)
  : wxCustomDataObject(m_mathmlFormat), m_databuf(data.utf8_str())

{
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::wxmDataObject::wxmDataObject() : wxCustomDataObject(m_wxmFormat) {}

Worksheet::wxmDataObject::wxmDataObject(const wxString &data) :
  wxCustomDataObject(m_wxmFormat),
  m_databuf(data.utf8_str()){
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::MathMLDataObject2::MathMLDataObject2()
  : wxCustomDataObject(m_mathmlFormat2) {}

Worksheet::MathMLDataObject2::MathMLDataObject2(const wxString &data)
  : wxCustomDataObject(m_mathmlFormat2),
    m_databuf(data.utf8_str())
{
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::RtfDataObject::RtfDataObject() : wxCustomDataObject(m_rtfFormat) {}

Worksheet::RtfDataObject::RtfDataObject(const wxString &data)
  : wxCustomDataObject(m_rtfFormat),
    m_databuf(data.utf8_str())
{
  SetData(m_databuf.length(), m_databuf.data());
}

Worksheet::RtfDataObject2::RtfDataObject2()
  : wxCustomDataObject(m_rtfFormat2) {}

Worksheet::RtfDataObject2::RtfDataObject2(const wxString &data)
  : wxCustomDataObject(m_rtfFormat2),
    m_databuf(data.utf8_str())
{
  SetData(m_databuf.length(), m_databuf.data());
}

wxString Worksheet::RTFStart() const {
  return WorksheetExport::RTFStart(m_configuration);
}

wxString Worksheet::RTFEnd() const {
  return WorksheetExport::RTFEnd();
}

void Worksheet::OnMouseCaptureLost(wxMouseCaptureLostEvent &) {
  m_leftDown = false;
}

#if wxUSE_ACCESSIBILITY
int Worksheet::GetAccessibilityId(Cell *cell) const {
  if (!cell)
    return 0;

  // Find the top-most group containing this cell
  GroupCell *targetGroup = cell->GetGroup();
  while (targetGroup && targetGroup->GetGroup() != targetGroup) {
    targetGroup = targetGroup->GetGroup();
  }

  int id = 0;
  for (GroupCell *tmp = GetTree(); tmp; tmp = tmp->GetNext()) {
    id++;
    if (tmp == targetGroup)
      return id;
  }
  return 0;
}

Worksheet::AccessibilityInfo::AccessibilityInfo(wxWindow *parent,
                                                Worksheet *worksheet)
  : wxAccessible(worksheet->GetTargetWindow()) {
  m_worksheet = worksheet;
  m_parent = parent;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetName(int WXUNUSED(childId), wxString *name) {
  if (name)
    return (*name = _("Cursor")), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetDescription(int WXUNUSED(childId), wxString *description) {
  if (description)
    return (*description = _("Insertion point between cells")), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetParent(wxAccessible **parent) {
  if (parent)
    return (*parent = m_parent), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetChildCount(int *childCount) {
  if (childCount)
    return (*childCount = 0), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetChild(int childId, wxAccessible **child) {
  if (childId == 0 && child)
    return (*child = this), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::CaretAccessibilityInfo::GetRole(int childId, wxAccRole *role) {
  if (childId == 0 && role)
    return (*role = wxROLE_SYSTEM_CARET), wxACC_OK;
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetName(int childId, wxString *name) {
  if (!name)
    return wxACC_FAIL;

  if (childId == 0) {
    *name = _("wxMaxima worksheet");
    return wxACC_OK;
  }

  wxAccessible *child = nullptr;
  if (GetChild(childId, &child) == wxACC_OK && child)
    return child->GetName(0, name);

  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetState(int childId, long *state) {
  if (!state)
    return wxACC_FAIL;

  if (childId == 0) {
    *state = wxACC_STATE_SYSTEM_FOCUSABLE;
    if (m_worksheet->HasFocus())
      *state |= wxACC_STATE_SYSTEM_FOCUSED;
    return wxACC_OK;
  }

  wxAccessible *child = nullptr;
  if (GetChild(childId, &child) == wxACC_OK && child)
    return child->GetState(0, state);

  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetChildCount(int *childCount) {
  if (!childCount)
    return wxACC_FAIL;

  *childCount = 0;
  for ([[maybe_unused]] const auto &cell : OnList(m_worksheet->GetTree()))
    (*childCount)++;

  (*childCount)++; // The caret is the last child

  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetChild(int childId,
                                                   wxAccessible **child) {
  if (!child)
    return wxACC_FAIL;

  if (childId == 0) {
    *child = this;
    return wxACC_OK;
  }

  int count = 0;
  GroupCell *cell = m_worksheet->GetTree();
  for (int i = 1; cell && i < childId; ++i) {
    cell = cell->GetNext();
    count++;
  }

  if (cell) {
    *child = cell->GetAccessible();
    return wxACC_OK;
  } else {
    // If it's the caret
    int totalCount = 0;
    for ([[maybe_unused]] const auto &c : OnList(m_worksheet->GetTree())) totalCount++;
    if (childId == totalCount + 1) {
      if (!m_caretAccessible) {
         m_caretAccessible = new CaretAccessibilityInfo(this, m_worksheet);
      }
      *child = m_caretAccessible;
      return wxACC_OK;
    }
  }

  return wxACC_FAIL;
}

wxAccStatus
Worksheet::AccessibilityInfo::GetDefaultAction(int childId,
                                               wxString *actionName) {
  if (!actionName)
    return wxACC_FAIL;

  if (childId == 0) {
    *actionName = _("Type");
    return wxACC_OK;
  }

  wxAccessible *acc = NULL;
  if (GetChild(childId, &acc) == wxACC_OK && acc)
    return acc->GetDefaultAction(0, actionName);

  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetParent(wxAccessible **parent) {
  if (!parent)
    return wxACC_FAIL;

  // This object IS the worksheet window's accessible (it is attached via
  // SetAccessible), so its parent is the parent *window's* accessible -- NOT
  // itself. Returning m_worksheet->GetAccessible() here returned `this`, so a
  // screen reader walking up the parent chain looped self->self forever and
  // hung. Let wxWidgets derive the real parent from the window hierarchy.
  *parent = nullptr;
  return wxACC_NOT_IMPLEMENTED;
}

wxAccStatus Worksheet::AccessibilityInfo::GetFocus(int *childId,
                                                  wxAccessible **child) {
  if (!m_worksheet->HasFocus()) {
    if (childId)
      *childId = 0;
    if (child)
      *child = NULL;
    return wxACC_FALSE;
  }

  if (m_worksheet->HCaretActive()) {
    int totalCount = 0;
    for ([[maybe_unused]] const auto &c : OnList(m_worksheet->GetTree())) totalCount++;
    if (childId)
      *childId = totalCount + 1;
    if (child) {
      if (!m_caretAccessible) {
         m_caretAccessible = new CaretAccessibilityInfo(this, m_worksheet);
      }
      *child = m_caretAccessible;
    }
    return wxACC_OK;
  }

  Cell *activeCell = m_worksheet->GetActiveCell();
  int id = m_worksheet->GetAccessibilityId(activeCell);

  if (id > 0) {
    if (childId)
      *childId = id;
    if (child) {
      GroupCell *cell = m_worksheet->GetTree();
      for (int i = 1; cell && i < id; ++i)
        cell = cell->GetNext();
      *child = cell ? cell->GetAccessible() : NULL;
    }
    return wxACC_OK;
  }

  if (childId)
    *childId = 0;
  if (child)
    *child = this;

  return wxACC_OK;
}

wxAccStatus Worksheet::AccessibilityInfo::GetLocation(wxRect &rect,
                                                      int elementId) {
  if (elementId == 0) {
    rect = m_worksheet->GetScreenRect();
    return wxACC_OK;
  }

  wxAccessible *acc = NULL;
  GetChild(elementId, &acc);
  return (acc && acc != this) ? acc->GetLocation(rect, 0) : wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::HitTest(const wxPoint &pt,
                                                  int *childId,
                                                  wxAccessible **childObject) {
  wxRect currentRect = m_worksheet->GetScreenRect();
  if (!currentRect.Contains(pt)) {
    if (childId)
      *childId = 0;
    if (childObject)
      *childObject = NULL;
    return wxACC_FALSE;
  }

  int id = 0;
  for (Cell *cell = m_worksheet->GetTree(); cell; cell = cell->GetNext()) {
    id++;
    Cell *childCell = nullptr;
    if (cell->HitTest(pt, childId, &childCell) == wxACC_OK) {
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

wxAccStatus
Worksheet::AccessibilityInfo::GetDescription(int childId,
                                             wxString *description) {
  if (!description)
    return wxACC_FAIL;

  if (childId == 0) {
    *description = _("The worksheet containing maxima's input and output");
    return wxACC_OK;
  }

  wxAccessible *child;
  if (GetChild(childId, &child) == wxACC_OK && child)
    return child->GetDescription(0, description);

  *description = wxString{};
  return wxACC_FAIL;
}

wxAccStatus Worksheet::AccessibilityInfo::GetRole(int childId,
                                                  wxAccRole *role) {
  if (!role)
    return wxACC_FAIL;

  if (childId == 0) {
    // A worksheet is a document (a sequence of cells), not a bare panel: a
    // document role lets screen readers treat it as readable content.
    *role = wxROLE_SYSTEM_DOCUMENT;
    return wxACC_OK;
  }

  wxAccessible *child;
  if (GetChild(childId, &child) == wxACC_OK && child)
    return child->GetRole(0, role);

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
std::mutex Worksheet::m_drawDCLock;

CellPointers *Cell::GetCellPointers() const {
  return &GetWorksheet()->GetCellPointers();
}

void Worksheet::UpdateTableOfContents()
{
  wxWindow *topLevelWindow = this;
  while(topLevelWindow->GetParent())
    topLevelWindow = topLevelWindow->GetParent();
  wxCommandEvent *event = new wxCommandEvent(TOC_UPDATE_NEEDED_EVENT);
  topLevelWindow->GetEventHandler()->QueueEvent(event);
}

wxDEFINE_EVENT(TOC_UPDATE_NEEDED_EVENT, wxCommandEvent);
