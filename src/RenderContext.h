// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Declares RenderContext: the per-render-pass state of a worksheet.

  First step of splitting the Configuration god class (which couples the
  persistent settings store with the state of the current render pass): the
  purely render-related state - which device contexts to measure and draw
  with, the canvas geometry, the update region/clipping, the background
  brush and the layout deadline - now lives in this class. Configuration
  owns a RenderContext and delegates its existing accessors to it, so the
  many Configuration dependents are unaffected.
*/

#ifndef RENDERCONTEXT_H
#define RENDERCONTEXT_H

#include <wx/brush.h>
#include <wx/dcclient.h>
#include <wx/gdicmn.h>
#include <wx/window.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <memory>
#include <vector>

class Cell;

/*! The state of the current worksheet render pass.

  Pure state: this class knows nothing about the configuration store, the
  worksheet or the cells (the redraw trace holds Cell pointers, but only as
  opaque keys). Copying a RenderContext (for printing, the diff viewer etc.)
  copies the geometry, the printing flag and the recalc-DC pointer but
  neither the owned worksheet client DC (the copy doesn't own the window),
  a running layout deadline, a redraw trace nor the worksheet-size-adjust
  request (all of which belong to the original's render pass).
*/
class RenderContext
{
public:
  RenderContext() = default;
  RenderContext(const RenderContext &o)
    : m_recalcDC(o.m_recalcDC),
      m_canvasSize(o.m_canvasSize),
      m_updateRegion(o.m_updateRegion),
      m_visibleRegion(o.m_visibleRegion),
      m_worksheetPosition(o.m_worksheetPosition),
      m_backgroundBrush(o.m_backgroundBrush),
      m_clipToDrawRegion(o.m_clipToDrawRegion),
      m_printing(o.m_printing)
    {
    }
  RenderContext &operator=(const RenderContext &) = delete;

  //! The dc "recalculate" (measuring text) currently draws to.
  wxDC *GetRecalcDC() const { return m_recalcDC; }
  //! Tells us which dc to measure text sizes on.
  void SetRecalcDC(wxDC *dc) { m_recalcDC = dc; }

  /*! Creates a client DC for the given window and measures text on it.

    Used when a worksheet is attached: measuring on the worksheet's own DC
    makes the measurements match what drawing on it will produce.
  */
  void AttachWorksheetDC(wxWindow *worksheet)
    {
      m_worksheetDC = std::unique_ptr<wxClientDC>(new wxClientDC(worksheet));
      m_recalcDC = m_worksheetDC.get();
    }
  //! Drops the worksheet client DC created by AttachWorksheetDC() again.
  void DetachWorksheetDC()
    {
      if (m_recalcDC == m_worksheetDC.get())
        m_recalcDC = NULL;
      m_worksheetDC.reset();
    }

  //! The size of the worksheet's visible window.
  wxSize GetCanvasSize() const { return m_canvasSize; }
  //! Sets the size of the worksheet's visible window.
  void SetCanvasSize(wxSize siz) { m_canvasSize = siz; }

  //! The rectangle of the worksheet that is currently being redrawn.
  wxRect GetUpdateRegion() const { return m_updateRegion; }
  //! Sets the rectangle of the worksheet that is currently being redrawn.
  void SetUpdateRegion(wxRect rect) { m_updateRegion = rect; }

  //! The rectangle of the worksheet that is currently visible.
  wxRect GetVisibleRegion() const { return m_visibleRegion; }
  //! Sets the rectangle of the worksheet that is currently visible.
  void SetVisibleRegion(wxRect visibleRegion) { m_visibleRegion = visibleRegion; }

  //! Are we currently rendering for the printer rather than the screen?
  bool Printing() const { return m_printing; }
  //! Sets whether we are currently rendering for the printer.
  void SetPrinting(bool printing) { m_printing = printing; }

  //! Starts (or restarts) recording which cells this render pass has drawn.
  void ClearAndEnableRedrawTracing()
    {
      if (!m_cellRedrawTrace)
        m_cellRedrawTrace.reset(new std::vector<const Cell *>);
      else
        m_cellRedrawTrace->clear();
    }
  //! Records that this cell has been drawn, if tracing is enabled.
  void NotifyOfCellRedraw(const Cell *cell)
    {
      // This operation is fast and doesn't allocate after the render context
      // was used for a few screen redraws.
      if (m_cellRedrawTrace && cell)
        m_cellRedrawTrace->push_back(cell);
    }
  /*! Calls report(count, cell) for each cell that was drawn more than once.

    The cells are opaque keys to this class; formatting the report is the
    caller's business. Sorting the trace is over two orders of magnitude
    faster, per cell, than having counters in a map or hash.
  */
  template <typename Report> void ReportMultipleRedraws(Report report)
    {
      if (!m_cellRedrawTrace)
        return;
      std::sort(m_cellRedrawTrace->begin(), m_cellRedrawTrace->end());
      size_t counter = 0;
      const Cell *prev = nullptr;
      for (const Cell *cell : *m_cellRedrawTrace) {
        if (prev != cell) {
          if (counter > 1)
            report(counter, prev);
          prev = cell;
          counter = 1;
        } else
          ++counter;
      }
      if (counter > 1)
        report(counter, prev);
    }

  //! Do we want to omit drawing outside the current update region?
  bool ClipToDrawRegion() const { return m_clipToDrawRegion; }
  //! Whether to omit drawing outside the current update region.
  void ClipToDrawRegion(bool clipToDrawRegion)
    { m_clipToDrawRegion = clipToDrawRegion; }

  //! Where is the worksheet to be drawn?
  wxPoint GetWorksheetPosition() const { return m_worksheetPosition; }
  //! Sets where the worksheet is to be drawn.
  void SetWorksheetPosition(wxPoint worksheetPosition)
    { m_worksheetPosition = worksheetPosition; }

  //! The brush the worksheet's background is painted with.
  wxBrush GetBackgroundBrush() const { return m_backgroundBrush; }
  //! Sets the brush the worksheet's background is painted with.
  void SetBackgroundBrush(const wxBrush &brush) { m_backgroundBrush = brush; }

  //! Starts a layout deadline this many seconds in the future.
  void SetLayoutDeadline(int seconds)
    {
      m_layoutDeadline = std::chrono::steady_clock::now() +
                         std::chrono::seconds(seconds);
      m_layoutCancelled.store(false, std::memory_order_relaxed);
      m_layoutDeadlineActive = true;
    }
  //! Stops the layout deadline and clears the cancelled flag.
  void ClearLayoutCancelled()
    {
      m_layoutDeadlineActive = false;
      m_layoutCancelled.store(false, std::memory_order_relaxed);
    }
  //! Has the current layout pass exceeded its deadline?
  bool IsLayoutCancelled() const
    {
      if (m_layoutCancelled.load(std::memory_order_relaxed))
        return true;
      if (m_layoutDeadlineActive &&
          std::chrono::steady_clock::now() >= m_layoutDeadline) {
        m_layoutCancelled.store(true, std::memory_order_relaxed);
        return true;
      }
      return false;
    }

private:
  //! The dc "recalculate" currently draws to, if any.
  wxDC *m_recalcDC = nullptr;
  //! The client DC of the attached worksheet, owned by us (see AttachWorksheetDC())
  std::unique_ptr<wxClientDC> m_worksheetDC;
  //! The size of the worksheet's visible window
  wxSize m_canvasSize;
  //! The rectangle of the worksheet that is currently being redrawn
  wxRect m_updateRegion;
  //! The rectangle of the worksheet that is currently visible
  wxRect m_visibleRegion;
  //! Where the worksheet is to be drawn
  wxPoint m_worksheetPosition;
  //! The brush the worksheet's background is painted with
  wxBrush m_backgroundBrush;
  //! Do we want to omit drawing outside the current update region?
  bool m_clipToDrawRegion = true;
  //! Are we currently rendering for the printer rather than the screen?
  bool m_printing = false;
  /*! The cells drawn so far in this render pass, if tracing is enabled

    Used (in debug mode) to detect cells that are drawn twice per refresh,
    which most likely means a 2D-displayed cell draws its sub-cells but
    didn't remove them from the list of cells to draw afterwards.
  */
  std::unique_ptr<std::vector<const Cell *>> m_cellRedrawTrace;
  //! When the current layout pass has to give up (see IsLayoutCancelled())
  std::chrono::steady_clock::time_point m_layoutDeadline;
  //! Is m_layoutDeadline currently armed?
  bool m_layoutDeadlineActive = false;
  //! Set once the deadline has fired; mutable so IsLayoutCancelled() can latch it
  mutable std::atomic<bool> m_layoutCancelled{false};
};

#endif // RENDERCONTEXT_H
