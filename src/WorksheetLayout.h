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
  The worksheet's layout engine: scheduling, the recalculation walk and the
  virtual-size update, independent of the Worksheet window class.

  WorksheetLayout owns the layout pipeline that used to be spread across
  Worksheet: RequestRecalculation() records where the next layout pass must
  start, RecalculateIfNeeded() performs the walk (sizing dirty cells,
  repositioning the rest), and AdjustSize()/GetMaxPoint() derive the scrollable
  size from the resulting cell positions. It touches the window only through
  the narrow WorksheetView interface (WorksheetSizeMath.h) and reaches the cell
  tree through callbacks, so the whole request -> recalculate -> resize
  pipeline can be driven headlessly by a mock view - see test_WorksheetLayout.
*/

#ifndef WORKSHEETLAYOUT_H
#define WORKSHEETLAYOUT_H

#include "WorksheetSizeMath.h"
#include "cells/CellPtr.h"
#include <functional>

class Cell;
class GroupCell;
class Configuration;

/*! The layout/recalculation engine of a worksheet.

  Talks to the cell tree via the callbacks handed to the constructor, to the
  window via a WorksheetView and to the Configuration for metrics and the
  canvas size. Worksheet holds one instance and forwards its layout entry
  points here; the headless unit test drives an instance without any window.
*/
class WorksheetLayout {
public:
  /*! Construct the engine.

    \param config      The Configuration supplying metrics, the canvas size and
                       the recalculation drawing context.
    \param view        The window surface (real Worksheet or a test mock). Held
                       by reference: must outlive this object.
    \param getTree     Returns the first GroupCell of the worksheet (or null).
    \param getLastCell Returns the last GroupCell of the worksheet (or null).
  */
  WorksheetLayout(Configuration *config, WorksheetView &view,
                  std::function<GroupCell *()> getTree,
                  std::function<GroupCell *()> getLastCell);
  // Holds a reference to its view and is bound to one worksheet's tree
  // callbacks; copying would alias that ownership.
  WorksheetLayout(const WorksheetLayout &) = delete;
  WorksheetLayout &operator=(const WorksheetLayout &) = delete;

  /*! Schedule a recalculation of the worksheet starting with the cell start.

    This only *records* where the next layout pass has to start (it marks the
    group dirty and moves m_recalculateStart); it does not size or position any
    cell. The actual work - and, crucially, AdjustSize() once the cell
    positions are correct - happens in RecalculateIfNeeded(). Calling
    AdjustSize() (or otherwise reading cell geometry) right after this, without
    a RecalculateIfNeeded() in between, reads stale positions.
  */
  void RequestRecalculation(Cell *start);

  /*! Perform the scheduled recalculation, if one is pending.

    One walk from the scheduled start point drives both modes: with \p timeout
    set the pass is time-sliced (yields after \p timeSliceMs, resuming on the
    next call); otherwise the whole scheduled range is laid out in one go.
    Ends by adjusting the virtual size once the cell positions are valid.

    \returns true if any layout work was done or remains pending.
  */
  bool RecalculateIfNeeded(bool timeout = false, long timeSliceMs = 50);

  /*! Adjust the virtual size and scrollbars to the document's extent.

    Defers (via RequestAdjustSize()) while a recalculation is still pending,
    since the cell positions it reads would be stale - see the implementation
    comment for the full story.
  */
  void AdjustSize();

  /*! Get the coordinates of the bottom right point of the worksheet.

    Only valid once no recalculation is pending; asserts on that.
  */
  void GetMaxPoint(int *width, int *height);

  //! Inform the configuration about the view's current client size.
  void UpdateConfigurationClientSize();

  /*! The horizontal space a group cell of the given width occupies.

    That is the cell's own width plus the equal left and right margins the
    worksheet reserves around every group cell. Used both when measuring the
    document width (GetMaxPoint) and while walking the recalculation.
  */
  int GroupCellWidthWithMargins(int cellWidth) const;

  /*! The scroll granularity (device px per scroll unit) AdjustSize() last
    applied. Worksheet's scroll handlers read it to convert scroll positions
    to pixels. */
  int GetScrollUnit() const { return m_scrollUnit; }

  /*! Flag the virtual (scroll) size as needing re-adjustment.

    Set when a cell changed height (via the callback Worksheet registers on the
    Configuration, so cells can flag it without depending on the view) or when
    layout is scheduled; consumed by RecalculateIfNeeded(), which calls
    AdjustSize() once the cell positions are valid.
  */
  void RequestAdjustSize() { m_adjustWorksheetSizeNeeded = true; }

  //! Drop any pending recalculation (e.g. when the document is cleared).
  void CancelPendingRecalculation() { m_recalculateStart = nullptr; }

  //! GroupCells walked by the most recent RecalculateIfNeeded() pass (visited
  //! includes cheap reposition-only cells; recalculated counts only the cells
  //! that actually needed the expensive re-layout). Exposed for tests and
  //! diagnostics: a localized change should recalculate very few cells, so a
  //! large count on a small edit signals an over-broad recalculation.
  int GetLastCellsVisited() const { return m_lastCellsVisited; }
  int GetLastCellsRecalculated() const { return m_lastCellsRecalculated; }

private:
  //! The settings storage, also supplying metrics and the recalc DC.
  Configuration *m_configuration;
  //! The narrow window surface the size pipeline drives.
  WorksheetView &m_view;
  //! Returns the first GroupCell of the worksheet's cell tree.
  std::function<GroupCell *()> m_getTree;
  //! Returns the last GroupCell of the worksheet's cell tree.
  std::function<GroupCell *()> m_getLastCell;
  //! Where to start recalculation. NULL = No recalculation needed.
  CellPtr<GroupCell> m_recalculateStart;
  //! Cache for the document's content width; < 0 = needs remeasuring.
  int m_maxWidth_Cached = -1;
  //! Dedupe cache for AdjustSize(): the last virtual size handed to the view.
  WorksheetVirtualSizeCache m_virtualSizeCache;
  /*! The size of a scroll step

    Defines the size of a scroll step, but besides that also the accuracy
    wxScrolledCanvas calculates some widths in.
  */
  int m_scrollUnit = 10;
  //! Does the worksheet's virtual (scroll) size need re-adjusting?
  bool m_adjustWorksheetSizeNeeded = false;
  //! Bookkeeping from the most recent RecalculateIfNeeded() pass; see the
  //! getters above.
  int m_lastCellsVisited = 0;
  int m_lastCellsRecalculated = 0;
};

#endif // WORKSHEETLAYOUT_H
