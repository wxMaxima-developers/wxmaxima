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

#ifndef MATRIXSCROLLBARS_H
#define MATRIXSCROLLBARS_H

#include "cells/CellPtr.h"
#include "cells/MatrCell.h"
#include "cells/MatrixScrollHost.h"

#include <wx/region.h>
#include <wx/scrolbar.h>
#include <wx/scrolwin.h>

#include <memory>
#include <vector>

/*! The native scrollbars of the matrices a worksheet shows in a viewport

  A MatrCell in Configuration::OversizedMatrices::scroll mode knows where its
  scrollbars belong, but owns no window: see MatrixScrollHost for why. This
  is the worksheet's side of that arrangement. It keeps one real wxScrollBar
  per scrollbar a matrix needs, as a child window of the worksheet, and
  keeps them in line with the matrices:

  - Every matrix that needs scrollbars reports in (MatrixDrawn()) each time
    it is drawn. A paint handler must not create, move or show windows, so
    that only notes the fact; EndPaint() then schedules Sync() for after the
    paint, which does the actual work.
  - A matrix that goes away -- deleted, re-evaluated, folded away, its output
    hidden -- simply stops being drawn. So a scrollbar is hidden when the
    place its matrix was last drawn at gets repainted without the matrix,
    and destroyed once the matrix itself has been (CellPtr notices that).
  - The worksheet's own scrolling needs nothing from here: wxScrolled moves
    its child windows along with its contents.

  Moving a matrix's scrollbar scrolls the matrix (MatrCell::ScrollTo()) and
  redraws just it; no relayout is needed. The mouse wheel over the matrix
  still scrolls the worksheet, so a matrix never traps the wheel, and the
  scrollbars never keep the keyboard focus: typing always goes to the
  worksheet.
*/
class MatrixScrollbars final : public MatrixScrollHost
{
public:
  explicit MatrixScrollbars(wxScrolled<wxWindow> *worksheet);
  ~MatrixScrollbars() override;
  MatrixScrollbars(const MatrixScrollbars &) = delete;
  MatrixScrollbars &operator=(const MatrixScrollbars &) = delete;

  wxCoord ScrollbarThickness() const override;
  void MatrixDrawn(MatrCell *matrix) override;

  //! To be called before a paint draws any cell
  void BeginPaint();
  /*! To be called once a paint has drawn all it is going to

    \param updateRegion what the paint repainted, in window coordinates
  */
  void EndPaint(const wxRegion &updateRegion);
  //! Creates, moves, shows and hides scrollbars as the last paint found
  void Sync();

  //! How many scrollbars are visible right now (for tests)
  size_t VisibleScrollbars() const;

private:
  //! One matrix and its scrollbars
  struct Entry
  {
    CellPtr<MatrCell> matrix;
    wxScrollBar *horizontal = nullptr;
    wxScrollBar *vertical = nullptr;
    //! Was the matrix drawn by the paint that is under way?
    bool drawnThisPaint = false;
    //! Should its scrollbars be visible?
    bool shown = false;
    //! Where the matrix was drawn last, in worksheet coordinates
    wxRect lastDrawnAt;
  };

  //! Brings one scrollbar in line with what its matrix wants
  void SyncScrollbar(wxScrollBar *&scrollbar, int orientation, bool wanted,
                     const wxRect &where, int position, int thumbSize,
                     int range);
  //! Scrolls the matrix whose scrollbar was moved
  void OnScroll(wxScrollEvent &event);
  //! Destroys an entry's scrollbars
  static void DestroyScrollbars(Entry &entry);

  wxScrolled<wxWindow> *const m_worksheet;
  std::vector<std::unique_ptr<Entry>> m_entries;
  //! Is a Sync() already waiting to run?
  bool m_syncPending = false;
};

#endif // MATRIXSCROLLBARS_H
