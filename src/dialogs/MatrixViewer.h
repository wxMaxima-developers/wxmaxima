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

#ifndef MATRIXVIEWER_H
#define MATRIXVIEWER_H

#include "precomp.h"
#include <wx/wx.h>
#include <memory>

class Cell;
class Configuration;
class GroupCell;
class MatrCell;
class Worksheet;

/*! A window that shows a matrix the worksheet only shows part of (GH #2344)

  A matrix too large for the window is elided or scrolls (see MatrCell).
  Double-clicking one opens this viewer, which shows a copy of the whole
  matrix -- every row and every column -- in a worksheet of its own that
  scrolls as far as the matrix needs.

  The viewer is independent of the matrix it was opened from: it takes a
  copy (through the matrix's XML, so the copy's cells belong to the viewer's
  own Configuration), so re-evaluating or deleting the original doesn't
  affect it, and several viewers can be open at once. It is read-only: its
  worksheet doesn't react to clicks or keys, so nothing can be typed into
  it or evaluated there.

  The viewer's Configuration is a copy of the worksheet's, so the matrix
  looks the same as in the worksheet (fonts, colours, zoom), except that
  ConfigureForViewer() makes it show every matrix in full, whatever
  wx_matrix()'s oversized option asked for, and hides the input and the
  output label.
*/
class MatrixViewer : public wxFrame
{
public:
  /*! Opens a viewer showing this matrix

    \param parent The window the viewer belongs to; it closes with it.
    \param matrix The matrix to show. Only copied: the viewer keeps no
                  reference to it.
    \param config The worksheet's configuration, which the viewer's own is
                  copied from.
  */
  MatrixViewer(wxWindow *parent, const MatrCell &matrix, Configuration *config);
  ~MatrixViewer() override;

  //! The worksheet the viewer shows the matrix in
  Worksheet *GetWorksheet() const { return m_worksheet; }

  /*! Makes a copy of the worksheet's configuration suitable for the viewer

    Shows every matrix in full -- the configuration doesn't let a matrix
    ask for anything else, so a nested matrix or one made by
    wx_matrix(..., oversized=elide) is shown in full, too -- and hides the
    code cell's (empty) input and the output label. Makes the configuration
    temporary, so none of that is written to the config file when it is
    destroyed.

    Call it only once the viewer's worksheet exists: the worksheet's
    constructor reads the config file into the configuration, undoing
    anything set before.
  */
  static void ConfigureForViewer(Configuration &config);

  /*! A group cell whose output is a copy of this matrix

    The copy is made through the matrix's XML, parsed with \p config, so
    every cell of it belongs to \p config -- which a Cell::Copy() would not
    ensure. The output label is empty: the viewer shows only the matrix.
  */
  static std::unique_ptr<GroupCell> CopyForViewer(const MatrCell &matrix,
                                                  Configuration *config);

  /*! The matrix a double-click at this point should open a viewer for

    \param group The group cell that was double-clicked.
    \param point The point that was clicked, in worksheet (unscrolled)
                 coordinates.

    Returns the outermost matrix at that point that the worksheet only shows
    part of (MatrCell::IsShownPartially()), or nullptr if there is none: a
    matrix that fits the window has nothing more to show.
  */
  static MatrCell *PartiallyShownMatrixAt(const GroupCell *group, wxPoint point);

private:
  //! Searches this cell list, and everything inside it, for PartiallyShownMatrixAt()
  static MatrCell *PartiallyShownMatrixIn(Cell *list, wxPoint point);
  //! Lays the worksheet out and repaints it, as wxMaxima's own idle handler does
  void OnIdle(wxIdleEvent &event);
  //! Closes the viewer on Escape
  void OnCharHook(wxKeyEvent &event);
  //! Sizes the window to fit the matrix, within limits of the screen
  void FitToMatrix();

  /*! The viewer's own copy of the worksheet's configuration

    Declared (and so destroyed) before the worksheet pointer, but it has to
    outlive the worksheet, which the destructor therefore destroys first.
  */
  std::unique_ptr<Configuration> m_configuration;
  //! The worksheet showing the matrix; a child window, owned by wxWidgets
  Worksheet *m_worksheet = nullptr;
};

#endif // MATRIXVIEWER_H
