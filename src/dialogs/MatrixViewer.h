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
#include <wx/fdrepdlg.h>
#include <memory>
#include <optional>
#include "cells/MatrixBlock.h"
#include "dialogs/FindReplacePane.h"

class Cell;
class Configuration;
class FindReplaceDialog;
class GroupCell;
class MatrCell;
class Worksheet;
namespace WorksheetSearch { class Matcher; }

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
  wx_matrix()'s oversized option asked for, hides the input and the
  output label and doesn't indent the output. The matrix itself is shown
  without brackets and with alternating row and column bands (see
  CopyForViewer()).

  Ctrl+F opens a search dialog that searches the matrix entry by entry:
  each match selects its entry, as a click on it would, and scrolls it into
  view (see FindNext()). Only the entries are searched, not the matrix as a
  whole: the worksheet's own search, which knows only whole output cells,
  would find the one matrix and nothing inside it.
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
  //! The viewer's copy of the matrix
  MatrCell *GetMatrix() const;

  /*! \name Searching the matrix
    @{
  */
  //! Opens the search dialog (Ctrl+F), or brings it to the front
  void OpenFindDialog();
  //! The search dialog, or nullptr if it hasn't been opened
  FindReplaceDialog *GetFindDialog() const { return m_findDialog; }

  /*! Selects the next entry that matches, and scrolls it into view

    \param str        What to search for
    \param down       Search in reading order (row by row), or backwards
    \param ignoreCase Only for a plain search: a regex decides that itself
    \param regex      Is str a regular expression?
    \param warn       Tell the user if nothing matched, or if the search
                      went past the end and started over

    Starts behind the selected entry, or at the first (or, searching
    backwards, the last) entry if none is selected, and wraps around, so
    repeating it steps through all matches. If the selected entry is the
    only match, it stays selected. Returns false if nothing matched.
  */
  bool FindNext(const wxString &str, bool down, bool ignoreCase, bool regex,
                bool warn = true);

  /*! The next entry of a matrix whose text matches

    \param matrix    The matrix to search
    \param start     The entry to start at; std::nullopt = before the first
                     entry (searching down) or after the last one (searching
                     up)
    \param down      Search row by row, left to right, or backwards
    \param inclusive Can start itself be the match? True for a search that
                     is refined while the search term is typed, false for
                     one that steps to the next match.
    \param matcher   What counts as a match
    \param wrapped   If not nullptr, set to whether the search went past the
                     end of the matrix and started over

    An entry matches if the text of all of it (Cell::ListToString()) does.
    Every entry counts, including ones the worksheet would elide, and even
    start itself is checked last, after a full round. std::nullopt if no
    entry matches.
  */
  static std::optional<MatrixEntry> FindEntry(const MatrCell &matrix,
                                              std::optional<MatrixEntry> start,
                                              bool down, bool inclusive,
                                              const WorksheetSearch::Matcher &matcher,
                                              bool *wrapped = nullptr);
  //! @}

  /*! Makes a copy of the worksheet's configuration suitable for the viewer

    Shows every matrix in full -- the configuration doesn't let a matrix
    ask for anything else, so a nested matrix or one made by
    wx_matrix(..., oversized=elide) is shown in full, too -- and hides the
    code cell's (empty) input, the output label and the cell brackets, and
    moves the output to the left margin. Makes the configuration
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
    The copy has no brackets and is banded (MatrCell::AlwaysBanded()).
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
  //! Closes the viewer on Escape, opens the search dialog on Ctrl+F
  void OnCharHook(wxKeyEvent &event);
  //! The search dialog's "Find" button or Enter
  void OnFind(wxFindDialogEvent &event);
  //! Searches as the search term is typed, if the user wants that
  void IncrementalSearch();
  //! Selects an entry of the matrix and scrolls the viewer to it
  void SelectEntry(const MatrixEntry &entry);
  //! The entry selected in the viewer, if exactly one entry is selected
  std::optional<MatrixEntry> SelectedEntry() const;
  //! Sizes the window to fit the matrix, within limits of the screen
  void FitToMatrix();

  /*! The viewer's own copy of the worksheet's configuration

    Declared (and so destroyed) before the worksheet pointer, but it has to
    outlive the worksheet, which the destructor therefore destroys first.
  */
  std::unique_ptr<Configuration> m_configuration;
  //! The worksheet showing the matrix; a child window, owned by wxWidgets
  Worksheet *m_worksheet = nullptr;
  /*! The search dialog, or nullptr

    A child window, but destroyed by the destructor: its pane still uses
    m_findData while it goes.
  */
  FindReplaceDialog *m_findDialog = nullptr;
  //! The search term and settings, starting as the worksheet's own search left them
  FindReplacePane::FindReplaceData m_findData;
  /*! Where a search refined while the search term is typed starts

    The entry that was selected when the search dialog was opened or the
    last match of an explicit search, so that typing another letter narrows
    the search down from there instead of moving on at every keystroke.
  */
  std::optional<MatrixEntry> m_searchOrigin;
  //! The search the last incremental search was for, to tell when it changes
  wxString m_oldFindString;
  long m_oldFindFlags = 0;
  bool m_oldRegexSearch = false;
};

#endif // MATRIXVIEWER_H
