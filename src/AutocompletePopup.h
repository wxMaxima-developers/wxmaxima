// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2016 Gunter Königsmann     <wxMaxima@physikbuch.de>
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

/*! The first number that is open for dynamic ID assignment

  If we want to add additional elements to a pop-up this is the
  lowest ID that is guaranteed to be free for this purpose.
*/

/*! \file
  This file declares the class ContentAssistantPopup.

  The content assistant offers more functionality than AutocompletePopup but
  only works on systems that allow popups to handle key presses.
*/

#ifndef AUTOCOMPLETEPOPUP_H
#define AUTOCOMPLETEPOPUP_H

#include "precomp.h"
#include "Autocomplete.h"
#include "cells/EditorCell.h"
#include <wx/combo.h>
#include <wx/listctrl.h>
#include <vector>

class AutocompletePopup final : public wxListView, public wxComboPopup
{
  // Return pointer to the created control
  wxWindow *GetControl() override { return this; }

  // Translate string into a list selection
  void SetStringValue(const wxString& s) override
    {
      int n = wxListView::FindItem(-1, s);
      if (n >= 0 && n < wxListView::GetItemCount() )
        wxListView::Select(n);
    }
  // Get list selection as a string
  wxString GetStringValue() const override
    { return (m_value >= 0) ? wxListView::GetItemText(m_value) : wxString(); }

private:
  struct DonePtr { AutocompletePopup*& observer; ~DonePtr() { observer = nullptr; } };
  int m_value = -1; // current item index
  //! The current string in the autocompletion
  wxString m_partial;

  wxWindow *m_parent = {};
  const DonePtr m_doneptr;
  std::vector<wxString> m_completions;
  AutoComplete *m_autocomplete = {};
  //! The cell being completed. A CellPtr (not a raw pointer) so it auto-nulls if
  //! the editor is destroyed while this popup is still open - the popup's key
  //! handlers check for that and dismiss themselves instead of dereferencing a
  //! freed cell (see "Long-lived cell references" in CellPtr.h).
  CellPtr<EditorCell> m_editor;
  AutoComplete::autoCompletionType m_type;
  /*! The directory relative file names are resolved against

    (the worksheet file's directory). Needed to re-scan directories while a
    file-name completion descends into subdirectories bash-style.
  */
  wxString m_fileBaseDir;

  //! The position of our pop-up
  wxPoint m_position;
  //! The visible rectangle of the screen
  wxRect m_screenRect;

  //! Does this popup complete file names?
  bool CompletesFiles() const { return AutoComplete::CompletesFiles(m_type); }
  //! Is this completion a directory (a quoted name with a trailing slash)?
  static bool IsDirectoryCompletion(const wxString &completion) {
    return completion.EndsWith(wxS("/\""));
  }
  /*! Descend into the directory named by \p completion instead of finishing

    Replaces the editor text by the completion without its closing quote,
    re-scans that directory and re-filters, so the popup now offers the
    directory's contents.

    \return false if the completion equals the current partial (an empty
    directory - there is nothing to descend into), true otherwise.
  */
  bool DescendIntoDirectory(const wxString &completion);

public:
  //! Define where the popup will appear on Create()
  void SetPosition(wxPoint pos){m_position = pos;}
  //! Create popup control
  bool Create(wxWindow* parent) override;
  virtual ~AutocompletePopup();
  //! Gets the info which keycode the current keypress results in
  void OnChar(wxKeyEvent &event);
  //! Gets the info which key has been pressed with which modifier
  void OnKeyDown(wxKeyEvent &event);

  /*! The constructor of the autocompletion window

    \param parent The parent window
    \param editor The cell that contains the text that is to be completed
    \param autocomplete The autocompletion data
    \param type The type of completion needed
    \param doneptr A pointer that will be set to NULL when the pop-up is destroyed.
    \param fileBaseDir The directory relative file names are resolved against
    (used by the file-name completion types to descend into subdirectories).
  */
  AutocompletePopup(wxWindow *parent, EditorCell *editor, AutoComplete *autocomplete,
                    AutoComplete::autoCompletionType type, AutocompletePopup **doneptr,
                    const wxString &fileBaseDir = {});

  /*! Re-filter the completion list against the current partial

    \param allowAutoFinish With true, a single remaining completion is
    applied immediately (finishing the completion or descending into the
    directory it names). Pass false when the user is deleting: silently
    re-inserting what was just deleted would fight the user - the single
    match is then only displayed.
  */
  void UpdateResults(bool allowAutoFinish = true);

  void OnClick(wxMouseEvent &event);
};

#endif // AUTOCOMPLETEPOPUP_H
