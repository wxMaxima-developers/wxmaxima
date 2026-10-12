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
  The configuration dialogue's "Toolbar" tab.
*/

#ifndef WXMAXIMA_TOOLBARSECTIONSPANEL_H
#define WXMAXIMA_TOOLBARSECTIONSPANEL_H

#include <vector>
#include <wx/panel.h>
#include <wx/rearrangectrl.h>
#include <wx/scrolwin.h>
#include "ToolBarSections.h"

/*! Lets the user reorder the main toolbar's sections and show or hide them

  A list of all sections with a check box each (shown or not), in the order
  the toolbar shows them. wxWidgets' toolbars can't be rearranged by dragging
  their buttons around, so the list is where that happens: an entry can be
  dragged to another position with the mouse, or moved using the "Up" and
  "Down" buttons -- which also work from the keyboard.

  Nothing is stored before Write() is called, so "Cancel" in the dialogue
  leaves the toolbar as it was.
*/
class ToolBarSectionsPanel : public wxScrolled<wxPanel>
{
public:
  explicit ToolBarSectionsPanel(wxWindow *parent);

  //! The sections in the order the list now shows them in
  std::vector<ToolBarSections::Section> GetOrder() const;
  //! Is the check box of this section set?
  bool IsSectionShown(ToolBarSections::Section section) const;
  //! Check or uncheck the box of a section
  void SetSectionShown(ToolBarSections::Section section, bool show);

  /*! Move the entry at position \p from of the list to position \p to

    What dragging an entry with the mouse does; also selects it.
    Positions outside the list are ignored.
  */
  void MoveEntry(int from, int to);

  //! Put the list back to the default order and visibility (stores nothing)
  void ResetToDefaults();

  //! Store the order and visibility in the config. ToolBar::AddTools() applies it.
  void Write() const;

private:
  //! The position in the list the entry for \p section is at
  int PositionOf(ToolBarSections::Section section) const;
  //! Fill the list with \p order, the sections shown checked
  void Fill(const std::vector<ToolBarSections::Section> &order,
            const std::vector<bool> &shown);

  void OnLeftDown(wxMouseEvent &event);
  void OnMotion(wxMouseEvent &event);
  void OnLeftUp(wxMouseEvent &event);

  //! The list with the check boxes. Its items are numbered as in ToolBarSections::DefaultOrder().
  wxRearrangeList *m_list = nullptr;
  //! The position of the entry the user is dragging, or wxNOT_FOUND
  int m_dragFrom = wxNOT_FOUND;
};

#endif // WXMAXIMA_TOOLBARSECTIONSPANEL_H
