// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

  This file contains the definition of the class TableOfContents that handles the 
  table of contents pane.
*/
#include <memory>
#include <wx/wx.h>
#include <wx/timer.h>
#include <wx/listctrl.h>
#include <wx/dragimag.h>
#include <vector>
#include "precomp.h"
#include "Configuration.h"
#include "RegexCtrl.h"
#include "GroupCell.h"

#ifndef TABLEOFCONTENTS_H
#define TABLEOFCONTENTS_H

enum
{
  structure_ctrl_id = 4,
  structure_regex_id
};

/*! This class generates a pane containing the table of contents.

 */
class TableOfContents : public wxPanel
{
public:
  enum PopIds
  {
    popid_Fold = wxID_HIGHEST + 1500,
    popid_Unfold,
    popid_SelectTocChapter,
    popid_EvalTocChapter,
    popid_ToggleTOCshowsSectionNumbers,
    popid_tocLevel1,
    popid_tocLevel2,
    popid_tocLevel3,
    popid_tocLevel4,
    popid_tocLevel5,
    popid_tocLevel6,
    popid_tocdnd,
    popid_tocMoveIn,
    popid_tocMoveOut
  };

  TableOfContents(wxWindow *parent, int id, Configuration *config, std::unique_ptr<GroupCell> *tree);

  /* The destructor
   */
  ~TableOfContents();

  void OnMouseRightDown(wxListEvent &event);

  //! What happens if someone changes the search box contents
  void OnRegExEvent(wxCommandEvent &ev);

  /*! Update the structure information from the tree

    Since this function traverses the tree and we don't want it 
    to impact the performance too much
    - we call it only on creation of a cell and on leaving it again
    - and we only traverse the tree if the pane is actually shown.
  */
  void UpdateTableOfContents(GroupCell *pos);

  //! Get the nth Cell in the table of contents.
  GroupCell *GetCell(long index);

  //! Returns the cell that was last right-clicked on.
  GroupCell *RightClickedOn()
    { return m_cellRightClickedOn; }

  GroupCell *DNDStart() {return m_dndStartCell;}
  GroupCell *DNDEnd() {return m_dndEndCell;}
protected:
  void OnSize(wxSizeEvent &event);
  void OnDragStart(wxListEvent &evt);
  void OnMouseUp(wxMouseEvent &evt);
  void OnMouseCaptureLost(wxMouseCaptureLostEvent &event);
  void OnMouseMotion(wxMouseEvent &event);
  void OnTimer(wxTimerEvent &event);

  wxString TocEntryString(GroupCell *cell);
private:
  void UpdateStruct();
  std::unique_ptr<GroupCell> *m_tree;
  GroupCell *m_dndStartCell;
  GroupCell *m_dndEndCell;
  wxTimer m_scrollUpTimer;
  wxTimer m_scrollDownTimer;
  wxDragImage *m_dragImage = NULL;
  std::vector<GroupCell *> m_displayedGroupCells;
  //! How many toc items did the user drag at the same time?
  unsigned int m_numberOfCaptionsDragged;
  GroupCell *m_cellRightClickedOn;
  //! The item that was dragged away at the start of the current drag-and-drop
  size_t m_dragStart = -1;
  size_t m_dragStop = -1;
  long m_dragCurrentPos = -1;
  //! The position the dragged item was when we last displayed the reordered toc
  int m_dragFeedback_Last = -1;
  //! The last selected item
  long m_lastSelection;
  
  //! Update the displayed contents.
  void UpdateDisplay();

  wxListCtrl *m_displayedItems;
  RegexCtrl *m_regex;
  //! The items we displayed the last time update() was called
  wxArrayString m_items_old;
  Configuration *m_configuration;

  std::vector<GroupCell *> m_structure;
};

#endif // TABLEOFCONTENTS_H
