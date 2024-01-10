// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class Structure

  Structure is the class that serves as the Table-Of-Contents sidebar.
*/

#include <list>
#include <memory>
#include <wx/sizer.h>
#include "TableOfContents.h"
#include "EventIDs.h"

TableOfContents::TableOfContents(wxWindow *parent, int id,
                                 Configuration *config,
                                 std::unique_ptr<GroupCell> *tree)
  : wxPanel(parent, id), m_tree(tree), m_scrollUpTimer(this, wxUP),
    m_scrollDownTimer(this, wxDOWN) {
  m_configuration = config;
  m_displayedItems = new wxListCtrl(
                                    this, structure_ctrl_id, wxDefaultPosition, wxDefaultSize,
                                    wxLC_SINGLE_SEL | wxLC_ALIGN_LEFT | wxLC_REPORT | wxLC_NO_HEADER);
  m_displayedItems->AppendColumn(wxEmptyString);
  m_regex = new RegexCtrl(this, structure_regex_id, config, "TableOfContents");

  // A box whose 1st row is growable
  wxSizer *box = new wxBoxSizer(wxVERTICAL);

  box->Add(m_displayedItems, wxSizerFlags(1).Expand());
  box->Add(m_regex, wxSizerFlags().Expand());
  m_lastSelection = -1;
  m_regex->Connect(REGEX_EVENT,
                   wxCommandEventHandler(TableOfContents::OnRegExEvent), NULL,
                   this);
  Connect(wxEVT_SIZE, wxSizeEventHandler(TableOfContents::OnSize));
  Connect(wxEVT_LIST_ITEM_RIGHT_CLICK,
          wxListEventHandler(TableOfContents::OnMouseRightDown));
  m_displayedItems->Connect(wxEVT_LIST_BEGIN_DRAG,
                            wxListEventHandler(TableOfContents::OnDragStart),
                            NULL, this);
  m_displayedItems->Connect(wxEVT_LEFT_UP,
                            wxMouseEventHandler(TableOfContents::OnMouseUp),
                            NULL, this);
  m_displayedItems->Connect(wxEVT_MOTION,
                            wxMouseEventHandler(TableOfContents::OnMouseMotion),
                            NULL, this);
  m_displayedItems->Connect(
                            wxEVT_MOUSE_CAPTURE_LOST,
                            wxMouseCaptureLostEventHandler(TableOfContents::OnMouseCaptureLost), NULL,
                            this);
  Connect(wxEVT_TIMER, wxTimerEventHandler(TableOfContents::OnTimer));
  SetSizer(box);
  FitInside();
}

void TableOfContents::OnTimer(wxTimerEvent &event) {
  switch (event.GetId()) {
  case wxUP: {
    if (m_displayedItems->GetItemCount() < 1)
      return;
    auto item = m_displayedItems->GetTopItem() - 1;
    if (item < 0) {
      item = 0;
    }
    m_displayedItems->EnsureVisible(item);
    break;
  }
  case wxDOWN: {
    if (m_displayedItems->GetItemCount() < 1)
      return;
    auto item =
      m_displayedItems->GetTopItem() + m_displayedItems->GetCountPerPage();
    if (item >= m_displayedItems->GetItemCount()) {
      item = m_displayedItems->GetItemCount() - 1;
    }
    m_displayedItems->EnsureVisible(item);
    break;
  }
  default: {
  }
  }
}

void TableOfContents::OnMouseMotion(wxMouseEvent &event) {
  if (m_dragImage != NULL) {
    int flags;
    m_dragCurrentPos =
      m_displayedItems->HitTest(event.GetPosition(), flags, NULL);
    if ((m_dragCurrentPos >= 0) &&
        (static_cast<std::size_t>(m_dragCurrentPos) >=
         m_displayedItems->GetItemCount() - m_numberOfCaptionsDragged))
      m_dragCurrentPos = m_numberOfCaptionsDragged - 1;
    if (m_dragFeedback_Last != m_dragCurrentPos) {
      m_dragImage->Hide();
      UpdateDisplay();
    }
    m_dragImage->Move(wxPoint(event.GetX(), event.GetY()));
    if (m_dragFeedback_Last != m_dragCurrentPos) {
      m_dragImage->Show();
      m_dragFeedback_Last = m_dragCurrentPos;
    }
    if (event.GetY() < 0) {
      if (!m_scrollUpTimer.IsRunning()) {
        m_scrollUpTimer.Start(50);
      }
    } else
      m_scrollUpTimer.Stop();
    if (event.GetY() > m_displayedItems->GetRect().GetHeight()) {
      if (!m_scrollDownTimer.IsRunning()) {
        m_scrollDownTimer.Start(50);
      }
    } else
      m_scrollDownTimer.Stop();
  }
  event.Skip();
}

void TableOfContents::OnMouseCaptureLost(wxMouseCaptureLostEvent &event) {
  m_dragStart = -1;
  m_scrollUpTimer.Stop();
  m_scrollDownTimer.Stop();
  UpdateDisplay();
  event.Skip();
}

void TableOfContents::OnDragStart(wxListEvent &evt) {
  //  UpdateStruct();
  m_dragStart = evt.GetIndex();
  m_dragFeedback_Last = m_dragStart;
  if (m_dragStart >= 0) {
    // Tell the OS that until the drop event this control wants to receive all
    // mouse events
    if (!m_displayedItems->HasCapture()) {
      wxString dragImageLabel = m_displayedItems->GetItemText(evt.GetIndex());
      if (dragImageLabel.Length() > 15)
        dragImageLabel = dragImageLabel.Left(12) + wxS("...");
      m_dragImage = new wxDragImage(dragImageLabel);
      m_dragImage->BeginDrag(
                             wxPoint(-20 * GetContentScaleFactor(), -20 * GetContentScaleFactor()),
                             m_displayedItems);
    }
    m_dragImage->Show();

    // For the visual feedback: How many toc items does the user drag?
    m_numberOfCaptionsDragged = 1;
    GroupCell *tmp = m_displayedGroupCells[evt.GetIndex()];
    auto index = evt.GetIndex() + 1;
    tmp = tmp->GetNext();
    while ((tmp != NULL) && (index <= m_displayedItems->GetItemCount())) {
      if (!tmp->IsLesserGCType(
                               m_displayedGroupCells[evt.GetIndex()]->GetGroupType()))
        break;
      if (m_displayedGroupCells[index] == tmp) {
        index++;
        m_numberOfCaptionsDragged++;
      }
      tmp = tmp->GetNext();
    }
  }
}

void TableOfContents::OnMouseUp(wxMouseEvent &evt) {
  m_scrollUpTimer.Stop();
  m_scrollDownTimer.Stop();
  if (m_dragImage != NULL) {
    m_dragImage->Hide();
    m_dragImage->EndDrag();
    delete m_dragImage;
    m_dragImage = NULL;
  }
  int flags;
  m_dragStop = m_displayedItems->HitTest(evt.GetPosition(), flags, NULL);
  if ((m_dragStop >= 0) &&
      (static_cast<std::size_t>(m_dragStop) >=
       m_displayedItems->GetItemCount() - m_numberOfCaptionsDragged))
    m_dragStop = m_numberOfCaptionsDragged - 1;
  if ((m_dragStart >= 0) && (m_dragStop >= 0) && (m_dragStart != m_dragStop)) {
    wxWindow *mainWin = this;
    while (mainWin->GetParent() != NULL)
      mainWin = mainWin->GetParent();
    wxCommandEvent *tocEv = new wxCommandEvent;
    tocEv->SetEventType(wxEVT_MENU);
    tocEv->SetId(EventIDs::popid_tocdnd);
    mainWin->GetEventHandler()->QueueEvent(tocEv);
  }
  UpdateDisplay();
  evt.Skip();
}

void TableOfContents::OnSize(wxSizeEvent &event) {
  m_displayedItems->SetColumnWidth(0, event.GetSize().x);
  event.Skip();
}

TableOfContents::~TableOfContents() {
  if (m_dragImage != NULL) {
    m_dragImage->EndDrag();
    m_dragImage = NULL;
  }
}

void TableOfContents::UpdateStruct() {
  m_structure.clear();

  // Get the current list of tokens that should be in the Table Of Contents.
  for (auto &cell : OnList(m_tree->get())) {
    int groupType = cell.GetGroupType();
    if ((groupType == GC_TYPE_TITLE) || (groupType == GC_TYPE_SECTION) ||
        (groupType == GC_TYPE_SUBSECTION) ||
        (groupType == GC_TYPE_SUBSUBSECTION) ||
        (groupType == GC_TYPE_HEADING5) || (groupType == GC_TYPE_HEADING6))
      m_structure.push_back(&cell);
  }
}

void TableOfContents::UpdateTableOfContents(GroupCell *pos) {
  auto selection = m_lastSelection;
  if (IsShown()) {
    auto item = m_displayedItems->GetNextItem(-1, wxLIST_NEXT_ALL,
                                              wxLIST_STATE_SELECTED);
    UpdateStruct();

    std::vector<GroupCell *>::const_iterator it = m_displayedGroupCells.begin();

    // Select the cell with the cursor
    if(!m_displayedGroupCells.empty())
      {
        int cursorItem = -1;
        for (const auto &cell : OnList(m_tree->get())) {
          if ((it != m_displayedGroupCells.end()) && (&cell == *it))
            {
              ++it;
              ++cursorItem;
            }
          if (&cell == pos) {
            if(cursorItem < 0)
              cursorItem = 0;
            selection = cursorItem;
            break;
          }
        }
      }
    if ((selection >= 0) && (item != selection)) {
      if (m_displayedItems->GetItemCount() < static_cast<std::size_t>(selection))
        selection = m_displayedItems->GetItemCount() - 1;
      if ((selection >= 0) && (selection < m_displayedItems->GetItemCount())) {
        m_displayedItems->SetItemState(
                                       selection, wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED,
                                       wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
        m_displayedItems->EnsureVisible(selection);
      }
      m_lastSelection = selection;
    }
    UpdateDisplay();
  }
}

void TableOfContents::UpdateDisplay() {
  m_displayedGroupCells.clear();

  // Create a wxArrayString containing all section/chapter/... titles we want
  // to display
  for (const auto &i: m_structure) {
    // Indentation further reduces the screen real-estate. So it is to be used
    // sparingly. But we should perhaps add at least a little bit of it to make
    // the list more readable.
    int tocDepth = 0;
    switch (i->GetGroupType()) {
    case GC_TYPE_TITLE:
      tocDepth = 0;
      break;
    case GC_TYPE_SECTION:
      tocDepth = 1;
      break;
    case GC_TYPE_SUBSECTION:
      tocDepth = 2;
      break;
    case GC_TYPE_SUBSUBSECTION:
      tocDepth = 3;
      break;
    case GC_TYPE_HEADING5:
      tocDepth = 4;
      break;
    case GC_TYPE_HEADING6:
      tocDepth = 5;
      break;
    default:
      break;
    }

    // Limit the toc depth shown
    if (m_configuration->TocDepth() <= tocDepth)
      continue;

    wxString curr = TocEntryString(i);

    if (m_regex->Matches(curr)) {
      m_displayedGroupCells.push_back(i);
    }
  }

  std::vector<GroupCell *> displayedCells_dndOrder;

  if ((m_dragStart >= 0) && (m_dragCurrentPos >= 0) &&
      (m_dragStart != m_dragCurrentPos)) {
    m_dndStartCell = m_displayedGroupCells[m_dragStart];

    std::list<GroupCell *> m_draggedCells;
    std::list<GroupCell *> m_otherCells;
    for (size_t i = 0; i < m_structure.size(); i++) {
      if ((i >= static_cast<std::size_t>(m_dragStart)) && (i < static_cast<unsigned long>(m_dragStart) + m_numberOfCaptionsDragged))
        m_draggedCells.push_back(m_displayedGroupCells[i]);
      else
        m_otherCells.push_back(m_displayedGroupCells[i]);
    }

    m_dndEndCell = NULL;

    for (size_t index = 0; index < m_structure.size(); index++) {
      if (index >= static_cast<std::size_t>(m_dragCurrentPos)) {
        m_dndEndCell = m_tree->get();
        if (m_otherCells.empty()) {
          while (m_dndEndCell->GetNext() != NULL)
            m_dndEndCell = m_dndEndCell->GetNext();
        } else {
          while ((m_dndEndCell->GetNext() != NULL) &&
                 (m_dndEndCell->GetNext() != m_otherCells.front()))
            m_dndEndCell = m_dndEndCell->GetNext();
          if (m_dndEndCell->GetNext() != m_otherCells.front())
            m_dndEndCell = NULL;
        }

        while (!m_draggedCells.empty()) {
          displayedCells_dndOrder.push_back(m_draggedCells.front());
          m_draggedCells.pop_front();
        }
        break;
      } else {
        if (!m_otherCells.empty()) {
          displayedCells_dndOrder.push_back(m_otherCells.front());
          m_otherCells.pop_front();
        }
      }
    }

    while (!m_otherCells.empty()) {
      displayedCells_dndOrder.push_back(m_otherCells.front());
      m_otherCells.pop_front();
    }
  } else
    displayedCells_dndOrder = m_displayedGroupCells;

  wxArrayString items;
  for (const auto &i : displayedCells_dndOrder)
    items.Add(TocEntryString(i));
  // Work around a wxWidgets bug: items==m_items_old if items is empty and
  // m_items_old isn't.
  if ((items != m_items_old) || (items.GetCount() == 0)) {
    // Delete superfluous items
    for (unsigned int i = m_displayedItems->GetItemCount();
         i > displayedCells_dndOrder.size(); i--)
      m_displayedItems->DeleteItem(i - 1);

    // Update the name of all existing items and add new items, if necessary.
    // We don't just empty the item list and create a new one since on Windows
    // this causes excessive flickering.
    for (signed int i = 0; i < (signed)items.GetCount(); i++) {
      if ((i < m_displayedItems->GetItemCount()) &&
          (m_displayedItems->GetItemCount() > 0))
        m_displayedItems->SetItemText(
                                      i, TocEntryString(displayedCells_dndOrder[i]));
      else
        m_displayedItems->InsertItem(
                                     i, TocEntryString(displayedCells_dndOrder[i]));

      if (displayedCells_dndOrder[i]->GetHiddenTree())
        m_displayedItems->SetItemTextColour(
                                            i, wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT));
      else
        m_displayedItems->SetItemTextColour(
                                            i, wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));
    }
    m_items_old = items;
  }
}

GroupCell *TableOfContents::GetCell(std::size_t index) {
  if (index > m_structure.size())
    return NULL;

  return m_displayedGroupCells[index];
}

wxString TableOfContents::TocEntryString(GroupCell *cell) {
  wxString curr;
  if (cell->GetEditable())
    curr = cell->GetEditable()->ToString();

  if (m_configuration->TocShowsSectionNumbers()) {
    if (cell->GetPrompt() != NULL)
      curr = cell->GetPrompt()->ToString() + wxS(" ") + curr;
    curr.Trim(false);
  } else
    switch (cell->GetGroupType()) {
    case GC_TYPE_TITLE:
      break;
    case GC_TYPE_SECTION:
      curr = wxS("  ") + curr;
      break;
    case GC_TYPE_SUBSECTION:
      curr = wxS("    ") + curr;
      break;
    case GC_TYPE_SUBSUBSECTION:
      curr = wxS("      ") + curr;
      break;
    case GC_TYPE_HEADING5:
      curr = wxS("        ") + curr;
      break;
    case GC_TYPE_HEADING6:
      curr = wxS("          ") + curr;
      break;
    default: {
    }
    }

  // Respecting linebreaks doesn't make much sense here.
  curr.Replace(wxS("\n"), wxS(" "));
  return curr;
}

void TableOfContents::OnRegExEvent(wxCommandEvent &WXUNUSED(ev)) {
  UpdateDisplay();
}

void TableOfContents::OnMouseRightDown(wxListEvent &event) {
  if (event.GetIndex() < 0)
    return;
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  m_cellRightClickedOn = m_structure[event.GetIndex()];

  if (m_cellRightClickedOn != NULL) {
    if (m_cellRightClickedOn->GetHiddenTree())
      popupMenu->Append(EventIDs::popid_Unfold, _("Unhide"), wxEmptyString,
                        wxITEM_NORMAL);
    else {
      popupMenu->Append(EventIDs::popid_Fold, _("Hide"), wxEmptyString, wxITEM_NORMAL);
      popupMenu->Append(EventIDs::popid_SelectTocChapter, _("Select"), wxEmptyString,
                        wxITEM_NORMAL);
      popupMenu->Append(EventIDs::popid_EvalTocChapter, _("Evaluate"), wxEmptyString,
                        wxITEM_NORMAL);
      if (m_cellRightClickedOn->SectioningCanMoveIn()) {
        wxString message;
        switch (m_cellRightClickedOn->GetGroupType()) {
        case GC_TYPE_HEADING5:
          message = _("Convert to Heading 6");
          break;
        case GC_TYPE_SUBSUBSECTION:
          message = _("Convert to Heading 5");
          break;
        case GC_TYPE_SUBSECTION:
          message = _("Convert to Sub-Subsection");
          break;
        case GC_TYPE_SECTION:
          message = _("Convert to Subsection");
          break;
        case GC_TYPE_TITLE:
          message = _("Convert to Section");
          break;
        default: {
        }
        }
        popupMenu->Append(EventIDs::popid_tocMoveIn, message, wxEmptyString,
                          wxITEM_NORMAL);
      }
      if (m_cellRightClickedOn->SectioningCanMoveOut()) {
        wxString message;
        switch (m_cellRightClickedOn->GetGroupType()) {
        case GC_TYPE_HEADING6:
          message = _("Convert to Heading 5");
          break;
        case GC_TYPE_HEADING5:
          message = _("Convert to Sub-Subsection");
          break;
        case GC_TYPE_SUBSUBSECTION:
          message = _("Convert to Subsection");
          break;
        case GC_TYPE_SUBSECTION:
          message = _("Convert to Section");
          break;
        case GC_TYPE_SECTION:
          message = _("Convert to Title");
          break;
        default: {
        }
        }
        popupMenu->Append(EventIDs::popid_tocMoveOut, message, wxEmptyString,
                          wxITEM_NORMAL);
      }
    }
  }

  wxMenu *tocLevelMenu = new wxMenu();
  tocLevelMenu->AppendRadioItem(EventIDs::popid_tocLevel1, _("1 Level"));
  for(int i = 2; i < EventIDs::NumberOfTocLevels() - 1; i++)
    tocLevelMenu->AppendRadioItem(EventIDs::popid_tocLevel1 + i - 1,
                                  wxString::Format(_("%li Levels"), static_cast<long>(i)));
  tocLevelMenu->AppendRadioItem(EventIDs::popid_tocLevel1 +
                                EventIDs::NumberOfTocLevels() - 1, _("All Levels"));

  if (m_configuration->TocDepth() < EventIDs::NumberOfTocLevels())
    tocLevelMenu->Check(EventIDs::popid_tocLevel1 + m_configuration->TocDepth() - 1,
                        true);
  else
    tocLevelMenu->Check(EventIDs::popid_tocLevel1 + EventIDs::NumberOfTocLevels() - 1, true);
  popupMenu->Append(wxID_ANY, _("Toc levels shown here"), tocLevelMenu);

  if (popupMenu->GetMenuItemCount() > 0)
    popupMenu->AppendSeparator();
  popupMenu->AppendCheckItem(EventIDs::popid_ToggleTOCshowsSectionNumbers,
                             _("Show section numbers"));
  popupMenu->Check(EventIDs::popid_ToggleTOCshowsSectionNumbers,
                   m_configuration->TocShowsSectionNumbers());

  // create menu if we have any items
  if (popupMenu->GetMenuItemCount() > 0)
    PopupMenu(popupMenu.get());
}
