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

#include "ToolBarSectionsPanel.h"
#include "ToolBar.h"
#include <algorithm>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>


ToolBarSectionsPanel::ToolBarSectionsPanel(wxWindow *parent)
  : wxScrolled<wxPanel>(parent, wxID_ANY) {
  SetScrollRate(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());
  const auto &defaults = ToolBarSections::DefaultOrder();

  // wxRearrangeList numbers its items in the order they were passed in and
  // describes the order they are shown in by these numbers, with an unchecked
  // item's number bit-inverted. Our items are numbered as in DefaultOrder().
  wxArrayString names;
  for (const auto section : defaults)
    names.Add(ToolBar::SectionName(section));
  wxArrayInt order;
  for (const auto section : ToolBar::SectionOrder()) {
    const int item = static_cast<int>(
      std::find(defaults.begin(), defaults.end(), section) - defaults.begin());
    order.Add(ToolBar::ShowSection(section) ? item : ~item);
  }

  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  auto *explanation = new wxStaticText(
    this, wxID_ANY,
    _("The sections of the toolbar, from left to right. Drag a section "
      "to another place or use the buttons to move it; uncheck it to "
      "hide it."));
  // Unwrapped, the text alone would make the tab wider than the dialogue.
  explanation->Wrap(400 * GetContentScaleFactor());
  vbox->Add(explanation,
            wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));
  auto *rearrange = new wxRearrangeCtrl(this, wxID_ANY, wxDefaultPosition,
                                        wxDefaultSize, order, names);
  m_list = rearrange->GetList();
  vbox->Add(rearrange, wxSizerFlags(1).Expand().Border(
              wxALL, 5 * GetContentScaleFactor()));
  auto *reset = new wxButton(this, wxID_ANY, _("Default order"));
  reset->SetToolTip(_("Show the toolbar's sections in their original order "
                      "and only the ones shown by default"));
  reset->Bind(wxEVT_BUTTON,
              [this](wxCommandEvent &WXUNUSED(event)) { ResetToDefaults(); });
  vbox->Add(reset, wxSizerFlags().Border(wxALL, 5 * GetContentScaleFactor()));
  SetSizer(vbox);
  FitInside();

  m_list->Bind(wxEVT_LEFT_DOWN, &ToolBarSectionsPanel::OnLeftDown, this);
  m_list->Bind(wxEVT_MOTION, &ToolBarSectionsPanel::OnMotion, this);
  m_list->Bind(wxEVT_LEFT_UP, &ToolBarSectionsPanel::OnLeftUp, this);
}

std::vector<ToolBarSections::Section> ToolBarSectionsPanel::GetOrder() const {
  const auto &defaults = ToolBarSections::DefaultOrder();
  std::vector<ToolBarSections::Section> result;
  for (int item : m_list->GetCurrentOrder()) {
    if (item < 0)
      item = ~item;
    result.push_back(defaults.at(item));
  }
  return result;
}

int ToolBarSectionsPanel::PositionOf(ToolBarSections::Section section) const {
  const auto order = GetOrder();
  return static_cast<int>(std::find(order.begin(), order.end(), section) -
                          order.begin());
}

bool ToolBarSectionsPanel::IsSectionShown(ToolBarSections::Section section) const {
  return m_list->IsChecked(PositionOf(section));
}

void ToolBarSectionsPanel::SetSectionShown(ToolBarSections::Section section, bool show) {
  m_list->Check(PositionOf(section), show);
}

void ToolBarSectionsPanel::MoveEntry(int from, int to) {
  const int count = static_cast<int>(m_list->GetCount());
  if ((from < 0) || (from >= count) || (to < 0) || (to >= count))
    return;
  // wxRearrangeList can only swap the selected entry with a neighbour.
  m_list->SetSelection(from);
  for (int position = from; position > to; position--)
    m_list->MoveCurrentUp();
  for (int position = from; position < to; position++)
    m_list->MoveCurrentDown();
}

void ToolBarSectionsPanel::Fill(const std::vector<ToolBarSections::Section> &order,
                                const std::vector<bool> &shown) {
  for (size_t i = 0; i < order.size(); i++) {
    MoveEntry(PositionOf(order[i]), static_cast<int>(i));
    m_list->Check(static_cast<unsigned int>(i), shown[i]);
  }
}

void ToolBarSectionsPanel::ResetToDefaults() {
  const auto &defaults = ToolBarSections::DefaultOrder();
  std::vector<bool> shown;
  for (const auto section : defaults)
    shown.push_back(ToolBarSections::ShownByDefault(section));
  Fill(defaults, shown);
}

void ToolBarSectionsPanel::Write() const {
  ToolBar::SectionOrder(GetOrder());
  for (const auto section : ToolBarSections::DefaultOrder())
    ToolBar::ShowSection(section, IsSectionShown(section));
}

void ToolBarSectionsPanel::OnLeftDown(wxMouseEvent &event) {
  m_dragFrom = m_list->HitTest(event.GetPosition());
  // Let the list select the entry and toggle its check box as usual
  event.Skip();
}

void ToolBarSectionsPanel::OnMotion(wxMouseEvent &event) {
  event.Skip();
  if ((m_dragFrom == wxNOT_FOUND) || !event.LeftIsDown())
    return;
  const int over = m_list->HitTest(event.GetPosition());
  if ((over == wxNOT_FOUND) || (over == m_dragFrom))
    return;
  // Move the entry as soon as the pointer is over another one, so the list
  // always shows where it will end up.
  MoveEntry(m_dragFrom, over);
  m_dragFrom = over;
}

void ToolBarSectionsPanel::OnLeftUp(wxMouseEvent &event) {
  m_dragFrom = wxNOT_FOUND;
  event.Skip();
}
