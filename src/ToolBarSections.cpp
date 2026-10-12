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
  The sections the main toolbar is made of, and the order they are shown in.
*/

#include "ToolBarSections.h"
#include <algorithm>
#include <wx/tokenzr.h>

namespace ToolBarSections {

namespace {
//! Everything that is known about a section, apart from its translated name
struct ToolBarSectionInfo {
  Section section;
  //! The name in the config file
  const wxChar *key;
  //! The config key that says if the section is shown
  const wxChar *visibilityKey;
  bool shownByDefault;
  /*! Neighbouring sections of the same group aren't separated

    0 means "never separate from anything", which only the flexible space is.
  */
  int group;
};

/*! The sections, in their default order

  The visibility keys of the sections that could already be hidden before
  their order could be changed are the ones wxMaxima always used, so the
  user's choices survive.
*/
const ToolBarSectionInfo g_toolBarSections[] = {
  {Section::New, wxS("new"), wxS("Toolbar/showNew"), true, 1},
  {Section::OpenSave, wxS("openSave"), wxS("Toolbar/showOpenSave"), true, 1},
  {Section::Print, wxS("print"), wxS("Toolbar/showPrint"), true, 2},
  {Section::UndoRedo, wxS("undoRedo"), wxS("Toolbar/showUndoRedo"), false, 3},
  {Section::Options, wxS("options"), wxS("Toolbar/showOptions"), true, 4},
  {Section::CopyPaste, wxS("copyPaste"), wxS("Toolbar/showCopyPaste"), true, 5},
  {Section::SelectAll, wxS("selectAll"), wxS("Toolbar/showSelectAll"), true, 5},
  {Section::Search, wxS("search"), wxS("Toolbar/showSearch"), true, 6},
  {Section::MaximaControl, wxS("maximaControl"), wxS("Toolbar/showMaximaControl"), true, 7},
  {Section::Evaluate, wxS("evaluate"), wxS("Toolbar/showEvaluate"), true, 8},
  {Section::HideCode, wxS("hideCode"), wxS("Toolbar/showHideCode"), true, 9},
  {Section::CellStyle, wxS("cellStyle"), wxS("Toolbar/showCellStyle"), true, 10},
  {Section::TextFormat, wxS("textFormat"), wxS("Toolbar/showTextFormat"), true, 10},
  {Section::Animation, wxS("animation"), wxS("Toolbar/showAnimation"), true, 10},
  {Section::FlexibleSpace, wxS("flexibleSpace"), wxS("Toolbar/showFlexibleSpace"), true, 0},
  {Section::Help, wxS("help"), wxS("Toolbar/showHelp"), true, 11}
};

const ToolBarSectionInfo &SectionInfoFor(Section section) {
  for (const auto &info : g_toolBarSections)
    if (info.section == section)
      return info;
  // Only reachable if a section was added to the enum but not to g_toolBarSections,
  // which every test of this file would notice.
  return g_toolBarSections[0];
}
} // namespace

const std::vector<Section> &DefaultOrder() {
  static const std::vector<Section> order = [] {
    std::vector<Section> result;
    for (const auto &info : g_toolBarSections)
      result.push_back(info.section);
    return result;
  }();
  return order;
}

wxString Key(Section section) { return SectionInfoFor(section).key; }

wxString VisibilityConfigKey(Section section) {
  return SectionInfoFor(section).visibilityKey;
}

bool ShownByDefault(Section section) { return SectionInfoFor(section).shownByDefault; }

std::vector<Section> ParseOrder(const wxString &stored) {
  std::vector<Section> order;
  wxStringTokenizer tokens(stored, wxS(","));
  while (tokens.HasMoreTokens()) {
    wxString key = tokens.GetNextToken();
    key.Trim(true);
    key.Trim(false);
    for (const auto &info : g_toolBarSections)
      if ((key == info.key) &&
          (std::find(order.begin(), order.end(), info.section) == order.end()))
        order.push_back(info.section);
  }

  // Insert every section the stored order didn't know after the closest
  // section that precedes it in the default order (or first, if there is
  // none). Walking the default order front to back means a run of several
  // new sections keeps its own order, too.
  const auto &defaults = DefaultOrder();
  for (auto missing = defaults.begin(); missing != defaults.end(); ++missing) {
    if (std::find(order.begin(), order.end(), *missing) != order.end())
      continue;
    auto insertAt = order.begin();
    for (auto predecessor = std::make_reverse_iterator(missing);
         predecessor != defaults.rend(); ++predecessor) {
      auto found = std::find(order.begin(), order.end(), *predecessor);
      if (found != order.end()) {
        insertAt = found + 1;
        break;
      }
    }
    order.insert(insertAt, *missing);
  }
  return order;
}

wxString OrderToString(const std::vector<Section> &order) {
  wxString result;
  for (const auto section : order) {
    if (!result.IsEmpty())
      result += wxS(",");
    result += Key(section);
  }
  return result;
}

bool SeparatorBetween(Section left, Section right) {
  const int leftGroup = SectionInfoFor(left).group;
  const int rightGroup = SectionInfoFor(right).group;
  if ((leftGroup == 0) || (rightGroup == 0))
    return false;
  return leftGroup != rightGroup;
}

} // namespace ToolBarSections
