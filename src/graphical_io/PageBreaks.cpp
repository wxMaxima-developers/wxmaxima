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
  Defines wxm::BreakIntoPages(), see PageBreaks.h.
*/

#include "PageBreaks.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"

#include <algorithm>

std::vector<wxCoord> wxm::BreakIntoPages(const GroupCell *tree, wxCoord pageHeight) {
  std::vector<wxCoord> pageStarts;
  if (!tree)
    return pageStarts;

  // The units a page break may not split, in reading order: the input part
  // of a group, and each line of its output.
  struct Block {
    wxCoord top;
    wxCoord bottom;
    //! The first block of its group?
    bool startsGroup;
    //! The group it belongs to
    const GroupCell *group;
  };
  std::vector<Block> blocks;
  for (const GroupCell &group : OnList(tree)) {
    const wxRect rect = group.GetRect();
    if (group.GetGroupType() == GC_TYPE_PAGEBREAK) {
      // An empty block that forces a page break before whatever follows.
      blocks.push_back({rect.GetTop(), rect.GetTop(), true, &group});
      continue;
    }
    // The input part ends where the output starts.
    const Cell *output = group.DisplayedOutput();
    wxCoord inputBottom = rect.GetBottom();
    if (output && !group.IsHidden())
      inputBottom = std::min(inputBottom, group.GetOutputRect().GetTop());
    blocks.push_back({rect.GetTop(), inputBottom, true, &group});
    if (!output || group.IsHidden())
      continue;
    bool first = true;
    for (const Cell &cell : OnDrawList(output)) {
      const wxRect cellRect = cell.GetRect();
      if (first || cell.BreakLineHere()) {
        blocks.push_back({cellRect.GetTop(), cellRect.GetBottom(), false, &group});
        first = false;
      } else {
        blocks.back().top = std::min(blocks.back().top, cellRect.GetTop());
        blocks.back().bottom = std::max(blocks.back().bottom, cellRect.GetBottom());
      }
    }
  }
  if (blocks.empty())
    return pageStarts;

  // The first page starts at the top of the worksheet, even if the first
  // block is a bit further down (the worksheet's top margin).
  pageStarts.push_back(std::min<wxCoord>(0, blocks.front().top));
  bool forceBreak = false;
  for (std::size_t i = 0; i < blocks.size(); i++) {
    const Block &block = blocks.at(i);
    if (block.group->GetGroupType() == GC_TYPE_PAGEBREAK) {
      forceBreak = true;
      continue;
    }
    const wxCoord pageStart = pageStarts.back();
    const bool overflows = (block.bottom - pageStart > pageHeight);
    if ((forceBreak || overflows) && (block.top > pageStart)) {
      wxCoord breakAt = block.top;
      // Keep a heading together with what follows it.
      if (block.startsGroup && (i > 0)) {
        const Block &previous = blocks.at(i - 1);
        if (previous.group->IsHeading() && (previous.top > pageStart))
          breakAt = previous.top;
      }
      pageStarts.push_back(breakAt);
    }
    forceBreak = false;
  }
  return pageStarts;
}
