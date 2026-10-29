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

#include "worksheet/OutputTextSelection.h"
#include "cells/TextCell.h"
#include <algorithm>
#include <limits>
#include <vector>

namespace {

//! The cell as a TextCell whose characters a drag may select, or null
TextCell *SelectableText(Cell *cell) {
  auto *text = dynamic_cast<TextCell *>(cell);
  if (text && text->AllowsCharacterSelection())
    return text;
  return nullptr;
}

//! How far y is above or below the rectangle: 0 if it is level with it
int VerticalDistance(const wxRect &rect, wxCoord y) {
  if (y < rect.GetTop())
    return rect.GetTop() - y;
  if (y > rect.GetBottom())
    return y - rect.GetBottom();
  return 0;
}

} // namespace

namespace wxm {

std::optional<DocumentCellPointers::TextRange>
OutputTextRange(Cell *output, wxPoint down, wxPoint up) {
  if (!output)
    return std::nullopt;

  // The output's cells in the order they are drawn, which is the order text
  // is read in.
  std::vector<Cell *> cells;
  for (Cell &cell : OnDrawList(output))
    cells.push_back(&cell);

  // The drag has to start on selectable text: a drag that starts on maths
  // selects maths.
  std::size_t downIndex = cells.size();
  for (std::size_t i = 0; i < cells.size(); ++i)
    if (SelectableText(cells[i]) && cells[i]->GetRect().Contains(down)) {
      downIndex = i;
      break;
    }
  if (downIndex == cells.size())
    return std::nullopt;

  // The pointer needn't be on a character: beside a line it selects up to
  // the line's end (or from its start), between lines or below the text the
  // nearest line counts. Of the selectable cells level with the pointer --
  // or, if none is, the nearest ones -- that is the last one that starts left
  // of it, or the first one if all start right of it. A pointer on maths,
  // though, has left the text.
  std::size_t upIndex = cells.size();
  for (std::size_t i = 0; i < cells.size(); ++i)
    if (cells[i]->GetRect().Contains(up)) {
      if (!SelectableText(cells[i]))
        return std::nullopt;
      upIndex = i;
      break;
    }
  if (upIndex == cells.size()) {
    int bestDistance = std::numeric_limits<int>::max();
    for (std::size_t i = 0; i < cells.size(); ++i) {
      if (!SelectableText(cells[i]))
        continue;
      const wxRect rect = cells[i]->GetRect();
      const int distance = VerticalDistance(rect, up.y);
      if (distance < bestDistance) {
        bestDistance = distance;
        upIndex = i;
      } else if ((distance == bestDistance) && (rect.GetLeft() <= up.x))
        upIndex = i;
    }
  }
  if (upIndex == cells.size())
    return std::nullopt;

  // Everything in between has to be selectable text as well. Otherwise the
  // drag has left the text, and selects whole cells instead.
  const std::size_t firstIndex = std::min(downIndex, upIndex);
  const std::size_t lastIndex = std::max(downIndex, upIndex);
  for (std::size_t i = firstIndex; i <= lastIndex; ++i)
    if (!SelectableText(cells[i]))
      return std::nullopt;

  auto *downCell = static_cast<TextCell *>(cells[downIndex]);
  auto *upCell = static_cast<TextCell *>(cells[upIndex]);
  const std::size_t downPos = downCell->CharacterIndexAt(down.x);
  const std::size_t upPos = upCell->CharacterIndexAt(up.x);

  DocumentCellPointers::TextRange range;
  if ((downIndex < upIndex) || ((downIndex == upIndex) && (downPos <= upPos)))
    range = {downCell, downPos, upCell, upPos};
  else
    range = {upCell, upPos, downCell, downPos};
  if ((range.first == range.last) && (range.from == range.to))
    return std::nullopt;
  return range;
}

wxString TextRangeToString(const DocumentCellPointers::TextRange &range) {
  wxString text;
  for (const Cell &cell : OnDrawList(range.first)) {
    const auto *textCell = dynamic_cast<const TextCell *>(&cell);
    if ((&cell != range.first) && cell.BreakLineHere() &&
        !cell.IsTextContinuation())
      text += wxS('\n');
    if (textCell) {
      const std::size_t from = (&cell == range.first) ? range.from : 0;
      const std::size_t to = (&cell == range.last) ? range.to :
        textCell->DisplayedLength();
      text += textCell->CharactersToString(from, to);
    }
    if (&cell == range.last)
      break;
  }
  return text;
}

} // namespace wxm
