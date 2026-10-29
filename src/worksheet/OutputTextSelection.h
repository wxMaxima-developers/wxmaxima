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
  Selecting some of the characters of the text in a cell's output (GH #825).

  A drag in a cell's output used to select whole cells only, so copying half
  of an error message meant copying all of it. A drag that starts in text
  Maxima printed (TextCell::AllowsCharacterSelection()) now selects
  characters, as long as everything between where it started and where the
  pointer is now is such text.

  How this stays out of the way of every other kind of selection:

  - The selection is worked out from scratch, from where the drag started and
    where the pointer is, on every mouse move (Worksheet::SelectOutputRect()).
    Nothing is carried over from the previous move, so dragging from
    characters out over maths (whole cells) and back gives characters again,
    without any state to get out of step.
  - The characters are a refinement of an ordinary selection of the cells
    they are in (DocumentCellPointers::SetSelectedText()), exactly like a
    selected block of matrix entries. Code that knows nothing about them sees
    those cells selected, and any other change to the selection -- a click,
    a key, a drag over group cells -- forgets them by itself.
*/

#ifndef OUTPUTTEXTSELECTION_H
#define OUTPUTTEXTSELECTION_H

#include "CellPointers.h"
#include <optional>
#include <wx/gdicmn.h>
#include <wx/string.h>

class Cell;

namespace wxm {

/*! The characters a drag from \p down to \p up selects in an output

  \param output the first cell of a cell's output (GroupCell::GetLabel()),
  laid out.

  Empty unless \p down is on a cell whose characters can be selected and every
  cell from there to the one \p up is at (or, if \p up is between or beside
  lines, the nearest one) can, too -- the caller then selects whole cells, as
  before. Also empty if the drag covers no character at all, so that a drag
  that hasn't got anywhere yet selects what a click selects.
*/
std::optional<DocumentCellPointers::TextRange>
OutputTextRange(Cell *output, wxPoint down, wxPoint up);

/*! The text of a range of selected characters, as it is copied

  A line break between two lines of the output, but none where the
  worksheet only broke a long line to fit it into the window.
*/
wxString TextRangeToString(const DocumentCellPointers::TextRange &range);

} // namespace wxm

#endif // OUTPUTTEXTSELECTION_H
