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
  Declares wxm::BreakIntoPages(), the page breaking printing and the PDF
  export share.
*/

#ifndef PAGEBREAKS_H
#define PAGEBREAKS_H

#include "precomp.h"
#include <wx/defs.h>
#include <vector>

class GroupCell;

namespace wxm {

/*! Decides where the pages of a printout or a PDF start

  The worksheet is laid out as one endless scroll as wide as the paper; this
  cuts that scroll into pages no taller than pageHeight. A page may only
  start between two blocks, a block being the input part of a group or one
  line of its output, so no line is ever cut in two. Beyond that:

  - a page break cell (GC_TYPE_PAGEBREAK) starts a new page with whatever
    follows it, even if that would still fit on the current page;
  - a heading is not left alone at the bottom of a page: if the group after
    it has to go onto the next page, the heading goes with it;
  - a block taller than a whole page gets a page of its own (and is cut off
    at its bottom, as nothing can be done about that).

  \param tree       The first group of the laid-out worksheet: its cells'
                    positions (GetRect()) have to be up to date.
  \param pageHeight The height of the printable area, in the units the
                    worksheet was laid out in.
  \return The worksheet y coordinate of the top of each page, in ascending
          order. Empty only for an empty worksheet. A page ends where the
          next one starts; the last one ends pageHeight below its start.
*/
std::vector<wxCoord> BreakIntoPages(const GroupCell *tree, wxCoord pageHeight);

} // namespace wxm

#endif // PAGEBREAKS_H
