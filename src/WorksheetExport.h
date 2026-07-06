// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Document serializers extracted from the Worksheet class.

  These functions turn a worksheet's cell tree into HTML, LaTeX and .mac/.wxm
  batch files, plus the RTF document frame used by the clipboard code. They
  are document functionality, not view functionality: they read only the cell
  tree and the Configuration, never the scrolling/cursor/mouse state, which is
  why they live outside the Worksheet class. Worksheet keeps thin wrappers
  that add the view-side bracketing (busy cursor, clip-region toggling, the
  saved-flag and backup-file dance).

  test_WorksheetExport pins the byte-exact output of everything in here.
*/

#ifndef WORKSHEETEXPORT_H
#define WORKSHEETEXPORT_H

#include <wx/gdicmn.h>
#include <wx/string.h>
#include <wx/textfile.h>
#include <memory>
#include <vector>

class Cell;
class CellPointers;
class GroupCell;
class Configuration;

namespace WorksheetExport {
/*! Export the tree to an HTML file.

  Writes the HTML to \a file and the stylesheet, the equation images (in the
  format selected by Configuration::HTMLequationFormat()) and a .wxmx copy of
  the document into a <name>_htmlimg directory next to it.

  \param cellPointers and \param hCaret are only passed through to the
  embedded .wxmx copy, which stores the cursor position.
*/
bool ExportToHTML(GroupCell *tree, Configuration *configuration,
                  const wxString &file,
                  CellPointers *cellPointers, GroupCell *hCaret);

/*! Copy the cell range into a fresh list of cells.

  \param asData
  - true:  The cells are copied in the cell list order.
  - false: The cells are copied in the draw list order
*/
std::unique_ptr<Cell> CopySelection(Cell *start, Cell *end, bool asData = false);

//! Render the cell range into an image file; returns the image's size.
wxSize CopyToFile(const wxString &file, Cell *start, Cell *end, bool asData,
                  double scale, const Configuration *const *configuration);

//! Export the tree to a LaTeX document; images go to a <name>_img directory.
bool ExportToTeX(GroupCell *tree, Configuration *configuration,
                 const wxString &file);

/*! Serialize the tree as .wxm (wxm = true) or .mac lines into output.

  \param cellMap With fixReorderedIndices, the (%iN)/(%oN) indices are
  rewritten through this map (see CalculateReorderedCellIndices) so the
  numbering matches a fresh evaluation order.
*/
void ExportToMAC(wxTextFile &output, GroupCell *tree, bool wxm,
                 const std::vector<int> &cellMap, bool fixReorderedIndices);

//! Add a (possibly multi-line) string to output as individual lines.
void AddLineToFile(wxTextFile &output, const wxString &s);

/*! Determine the (%iN)/(%oN) indices a fresh evaluation would assign.

  Fills cellMap with old-index -> new-index so ExportToMAC can renumber
  references inside the exported code.
*/
void CalculateReorderedCellIndices(GroupCell *tree, int &cellIndex,
                                   std::vector<int> &cellMap);

//! The RTF document header: font, color and stylesheet tables.
wxString RTFStart(Configuration *configuration);

//! The RTF document footer matching RTFStart().
wxString RTFEnd();
} // namespace WorksheetExport

#endif // WORKSHEETEXPORT_H
