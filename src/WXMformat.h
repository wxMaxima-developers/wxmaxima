// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef WXMFORMAT_H
#define WXMFORMAT_H

#include "GroupCell.h"

class wxTextBuffer;

//! An identifier for each of the headers in a WXM file
// This enum's elements must be synchronized with (GroupCell.h) GroupType
enum WXMHeaderId {
  WXM_INVALID = GC_TYPE_INVALID,
  WXM_INPUT = GC_TYPE_CODE,
  WXM_TITLE = GC_TYPE_TITLE,
  WXM_SECTION = GC_TYPE_SECTION,
  WXM_SUBSECTION = GC_TYPE_SUBSECTION,
  WXM_SUBSUBSECTION = GC_TYPE_SUBSUBSECTION,
  WXM_HEADING5 = GC_TYPE_HEADING5,
  WXM_HEADING6 = GC_TYPE_HEADING6,
  WXM_COMMENT = GC_TYPE_TEXT,
  WXM_CAPTION = GC_TYPE_IMAGE,
  WXM_PAGEBREAK = GC_TYPE_PAGEBREAK,
  WXM_IMAGE,
  WXM_ANSWER,
  WXM_QUESTION,
  WXM_FOLD, WXM_FOLD_END,
  WXM_HIDE,
  WXM_AUTOANSWER,

  WXM_MAX // Must be the last member
};

namespace Format
{

/*! Convert a given cell to its wxm representation.
 * \param wxm
 * - true: We mean to export to a .wxm file.
 * - false: We generate a.mac file instead that doesn't look nice with a dedicated comment per input line.
 * \param cell The starting cell of the tree of cells that is to be converted
 */
wxString TreeToWXM(GroupCell *cell, bool wxm = true);

//! Converts a wxm description into individual cells
std::unique_ptr<GroupCell> TreeFromWXM(const wxArrayString &wxmLines, Configuration **config);

/*! Parses the contents of a .wxm file into individual cells.
 * Invokes TreeFromWXM on pre-processed data,
 * concatenates the results.
 * \returns the tree, or nullptr on failure.
 */
std::unique_ptr<GroupCell> ParseWXMFile(wxTextBuffer &buf, Configuration **config);

/*! Parses the contents of a preloaded .mac file into individual cells.
 *
 * Invokes TreeFromWXM on pre-processed data.
 * \returns the cell tree, or nullptr on failure.
 */
std::unique_ptr<GroupCell> ParseMACContents(const wxString &macContents, Configuration **config);

/*! Parses the contents of a .mac or a .out file into individual cells.
 * Invokes ParseMACContents on pre-processed data.
 * \returns the cell tree, or nullptr on failure.
 */
std::unique_ptr<GroupCell> ParseMACFile(wxTextBuffer &buf, bool xMaximaFile, Configuration **config);

//! First line of the WXM files - used by both loading and saving code.
extern const wxString WXMFirstLine;

}; // namespace Format

#endif // WXMFORMAT_H
