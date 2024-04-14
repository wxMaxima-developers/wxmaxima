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

#ifndef WXMXFORMAT_H
#define WXMXFORMAT_H

#include <memory>
#include "cells/GroupCell.h"
#include "Configuration.h"
#include <vector>

#define DOCUMENT_VERSION_MAJOR 1
/*! The part of the .wxmx format version number that appears after the dot.

  - Updated to version 1.1 after user selectable animation-speeds were introduced:
  Old wxMaxima versions play them back in the default speed instead but still
  open the file.
  - Bumped to version 1.2 after saving highlighting was introduced: Older versions
  of wxMaxima will ignore the highlighting on opening .wxmx files.
  - Bumped to version 1.3 after sub-subsections were introduced:
  Old wxMaxima versions interpret them as subsections but still open the file.
  - Bumped to version 1.4 when we started allowing to embed .jpg images in a .wxmx
  file. wxMaxima versions between 13.04 and 15.08 replace these images by a
  "image cannot be loaded" marker but will correctly display the rest of the file.
  - Bumped to version 1.5 when GroupCells were added an attribute that allows them to
  be used as an answer to questions.
*/
#define DOCUMENT_VERSION_MINOR 5

namespace Format
{
  bool ExportToWXMX(GroupCell *cells, const wxString &file,
                    Configuration *configuration, CellPointers *cellPointers,
                    const std::vector<wxString> &variables, const GroupCell * const cursorCell = NULL);

}

#endif // WXMXFORMAT_H
