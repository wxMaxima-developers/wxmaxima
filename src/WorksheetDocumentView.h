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
  The narrow "something in the document changed" surface a WorksheetDocument
  needs to poke its view.

  When the document edits its own structure (inserting or removing cells) it has
  to tell the view to re-lay-out and redraw; and when its saved-state flips the
  view has to refresh the controls that depend on it (the title-bar "modified"
  marker). Rather than let WorksheetDocument depend on the whole Worksheet, it
  talks back through this small one-way interface, which Worksheet implements by
  forwarding to its real layout/redraw/control machinery. All notifications; no
  queries, no return values - so the document stays the source of truth for its
  own state (including whether it is saved).

  The method names deliberately differ from the Worksheet methods they end up
  forwarding to, so Worksheet can inherit this alongside its other interfaces
  without any override-signature collisions (same convention as WorksheetView).
*/

#ifndef WORKSHEETDOCUMENTVIEW_H
#define WORKSHEETDOCUMENTVIEW_H

class GroupCell;

/*! The view notifications a WorksheetDocument emits when it edits itself.

  Implemented by Worksheet; a headless test can implement it with a mock that
  merely records the calls.
*/
class WorksheetDocumentView {
public:
  virtual ~WorksheetDocumentView() = default;
  //! Schedule a re-layout of the document starting at \p start (null = all).
  virtual void NotifyRecalculation(GroupCell *start) = 0;
  //! Schedule a redraw of the document starting at \p start (null = all).
  virtual void NotifyRedraw(GroupCell *start) = 0;
  //! The scrollable size will need re-adjusting once cell positions settle.
  virtual void NotifyAdjustSizeNeeded() = 0;
  //! The document's saved-state flipped; refresh the controls that show it.
  virtual void NotifySavedStateChanged() = 0;
};

#endif // WORKSHEETDOCUMENTVIEW_H
