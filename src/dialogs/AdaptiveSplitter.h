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
  Declares AdaptiveSplitter, a splitter that puts a preview below or beside
  a settings pane, whichever leaves the preview more room.
*/

#ifndef ADAPTIVESPLITTER_H
#define ADAPTIVESPLITTER_H

#include <wx/splitter.h>

/*! A splitter between a settings pane and a preview of what the settings do

  The first pane holds the settings, the second one a preview (in the
  configuration dialog: the example worksheet). Whenever the splitter is
  resized it decides whether the preview goes below the settings (a
  horizontal split) or to their right (a vertical split), see
  ChooseSplitMode(). The user can move the sash to give the preview more
  room: the settings pane can shrink down to its minimum size, which the
  caller sets to something smaller than its preferred size (in the Style tab
  the "Style for" list shrinks to a few lines).

  Where the user drags the sash becomes the settings pane's preferred size
  in that direction, so it survives resizing the dialog, and switching to
  the other split direction and back. When the splitter grows, the preview
  gets the new room.
*/
class AdaptiveSplitter : public wxSplitterWindow
{
public:
  AdaptiveSplitter(wxWindow *parent, wxWindowID id = wxID_ANY);

  /*! Shows settings and preview, split in whichever direction fits best

    \param settings The first pane. Its GetMinSize() is how far the user can
                    shrink it by moving the sash.
    \param preview  The second pane.
    \param preferredSettingsSize The size the settings pane gets whenever the
                    split direction is (re-)chosen, and what ChooseSplitMode()
                    reserves for it. wxDefaultSize means settings->GetBestSize().
  */
  void SplitAdaptively(wxWindow *settings, wxWindow *preview,
                       wxSize preferredSettingsSize = wxDefaultSize);

  /*! Which split leaves more room for the preview

    The preview gets what the settings pane at its preferred size leaves of
    \p available: below it, available.x by available.y - settings.y; beside
    it, available.x - settings.x by available.y. The larger area wins, and a
    tie goes to the horizontal split (preview below the settings), which is
    what the Style tab always used to look like.

    Pure geometry, so it is tested directly.
  */
  static wxSplitMode ChooseSplitMode(wxSize available, wxSize preferredSettingsSize);

  //! The size the settings pane gets whenever the split direction is chosen anew
  wxSize GetPreferredSettingsSize() const {return m_preferredSettingsSize;}

  /*! Remembers where the user puts the sash, across dialog sessions

    Call after SplitAdaptively(). Whenever the user drags the sash, the
    settings pane's new height (horizontal split) or width (vertical split)
    becomes its preferred size in that direction and is written to the
    configuration under \p configKey; this call reads what was stored there
    before. A user who shrank the settings to see more preview therefore
    gets the same layout the next time.
  */
  void RememberSashIn(const wxString &configKey);

private:
  //! Re-decides the split direction for the current size
  void OnSize(wxSizeEvent &event);
  //! Splits in \p mode, giving the settings pane its preferred size
  void SplitIn(wxSplitMode mode);
  //! Makes the size the user dragged the settings pane to its preferred size
  void OnSashMoved(wxSplitterEvent &event);

  wxWindow *m_settings = nullptr;
  wxWindow *m_preview = nullptr;
  wxSize m_preferredSettingsSize;
  //! Where RememberSashIn() stores the preferred size; empty = nowhere
  wxString m_configKey;
};

#endif // ADAPTIVESPLITTER_H
