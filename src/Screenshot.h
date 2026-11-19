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
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  Screenshots for the manual, made by wxMaxima itself (--screenshot).

  The manual's screenshots used to be made by hand, on whatever desktop the
  person making them had, and so they aged: in 2026 the main window still
  showed version 19.10. info/screenshots/ describes each one as a scene (a
  worksheet, a configuration and what to show), and wxMaxima, started with
  --screenshot, opens the scene, evaluates it, waits until Maxima is idle and
  every image has been decoded, shows what was asked for, saves a PNG of it
  and exits. Doing that from inside the program means nothing has to guess
  how long evaluating or rendering takes.

  The picture is read back from the screen, so it is meant to run under Xvfb
  (or any X11 display): a Wayland compositor does not let a program read the
  screen.
*/
#ifndef WXM_SCREENSHOT_H
#define WXM_SCREENSHOT_H

#include <wx/arrstr.h>
#include <wx/confbase.h>
#include <wx/gdicmn.h>
#include <wx/image.h>
#include <wx/string.h>
#include <optional>

namespace wxm {

//! What --screenshot-of asks to be shown and captured
struct ScreenshotTarget {
  enum class Kind {
    Window,    //!< The whole main window ("window", the default)
    Worksheet, //!< The worksheet window only ("worksheet")
    /*! The worksheet's cells: the worksheet window cropped to what is drawn
        in it, without the cursor ("cells") */
    Cells,
    Pane,      //!< A sidebar, by its AUI pane name ("pane:<name>")
    Config     //!< A page of the configuration dialog ("config:<page>")
  };
  Kind kind = Kind::Window;
  //! The pane's name for Kind::Pane
  wxString pane;
  //! The page's index for Kind::Config
  int page = 0;
};

//! Everything a scene's [Screenshot] group asks for
struct ScreenshotScene {
  //! What is shown and saved
  ScreenshotTarget target;
  //! True if the scene names its sidebars (all others are hidden)
  bool setSidebars = false;
  //! The AUI pane names of the sidebars to show
  wxArrayString sidebars;
  //! The size of the main window, or of the dialog shown; default: unchanged
  wxSize size = wxDefaultSize;
  //! The minimum width of each sidebar shown; 0: leave it as it is
  int sidebarWidth = 0;
};

/*! Reads a scene's [Screenshot] group

  - show: window (the default), worksheet, cells, pane:<name> or
    config:<page>
  - sidebars: the sidebars to show, by AUI pane name, separated by commas;
    every other one is hidden. Left out: the layout stays as it is.
  - sidebarWidth: the minimum width of every sidebar shown.
  - width, height: the size of the main window, or of the dialog shown.

  \return the scene, or no value if show names nothing we can show
*/
std::optional<ScreenshotScene> ReadScreenshotScene(const wxConfigBase &config);

/*! Reads the value of Screenshot/show

  \return the target, or no value if the text names none
*/
std::optional<ScreenshotTarget> ParseScreenshotTarget(const wxString &text);

/*! What the screen shows inside rect (screen coordinates)

  \return the picture, or an invalid image if nothing could be read
*/
wxImage ScreenRect(const wxRect &rect);

/*! The image cropped to everything that isn't its background, plus padding

  The background is the colour of the top-left pixel. Padding stays within
  the image. An image that is all background is returned unchanged.
*/
wxImage TrimToContent(const wxImage &image, int padding);

} // namespace wxm

#endif // WXM_SCREENSHOT_H
