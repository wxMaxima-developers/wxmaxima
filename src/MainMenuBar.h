// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2019      Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares a (hopefully) flicker-free main menu bar.
 */

#ifndef MAINMENUBAR_H
#define MAINMENUBAR_H

#include <wx/wx.h>
#include <wx/menu.h>

/*! A panel that shows an example image

  From https://forums.wxwidgets.org/viewtopic.php?t=21664 with a few modifications.
 */
class MainMenuBar : public wxMenuBar
{
 public:
  MainMenuBar();
  //! Enable or disable an item - but only if the item needs enabling or disabling
  void Enable(int id, bool enable);
};
#endif // MAINMENUBAR_H
