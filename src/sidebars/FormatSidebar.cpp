// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2011-2011 cw.ahbong <cwahbong@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class FormatSidebar

  FormatSidebar shows some miscellaneous unicode symbols the user might find useful.
*/
#include "FormatSidebar.h"
#include "ButtonWrapSizer.h"
#include <wx/windowptr.h>
#include "wizards/Gen1Wiz.h"

FormatSidebar::FormatSidebar(wxWindow *parent, int ID)
  : wxScrolled<wxPanel>(parent, ID)
{
  wxSizer *grid = new Buttonwrapsizer();
  SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(this, EventIDs::menu_format_text, _("Text"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_title, _("Title"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_section, _("Section"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_subsection, _("Subsection"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_subsubsection, _("Subsubsection"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_heading5, _("Heading 5"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_heading6, _("Heading 6"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_image, _("Image"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::menu_format_pagebreak, _("Pagebreak"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  SetSizer(grid);
  FitInside();
}
