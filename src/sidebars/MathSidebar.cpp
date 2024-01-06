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
  This file defines the class SymbolsSidebar

  SymbolsSidebar shows some miscellaneous unicode symbols the user might find useful.
*/
#include "MathSidebar.h"
#include <wx/windowptr.h>
#include "wizards/Gen1Wiz.h"

MathSidebar::MathSidebar(wxWindow *parent, int ID)
  : wxScrolled<wxPanel>(parent, ID)
{
  wxSizer *grid = new Buttonwrapsizer();
  SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;

  grid->Add(new wxButton(this, EventIDs::button_ratsimp, _("Simplify"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_radcan, _("Simplify (r)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_factor, _("Factor"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_expand, _("Expand"), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_rectform, _("Rectform"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_subst, _("Subst..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_trigrat, _("Canonical (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_trigsimp, _("Simplify (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_trigexpand, _("Expand (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_trigreduce, _("Reduce (tr)"),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_solve, _("Solve..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_solve_ode, _("Solve ODE..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_diff, _("Diff..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_integrate, _("Integrate..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_limit, _("Limit..."), wxDefaultPosition,
                         wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_plot2, _("Plot 2D..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  grid->Add(new wxButton(this, EventIDs::button_plot3, _("Plot 3D..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  SetSizer(grid);
  FitInside();
}
