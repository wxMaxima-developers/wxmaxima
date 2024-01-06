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
  This file defines the class StatSidebar

  StatSidebar shows some miscellaneous unicode symbols the user might find useful.
*/
#include "StatSidebar.h"
#include <wx/windowptr.h>
#include "wizards/Gen1Wiz.h"

StatSidebar::StatSidebar(wxWindow *parent, int ID = wxID_ANY)
  : wxScrolled<wxPanel>(parent, ID)
{
  wxSizer *grid1 = new Buttonwrapsizer();
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *box1 = new wxBoxSizer(wxVERTICAL);
  wxGridSizer *grid2 = new wxGridSizer(2);
  wxGridSizer *grid3 = new wxGridSizer(2);
  wxBoxSizer *box3 = new wxBoxSizer(wxVERTICAL);
  SetScrollRate(5, 5);

  int style = wxALL | wxEXPAND;
  int border = 0;
  int sizerBorder = 2;

  grid1->Add(new wxButton(this, EventIDs::menu_stats_mean, _("Mean..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(this, EventIDs::menu_stats_median, _("Median..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(this, EventIDs::menu_stats_var, _("Variance..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid1->Add(new wxButton(this, EventIDs::menu_stats_dev, _("Deviation..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid1, 0, style, sizerBorder);

  box1->Add(new wxButton(this, EventIDs::menu_stats_tt1, _("Mean Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(this, EventIDs::menu_stats_tt2, _("Mean Difference Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(this, EventIDs::menu_stats_tnorm, _("Normality Test..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(this, EventIDs::menu_stats_linreg, _("Linear Regression..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);
  box1->Add(new wxButton(this, EventIDs::menu_stats_lsquares, _("Least Squares Fit..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  box->Add(box1, 0, style, sizerBorder);

  grid2->Add(new wxButton(this, EventIDs::menu_stats_histogram, _("Histogram..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(this, EventIDs::menu_stats_scatterplot, _("Scatterplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(this, EventIDs::menu_stats_barsplot, _("Barsplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(this, EventIDs::menu_stats_piechart, _("Piechart..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid2->Add(new wxButton(this, EventIDs::menu_stats_boxplot, _("Boxplot..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid2, 0, style, sizerBorder);

  grid3->Add(new wxButton(this, EventIDs::menu_stats_readm, _("Read Matrix..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);
  grid3->Add(new wxButton(this, EventIDs::menu_stats_enterm, _("Enter Matrix..."),
                          wxDefaultPosition, wxDefaultSize),
             0, style, border);

  box->Add(grid3, 0, style, sizerBorder);

  box3->Add(new wxButton(this, EventIDs::menu_stats_subsample, _("Subsample..."),
                         wxDefaultPosition, wxDefaultSize),
            0, style, border);

  box->Add(box3, 0, style, sizerBorder);

  SetSizer(box);
  FitInside();
}
