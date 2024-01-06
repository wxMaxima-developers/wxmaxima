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
#include "DrawSidebar.h"
#include <wx/windowptr.h>
#include "wizards/Gen1Wiz.h"

DrawSidebar::DrawSidebar(wxWindow *parent, int ID)
  : wxScrolled<wxPanel>(parent, ID)
{
    wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  SetScrollRate(5, 5);
  m_grid = new Buttonwrapsizer(wxHORIZONTAL);
  m_dimensions = -1;
  int style = wxALL | wxEXPAND;
  int border = 0;

  m_grid->Add(m_draw_setup2d = new wxButton(this, EventIDs::menu_draw_2d, _("2D"),
                                            wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_setup2d->SetToolTip(_("Setup a 2D plot"));
  m_grid->Add(m_draw_setup3d = new wxButton(this, EventIDs::menu_draw_3d, _("3D"),
                                            wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_setup3d->SetToolTip(_("Setup a 3D plot"));
  m_grid->Add(m_draw_explicit =
              new wxButton(this, EventIDs::menu_draw_explicit, _("Expression"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_explicit->SetToolTip(
                              _("The standard plot command: Plot an equation as a curve"));
  m_grid->Add(m_draw_implicit =
              new wxButton(this, EventIDs::menu_draw_implicit, _("Implicit Plot"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_parametric =
              new wxButton(this, EventIDs::menu_draw_parametric, _("Parametric Plot"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_points = new wxButton(this, EventIDs::menu_draw_points, _("Points"),
                                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_title =
              new wxButton(this, EventIDs::menu_draw_title, _("Diagram title"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_title->SetToolTip(_("The diagram title"));
  m_grid->Add(m_draw_axis = new wxButton(this, EventIDs::menu_draw_axis, _("Axis"),
                                         wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_axis->SetToolTip(_("Setup the axis"));
  m_grid->Add(m_draw_contour =
              new wxButton(this, EventIDs::menu_draw_contour, _("Contour"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_grid->Add(m_draw_key = new wxButton(this, EventIDs::menu_draw_key, _("Plot name"),
                                        wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_key->SetToolTip(_("The next plot's title"));
  m_grid->Add(m_draw_fgcolor =
              new wxButton(this, EventIDs::menu_draw_fgcolor, _("Line color"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_fgcolor->SetToolTip(_("The color of the next line to draw"));
  m_grid->Add(m_draw_fillcolor =
              new wxButton(this, EventIDs::menu_draw_fillcolor, _("Fill color"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_fillcolor->SetToolTip(_("The fill color for the next objects"));
  m_grid->Add(m_draw_grid = new wxButton(this, EventIDs::menu_draw_grid, _("Grid"),
                                         wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_grid->SetToolTip(_("The grid in the background of the diagram"));
  m_draw_contour->SetToolTip(_("Contour lines for 3d plots"));
  m_grid->Add(m_draw_accuracy =
              new wxButton(this, EventIDs::menu_draw_accuracy, _("Accuracy"),
                           wxDefaultPosition, wxDefaultSize),
              0, style, border);
  m_draw_accuracy->SetToolTip(_("The accuracy versus speed tradeoff"));
  Connect(wxEVT_SIZE, wxSizeEventHandler(DrawSidebar::OnSize), NULL,
          this);
  vbox->Add(m_grid, wxSizerFlags(2).Expand());

  SetSizer(vbox);
  FitInside();
}


void DrawSidebar::SetDimensions(int dimensions) {
  if (dimensions == m_dimensions)
    return;

  if (dimensions > 0) {
    m_draw_explicit->Enable(true);
    m_draw_implicit->Enable(true);
    m_draw_parametric->Enable(true);
    m_draw_points->Enable(true);
    m_draw_title->Enable(true);
    m_draw_key->Enable(true);
    m_draw_fgcolor->Enable(true);
    m_draw_fillcolor->Enable(true);
    m_draw_setup2d->Enable(false);
    m_draw_grid->Enable(true);
    m_draw_axis->Enable(true);
    m_draw_accuracy->Enable(true);
    if (dimensions > 2) {
      m_draw_contour->Enable(true);
      m_draw_setup3d->Enable(true);
    } else {
      m_draw_contour->Enable(false);
      m_draw_setup3d->Enable(false);
    }
  } else {
    m_draw_accuracy->Enable(true);
    m_draw_explicit->Enable(true);
    m_draw_implicit->Enable(true);
    m_draw_parametric->Enable(true);
    m_draw_points->Enable(true);
    m_draw_title->Enable(true);
    m_draw_key->Enable(true);
    m_draw_fgcolor->Enable(true);
    m_draw_fillcolor->Enable(true);
    m_draw_setup2d->Enable(true);
    m_draw_setup3d->Enable(true);
    m_draw_grid->Enable(true);
    m_draw_axis->Enable(true);
  }
  m_dimensions = dimensions;
}

void DrawSidebar::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}
