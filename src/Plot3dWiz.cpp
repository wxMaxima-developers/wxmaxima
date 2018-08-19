// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "Plot3dWiz.h"

#include <wx/config.h>
#include <wx/artprov.h>

Plot3DWiz::Plot3DWiz(wxWindow *parent, int id,
                     Configuration *cfg,
                     const wxString &title, const wxPoint &pos,
                     const wxSize &size, long style) :
        wxDialog(parent, id, title, pos, size, style)
{
  label_2 = new wxStaticText(this, -1, _("Expression"));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition);
  label_3 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxT("x"), wxDefaultPosition,
                              wxSize(40, -1));
  label_4 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_3 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_5 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_4 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_6 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_5 = new BTextCtrl(this, -1, cfg, wxT("y"), wxDefaultPosition,
                              wxSize(40, -1));
  label_7 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_6 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_8 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_7 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_9 = new wxStaticText(this, -1, _("Grid:"));
  text_ctrl_8 = new wxSpinCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(70, -1), wxSP_ARROW_KEYS, 0, 100, 30);
  text_ctrl_8->SetValue(30);
  label_10 = new wxStaticText(this, -1, wxT("x"));
  text_ctrl_9 = new wxSpinCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(70, -1), wxSP_ARROW_KEYS, 0, 100, 30);
  text_ctrl_9->SetValue(30);
  label_11 = new wxStaticText(this, -1, _("Format:"));
  const wxString combo_box_1_choices[] =
          {
                  _("default"),
                  _("inline"),
                  wxT("gnuplot"),
                  wxT("xmaxima")
          };
  combo_box_1 = new wxComboBox(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(150, -1), 4, combo_box_1_choices,
                               wxCB_DROPDOWN);
  label_12 = new wxStaticText(this, -1, _("Options:"));
  const wxString combo_box_2_choices[] =
          {
                  wxT("set pm3d at b"),
                  wxT("set pm3d at s; unset surf; unset colorbox"),
                  wxT("set pm3d map; unset surf"),
                  wxT("set hidden3d"),
                  wxT("set mapping spherical"),
                  wxT("set mapping cylindrical")
          };
  combo_box_2 = new wxComboBox(this, combobox, wxEmptyString, wxDefaultPosition,
                               wxSize(250, -1), 6, combo_box_2_choices,
                               wxCB_DROPDOWN);
  check_box_1 = new wxCheckBox(this, -1, _("&pm3d"));
  label_13 = new wxStaticText(this, -1, _("Plot to file:"));
  text_ctrl_10 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                               wxSize(250, -1));
  button_3 = new wxBitmapButton(this, file_browse_3d,
                                wxArtProvider::GetBitmap(wxART_FILE_OPEN,
                                                         wxART_HELP_BROWSER));
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  type = cartesian;

  set_properties();
  do_layout();
}


void Plot3DWiz::set_properties()
{
  SetTitle(_("Plot 3D"));
  text_ctrl_3->SetValue(wxT("-5"));
  text_ctrl_4->SetValue(wxT("5"));
  text_ctrl_6->SetValue(wxT("-5"));
  text_ctrl_7->SetValue(wxT("5"));

  button_3->SetToolTip(_("Browse"));

  bool pm3dValue;
#if defined __WXMSW__
  button_1->SetDefault();
  pm3dValue = true;
#else
  button_2->SetDefault();
#endif

  int selection = 1;
  wxConfig::Get()->Read(wxT("Wiz/Plot3D/format"), &selection);
  wxConfig::Get()->Read(wxT("Wiz/Plot3D/pm3d"), &pm3dValue);

  combo_box_1->SetSelection(selection);
  check_box_1->SetValue(pm3dValue);

  text_ctrl_1->SetFocus();
}


void Plot3DWiz::do_layout()
{
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(7, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_5 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_6 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(text_ctrl_1, 1, wxEXPAND | wxALL, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_2->Add(text_ctrl_2, 0, wxALL, 5);
  sizer_2->Add(label_4, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_2->Add(text_ctrl_3, 0, wxALL, 5);
  sizer_2->Add(label_5, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_2->Add(text_ctrl_4, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_6, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_3->Add(text_ctrl_5, 0, wxALL, 5);
  sizer_3->Add(label_7, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(text_ctrl_6, 0, wxALL, 5);
  sizer_3->Add(label_8, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(text_ctrl_7, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_3, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_9, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_4->Add(text_ctrl_8, 0, wxALL, 5);
  sizer_4->Add(label_10, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER_HORIZONTAL | wxALL, 5);
  sizer_4->Add(text_ctrl_9, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_4, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_11, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(combo_box_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_12, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_5->Add(combo_box_2, 0, wxALL, 5);
  sizer_5->Add(check_box_1, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_2->Add(sizer_5, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_13, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_6->Add(text_ctrl_10, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_6->Add(button_3, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_6, 1, wxEXPAND, 0);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 2);
  sizer_1->Add(button_1, 0, wxALL, 5);
  sizer_1->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_1, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

void Plot3DWiz::SetValue(wxString s)
{
  if (s.StartsWith(wxT("plot3d")))
    Parse(s);
  else if (s.StartsWith(wxT("wxplot3d")))
  {
    Parse(s.SubString(2, s.Length()));
    combo_box_1->SetValue(_("inline"));
  }
  else
    text_ctrl_1->SetValue(s);

  text_ctrl_1->SetSelection(-1, -1);
}

void Plot3DWiz::Parse(wxString s)
{
  unsigned int i = 0;
  wxString curr;
  s = s.SubString(7, s.Length());
  // Function to plot
  if (s.StartsWith(wxT("[")))
  {
    int depth = 0;
    do
    {
      if (s.GetChar(i) == '[')
      {
        depth++;
        if (depth > 1)
          curr += s.GetChar(i);
      }
      else if (s.GetChar(i) == ']')
      {
        depth--;
        if (depth > 0)
          curr += s.GetChar(i);
      }
      else
        curr += s.GetChar(i);
      i++;
    } while (depth > 0);
  }
  else
  {
    while (i < s.Length() && s.GetChar(i) != ',')
    {
      curr += s.GetChar(i);
      i++;
    }
  }
  text_ctrl_1->SetValue(curr);
  // Independent variable 1
  while (i < s.Length() && s.GetChar(i) != '[')
    i++;
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_2->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_3->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ']')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_4->SetValue(curr);
  i++;
  // Independent variable 2
  while (i < s.Length() && s.GetChar(i) != '[')
    i++;
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_5->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_6->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ']')
  {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_7->SetValue(curr);
  i++;
  // Optional parameters
  while (i < s.Length())
  {
    if (s.GetChar(i) == '[')
    {
      i++;
      curr = wxEmptyString;
      while (i < s.Length() && s.GetChar(i) != ',')
      {
        curr += s.GetChar(i);
        i++;
      }
      curr.Trim();
      curr.Trim(false);
      if (curr == wxT("gnuplot_postamble"))
      {
        while (i < s.Length() && s.GetChar(i) != '"')
          i++;
        i++;
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != '"')
        {
          curr += s.GetChar(i);
          i++;
        }
        combo_box_2->SetValue(curr);
      }
      else if (curr == wxT("gnuplot_out_file"))
      {
        while (i < s.Length() && s.GetChar(i) != '"')
          i++;
        i++;
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != '"')
        {
          curr += s.GetChar(i);
          i++;
        }
        text_ctrl_10->SetValue(curr);
      }
      else if (curr == wxT("gnuplot_pm3d"))
      {
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != ']')
        {
          curr += s.GetChar(i);
          i++;
        }
        if (curr.Find(wxT("true")) > -1)
          check_box_1->SetValue(true);
        else if (curr.Find(wxT("false")) > -1)
          check_box_1->SetValue(false);
      }
    }
    i++;
  }
}


wxString Plot3DWiz::GetValue()
{
  wxString s = wxT("plot3d(");
  wxString p = combo_box_2->GetValue();
  wxString pl = text_ctrl_1->GetValue();
  wxString f = combo_box_1->GetValue();
  wxString file = text_ctrl_10->GetValue();
  int xg = text_ctrl_8->GetValue();
  int yg = text_ctrl_9->GetValue();
  if (pl.Contains(wxT(", ")))
    pl = wxT("[") + pl + wxT("]");
  s += pl;
  s += wxT(", [");
  s += text_ctrl_2->GetValue();
  s += wxT(",") + text_ctrl_3->GetValue();
  s += wxT(",") + text_ctrl_4->GetValue();;
  s += wxT("], [");
  s += text_ctrl_5->GetValue();
  s += wxT(",") + text_ctrl_6->GetValue();
  s += wxT(",") + text_ctrl_7->GetValue();
  s += wxT("]");
  if (f != _("default") && f != _("inline"))
    s += wxT(", [plot_format,") + f + wxT("]");
  if (xg != 30 || yg != 30)
  {
    s += wxT(",\n [grid,");
    s += wxString::Format(wxT("%d"), xg);
    s += wxT(",");
    s += wxString::Format(wxT("%d"), yg);
    s += wxT("]");
  }
#if defined (__WXMSW__)
  if (!check_box_1->IsChecked())
    s += wxT(",\n [gnuplot_pm3d,false]");
#else
  if (check_box_1->IsChecked())
    s += wxT(",\n [gnuplot_pm3d,true]");
#endif

  if (p.Length() > 0)
    s += wxT(",\n [gnuplot_postamble, \"") + p + wxT("\"]");
  if (file.Length())
  {
    s += wxT(",\n [gnuplot_term, ps]");
#if defined (__WXMSW__)
    file.Replace(wxT("\\"), wxT("/"));
#endif

    if (file.Right(4) != wxT(".eps") && file.Right(3) != wxT(".ps"))
      file = file + wxT(".eps");
    s += wxT(",\n [gnuplot_out_file, \"") + file + wxT("\"]");
  }
  else if (f == _("inline"))
    s = wxT("wx") + s;

  s += wxT(")$");

  wxConfig::Get()->Write(wxT("Wiz/Plot3D/format"), combo_box_1->GetSelection());
  wxConfig::Get()->Write(wxT("Wiz/Plot3D/pm3d"), check_box_1->GetValue());
  return s;
}

void Plot3DWiz::OnCombobox(wxCommandEvent &WXUNUSED(event))
{
  wxString selection = combo_box_2->GetStringSelection();
  if (selection.StartsWith(wxT("set mapping cylindrical")))
  {
    text_ctrl_2->SetValue(wxT("ph"));
    text_ctrl_3->SetValue(wxT("0"));
    text_ctrl_4->SetValue(wxT("2*%pi"));
    text_ctrl_5->SetValue(wxT("z"));
    text_ctrl_6->SetValue(wxT("0"));
    text_ctrl_7->SetValue(wxT("5"));
    type = cylindrical;
  }
  else if (selection.StartsWith(wxT("set mapping spherical")))
  {
    text_ctrl_2->SetValue(wxT("th"));
    text_ctrl_3->SetValue(wxT("0"));
    text_ctrl_4->SetValue(wxT("2*%pi"));
    text_ctrl_5->SetValue(wxT("ph"));
    text_ctrl_6->SetValue(wxT("-%pi/2"));
    text_ctrl_7->SetValue(wxT("%pi/2"));
    type = spherical;
  }
  else
    type = cartesian;
}


void Plot3DWiz::OnFileBrowse(wxCommandEvent &WXUNUSED(event))
{
  wxString file = wxFileSelector(_("Save plot to file"), wxEmptyString,
                                 wxT("plot3d.eps"), wxT("eps"),
                                 _("Postscript file (*.eps)|*.eps|All|*"),
                                 wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
  if (file.Length() > 0)
    text_ctrl_10->SetValue(file);
}

BEGIN_EVENT_TABLE(Plot3DWiz, wxDialog)
                EVT_COMBOBOX(combobox, Plot3DWiz::OnCombobox)
                EVT_BUTTON(file_browse_3d, Plot3DWiz::OnFileBrowse)
END_EVENT_TABLE()
