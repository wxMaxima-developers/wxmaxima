// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#include "Plot2dWiz.h"

#include <wx/artprov.h>
#include <wx/config.h>

Plot2DWiz::Plot2DWiz(wxWindow *parent, int id, Configuration *cfg,
                     const wxString &title, const wxPoint &pos,
                     const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  m_configuration = cfg;
  label_2 = new wxStaticText(this, -1, _("Expression(s):"));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(300, -1));
  button_3 = new wxButton(this, special, _("&Special"));
  label_3 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_2 =
    new BTextCtrl(this, -1, cfg, wxT("x"), wxDefaultPosition, wxSize(40, -1));
  label_4 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_3 = new BTextCtrl(this, -1, cfg, wxT("-5"), wxDefaultPosition,
                              wxSize(70, -1));
  label_5 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_4 =
    new BTextCtrl(this, -1, cfg, wxT("5"), wxDefaultPosition, wxSize(70, -1));
  check_box_1 = new wxCheckBox(this, -1, _("logscale"));
  label_6 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_5 = new BTextCtrl(this, -1, cfg, wxT("y"), wxDefaultPosition,
                              wxSize(40, -1), wxTE_READONLY);
  label_7 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_6 = new BTextCtrl(this, -1, cfg, wxT("-5"), wxDefaultPosition,
                              wxSize(70, -1));
  label_8 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_7 =
    new BTextCtrl(this, -1, cfg, wxT("5"), wxDefaultPosition, wxSize(70, -1));
  check_box_2 = new wxCheckBox(this, -1, _("logscale"));
  label_9 = new wxStaticText(this, -1, _("Ticks:"));
  text_ctrl_8 = new wxSpinCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(70, -1), wxSP_ARROW_KEYS, 0, 1000, 10);
  text_ctrl_8->SetValue(10);
  label_10 = new wxStaticText(this, -1, _("Format:"));
  const wxString combo_box_1_choices[] = {_("default"), _("inline"),
    wxT("gnuplot"), wxT("xmaxima")};
  combo_box_1 =
    new wxComboBox(this, -1, wxEmptyString, wxDefaultPosition,
		   wxSize(150, -1), 4, combo_box_1_choices, wxCB_DROPDOWN);
  label_11 = new wxStaticText(this, -1, _("Options:"));
  const wxString combo_box_2_choices[] = {
    wxT("set zeroaxis;"), wxT("set size ratio 1; set zeroaxis;"),
    wxT("set grid;"), wxT("set polar; set zeroaxis;")};
  combo_box_2 =
    new wxComboBox(this, combobox, wxEmptyString, wxDefaultPosition,
		   wxSize(280, -1), 4, combo_box_2_choices, wxCB_DROPDOWN);
  label_12 = new wxStaticText(this, -1, _("File:"));
  text_ctrl_9 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(280, -1));
  button_4 = new wxBitmapButton(
				this, file_browse_2d,
				wxArtProvider::GetBitmap(wxART_FILE_OPEN, wxART_HELP_BROWSER));
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

void Plot2DWiz::set_properties() {
  SetTitle(_("Plot 2D"));
  text_ctrl_3->SetValue(wxT("-5"));
  text_ctrl_4->SetValue(wxT("5"));
  text_ctrl_6->SetValue(wxT("0"));
  text_ctrl_7->SetValue(wxT("0"));

  button_4->SetToolTip(_("Browse"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  int selection = 1;
  bool sendRanges = false;
  bool logx = false, logy = false;

  wxConfig::Get()->Read(wxT("Wiz/Plot2D/format"), &selection);
  wxConfig::Get()->Read(wxT("Wiz/Plot2D/sendRanges"), &sendRanges);
  wxConfig::Get()->Read(wxT("Wiz/Plot2D/logx"), &logx);
  wxConfig::Get()->Read(wxT("Wiz/Plot2D/logy"), &logy);

  check_box_1->SetValue(logx);
  check_box_2->SetValue(logy);

  combo_box_1->SetSelection(selection);

  text_ctrl_1->SetFocus();
}

void Plot2DWiz::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(7, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_4 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer_5 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_2->Add(text_ctrl_1, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_2->Add(button_3, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_2, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_3->Add(text_ctrl_2, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(label_4, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(text_ctrl_3, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(label_5, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(text_ctrl_4, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_3->Add(check_box_1, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_2->Add(sizer_3, 1, 0, 0);
  grid_sizer_2->Add(label_6, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  sizer_4->Add(text_ctrl_5, 0, wxALL, 5);
  sizer_4->Add(label_7, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_4->Add(text_ctrl_6, 0, wxALL, 5);
  sizer_4->Add(label_8, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
  sizer_4->Add(text_ctrl_7, 0, wxALL, 5);
  sizer_4->Add(check_box_2, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  grid_sizer_2->Add(sizer_4, 1, wxEXPAND, 0);
  grid_sizer_2->Add(label_9, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_8, 0, wxALL, 5);
  grid_sizer_2->Add(label_10, 0,
                    wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(combo_box_1, 0, wxALL, 5);
  grid_sizer_2->Add(label_11, 0,
                    wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(combo_box_2, 0, wxALL, 5);
  sizer_5->Add(text_ctrl_9, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_5->Add(button_4, 0, wxALL, 5);
  grid_sizer_2->Add(label_12, 0,
                    wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->Add(sizer_5, 1, wxEXPAND, 0);
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

void Plot2DWiz::SetValue(wxString s) {
  if (s.StartsWith(wxT("plot2d")))
    Parse(s);
  else if (s.StartsWith(wxT("wxplot2d"))) {
    Parse(s.SubString(2, s.Length()));
    combo_box_1->SetValue(_("inline"));
  } else
    text_ctrl_1->SetValue(s);

  text_ctrl_1->SetSelection(-1, -1);
}

void Plot2DWiz::Parse(wxString s) {
  int depth = 0;
  unsigned int i = 0;
  wxString curr;

  check_box_1->SetValue(false);
  check_box_2->SetValue(false);

  s = s.SubString(7, s.Length());
  // Function to plot
  do {
    if (s.GetChar(i) == '[') {
      depth++;
      if (depth > 1)
        curr += s.GetChar(i);
    } else if (s.GetChar(i) == ']') {
      depth--;
      if (depth > 0)
        curr += s.GetChar(i);
    } else
      curr += s.GetChar(i);
    i++;
  } while (depth > 0);
  text_ctrl_1->SetValue(curr);
  // Independent variable
  while (i < s.Length() && s.GetChar(i) != '[')
    i++;
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',') {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_2->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ',') {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_3->SetValue(curr);
  i++;
  curr = wxEmptyString;
  while (i < s.Length() && s.GetChar(i) != ']') {
    curr += s.GetChar(i);
    i++;
  }
  text_ctrl_4->SetValue(curr);
  i++;
  // Optional parameters
  while (i < s.Length()) {
    if (s.GetChar(i) == '[') {
      i++;
      curr = wxEmptyString;
      while (i < s.Length() && s.GetChar(i) != ',' && s.GetChar(i) != ']') {
        curr += s.GetChar(i);
        i++;
      }
      curr.Trim();
      curr.Trim(false);
      if (curr == wxT("y")) {
        curr = wxEmptyString;
        i++;
        while (i < s.Length() && s.GetChar(i) != ',') {
          curr += s.GetChar(i);
          i++;
        }
        text_ctrl_6->SetValue(curr);
        i++;
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != ']') {
          curr += s.GetChar(i);
          i++;
        }
        text_ctrl_7->SetValue(curr);
        i++;
      } else if (curr == wxT("gnuplot_postamble")) {
        while (i < s.Length() && s.GetChar(i) != '"')
          i++;
        i++;
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != '"') {
          curr += s.GetChar(i);
          i++;
        }
        combo_box_2->SetValue(curr);
      } else if (curr == wxT("gnuplot_out_file")) {
        while (i < s.Length() && s.GetChar(i) != '"')
          i++;
        i++;
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != '"') {
          curr += s.GetChar(i);
          i++;
        }
        text_ctrl_9->SetValue(curr);
      } else if (curr == wxT("nticks")) {
        curr = wxEmptyString;
        while (i < s.Length() && s.GetChar(i) != ',')
          i++;
        i++;
        while (i < s.Length() && s.GetChar(i) != ']') {
          curr += s.GetChar(i);
          i++;
        }
        text_ctrl_8->SetValue(curr);
      } else if (curr == wxT("logx")) {
        check_box_1->SetValue(true);
        while (i < s.Length() && s.GetChar(i) != ']')
          i++;
      } else if (curr == wxT("logy")) {
        check_box_2->SetValue(true);
        while (i < s.Length() && s.GetChar(i) != ']')
          i++;
      }
    }
    i++;
  }
}

wxString Plot2DWiz::GetValue() {
  wxString f = combo_box_1->GetValue(); // format
  wxString p = combo_box_2->GetValue(); // preamble
  wxString s;                           // result
  wxString x1 = text_ctrl_3->GetValue();
  wxString x2 = text_ctrl_4->GetValue();
  wxString y1 = text_ctrl_6->GetValue();
  wxString y2 = text_ctrl_7->GetValue();
  int t = text_ctrl_8->GetValue();         // Number of ticks
  wxString file = text_ctrl_9->GetValue(); // plot to file

  // Expression
  s = wxT("plot2d([") + text_ctrl_1->GetValue();
  s += wxT("], [");

  // x-range
  s += text_ctrl_2->GetValue();
  s += wxT(",");
  if (x1 != wxT("0") || x2 != wxT("0"))
    s += x1 + wxT(",") + x2;
  else if (type == polar)
    s += wxT("0,2*%pi");
  else
    s += wxT("-5,5");
  s += wxT("]");

  // y-range
  if (y1 != wxT("0") || y2 != wxT("0")) {
    s += wxT(", [");
    s += text_ctrl_5->GetValue();
    s += wxT(",") + y1 + wxT(",") + y2 + wxT("]");
  }

  // plot format
  if (f != _("default") && f != _("inline"))
    s += wxT(",\n [plot_format, ") + f + wxT("]");

  // gnuplot_postamble
  if (p.Length() > 0)
    s += wxT(",\n [gnuplot_postamble, \"") + p + wxT("\"]");
  if (t != 10) {
    s += wxT(",\n [nticks,");
    s += wxString::Format(wxT("%d"), t);
    s += wxT("]");
  }

  // check for logscales
  if (check_box_1->IsChecked())
    s += wxT(", [logx]");
  if (check_box_2->IsChecked())
    s += wxT(", [logy]");

  // plot to file
  if (file.Length()) {
    s += wxT(", [gnuplot_term, ps]");
#if defined(__WXMSW__)
    file.Replace(wxT("\\"), wxT("/"));
#endif

    if (file.Right(4) != wxT(".eps") && file.Right(3) != wxT(".ps"))
      file = file + wxT(".eps");
    s += wxT(",\n [gnuplot_out_file, \"") + file + wxT("\"]");
  }
  // inline?
  else if (f == _("inline"))
    s = wxT("wx") + s;

  s += wxT(")$");

  wxConfig::Get()->Write(wxT("Wiz/Plot2D/format"), combo_box_1->GetSelection());
  wxConfig::Get()->Write(wxT("Wiz/Plot2D/logx"), check_box_1->GetValue());
  wxConfig::Get()->Write(wxT("Wiz/Plot2D/logy"), check_box_2->GetValue());

  return s;
}

void Plot2DWiz::OnButton(wxCommandEvent &WXUNUSED(event)) {
  wxMenu *popupMenu = new wxMenu();

  popupMenu->Append(parametric_plot, _("Parametric plot"));
  popupMenu->Append(discrete_plot, _("Discrete plot"));

  wxPoint pos = button_3->GetPosition();
  pos.y += button_3->GetRect().height;

  PopupMenu(popupMenu, pos);
}

void Plot2DWiz::OnPopupMenu(wxCommandEvent &event) {
  switch (event.GetId()) {
  case parametric_plot: {
    Plot2DPar *wiz = new Plot2DPar(this, -1, m_configuration, _("Plot 2D"));
    wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK) {
      if (text_ctrl_1->GetValue() == wxT("%"))
        text_ctrl_1->SetValue(wxEmptyString);
      if (((text_ctrl_1->GetValue()).Strip()).Length())
        text_ctrl_1->AppendText(wxT(", "));
      text_ctrl_1->AppendText(wiz->GetValue());
    }
  } //-V773
    break;
  case discrete_plot: {
    Plot2DDiscrete *wiz =
      new Plot2DDiscrete(this, -1, m_configuration, _("Plot 2D"));
    wiz->Centre(wxBOTH);
    if (wiz->ShowModal() == wxID_OK) {
      if (text_ctrl_1->GetValue() == wxT("%"))
        text_ctrl_1->SetValue(wxEmptyString);
      if (((text_ctrl_1->GetValue()).Strip()).Length())
        text_ctrl_1->AppendText(wxT(", "));
      text_ctrl_1->AppendText(wiz->GetValue());
    }
  } //-V773
    break;
  }
}

void Plot2DWiz::OnCombobox(wxCommandEvent &WXUNUSED(event)) {
  wxString selection = combo_box_2->GetStringSelection();
  if (selection.StartsWith(wxT("set polar"))) {
    text_ctrl_2->SetValue(wxT("ph"));
    text_ctrl_3->SetValue(wxT("0"));
    text_ctrl_4->SetValue(wxT("2*%pi"));
    type = polar;
  } else
    type = cartesian;

  if (selection.StartsWith(wxT("set logscale x"))) {
    text_ctrl_3->SetValue(wxT("0"));
    text_ctrl_4->SetValue(wxT("100"));
  }
}

void Plot2DWiz::OnFileBrowse(wxCommandEvent &WXUNUSED(event)) {
  wxString file =
    wxFileSelector(_("Save plot to file"), wxEmptyString, wxT("plot2d.eps"),
		   wxT("eps"), _("Postscript file (*.eps)|*.eps|All|*"),
		   wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
  if (file.Length() > 0)
    text_ctrl_9->SetValue(file);
}

wxBEGIN_EVENT_TABLE(Plot2DWiz, wxDialog)
EVT_COMBOBOX(combobox, Plot2DWiz::OnCombobox)
EVT_BUTTON(special, Plot2DWiz::OnButton)
EVT_BUTTON(file_browse_2d, Plot2DWiz::OnFileBrowse)
EVT_MENU(parametric_plot, Plot2DWiz::OnPopupMenu)
EVT_MENU(discrete_plot, Plot2DWiz::OnPopupMenu)
wxEND_EVENT_TABLE()

///////////////////////
//
// Plot2DPar
//
///////////////////////

Plot2DPar::Plot2DPar(wxWindow *parent, int id, Configuration *cfg,
		     const wxString &title, const wxPoint &pos,
		     const wxSize &size, long style)
: wxDialog(parent, id, title, pos, size, style) {
  label_2 = new wxStaticText(this, -1, wxT("x = "));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, wxT("y = "));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_4 = new wxStaticText(this, -1, _("Variable:"));
  text_ctrl_3 =
    new BTextCtrl(this, -1, cfg, wxT("t"), wxDefaultPosition, wxSize(40, -1));
  label_5 = new wxStaticText(this, -1, _("From:"));
  text_ctrl_4 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_6 = new wxStaticText(this, -1, _("To:"));
  text_ctrl_5 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(70, -1));
  label_7 = new wxStaticText(this, -1, _("Ticks:"));
  spin_ctrl_1 = new wxSpinCtrl(this, -1, wxEmptyString, wxDefaultPosition,
                               wxSize(70, -1), wxSP_ARROW_KEYS, 0, 1000, 300);
  spin_ctrl_1->SetValue(300);
  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
}

void Plot2DPar::set_properties() {
  SetTitle(_("Parametric plot"));
  text_ctrl_4->SetValue(wxT("-6"));
  text_ctrl_5->SetValue(wxT("6"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetFocus();
}

void Plot2DPar::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxBoxSizer *sizer_1 = new wxBoxSizer(wxHORIZONTAL);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(4, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(label_4, 0, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL,
                    5);
  sizer_1->Add(text_ctrl_3, 0, wxALL, 5);
  sizer_1->Add(label_5, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_1->Add(text_ctrl_4, 0, wxALL, 5);
  sizer_1->Add(label_6, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
  sizer_1->Add(text_ctrl_5, 0, wxALL, 5);
  grid_sizer_2->Add(sizer_1, 1, 0, 0);
  grid_sizer_2->Add(label_7, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(spin_ctrl_1, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
  grid_sizer_2->AddGrowableCol(1);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 5);
  sizer_2->Add(button_1, 0, wxALL, 5);
  sizer_2->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString Plot2DPar::GetValue() {
  wxString s;
  s = wxT("'parametric, ");
  s += text_ctrl_1->GetValue();
  s += wxT(", ");
  s += text_ctrl_2->GetValue();
  s += wxT(", [");
  s += text_ctrl_3->GetValue();
  s += wxT(", ");
  s += text_ctrl_4->GetValue();
  s += wxT(", ");
  s += text_ctrl_5->GetValue();
  s += wxT("]], ");
  s += wxString::Format(wxT("[nticks, %d"), spin_ctrl_1->GetValue());

  return s;
}

///////////////////////
//
// Plot2DDiscrete
//
///////////////////////

Plot2DDiscrete::Plot2DDiscrete(wxWindow *parent, int id, Configuration *cfg,
                               const wxString &title, const wxPoint &pos,
                               const wxSize &size, long style)
  : wxDialog(parent, id, title, pos, size, style) {
  label_2 = new wxStaticText(this, -1, wxT("x = "));
  text_ctrl_1 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));
  label_3 = new wxStaticText(this, -1, wxT("y = "));
  text_ctrl_2 = new BTextCtrl(this, -1, cfg, wxEmptyString, wxDefaultPosition,
                              wxSize(230, -1));

  static_line_1 = new wxStaticLine(this, -1);
#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
#endif

  set_properties();
  do_layout();
}

void Plot2DDiscrete::set_properties() {
  SetTitle(_("Discrete plot"));
#if defined __WXMSW__
  button_1->SetDefault();
#else
  button_2->SetDefault();
#endif

  text_ctrl_1->SetToolTip(_("Comma separated x coordinates"));
  text_ctrl_2->SetToolTip(_("Comma separated y coordinates"));

  text_ctrl_1->SetFocus();
}

void Plot2DDiscrete::do_layout() {
  wxFlexGridSizer *grid_sizer_1 = new wxFlexGridSizer(4, 1, 0, 0);
  wxFlexGridSizer *grid_sizer_2 = new wxFlexGridSizer(2, 2, 0, 0);
  wxBoxSizer *sizer_2 = new wxBoxSizer(wxHORIZONTAL);
  grid_sizer_2->Add(label_2, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_1, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->Add(label_3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL,
                    5);
  grid_sizer_2->Add(text_ctrl_2, 0, wxALL | wxEXPAND, 5);
  grid_sizer_2->AddGrowableCol(1);
  grid_sizer_1->Add(grid_sizer_2, 1, wxEXPAND, 0);
  grid_sizer_1->Add(static_line_1, 0, wxEXPAND | wxLEFT | wxRIGHT, 5);
  sizer_2->Add(button_1, 0, wxALL, 5);
  sizer_2->Add(button_2, 0, wxALL, 5);
  grid_sizer_1->Add(sizer_2, 1, wxALIGN_RIGHT, 0);
  SetAutoLayout(true);
  SetSizer(grid_sizer_1);
  grid_sizer_1->Fit(this);
  grid_sizer_1->SetSizeHints(this);
  Layout();
}

wxString Plot2DDiscrete::GetValue() {
  wxString s;
  s = wxT("['discrete, [");
  s += text_ctrl_1->GetValue();
  s += wxT("], [");
  s += text_ctrl_2->GetValue();
  s += wxT("]]");

  return s;
}
