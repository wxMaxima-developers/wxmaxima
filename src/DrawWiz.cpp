// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "DrawWiz.h"

ExplicitWiz::ExplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(new wxStaticText(this,-1, _("Expression to plot")), wxSizerFlags());
  m_expression = new BTextCtrl(this,-1, config, expression);
  vbox->Add(m_expression, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Variable for the x value")), wxSizerFlags());
  m_x = new BTextCtrl(this,-1, config, "x");
  vbox->Add(m_x, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Start of the x value")), wxSizerFlags());
  m_xStart = new BTextCtrl(this,-1, config, "-10");
  vbox->Add(m_xStart, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End of the x value")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "10");
  vbox->Add(m_xEnd, wxSizerFlags().Expand());

  if(m_dimensions > 2)
  {
    vbox->Add(new wxStaticText(this,-1, _("Variable for the y value")), wxSizerFlags());
    m_y = new BTextCtrl(this,-1, config, "y");
    vbox->Add(m_y, wxSizerFlags().Expand()); 

    vbox->Add(new wxStaticText(this,-1, _("Start of the y value")), wxSizerFlags());
    m_yStart = new BTextCtrl(this,-1, config, "-10");
    vbox->Add(m_yStart, wxSizerFlags().Expand()); 
    
    vbox->Add(new wxStaticText(this,-1, _("End of the y value")), wxSizerFlags());
    m_yEnd = new BTextCtrl(this,-1, config, "10");
    vbox->Add(m_yEnd, wxSizerFlags().Expand());
  }
  else
  {
    vbox->Add(new wxStaticText(this,-1, _("Optional: A 2nd expression that defines a region to fill")), wxSizerFlags());
    m_filledfunc = new BTextCtrl(this,-1, config, "");
    vbox->Add(m_filledfunc, wxSizerFlags().Expand());
  }

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  wxButton *cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
  #if defined __WXMSW__
  buttonSizer->Add(okButton);
  buttonSizer->Add(cancelButton);
#else
  buttonSizer->Add(cancelButton);
  buttonSizer->Add(okButton);
#endif
  okButton->SetDefault(); 
  vbox->Add(buttonSizer, wxSizerFlags().Right());
  SetSizerAndFit(vbox);
}

wxString ExplicitWiz::GetValue()
{
  wxString retval;
  if((m_dimensions < 3) && (m_filledfunc->GetValue() != wxEmptyString))
    retval = "filledfunc=" + m_filledfunc->GetValue() + "\n    ";
  
  retval += wxT("explicit(\n        ") + m_expression->GetValue() + ",\n        ";
  retval += m_x->GetValue() + "," + m_xStart->GetValue() + "," + m_xEnd->GetValue();
  if(m_dimensions > 2)
    retval += ",\n        " + m_y->GetValue() + "," + m_yStart->GetValue() + "," +
      m_yEnd->GetValue();
  retval += "\n    )";

  if((m_dimensions < 3) && (m_filledfunc->GetValue() != wxEmptyString))
    retval += "\n    filledfunc=false";

  return retval;
}

ImplicitWiz::ImplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(new wxStaticText(this,-1, _("Expression to plot")), wxSizerFlags());
  m_expression = new BTextCtrl(this,-1, config, expression);
  vbox->Add(m_expression, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Variable for the x value")), wxSizerFlags());
  m_x = new BTextCtrl(this,-1, config, "x");
  vbox->Add(m_x, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Start of the x value")), wxSizerFlags());
  m_xStart = new BTextCtrl(this,-1, config, "-10");
  vbox->Add(m_xStart, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End of the x value")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "10");
  vbox->Add(m_xEnd, wxSizerFlags().Expand());

  vbox->Add(new wxStaticText(this,-1, _("Variable for the y value")), wxSizerFlags());
  m_y = new BTextCtrl(this,-1, config, "y");
  vbox->Add(m_y, wxSizerFlags().Expand()); 
  
  vbox->Add(new wxStaticText(this,-1, _("Start of the y value")), wxSizerFlags());
  m_yStart = new BTextCtrl(this,-1, config, "-10");
  vbox->Add(m_yStart, wxSizerFlags().Expand()); 
  
  vbox->Add(new wxStaticText(this,-1, _("End of the y value")), wxSizerFlags());
  m_yEnd = new BTextCtrl(this,-1, config, "10");
  vbox->Add(m_yEnd, wxSizerFlags().Expand());
  
  if(m_dimensions > 2)
  {
    vbox->Add(new wxStaticText(this,-1, _("Variable for the z value")), wxSizerFlags());
    m_z = new BTextCtrl(this,-1, config, "z");
    vbox->Add(m_z, wxSizerFlags().Expand()); 

    vbox->Add(new wxStaticText(this,-1, _("Start of the z value")), wxSizerFlags());
    m_zStart = new BTextCtrl(this,-1, config, "-10");
    vbox->Add(m_zStart, wxSizerFlags().Expand()); 
    
    vbox->Add(new wxStaticText(this,-1, _("End of the z value")), wxSizerFlags());
    m_zEnd = new BTextCtrl(this,-1, config, "10");
    vbox->Add(m_zEnd, wxSizerFlags().Expand());
  }

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  wxButton *cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
  #if defined __WXMSW__
  buttonSizer->Add(okButton);
  buttonSizer->Add(cancelButton);
#else
  buttonSizer->Add(cancelButton);
  buttonSizer->Add(okButton);
#endif
  okButton->SetDefault(); 
  vbox->Add(buttonSizer, wxSizerFlags().Right());
  SetSizerAndFit(vbox);
}

wxString ImplicitWiz::GetValue()
{
  wxString retval;
  
  retval += wxT("implicit(\n        ") + m_expression->GetValue() + ",\n        ";
  retval += m_x->GetValue() + "," + m_xStart->GetValue() + "," + m_xEnd->GetValue();
  retval += ",\n        " + m_y->GetValue() + "," + m_yStart->GetValue() + "," +
    m_yEnd->GetValue();
  if(m_dimensions > 2)
    retval += ",\n        " + m_z->GetValue() + "," + m_zStart->GetValue() + "," +
      m_zEnd->GetValue();
  retval += "\n    )";

  return retval;
}
