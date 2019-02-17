﻿// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

/*! \file
  This file contains all the wizards the draw sidepane needs.
 */

#include "DrawWiz.h"
#include <wx/statbox.h>
#include <wx/display.h>
#include <wx/persist/toplevel.h>
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include "../art/draw/images.h"

ExplicitWiz::ExplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(new wxStaticText(this, -1,
                             _("Plot an expression, for example sin(x) or (x+1)^2.")),
            wxSizerFlags().Expand().Border(wxBOTTOM,16));
  
  vbox->Add(new wxStaticText(this,-1, _("Expression to plot")), wxSizerFlags());
  m_expression = new BTextCtrl(this,-1, config, expression);
  vbox->Add(m_expression, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Variable for the x value")), wxSizerFlags());
  m_x = new BTextCtrl(this,-1, config, "x");
  vbox->Add(m_x, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Start of the x value")), wxSizerFlags());
  m_xStart = new BTextCtrl(this,-1, config, "-2");
  vbox->Add(m_xStart, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End of the x value")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "2");
  vbox->Add(m_xEnd, wxSizerFlags().Expand());

  if(m_dimensions > 2)
  {
    SetName("DrawExplicitWiz3D");
    vbox->Add(new wxStaticText(this,-1, _("Variable for the y value")), wxSizerFlags());
    m_y = new BTextCtrl(this,-1, config, "y");
    vbox->Add(m_y, wxSizerFlags().Expand()); 

    vbox->Add(new wxStaticText(this,-1, _("Start of the y value")), wxSizerFlags());
    m_yStart = new BTextCtrl(this,-1, config, "-2");
    vbox->Add(m_yStart, wxSizerFlags().Expand()); 
    
    vbox->Add(new wxStaticText(this,-1, _("End of the y value")), wxSizerFlags());
    m_yEnd = new BTextCtrl(this,-1, config, "2");
    vbox->Add(m_yEnd, wxSizerFlags().Expand());
  }
  else
  {
    SetName("DrawExplicitWiz2D");
    vbox->Add(new wxStaticText(this,-1, _("Optional: A 2nd expression that defines a region to fill")), wxSizerFlags());
    m_filledfunc = new BTextCtrl(this,-1, config, "");
    vbox->Add(m_filledfunc, wxSizerFlags().Expand());
  }

  vbox->Add(
    new wxImagePanel(
      this,
      Draw_Explicit_png,Draw_Explicit_png_len),
    wxSizerFlags().Shaped().Border(wxALL,5).Center()
    );
  
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
  m_expression ->SetValue(expression);
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString ExplicitWiz::GetValue()
{
  wxString retval;
  if((m_dimensions < 3) && (m_filledfunc->GetValue() != wxEmptyString))
    retval = "filled_func=" + m_filledfunc->GetValue() + ",\n    ";
  
  retval += wxT("explicit(\n        ") + m_expression->GetValue() + ",\n        ";
  retval += m_x->GetValue() + "," + m_xStart->GetValue() + "," + m_xEnd->GetValue();
  if(m_dimensions > 2)
    retval += ",\n        " + m_y->GetValue() + "," + m_yStart->GetValue() + "," +
      m_yEnd->GetValue();
  retval += "\n    )";

  if((m_dimensions < 3) && (m_filledfunc->GetValue() != wxEmptyString))
    retval += ",\n    filled_func=false";

  return retval;
}

ImplicitWiz::ImplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(new wxStaticText(this, -1,
                             _("Draw all points where an equation is true.")),
            wxSizerFlags().Expand().Border(wxBOTTOM,16));
  vbox->Add(new wxStaticText(this, -1,
                             _("See also the speed <-> accuracy button.")),
            wxSizerFlags().Expand().Border(wxBOTTOM,16));
  
  vbox->Add(new wxStaticText(this,-1, _("Equation")), wxSizerFlags());
  m_expression = new BTextCtrl(this,-1, config, expression);
  vbox->Add(m_expression, wxSizerFlags().Expand()); 
  m_expression->SetToolTip(_("A good example equation would be x^2+y^2=1"));
  
  vbox->Add(new wxStaticText(this,-1, _("Variable for the x value")), wxSizerFlags());
  m_x = new BTextCtrl(this,-1, config, "x");
  vbox->Add(m_x, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Start of the x value")), wxSizerFlags());
  m_xStart = new BTextCtrl(this,-1, config, "-2");
  vbox->Add(m_xStart, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End of the x value")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "2");
  vbox->Add(m_xEnd, wxSizerFlags().Expand());

  vbox->Add(new wxStaticText(this,-1, _("Variable for the y value")), wxSizerFlags());
  m_y = new BTextCtrl(this,-1, config, "y");
  vbox->Add(m_y, wxSizerFlags().Expand()); 
  
  vbox->Add(new wxStaticText(this,-1, _("Start of the y value")), wxSizerFlags());
  m_yStart = new BTextCtrl(this,-1, config, "-2");
  vbox->Add(m_yStart, wxSizerFlags().Expand()); 
  
  vbox->Add(new wxStaticText(this,-1, _("End of the y value")), wxSizerFlags());
  m_yEnd = new BTextCtrl(this,-1, config, "2");
  vbox->Add(m_yEnd, wxSizerFlags().Expand());
  
  if(m_dimensions > 2)
  {
    SetName("DrawImplicitWiz3D");
    vbox->Add(new wxStaticText(this,-1, _("Variable for the z value")), wxSizerFlags());
    m_z = new BTextCtrl(this,-1, config, "z");
    vbox->Add(m_z, wxSizerFlags().Expand()); 

    vbox->Add(new wxStaticText(this,-1, _("Start of the z value")), wxSizerFlags());
    m_zStart = new BTextCtrl(this,-1, config, "-2");
    vbox->Add(m_zStart, wxSizerFlags().Expand()); 
    
    vbox->Add(new wxStaticText(this,-1, _("End of the z value")), wxSizerFlags());
    m_zEnd = new BTextCtrl(this,-1, config, "2");
    vbox->Add(m_zEnd, wxSizerFlags().Expand());
  }
  else
    SetName("DrawImplicitWiz2D");

  vbox->Add(
    new wxImagePanel(
      this,
      Draw_Implicit_png,Draw_Implicit_png_len),
    wxSizerFlags().Shaped().Border(wxALL,5).Center()
    );

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
  m_expression ->SetValue(expression);
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
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


AxisWiz::AxisWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  vbox->Add(new wxStaticText(this, -1,
                             _("Setup the axis for the current plot.")),
            wxSizerFlags().Expand().Border(wxBOTTOM,16));
  
  vbox->Add(new wxStaticText(this,-1, _("x axis label")), wxSizerFlags());
  m_xLabel = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_xLabel, wxSizerFlags().Expand());  
  vbox->Add(new wxStaticText(this,-1, _("x range (automatic if incomplete)")), wxSizerFlags());
  wxBoxSizer *xrangeBox = new wxBoxSizer(wxHORIZONTAL);
  m_xStart = new BTextCtrl(this,-1, config, "");
  xrangeBox->Add(m_xStart, wxSizerFlags().Expand()); 
  xrangeBox->Add(new wxStaticText(this,-1, _("-")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "");
  xrangeBox->Add(m_xEnd, wxSizerFlags().Expand());
  vbox->Add(xrangeBox, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("y axis label")), wxSizerFlags());
  m_yLabel = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_yLabel, wxSizerFlags().Expand());  
  vbox->Add(new wxStaticText(this,-1, _("y range (automatic if incomplete)")), wxSizerFlags());
  wxBoxSizer *yrangeBox = new wxBoxSizer(wxHORIZONTAL);
  m_yStart = new BTextCtrl(this,-1, config, "");
  yrangeBox->Add(m_yStart, wxSizerFlags().Expand()); 
  yrangeBox->Add(new wxStaticText(this,-1, _("-")), wxSizerFlags());
  m_yEnd = new BTextCtrl(this,-1, config, "");
  yrangeBox->Add(m_yEnd, wxSizerFlags().Expand());
  vbox->Add(yrangeBox, wxSizerFlags().Expand()); 

  if(dimensions > 2)
  {
    SetName("DrawAxisWiz3D");
    vbox->Add(new wxStaticText(this,-1, _("z axis label")), wxSizerFlags());
    m_zLabel = new BTextCtrl(this,-1, config, "");
    vbox->Add(m_zLabel, wxSizerFlags().Expand());  
    vbox->Add(new wxStaticText(this,-1, _("z range (automatic if incomplete)")), wxSizerFlags());
    wxBoxSizer *zrangeBox = new wxBoxSizer(wxHORIZONTAL);
    m_zStart = new BTextCtrl(this,-1, config, "");
    zrangeBox->Add(m_zStart, wxSizerFlags().Expand()); 
    zrangeBox->Add(new wxStaticText(this,-1, _("-")), wxSizerFlags());
    m_zEnd = new BTextCtrl(this,-1, config, "");
    zrangeBox->Add(m_zEnd, wxSizerFlags().Expand());
    vbox->Add(zrangeBox, wxSizerFlags().Expand()); 
  }
  else
    SetName("DrawAxisWiz2D");
  
  vbox->Add(new wxStaticText(this,-1, _("secondary x axis label")), wxSizerFlags());
  m_x2Label = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_x2Label, wxSizerFlags().Expand());  
  vbox->Add(new wxStaticText(this,-1, _("secondary x range (automatic if incomplete)")), wxSizerFlags());
  wxBoxSizer *x2rangeBox = new wxBoxSizer(wxHORIZONTAL);
  m_x2Start = new BTextCtrl(this,-1, config, "");
  x2rangeBox->Add(m_x2Start, wxSizerFlags().Expand()); 
  x2rangeBox->Add(new wxStaticText(this,-1, _("-")), wxSizerFlags());
  m_x2End = new BTextCtrl(this,-1, config, "");
  x2rangeBox->Add(m_x2End, wxSizerFlags().Expand());
  vbox->Add(x2rangeBox, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("secondary y axis label")), wxSizerFlags());
  m_y2Label = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_y2Label, wxSizerFlags().Expand());  
  vbox->Add(new wxStaticText(this,-1, _("secondary y range (automatic if incomplete)")), wxSizerFlags());
  wxBoxSizer *y2rangeBox = new wxBoxSizer(wxHORIZONTAL);
  m_y2Start = new BTextCtrl(this,-1, config, "");
  y2rangeBox->Add(m_y2Start, wxSizerFlags().Expand()); 
  y2rangeBox->Add(new wxStaticText(this,-1, _("-")), wxSizerFlags());
  m_y2End = new BTextCtrl(this,-1, config, "");
  y2rangeBox->Add(m_y2End, wxSizerFlags().Expand());
  vbox->Add(y2rangeBox, wxSizerFlags().Expand()); 

  m_useSecondaryX = new wxCheckBox(this, -1, _("Following plots use secondary x axis"),
                                   wxDefaultPosition, wxDefaultSize, wxCHK_3STATE);
  m_useSecondaryX -> Set3StateValue(wxCHK_UNDETERMINED);
  vbox->Add(m_useSecondaryX, wxSizerFlags().Expand()); 

  m_useSecondaryY = new wxCheckBox(this, -1, _("Following plots use secondary y axis"),
                                   wxDefaultPosition, wxDefaultSize, wxCHK_3STATE);
  m_useSecondaryY -> Set3StateValue(wxCHK_UNDETERMINED);
  vbox->Add(m_useSecondaryY, wxSizerFlags().Expand()); 

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
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString AxisWiz::GetValue()
{
  wxString retval;
  if(m_xLabel->GetValue() != wxEmptyString)
  {
    retval = "xlabel=\"" + m_xLabel->GetValue() + "\"";
  }
  if(m_yLabel->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "ylabel=\"" + m_yLabel->GetValue() + "\"";
  }
  if((m_dimensions == 3) && (m_zLabel->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "zlabel=\"" + m_zLabel->GetValue() + "\"";
  }

  if(m_x2Label->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "xlabel_secondary=\"" + m_x2Label->GetValue() + "\"";
  }
  if(m_y2Label->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "ylabel_secondary=\"" + m_y2Label->GetValue() + "\"";
  }

  if((m_xStart->GetValue() != wxEmptyString) && (m_xEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "xrange=[" + m_xStart->GetValue() + "," + m_xEnd->GetValue() + "]";
  }
  if((m_yStart->GetValue() != wxEmptyString) && (m_yEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "yrange=[" + m_yStart->GetValue() + "," + m_yEnd->GetValue() + "]";
  }
  if((m_dimensions == 3) && (m_zStart->GetValue() != wxEmptyString) && (m_zEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "zrange=[" + m_zStart->GetValue() + "," + m_zEnd->GetValue() + "]";
  }

  if((m_x2Start->GetValue() != wxEmptyString) && (m_x2End->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "xrange_secondary=[" + m_x2Start->GetValue() + "," + m_x2End->GetValue() + "]";
  }
  if((m_y2Start->GetValue() != wxEmptyString) && (m_y2End->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "yrange_secondary=[" + m_y2Start->GetValue() + "," + m_y2End->GetValue() + "]";
  }

  if(m_useSecondaryX->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "xaxis_secondary=true";
  }
  if(m_useSecondaryX->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "xaxis_secondary=false";
  }

  if(m_useSecondaryY->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "yaxis_secondary=true";
  }
  if(m_useSecondaryY->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "yaxis_secondary=false";
  }

  return retval;
}

void DrawWiz::OnParametricFocus(wxFocusEvent &WXUNUSED(event))
{
  m_multipleFrames->SetValue(true);
}

DrawWiz::DrawWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, wxString::Format(_("Setup a %iD scene"),dimensions))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  vbox->Add(new wxStaticText(this, -1,
                             wxString::Format(
                               _("This wizard setups a %iD scene."), dimensions)));

  vbox->Add(new wxStaticText(this, -1,
                             wxString::Format(
                               _("It needs to be filled with objects to draw afterwards."))),
            wxSizerFlags().Border(wxBOTTOM,16));
  m_singleFrame = new wxRadioButton(this, -1, _("A static plot"), wxDefaultPosition,
                                    wxDefaultSize, wxRB_GROUP);
  vbox->Add(m_singleFrame, wxSizerFlags().Expand().Border(wxALL,5));

  m_multipleFrames = new wxRadioButton(this, -1, _("An animation with multiple frames"));
  vbox->Add(m_multipleFrames, wxSizerFlags().Expand().Border(wxALL,5));
  wxPanel *animPanel = new wxPanel(this,-1);
  wxBoxSizer *animPanelVbox = new wxBoxSizer(wxVERTICAL);
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter")), wxSizerFlags());
  m_frameVar = new BTextCtrl(animPanel, -1, config, "t");
  m_frameVar->Connect(wxEVT_SET_FOCUS,
                      wxFocusEventHandler(DrawWiz::OnParametricFocus),
                      NULL, this);

  animPanelVbox->Add(m_frameVar, wxSizerFlags().Expand().Border(wxALL,5));
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter start")), wxSizerFlags());
  m_varStart = new BTextCtrl(animPanel,-1, config, "1");
  m_varStart->Connect(wxEVT_SET_FOCUS,
                      wxFocusEventHandler(DrawWiz::OnParametricFocus),
                      NULL, this);
  animPanelVbox->Add(m_varStart, wxSizerFlags().Expand().Border(wxALL,5));
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter end")), wxSizerFlags());
  m_varEnd = new BTextCtrl(animPanel,-1, config, "10");
  m_varEnd->Connect(wxEVT_SET_FOCUS,
                    wxFocusEventHandler(DrawWiz::OnParametricFocus),
                    NULL, this);
  animPanelVbox->Add(m_varEnd, wxSizerFlags().Expand().Border(wxALL,5));
  animPanel->SetSizerAndFit(animPanelVbox);
  vbox->Add(animPanel, wxSizerFlags().Expand().Border(wxALL,5));
  
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

  SetName(wxString::Format("Draw_%idWiz", dimensions));  
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString DrawWiz::GetValue()
{
  if(m_dimensions < 3)
  {
    if(m_singleFrame->GetValue())
      return wxT("wxdraw2d(\n)$");
    else
    {
      return wxString("with_slider_draw(\n") + m_frameVar->GetValue() + ",makelist(i,i," +
        m_varStart->GetValue() + "," + m_varEnd->GetValue() + ")\n)$";
    }
  }
  else
  {
    if(m_singleFrame->GetValue())
      return wxT("wxdraw3d(\n)$");
    else
    {
      return wxString("with_slider_draw3d(\n") + m_frameVar->GetValue() + ",makelist(i,i," +
        m_varStart->GetValue() + "," + m_varEnd->GetValue() + ")\n)$";
    }
  }
}


Wiz3D::Wiz3D(wxWindow *parent, Configuration *WXUNUSED(config)) :
  wxDialog(parent, -1, _("Settings for the following 3d plots"))
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  vbox->Add(new wxStaticText(this, -1,
                             _("Enhanced 3D settings:")),
            wxSizerFlags().Expand().Border(wxBOTTOM,16));
  
  m_hidden3d = new wxCheckBox(this, -1, _("Hide objects behind the surface"),
                          wxDefaultPosition, wxDefaultSize, wxCHK_3STATE);
  m_hidden3d -> Set3StateValue(wxCHK_UNDETERMINED);
  vbox->Add(m_hidden3d, wxSizerFlags().Expand()); 

  m_enhanced3d = new wxCheckBox(this, -1, _("Take surface color from steepness"),
                          wxDefaultPosition, wxDefaultSize, wxCHK_3STATE);
  m_enhanced3d -> Set3StateValue(wxCHK_UNDETERMINED);
  vbox->Add(m_enhanced3d, wxSizerFlags().Expand()); 
  
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

  SetName("Draw_Wiz3D");
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString Wiz3D::GetValue()
{
  wxString retval;
  if(m_hidden3d->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "surface_hide=true";
  }
  if(m_hidden3d->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "surface_hide=false";
  }
  if(m_enhanced3d->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "enhanced3d=true";
  }
  if(m_enhanced3d->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "enhanced3d=false";
  }
  return retval;
}

void WizContour::OnRadioButton(wxCommandEvent& WXUNUSED(dummy))
{
  if(m_contourNone->GetValue())
  {
    m_image->Load(Draw_ContourNone_png, Draw_ContourNone_png_len);
  }
  if(m_contourBase->GetValue())
  {
    m_image->Load(Draw_ContourBase_png, Draw_ContourBase_png_len);
  }
  if(m_contourBoth->GetValue())
  {
    m_image->Load(Draw_ContourBoth_png, Draw_ContourBoth_png_len);
  }
  if(m_contourSurface->GetValue())
  {
    m_image->Load(Draw_ContourSurface_png, Draw_ContourSurface_png_len);
  }
  if(m_contourOnly->GetValue())
  {
    m_image->Load(Draw_ContourMap_png, Draw_ContourMap_png_len);
  }
}

WizContour::WizContour(wxWindow *parent, Configuration *WXUNUSED(config)) :
  wxDialog(parent, -1, _("Contour lines settings for the following 3d plots"))
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  m_contourNone = new wxRadioButton(this, -1, _("No contour lines"), wxDefaultPosition,
                                    wxDefaultSize, wxRB_GROUP);
  m_contourNone->Connect(
          wxEVT_RADIOBUTTON,
          wxCommandEventHandler(WizContour::OnRadioButton),
          NULL, this
  );
  vbox->Add(m_contourNone, wxSizerFlags().Expand()); 
  m_contourSurface = new wxRadioButton(this, -1, _("Contour lines on the Surface"));
  m_contourSurface->Connect(
          wxEVT_RADIOBUTTON,
          wxCommandEventHandler(WizContour::OnRadioButton),
          NULL, this
  );
  vbox->Add(m_contourSurface, wxSizerFlags().Expand()); 
  m_contourBase = new wxRadioButton(this, -1, _("Contour lines on the Bottom"));
  m_contourBase->Connect(
          wxEVT_RADIOBUTTON,
          wxCommandEventHandler(WizContour::OnRadioButton),
          NULL, this
  );
  vbox->Add(m_contourBase, wxSizerFlags().Expand()); 
  m_contourBoth = new wxRadioButton(this, -1, _("Contour lines on surface and Bottom"));
  m_contourBoth->Connect(
          wxEVT_RADIOBUTTON,
          wxCommandEventHandler(WizContour::OnRadioButton),
          NULL, this
  );
  vbox->Add(m_contourBoth, wxSizerFlags().Expand()); 
  m_contourOnly = new wxRadioButton(this, -1, _("No plot, only contour lines"));
  m_contourOnly->Connect(
          wxEVT_RADIOBUTTON,
          wxCommandEventHandler(WizContour::OnRadioButton),
          NULL, this
  );
  vbox->Add(m_contourOnly, wxSizerFlags().Expand()); 

  m_contourBoth->SetValue(true);
  
  vbox->Add(
    m_image = new wxImagePanel(
      this,
      Draw_ContourBoth_png,Draw_ContourBoth_png_len),
    wxSizerFlags().Shaped().Border(wxALL,5).Center()
    );
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

  SetName("Draw_ContourWiz");
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString WizContour::GetValue()
{
  wxString retval;
  if(m_contourNone->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "contour='none";
  }
  if(m_contourBase->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "contour='base";
  }
  if(m_contourBoth->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "contour='both";
  }
  if(m_contourSurface->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "contour='surface";
  }
  if(m_contourOnly->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "contour='map";
  }
  return retval;
}

ParametricWiz::ParametricWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, _("Plot an parametric curve"))
{  
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  vbox->Add(new wxStaticText(this, -1,
                             _("Allows to provide separate expressions for calculating")),
            wxSizerFlags().Expand());
  if(dimensions < 3)
    vbox->Add(new wxStaticText(this, -1,
                               _("the x and the y coordinate.")),
              wxSizerFlags().Expand().Border(wxBOTTOM,16));
  else
    vbox->Add(new wxStaticText(this, -1,
                               _("the x, the y and the z coordinate.")),
              wxSizerFlags().Expand().Border(wxBOTTOM,16));

  vbox->Add(new wxStaticText(this,-1, _("Expression that calculates the x value")), wxSizerFlags());
  m_expression_x = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_expression_x, wxSizerFlags().Expand()); 
  vbox->Add(new wxStaticText(this,-1, _("Expression that calculates the y value")), wxSizerFlags());
  m_expression_y = new BTextCtrl(this,-1, config, "");
  vbox->Add(m_expression_y, wxSizerFlags().Expand());
  if(dimensions > 2)
  {
    vbox->Add(new wxStaticText(this,-1, _("Expression that calculates the z value")), wxSizerFlags());
    m_expression_z = new BTextCtrl(this,-1, config, "");
    vbox->Add(m_expression_z, wxSizerFlags().Expand()); 
  }
  
  vbox->Add(new wxStaticText(this,-1, _("Name of the parameter")), wxSizerFlags());
  vbox->Add(m_parameter = new BTextCtrl(this,-1, config, "t"), wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("Start value of the parameter")), wxSizerFlags());
  vbox->Add(m_parameterStart = new BTextCtrl(this,-1, config, "-2"), wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End value of the parameter")), wxSizerFlags());
  vbox->Add(m_parameterEnd = new BTextCtrl(this,-1, config, "2"), wxSizerFlags().Expand());

  vbox->Add(
    new wxImagePanel(
      this,
      Draw_Parametric_png,Draw_Parametric_png_len),
    wxSizerFlags().Shaped().Border(wxALL,5).Center()
    );
  
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
  SetName("Draw_%idParametricWiz");
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString ParametricWiz::GetValue()
{
  wxString retval;
  retval += wxT("parametric(\n    ") + m_expression_x->GetValue() + ",\n    ";
  retval += m_expression_y->GetValue() + ",\n    ";
  if(m_dimensions > 2)
    retval += m_expression_z->GetValue() + ",\n    ";
  retval += m_parameter->GetValue() + ",";
  retval += m_parameterStart->GetValue() + ",";
  retval += m_parameterEnd->GetValue() + "\n)";
  return retval;
}

WizPoints::WizPoints(wxWindow *parent, Configuration *config, int dimensions, wxString expr) :
  wxDialog(parent,-1, _("Draw points"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(m_data = new BTextCtrl(this,-1, config, expr), wxSizerFlags().Expand());

  wxStaticBox *formatBox = new wxStaticBox(this,-1,_("Data format"));
  wxStaticBoxSizer *formatSizer = new wxStaticBoxSizer(formatBox, wxVERTICAL);
  m_formatStd = new wxRadioButton(formatBox, -1,
                                  _("[x_1, x_2,...] or [x_1, x_2,...],[y_1, y_2,...] or matrix([x_1,y_1],[x_2,y_2],...)"), wxDefaultPosition,
                                    wxDefaultSize, wxRB_GROUP);
  formatSizer->Add(m_formatStd, wxSizerFlags().Expand()); 
  m_formatListOfLists = new wxRadioButton(formatBox, -1,
                                          _("[[x_1,x_2,...],[y_1,y_2,...]]")); 
  formatSizer->Add(m_formatListOfLists, wxSizerFlags().Expand()); 
  m_transposedMatrix = new wxRadioButton(formatBox, -1,
                                         _("matrix([x_1,x_2,...],[y_1,y_2,...])"));
  formatSizer->Add(m_transposedMatrix, wxSizerFlags().Expand()); 
  m_transposedListOfLists = new wxRadioButton(formatBox, -1,
                                              _("[[x_1,x_2,...],[y_1,y_2,...]]"));
  formatSizer->Add(m_transposedListOfLists, wxSizerFlags().Expand()); 
  vbox->Add(formatSizer, wxSizerFlags().Expand()); 
  m_pointsJoined = new wxCheckBox(this, -1, _("Connect the dots"),
                                  wxDefaultPosition, wxDefaultSize, wxCHK_3STATE);
  m_pointsJoined -> Set3StateValue(wxCHK_UNDETERMINED);
  vbox->Add(m_pointsJoined, wxSizerFlags().Expand()); 
  
  wxBoxSizer *pointTypeSizer = new wxBoxSizer(wxHORIZONTAL);
  wxArrayString pointTypes;
  pointTypes.Add(_("Reuse last"));
  pointTypes.Add(_("No points"));
  for (int i=0; i<15;i++)
    pointTypes.Add(wxString::Format("%i",i));  
  m_pointStyle = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize, pointTypes);
  pointTypeSizer->Add(new wxStaticText(this,-1,_("Point type:")), wxSizerFlags().Expand()); 
  pointTypeSizer->Add(m_pointStyle, wxSizerFlags().Expand()); 
  vbox->Add(pointTypeSizer, wxSizerFlags().Expand()); 
  
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
  SetName(wxString::Format("Draw_%idPointWiz", dimensions));
  m_data->SetValue(expr);
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString WizPoints::GetValue()
{
  wxString retval;
  if(m_pointStyle->GetSelection() > 0)
  {
    if(m_pointStyle->GetSelection() == 1)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "point_type='none";
    }
    else
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "point_type=" + m_pointStyle->GetStringSelection();
    }
  }
  if(m_pointsJoined->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "points_joined=false";
  }
  if(m_pointsJoined->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "points_joined=true";
  }

  wxString data = m_data->GetValue();
  if(data != wxEmptyString)
  {
    if(m_formatStd->GetValue())
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "points(" + data +")";
    }
    if(m_formatListOfLists->GetValue())
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "apply('points," + data +")";
    }
    if(m_transposedMatrix->GetValue())
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "points(transpose(" + data +"))";
    }
    if(m_transposedListOfLists->GetValue())
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "points(transpose(apply('matrix," + data +")))";
    }
  }
  return retval;
}


//! A wizard that sets the draw accuracy
WizDrawAccuracy::WizDrawAccuracy(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent,-1, _("Speed versus accuracy"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  vbox->Add(new wxStaticText(this,-1, _("Samples on a line:")), wxSizerFlags());
  wxBoxSizer *nticksBox = new wxBoxSizer(wxHORIZONTAL);
  nticksBox->Add(m_nticks = new BTextCtrl(this, -1, config, wxT("")), wxSizerFlags().Expand());
  nticksBox->Add(new wxStaticText(this,-1, _(" samples, on demand split ")), wxSizerFlags());
  nticksBox->Add(m_adapt_depth = new BTextCtrl(this, -1, config, wxT("")), wxSizerFlags().Expand());
  nticksBox->Add(new wxStaticText(this,-1, _(" times")), wxSizerFlags());
  vbox->Add(nticksBox, wxSizerFlags().Expand());

  if(dimensions < 3)
  {
    vbox->Add(new wxStaticText(this,-1, _("Samples for implicit plots:")), wxSizerFlags());
    wxBoxSizer *ipGridBox = new wxBoxSizer(wxHORIZONTAL);
    ipGridBox->Add(m_ip_grid_x = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    ipGridBox->Add(new wxStaticText(this,-1, _("X")), wxSizerFlags());
    ipGridBox->Add(m_ip_grid_y = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    ipGridBox->Add(new wxStaticText(this,-1, _(" samples, on demand split ")), wxSizerFlags());
    ipGridBox->Add(m_ip_grid_in_x = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    ipGridBox->Add(new wxStaticText(this,-1, _("X")), wxSizerFlags());
    ipGridBox->Add(m_ip_grid_in_y = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    ipGridBox->Add(new wxStaticText(this,-1, _(" times")), wxSizerFlags());
    vbox->Add(ipGridBox, wxSizerFlags().Expand());
  }
  else
  {
    vbox->Add(new wxStaticText(this,-1, _("Samples for explicit and parametric plots:")), wxSizerFlags());
    wxBoxSizer *exp3dGridBox = new wxBoxSizer(wxHORIZONTAL);
    exp3dGridBox->Add(m_xu_grid = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    exp3dGridBox->Add(new wxStaticText(this,-1, _("X")), wxSizerFlags());
    exp3dGridBox->Add(m_yv_grid = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    exp3dGridBox->Add(new wxStaticText(this,-1, _(" samples")), wxSizerFlags());
    vbox->Add(exp3dGridBox, wxSizerFlags().Expand());

    vbox->Add(new wxStaticText(this,-1, _("Samples for implicit plots and regions:")), wxSizerFlags());
    wxBoxSizer *regionGridBox = new wxBoxSizer(wxHORIZONTAL);
    regionGridBox->Add(m_x_voxel = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    regionGridBox->Add(new wxStaticText(this,-1, _("X")), wxSizerFlags());
    regionGridBox->Add(m_y_voxel = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    regionGridBox->Add(new wxStaticText(this,-1, _("X")), wxSizerFlags());
    regionGridBox->Add(m_z_voxel = new BTextCtrl(this,-1, config, wxT("")), wxSizerFlags().Expand());
    regionGridBox->Add(new wxStaticText(this,-1, _(" samples")), wxSizerFlags());
    vbox->Add(regionGridBox, wxSizerFlags().Expand());
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
  SetName(wxString::Format("Draw_Accuracy%idWiz", dimensions));
  SetSizerAndFit(vbox);
  wxPersistenceManager::Get().RegisterAndRestore(this);
}

wxString WizDrawAccuracy::GetValue()
{
  wxString retval;
  if(m_nticks->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "nticks=" + m_nticks->GetValue();
  }

  if(m_adapt_depth->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n";
    retval += "adapt_depth=" + m_adapt_depth->GetValue();
  }
  
  if(m_dimensions < 3)
  {
    if((m_ip_grid_x->GetValue() != wxEmptyString) && (m_ip_grid_y->GetValue() != wxEmptyString))
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "ip_grid=[" + m_ip_grid_x->GetValue() + "," + m_ip_grid_y->GetValue()+"]";
    }
  }
  else
  {
    if(m_xu_grid->GetValue() != wxEmptyString)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "xu_grid=" + m_xu_grid->GetValue();
    }
    if(m_yv_grid->GetValue() != wxEmptyString)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "yv_grid=" + m_yv_grid->GetValue();
    }
      
    if(m_x_voxel->GetValue() != wxEmptyString)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "x_voxel=" + m_x_voxel->GetValue();
    }
    if(m_y_voxel->GetValue() != wxEmptyString)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "y_voxel=" + m_y_voxel->GetValue();
    }
    if(m_z_voxel->GetValue() != wxEmptyString)
    {
      if(retval != wxEmptyString)
        retval += ",\n";
      retval += "z_voxel=" + m_z_voxel->GetValue();
    }
  }
  return retval;
}
 
#define MAX(a, b) ((a)>(b) ? (a) : (b))
wxImagePanel::wxImagePanel(wxWindow* parent, unsigned char *data, size_t len) :
wxPanel(parent)
{  
  Load(data,len);
  int ppi;
#if wxCHECK_VERSION(3, 1, 1)
  wxDisplay display;
  ppi = display.GetPPI().x;
#else
  ppi = wxGetDisplayPPI().x;
#endif
  ppi = MAX(ppi,75);
  SetMinSize(wxSize(ppi*6,m_image.GetHeight()*ppi*6/m_image.GetWidth()));
  Connect(wxEVT_PAINT,
          wxPaintEventHandler(wxImagePanel::paintEvent),
          NULL, this);
  Connect(wxEVT_SIZE,
          wxSizeEventHandler(wxImagePanel::OnSize),
          NULL, this);
}

void wxImagePanel::Load(unsigned char *data, size_t len)
{
  wxMemoryInputStream istream(data,len);
  m_image.LoadFile(istream);
  m_w = m_h = -1;
  Refresh(true);
}

/*
 * Called by the system of by wxWidgets when the panel needs
 * to be redrawn. You can also trigger this call by
 * calling Refresh()/Update().
 */
 
void wxImagePanel::paintEvent(wxPaintEvent& WXUNUSED(evt))
{
  // depending on your system you may need to look at double-buffered dcs
  wxMemoryDC dcm;
  wxPaintDC dc(this);
  int neww, newh;
  dc.GetSize( &neww, &newh );
  
  if( neww != m_w || newh != m_h )
  {
    m_resized = wxBitmap( m_image.Scale( neww, newh, wxIMAGE_QUALITY_HIGH) );
    m_w = neww;
    m_h = newh;
  }
  dc.DrawBitmap( m_resized, 0, 0, false );
}
 
/*
 * Here we call refresh to tell the panel to draw itself again.
 * So when the user resizes the image panel the image should be resized too.
 */
void wxImagePanel::OnSize(wxSizeEvent& event){
    Refresh();
    event.Skip();
}
