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

/*! \file
  This file contains all the wizards the draw sidepane needs.
 */

#include "DrawWiz.h"
#include <wx/statbox.h>

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
  m_xStart = new BTextCtrl(this,-1, config, "-2");
  vbox->Add(m_xStart, wxSizerFlags().Expand()); 

  vbox->Add(new wxStaticText(this,-1, _("End of the x value")), wxSizerFlags());
  m_xEnd = new BTextCtrl(this,-1, config, "2");
  vbox->Add(m_xEnd, wxSizerFlags().Expand());

  if(m_dimensions > 2)
  {
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


AxisWiz::AxisWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, _("Plot an explicit expression"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  
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
};

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
      retval += ",\n    ";
    retval += "ylabel=\"" + m_yLabel->GetValue() + "\"";
  }
  if((m_dimensions == 3) && (m_zLabel->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "zlabel=\"" + m_zLabel->GetValue() + "\"";
  }

  if(m_x2Label->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "xlabel_secondary=\"" + m_x2Label->GetValue() + "\"";
  }
  if(m_y2Label->GetValue() != wxEmptyString)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "ylabel_secondary=\"" + m_y2Label->GetValue() + "\"";
  }

  if((m_xStart->GetValue() != wxEmptyString) && (m_xEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "xrange=[" + m_xStart->GetValue() + "," + m_xEnd->GetValue() + "]";
  }
  if((m_yStart->GetValue() != wxEmptyString) && (m_yEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "yrange=[" + m_yStart->GetValue() + "," + m_yEnd->GetValue() + "]";
  }
  if((m_dimensions == 3) && (m_zStart->GetValue() != wxEmptyString) && (m_zEnd->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "zrange=[" + m_zStart->GetValue() + "," + m_zEnd->GetValue() + "]";
  }

  if((m_x2Start->GetValue() != wxEmptyString) && (m_x2End->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "xrange_secondary=[" + m_x2Start->GetValue() + "," + m_x2End->GetValue() + "]";
  }
  if((m_y2Start->GetValue() != wxEmptyString) && (m_y2End->GetValue() != wxEmptyString))
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "yrange_secondary=[" + m_y2Start->GetValue() + "," + m_y2End->GetValue() + "]";
  }

  if(m_useSecondaryX->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "xaxis_secondary=true";
  }
  if(m_useSecondaryX->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "xaxis_secondary=false";
  }

  if(m_useSecondaryY->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "yaxis_secondary=true";
  }
  if(m_useSecondaryY->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "yaxis_secondary=false";
  }

  return retval;
}

DrawWiz::DrawWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, wxString::Format(_("Setup a %iD scene"),dimensions))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  m_singleFrame = new wxRadioButton(this, -1, _("Single frame"), wxDefaultPosition,
                                    wxDefaultSize, wxRB_GROUP);
  vbox->Add(m_singleFrame, wxSizerFlags().Expand().Border(wxALL,5));

  m_multipleFrames = new wxRadioButton(this, -1, _("Multiple frames"));
  vbox->Add(m_multipleFrames, wxSizerFlags().Expand().Border(wxALL,5));
  wxPanel *animPanel = new wxPanel(this,-1);
  wxBoxSizer *animPanelVbox = new wxBoxSizer(wxVERTICAL);
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter")), wxSizerFlags());
  m_frameVar = new BTextCtrl(animPanel, -1, config, "t");
  animPanelVbox->Add(m_frameVar, wxSizerFlags().Expand().Border(wxALL,5));
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter start")), wxSizerFlags());
  m_varStart = new BTextCtrl(animPanel,-1, config, "1");
  animPanelVbox->Add(m_varStart, wxSizerFlags().Expand().Border(wxALL,5));
  animPanelVbox->Add(new wxStaticText(animPanel,-1, _("Frame counter end")), wxSizerFlags());
  m_varEnd = new BTextCtrl(animPanel,-1, config, "10");
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
  
  SetSizerAndFit(vbox);
}

wxString DrawWiz::GetValue()
{
  if(m_dimensions < 3)
  {
    if(m_singleFrame->GetValue())
      return wxT("wxdraw2d(\n)$");
    else
    {
      return wxString("with_slider_draw(\n    ") + m_frameVar->GetValue() + ",makelist(i,i," +
        m_varStart->GetValue() + "," + m_varEnd->GetValue() + ")\n)$";
    }
  }
  else
  {
    if(m_singleFrame->GetValue())
      return wxT("wxdraw3d(\n)$");
    else
    {
      return wxString("with_slider_draw3d(\n    ") + m_frameVar->GetValue() + ",makelist(i,i," +
        m_varStart->GetValue() + "," + m_varEnd->GetValue() + ")\n)$";
    }
  }
}


Wiz3D::Wiz3D(wxWindow *parent, Configuration *config) :
  wxDialog(parent, -1, _("Settings for the following 3d plots"))
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

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

  SetSizerAndFit(vbox);
};

wxString Wiz3D::GetValue()
{
  wxString retval;
  if(m_hidden3d->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "surface_hide=true";
  }
  if(m_hidden3d->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "surface_hide=false";
  }
  if(m_enhanced3d->Get3StateValue() == wxCHK_CHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "enhanced3d=true";
  }
  if(m_enhanced3d->Get3StateValue() == wxCHK_UNCHECKED)
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "enhanced3d=false";
  }
  return retval;
}

WizContour::WizContour(wxWindow *parent, Configuration *config) :
  wxDialog(parent, -1, _("Contour lines settings for the following 3d plots"))
{
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  m_contourNone = new wxRadioButton(this, -1, _("No contour lines"), wxDefaultPosition,
                                    wxDefaultSize, wxRB_GROUP);
  vbox->Add(m_contourNone, wxSizerFlags().Expand()); 
  m_contourSurface = new wxRadioButton(this, -1, _("Contour lines on the Surface"));
  vbox->Add(m_contourSurface, wxSizerFlags().Expand()); 
  m_contourBase = new wxRadioButton(this, -1, _("Contour lines on the Bottom"));
  vbox->Add(m_contourBase, wxSizerFlags().Expand()); 
  m_contourBoth = new wxRadioButton(this, -1, _("Contour lines on surface and Bottom"));
  vbox->Add(m_contourBoth, wxSizerFlags().Expand()); 
  m_contourOnly = new wxRadioButton(this, -1, _("No plot, only contour lines"));
  vbox->Add(m_contourOnly, wxSizerFlags().Expand()); 
  
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
};

wxString WizContour::GetValue()
{
  wxString retval;
  if(m_contourNone->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "contour='none";
  }
  if(m_contourBase->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "contour='base";
  }
  if(m_contourBoth->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "contour='both";
  }
  if(m_contourSurface->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "contour='surface";
  }
  if(m_contourOnly->GetValue())
  {
    if(retval != wxEmptyString)
      retval += ",\n    ";
    retval += "contour='map";
  }
  return retval;
}

ParametricWiz::ParametricWiz(wxWindow *parent, Configuration *config, int dimensions) :
  wxDialog(parent, -1, _("Plot an parametric curve"))
{
  m_dimensions = dimensions;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
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

wxString ParametricWiz::GetValue()
{
  wxString retval;
  retval += wxT("parametric(\n        ") + m_expression_x->GetValue() + ",\n        ";
  retval += m_expression_y->GetValue() + ",\n        ";
  if(m_dimensions > 2)
    retval += m_expression_z->GetValue() + ",\n        ";
  retval += m_parameter->GetValue() + ",";
  retval += m_parameterStart->GetValue() + ",";
  retval += m_parameterEnd->GetValue() + "\n)";
  return retval;
}
