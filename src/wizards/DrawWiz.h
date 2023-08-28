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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares all the wizards the draw sidepane needs.
*/

#ifndef DRAWWIZ_H
#define DRAWWIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/statline.h>
#include <wx/image.h>
#include <wx/radiobut.h>
#include <wx/persist.h>
#include <wx/persist/toplevel.h>

#include "BTextCtrl.h"
#include "Configuration.h"
#include "SvgPanel.h"

//! A wizard for explicit plots using draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class ExplicitWiz : public wxDialog
{
public:
  ExplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions = 2);
  wxString GetValue();
private:
  int m_dimensions;
  BTextCtrl *m_expression;
  BTextCtrl *m_filledfunc  = NULL;
  BTextCtrl *m_x;
  BTextCtrl *m_xStart;
  BTextCtrl *m_xEnd;
  BTextCtrl *m_y = NULL;
  BTextCtrl *m_yStart = NULL;
  BTextCtrl *m_yEnd = NULL;
};

//! A wizard for parametric plots using draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class ParametricWiz : public wxDialog
{
public:
  ParametricWiz(wxWindow *parent, Configuration *config, int dimensions = 2);
  wxString GetValue();
private:
  int m_dimensions;
  BTextCtrl *m_expression_x;
  BTextCtrl *m_expression_y;
  BTextCtrl *m_expression_z = NULL;
  BTextCtrl *m_parameter;
  BTextCtrl *m_parameterStart = NULL;
  BTextCtrl *m_parameterEnd;
};

//! A wizard for implicit plots using draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class ImplicitWiz : public wxDialog
{
public:
  ImplicitWiz(wxWindow *parent, Configuration *config, wxString expression, int dimensions = 2);
  wxString GetValue();
private:
  int m_dimensions;
  BTextCtrl *m_expression;
  BTextCtrl *m_x;
  BTextCtrl *m_xStart;
  BTextCtrl *m_xEnd;
  BTextCtrl *m_y;
  BTextCtrl *m_yStart;
  BTextCtrl *m_yEnd;
  BTextCtrl *m_z = NULL;
  BTextCtrl *m_zStart = NULL;
  BTextCtrl *m_zEnd = NULL;
};

//! A wizard for axis setup for draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class AxisWiz : public wxDialog
{
public:
  AxisWiz(wxWindow *parent, Configuration *config, int dimensions = 2);
  wxString GetValue();
private:
  int m_dimensions;
  BTextCtrl *m_xLabel;
  BTextCtrl *m_xStart;
  BTextCtrl *m_xEnd;
  BTextCtrl *m_yLabel;
  BTextCtrl *m_yStart;
  BTextCtrl *m_yEnd;
  BTextCtrl *m_zLabel = NULL;
  BTextCtrl *m_zStart = NULL;
  BTextCtrl *m_zEnd = NULL;
  wxCheckBox *m_useSecondaryX;
  wxCheckBox *m_useSecondaryY;
  BTextCtrl *m_x2Label = NULL;
  BTextCtrl *m_x2Start = NULL;
  BTextCtrl *m_x2End = NULL;
  BTextCtrl *m_y2Label = NULL;
  BTextCtrl *m_y2Start = NULL;
  BTextCtrl *m_y2End = NULL;
};


//! A wizard for explicit plots using draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class DrawWiz : public wxDialog
{
public:
  DrawWiz(wxWindow *parent, Configuration *config, int dimensions);
  wxString GetValue();
protected:
  void OnParametricFocus(wxFocusEvent &event);
private:
  int m_dimensions;
  wxRadioButton *m_singleFrame;
  wxRadioButton *m_multipleFrames;
  BTextCtrl *m_frameVar;
  BTextCtrl *m_varStart;
  BTextCtrl *m_varEnd;
};

//! A wizard for axis setup for draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class Wiz3D : public wxDialog
{
public:
  Wiz3D(wxWindow *parent, Configuration *config);
  wxString GetValue();
private:
  wxCheckBox *m_hidden3d;
  wxCheckBox *m_enhanced3d;
};

//! A wizard for contour plots
// cppcheck-suppress ctuOneDefinitionRuleViolation
class WizContour : public wxDialog
{
public:
  WizContour(wxWindow *parent, Configuration *config);
  wxString GetValue();
protected:
  void OnRadioButton(wxCommandEvent &dummy);
private:
  SvgPanel *m_image;
  wxRadioButton *m_contourNone; 
  wxRadioButton *m_contourSurface; 
  wxRadioButton *m_contourBase;
  wxRadioButton *m_contourBoth;
  wxRadioButton *m_contourOnly; 
};

//! A wizard for the points object for draw
// cppcheck-suppress ctuOneDefinitionRuleViolation
class WizPoints : public wxDialog
{
public:
  WizPoints(wxWindow *parent, Configuration *config, int dimensions, wxString expr);
  wxString GetValue();
private:
  BTextCtrl *m_data;  
  wxRadioButton *m_formatStd; 
  wxRadioButton *m_formatListOfLists; 
  wxRadioButton *m_transposedMatrix;
  wxRadioButton *m_transposedListOfLists; 
  wxCheckBox *m_pointsJoined;
  wxChoice *m_pointStyle;
  int m_dimensions;
};

//! A wizard that sets the draw accuracy
// cppcheck-suppress ctuOneDefinitionRuleViolation
class WizDrawAccuracy : public wxDialog
{
public:
  WizDrawAccuracy(wxWindow *parent, Configuration *config, int dimensions);
  wxString GetValue();
private:
  int m_dimensions;
  BTextCtrl *m_nticks;  
  BTextCtrl *m_adapt_depth;  
  BTextCtrl *m_xu_grid  = NULL;  
  BTextCtrl *m_yv_grid  = NULL;
  BTextCtrl *m_ip_grid_x = NULL;
  BTextCtrl *m_ip_grid_y = NULL;
  BTextCtrl *m_ip_grid_in_x = NULL;
  BTextCtrl *m_ip_grid_in_y = NULL;
  BTextCtrl *m_x_voxel = NULL;
  BTextCtrl *m_y_voxel = NULL;
  BTextCtrl *m_z_voxel = NULL;
};

#endif // DRAWWIZ_H
