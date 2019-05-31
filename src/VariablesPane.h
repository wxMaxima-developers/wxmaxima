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

#ifndef VARIABLESPANE_H
#define VARIABLESPANE_H

#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/arrstr.h>

/*! A "variables" sidepane

*/
class Variablespane : public wxGrid
{
public:
  enum VarIds
  {
    varID_newVar = wxID_HIGHEST + 3000,
  };

  Variablespane(wxWindow *parent, wxWindowID id = wxID_ANY);
  void OnTextChange(wxGridEvent &event);
  bool IsValidVariable(wxString var);
  wxArrayString GetEscapedVarnames();
  void ResetValues();
  wxString EscapeVarname(wxString var);
  wxString UnescapeVarname(wxString var);
  //! Tell the variables pane about a variable value
  void VariableValue(wxString var, wxString val);
  ~Variablespane();
};

#endif // VARIABLESPANE_H
