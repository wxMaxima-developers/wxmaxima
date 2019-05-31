// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "VariablesPane.h"

Variablespane::Variablespane(wxWindow *parent, wxWindowID id) : wxGrid(parent, id)
{
  SetMinSize(wxSize(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )/10,
                    wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )/10));
  CreateGrid(1,2);
  wxGridCellAttr *attr0, *attr1;
  attr0 = new wxGridCellAttr;
  SetColAttr(0,attr0);
  SetColLabelValue(0,_("Variable"));
  attr1 = new wxGridCellAttr;
  attr1->SetReadOnly();
  attr1->SetRenderer(new wxGridCellAutoWrapStringRenderer);
  SetColAttr(1,attr1);
  SetColLabelValue(1,_("Contents"));
  Connect(wxEVT_GRID_CELL_CHANGED,
          wxGridEventHandler(Variablespane::OnTextChange),
          NULL, this);
  HideRowLabels();
  EnableDragCell();
}

void Variablespane::OnTextChange(wxGridEvent &event)
{
  SetCellValue(event.GetRow(),1,wxT(""));
  if((GetNumberRows() == 0) || (GetCellValue(GetNumberRows()-1,0) != wxEmptyString))
    AppendRows();
  else
    for(int i = 0; i < GetNumberRows() - 1; i++)
      if(GetCellValue(i,0) == wxEmptyString)
        DeleteRows(i);
  wxMenuEvent *VarReadEvent = new wxMenuEvent(wxEVT_MENU, varID_newVar);
  GetParent()->GetEventHandler()->QueueEvent(VarReadEvent);
}

void Variablespane::VariableValue(wxString var, wxString val)
{
  for(int i = 0; i < GetNumberRows(); i++)
    if(GetCellValue(i,0) == UnescapeVarname(var))
      SetCellValue(i,1,val);
}

wxArrayString Variablespane::GetEscapedVarnames()
{
  wxArrayString retVal;
  for(int i = 0; i < GetNumberRows(); i++)
  {
    wxString var = GetCellValue(i,0);
    if(IsValidVariable(var))
      retVal.Add(EscapeVarname(var));
  }
  return retVal;
}

wxString Variablespane::UnescapeVarname(wxString var)
{
  if(var.StartsWith(wxT("?")))
    var = var.Right(var.Length()-1);
  return var;
}

wxString Variablespane::EscapeVarname(wxString var)
{
  var.Replace("\\","\\\\");
  var.Replace("+","\\+");
  var.Replace("#","\\+");
  var.Replace("'","\\+");
  var.Replace("!","\\+");
  var.Replace("-","\\-");
  var.Replace("*","\\*");
  var.Replace("/","\\/");
  var.Replace("^","\\^");
  var.Replace(",","\\,");
  var.Replace("<","\\<");
  var.Replace(">","\\>");
  var.Replace("@","\\@");
  var.Replace("!","\\!");
  var.Replace("~","\\~");
  var.Replace("`","\\`");
  var.Replace("?","\\?");
  if(var.StartsWith("\\?"))
    var = var.Right(var.Length()-1);
  if(!var.StartsWith(wxT("?")))
    var = "$" + var;
  return var;
}

bool Variablespane::IsValidVariable(wxString var)
{
  for (wxString::iterator it = var.begin(); it != var.end(); ++it)
  {
    if(!wxIsprint(*it))
      return false;
  }

  if(var==wxEmptyString)
    return false;
  if(var.Contains(":"))
    return false;
  if(var.Contains("\'"))
    return false;
  if(var.Contains("\""))
    return false;
  if(var.Contains("\\"))
    return false;
  if(var.Contains(";"))
    return false;
  if(var.Contains("$"))
    return false;
  if(var.Contains("("))
    return false;
  if(var.Contains(")"))
    return false;
  return true;
}

void Variablespane::ResetValues()
{
  for(int i = 0; i < GetNumberRows(); i++)
    SetCellValue(i,1,wxT(""));
}

Variablespane::~Variablespane()
{
}
