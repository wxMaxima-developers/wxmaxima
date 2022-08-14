// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#include "VariablesPane.h"
#include <memory>

Variablespane::Variablespane(wxWindow *parent, wxWindowID id)
    : wxPanel(parent, id) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  m_grid = new wxGrid(this, -1);
  m_grid->BeginBatch();
  m_grid->CreateGrid(1, 2);
  m_grid->SetUseNativeColLabels();
  wxGridCellAttr *attr0, *attr1;
  attr0 = new wxGridCellAttr;
  m_grid->SetColAttr(0, attr0);
  m_grid->SetColLabelValue(0, _("Variable"));
  attr1 = new wxGridCellAttr;
  //  attr1->SetReadOnly();
  m_grid->SetColAttr(1, attr1);
  m_grid->SetColLabelValue(1, _("Contents"));
  m_rightClickRow = -1;
  m_grid->Connect(wxEVT_GRID_CELL_CHANGED,
                  wxGridEventHandler(Variablespane::OnTextChange), NULL, this);
  m_grid->Connect(wxEVT_GRID_CELL_CHANGING,
                  wxGridEventHandler(Variablespane::OnTextChanging), NULL,
                  this);
  m_grid->Connect(wxEVT_GRID_CELL_RIGHT_CLICK,
                  wxGridEventHandler(Variablespane::OnRightClick), NULL, this);
  m_grid->Connect(wxEVT_MENU, wxCommandEventHandler(Variablespane::InsertMenu),
                  NULL, this);
  m_grid->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(Variablespane::OnKey), NULL,
                  this);
  //  Connect(wxEVT_CHAR,
  //          wxKeyEventHandler(Variablespane::OnChar),
  //          NULL, this);
  m_grid->HideRowLabels();
  m_grid->EnableDragCell();
  m_grid->EndBatch();
  vbox->Add(m_grid, wxSizerFlags(10).Expand());
  SetSizerAndFit(vbox);
  SetMinSize(wxSize(wxSystemSettings::GetMetric(wxSYS_SCREEN_X) / 10,
                    wxSystemSettings::GetMetric(wxSYS_SCREEN_Y) / 10));
}

void Variablespane::OnChar(wxKeyEvent &event) {
  wxChar txt(event.GetUnicodeKey());
  if ((wxIsprint(txt)) && (m_grid->GetGridCursorRow() >= 0)) {
    m_grid->SetCellValue(m_grid->GetGridCursorRow(), 0, wxString(txt));
    m_grid->GoToCell(m_grid->GetGridCursorRow(), 0);
    m_grid->ShowCellEditControl();
    m_grid->EnableCellEditControl();
  }
}

void Variablespane::OnKey(wxKeyEvent &event) {
  switch (event.GetKeyCode()) {

  case WXK_DELETE:
  case WXK_NUMPAD_DELETE: {
    if (m_grid->GetNumberRows() > 1) {
      m_grid->BeginBatch();
      wxArrayInt selectedRows = m_grid->GetSelectedRows();
      selectedRows.Sort(CompareInt);

      if (!selectedRows.IsEmpty()) {
        int offset = 0;
        for (wxArrayInt::const_iterator it = selectedRows.begin();
             it != selectedRows.end(); ++it) {
          m_grid->DeleteRows(*it - offset);
          offset++;
        }
      } else {
        m_grid->DeleteRows(m_grid->GetGridCursorRow());
      }
      wxGridEvent evt(wxID_ANY, wxEVT_GRID_CELL_CHANGED, this,
                      m_grid->GetNumberRows() - 1, 0);
      OnTextChange(evt);
      m_grid->EndBatch();
    }
    break;
  }
  default:
    event.Skip();
  }
}

void Variablespane::InsertMenu(wxCommandEvent &event) {
  wxString varname;
  switch (event.GetId()) {
  case varID_values:
    varname = "values";
    break;
  case varID_functions:
    varname = "functions";
    break;
  case varID_arrays:
    varname = "arrays";
    break;
  case varID_myoptions:
    varname = "myoptions";
    break;
  case varID_rules:
    varname = "rules";
    break;
  case varID_aliases:
    varname = "aliases";
    break;
  case varID_structs:
    varname = "structures";
    break;
  case varID_labels:
    varname = "labels";
    break;
  case varID_dependencies:
    varname = "dependencies";
    break;
  case varID_gradefs:
    varname = "gradefs";
    break;
  case varID_prop:
    varname = "props";
    break;
  case varID_macros:
    varname = "macros";
    break;
  case varID_let_rule_packages:
    varname = "let_rule_packages";
    break;
  case varID_clear:
    Clear();
    break;
  case varID_add_all: {
    wxMenuEvent *VarAddEvent = new wxMenuEvent(wxEVT_MENU, varID_add_all);
    wxWindow *top = this;
    while (top->GetParent())
      top = top->GetParent();
    top->GetEventHandler()->QueueEvent(VarAddEvent);
    break;
  }
  case varID_delete_row:
    if ((m_rightClickRow >= 0) && (m_rightClickRow < m_grid->GetNumberRows())) {
      m_grid->DeleteRows(m_rightClickRow);
      wxGridEvent evt(wxID_ANY, wxEVT_GRID_CELL_CHANGED, this, m_rightClickRow,
                      0);
      OnTextChange(evt);
    }
    break;
  }
  m_grid->SetCellValue(m_grid->GetNumberRows() - 1, 0, varname);
  wxGridEvent evt(wxID_ANY, wxEVT_GRID_CELL_CHANGED, this,
                  m_grid->GetNumberRows() - 1, 0);
  OnTextChange(evt);
}

void Variablespane::OnRightClick(wxGridEvent &event) {
  m_rightClickRow = event.GetRow();
  m_vars.clear();
  for (int i = 0; i < m_grid->GetNumberRows(); i++)
    m_vars[m_grid->GetCellValue(i, 0)] = 1;

  std::unique_ptr<wxMenu> popupMenu(new wxMenu);
  if (m_vars["dependencies"] != 1)
    popupMenu->Append(varID_dependencies, _("List of functional dependencies"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["values"] != 1)
    popupMenu->Append(varID_values, _("List of user variables"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["functions"] != 1)
    popupMenu->Append(varID_functions, _("List of user functions"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["arrays"] != 1)
    popupMenu->Append(varID_arrays, _("List of arrays"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["myoptions"] != 1)
    popupMenu->Append(varID_myoptions, _("List of changed options"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["rules"] != 1)
    popupMenu->Append(varID_rules, _("List of user rules"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["aliases"] != 1)
    popupMenu->Append(varID_aliases, _("List of user aliases"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["structures"] != 1)
    popupMenu->Append(varID_structs, _("List of structs"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["labels"] != 1)
    popupMenu->Append(varID_structs, _("List of labels"), wxEmptyString,
                      wxITEM_NORMAL);
  if (m_vars["gradefs"] != 1)
    popupMenu->Append(varID_gradefs, _("List of user-defined derivatives"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["props"] != 1)
    popupMenu->Append(varID_prop, _("List of user-defined properties"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["macros"] != 1)
    popupMenu->Append(varID_macros, _("List of user-defined macros"),
                      wxEmptyString, wxITEM_NORMAL);
  if (m_vars["let_rule_packages"] != 1)
    popupMenu->Append(varID_let_rule_packages,
                      _("List of user-defined let rule packages"),
                      wxEmptyString, wxITEM_NORMAL);
  popupMenu->AppendSeparator();
  if (m_grid->GetGridCursorRow() >= 0) {
    popupMenu->Append(varID_delete_row, _("Remove"), wxEmptyString,
                      wxITEM_NORMAL);
  }

  if (m_grid->GetNumberRows() > 2) {
    popupMenu->Append(varID_clear, _("Remove all"), wxEmptyString,
                      wxITEM_NORMAL);
  }
  popupMenu->Append(varID_add_all, _("Add all"), wxEmptyString, wxITEM_NORMAL);

  if (popupMenu->GetMenuItemCount() > 0)
    PopupMenu(popupMenu.get());
}

void Variablespane::OnTextChanging(wxGridEvent &event) {
  // Setting the 2nd column to "Read-only" prevents copy-and-paste.
  // Preventing edits is therefore perhaps better for our purposes.
  if (event.GetCol() == 1) {
    event.Veto();
    return;
  }
}

void Variablespane::OnTextChange(wxGridEvent &event) {
  if ((event.GetRow() > m_grid->GetNumberRows()) || (event.GetRow() < 0))
    return;
  m_grid->BeginBatch();
  if (IsValidVariable(m_grid->GetCellValue(event.GetRow(), 0))) {
    m_grid->SetCellValue(event.GetRow(), 1, wxT(""));
    m_grid->SetCellTextColour(event.GetRow(), 0, *wxBLACK);
  } else {
    if (m_grid->GetCellValue(event.GetRow(), 0) != wxEmptyString) {
      m_grid->SetCellTextColour(event.GetRow(), 0, *wxRED);
      m_grid->SetCellTextColour(event.GetRow(), 1, *wxLIGHT_GREY);
      m_grid->SetCellValue(event.GetRow(), 1, _("(Not a valid variable name)"));
      m_grid->RefreshAttr(event.GetRow(), 1);
    }
  }
  m_grid->RefreshAttr(event.GetRow(), 0);

  if ((m_grid->GetNumberRows() == 0) ||
      (m_grid->GetCellValue(m_grid->GetNumberRows() - 1, 0) != wxEmptyString))
    m_grid->AppendRows();
  else
    for (int i = 0; i < m_grid->GetNumberRows() - 1; i++)
      if (m_grid->GetCellValue(i, 0) == wxEmptyString)
        m_grid->DeleteRows(i);
  wxMenuEvent *VarReadEvent = new wxMenuEvent(wxEVT_MENU, varID_newVar);
  wxWindow *top = this;
  while (top->GetParent())
    top = top->GetParent();
  top->GetEventHandler()->QueueEvent(VarReadEvent);

  // Avoid introducing a cell with the same name twice.
  m_vars.clear();
  for (int i = 0; i < m_grid->GetNumberRows(); i++) {
    if (i != event.GetRow())
      m_vars[m_grid->GetCellValue(i, 0)] = i + 1;
  }
  int identicalVar = m_vars[m_grid->GetCellValue(event.GetRow(), 0)];
  if (identicalVar > 0) {
    wxEventBlocker blocker(this);
    m_grid->DeleteRows(identicalVar - 1);
  }
  m_grid->EndBatch();
}

void Variablespane::VariableValue(wxString var, wxString val) {
  for (int i = 0; i < m_grid->GetNumberRows(); i++)
    if (m_grid->GetCellValue(i, 0) == UnescapeVarname(var)) {
      m_grid->SetCellTextColour(i, 1, *wxBLACK);
      if (m_grid->GetCellValue(i, 1) != val) {
        m_updateSizeNeeded = true;
        m_grid->SetCellValue(i, 1, val);
      }
      m_grid->RefreshAttr(i, 1);
    }
}

void Variablespane::VariableUndefined(wxString var) {
  for (int i = 0; i < m_grid->GetNumberRows(); i++)
    if (m_grid->GetCellValue(i, 0) == UnescapeVarname(var)) {
      m_grid->SetCellTextColour(i, 1, *wxLIGHT_GREY);
      m_grid->SetCellValue(i, 1, _("Undefined"));
      m_grid->RefreshAttr(i, 1);
    }
}

wxArrayString Variablespane::GetEscapedVarnames() {
  wxArrayString retVal;
  for (int i = 0; i < m_grid->GetNumberRows(); i++) {
    wxString var = m_grid->GetCellValue(i, 0);
    if (IsValidVariable(var))
      retVal.Add(InvertCase(EscapeVarname(var)));
  }
  return retVal;
}

wxArrayString Variablespane::GetVarnames() {
  wxArrayString retVal;
  for (int i = 0; i < m_grid->GetNumberRows(); i++) {
    wxString var = m_grid->GetCellValue(i, 0);
    retVal.Add(var);
  }
  return retVal;
}

wxString Variablespane::InvertCase(wxString var) {
  wxString retval;
  for (wxString::const_iterator it = var.begin(); it != var.end(); ++it) {
    if (wxIsupper(*it))
      retval += wxString(*it).Lower();
    else {
      if (wxIslower(*it))
        retval += wxString(*it).Upper();
      else
        retval += *it;
    }
  }
  return retval;
}

void Variablespane::AddWatchCode(wxString code) {
  m_updateSizeNeeded = true;
  wxString unescapedCode;
  for (wxString::const_iterator it = code.begin(); it != code.end(); ++it) {
    if (*it != '\\')
      unescapedCode += *it;
    else {
      ++it;
      if (it != code.end())
        unescapedCode += *it;
    }
  }
  AddWatch(unescapedCode);
}

void Variablespane::AddWatch(wxString watch) {
  m_updateSizeNeeded = true;
  m_grid->BeginBatch();
  m_grid->SetCellValue(m_grid->GetNumberRows() - 1, 0, watch);
  wxGridEvent evt(wxID_ANY, wxEVT_GRID_CELL_CHANGED, this,
                  m_grid->GetNumberRows() - 1, 0);
  OnTextChange(evt);
  m_grid->EndBatch();
}

wxString Variablespane::UnescapeVarname(wxString var) {
  if (var.StartsWith(wxT("$")))
    var = var.Right(var.Length() - 1);
  else
    var = "?" + var;
  return var;
}

wxString Variablespane::EscapeVarname(wxString var) {
  var.Replace("\\", "\\\\");
  var.Replace("+", "\\+");
  var.Replace("#", "\\#");
  var.Replace("'", "\\'");
  var.Replace("\"", "\\\"");
  var.Replace("!", "\\!");
  var.Replace("-", "\\-");
  var.Replace("*", "\\*");
  var.Replace("/", "\\/");
  var.Replace("^", "\\^");
  var.Replace("$", "\\$");
  var.Replace(";", "\\;");
  var.Replace(",", "\\,");
  var.Replace("<", "\\<");
  var.Replace(">", "\\>");
  var.Replace("@", "\\@");
  var.Replace("!", "\\!");
  var.Replace("~", "\\~");
  var.Replace("`", "\\`");
  var.Replace("?", "\\?");
  var.Replace("(", "\\(");
  var.Replace(")", "\\)");
  var.Replace("{", "\\{");
  var.Replace("}", "\\}");
  var.Replace("[", "\\[");
  var.Replace("]", "\\]");
  var.Replace(" ", "\\ ");
  if (var.StartsWith("\\?"))
    var = var.Right(var.Length() - 1);
  if (!var.StartsWith(wxT("?")))
    var = "$" + var;
  return var;
}

bool Variablespane::IsValidVariable(wxString var) {
  for (wxString::const_iterator it = var.begin(); it != var.end(); ++it) {
    if (!wxIsprint(*it))
      return false;
  }

  if (var == wxEmptyString)
    return false;
  if ((var[0] >= '0') && (var[0] <= '9'))
    return false;
  if (var.Contains(":"))
    return false;
  return true;
}

void Variablespane::UpdateSize() {
  if (m_updateSizeNeeded) {
    m_updateSizeNeeded = false;
    m_grid->AutoSize();
    Layout();
  }
}

void Variablespane::ResetValues() {
  for (int i = 0; i < m_grid->GetNumberRows(); i++) {
    if (m_grid->GetCellValue(i, 0) != wxEmptyString) {
      m_grid->SetCellTextColour(i, 1, *wxLIGHT_GREY);
      m_grid->SetCellValue(i, 1, _("Undefined"));
      m_grid->RefreshAttr(i, 1);
    } else
      m_grid->SetCellValue(i, 1, wxT(""));
  }
}

void Variablespane::Clear() {
  m_updateSizeNeeded = true;
  while (m_grid->GetNumberRows() > 1)
    m_grid->DeleteRows(0);
}

Variablespane::~Variablespane() {}
