///
///  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "Inspector.h"
#include "MathParser.h"

#include <wx/tokenzr.h>
#include <wx/config.h>
#include <wx/settings.h>

Inspector::Inspector(wxWindow* parent, int id) : wxPanel(parent, id)
{
  // splitter window
  //m_splitter = new wxSplitterWindow(this, -1);
  //m_splitter->SetSashGravity(0.5);

  // combo
  const wxString m_combochoices[] =
    {
      _("Variables"),
      _("Functions")
    };
  m_combo = new wxComboBox(this, inspector_combo_id, wxEmptyString,
      wxDefaultPosition, wxDefaultSize, 2, m_combochoices, wxCB_DROPDOWN | wxCB_READONLY);
  m_combo->SetValue(_("Variables"));
  m_category = INSPECTOR_VARIABLES;
  //listbox
  m_listbox = new wxListBox(this, inspector_listbox_id,
      wxDefaultPosition, wxDefaultSize,
      0, NULL, wxLB_EXTENDED | wxLB_NEEDED_SB);

  //mathctrl
  m_minimathctrl = new MiniMathCtrl(this, -1, wxDefaultPosition, wxDefaultSize);

  // LAYOUT
  wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *vboxleft = new wxBoxSizer(wxVERTICAL);

  // left side
  vboxleft->Add(m_combo, 0, wxTOP | wxLEFT, 10);
  vboxleft->Add(m_listbox, 1, wxEXPAND | wxALL, 5);

  hbox->Add(vboxleft, 2, wxEXPAND, 0); // left side
  hbox->Add(m_minimathctrl, 3, wxEXPAND | wxALL, 0); // right side

  //SetAutoLayout(true);
  SetSizer(hbox);
  hbox->Fit(this);
  hbox->SetSizeHints(this);
//  Layout();

  m_wantListUpdate = false;
}

Inspector::~Inspector()
{
}

void Inspector::SetValues(wxArrayString arrstr)
{
  m_lbStrings = arrstr;
  m_listbox->Set(arrstr);
}

int sortingfunc(int* a, int* b)
{
  if ((*a) > (*b))
    return 1;
  else if ((*a) == (*b))
    return 0;
  else
    return -1;
}

// Returns a string that is to be sent to maxima
// so the Inspector will be updated correctly
//
// This is called from wxMaxima.cpp, when a selection
// in listbox is changed (and minimathctrl needs update).
wxString Inspector::GetMaximaCommand()
{

  if (m_wantListUpdate) {
    switch (m_category){
      case INSPECTOR_VARIABLES:
        return wxT(":lisp (inspector-list-vars)");
        break;
      case INSPECTOR_FUNCTIONS:
        return wxT(":lisp (inspector-list-funs)");
        break;
      default:
        return wxEmptyString;
    }
  }
  else { // we want to update minimathctrl
    wxString ans = wxEmptyString;
    wxArrayInt selections;
    int numsel = m_listbox->GetSelections(selections);
    if (numsel < 1)
      return wxEmptyString;
    selections.Sort(&sortingfunc);

    if (m_category == INSPECTOR_VARIABLES) {
      ans = wxT(":lisp (inspector-get-vars");
      for (int i = 0; i < numsel; i++) {
        ans << wxT(" '$") << m_lbStrings[selections[i]];
      }
      ans << wxT(")");
    }
    else if (m_category == INSPECTOR_FUNCTIONS) {
      ans = wxT(":lisp (inspector-get-funs");
      for (int i = 0; i < numsel; i++) {
        wxString funname = m_lbStrings[selections[i]];
        ans << wxT(" '$") << funname.SubString(0,funname.Find(wxT("("))-1);
      }
      ans << wxT(")");
    }
    return ans;
  }

  return wxEmptyString;
}

// Parses the string between <insp> </insp> tags in
// Maxima's output.
void Inspector::ParseMaximaResult(wxString result)
{
  if (result.Left(9) == wxT("<varlist>"))
  {
    result = result.SubString(9,
          result.Find(wxT("</varlist>")) - 1);

    wxArrayString list;
    wxStringTokenizer tokens(result, wxT(";"));
    while (tokens.HasMoreTokens()) {
      wxString token = tokens.GetNextToken();
      if (token.Length())
        list.Add(token);
    }
    m_wantListUpdate = false;
    SetValues(list);
  }
  else if (result.Left(9) == wxT("<funlist>"))
  {
    result = result.SubString(9,
          result.Find(wxT("</funlist>")) - 1);

    wxArrayString list;
    wxStringTokenizer tokens(result, wxT(";"));
    while (tokens.HasMoreTokens()) {
      wxString token = tokens.GetNextToken();
      if (token.Length())
        list.Add(token);
    }
    m_wantListUpdate = false;
    SetValues(list);
  }
  else if (result.Left(6) == wxT("<vars>"))
  {
    result = result.SubString(6,
          result.Find(wxT("</vars>")) - 1);
    if (result.Length() > 0)
    {
      MathParser mp;
      MathCell* res = mp.ParseLine(wxT("<mth>") + result + wxT("</mth>"), MC_TYPE_DEFAULT);
      m_minimathctrl->SetTree(res);
    }
  }
  else if (result.Left(6) == wxT("<funs>"))
  {


  }

}

void Inspector::OnCombo(wxCommandEvent &ev)
{
  wxString newcombo = m_combo->GetValue();
  if (newcombo == _("Variables")) {
    if (m_category == INSPECTOR_VARIABLES)
      return;
    else {
      m_category = INSPECTOR_VARIABLES;
      wxArrayString empty;
      SetValues(empty);
      m_wantListUpdate = true;
      m_minimathctrl->ClearWindow();

      wxCommandEvent ev(wxEVT_COMMAND_LISTBOX_SELECTED, inspector_listbox_id);
      (wxTheApp->GetTopWindow())->ProcessEvent(ev);
    }
  }
  else if (newcombo == _("Functions")) {
    if (m_category == INSPECTOR_FUNCTIONS)
      return;
    else {
      m_category = INSPECTOR_FUNCTIONS;
      wxArrayString empty;
      SetValues(empty);
      m_wantListUpdate = true;
      m_minimathctrl->ClearWindow();

      wxCommandEvent ev(wxEVT_COMMAND_LISTBOX_SELECTED, inspector_listbox_id);
      (wxTheApp->GetTopWindow())->ProcessEvent(ev);
    }
  }
}

BEGIN_EVENT_TABLE(Inspector, wxPanel)
  EVT_COMBOBOX(inspector_combo_id, Inspector::OnCombo)
END_EVENT_TABLE()
