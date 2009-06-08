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
  wxGridSizer *buttonbox = new wxGridSizer(1, 4, 0,0);

  // toggle buttons
  m_tVars = new wxToggleButton(this, inspector_vars_id, wxT("x"), wxDefaultPosition, wxSize(30,20));
  m_tFuns = new wxToggleButton(this, inspector_funs_id, wxT("f(x)"), wxDefaultPosition, wxSize(30,20));
  m_tLabs = new wxToggleButton(this, inspector_labs_id, wxT("\%i1"), wxDefaultPosition, wxSize(30,20));
  m_tOpts = new wxToggleButton(this, inspector_labs_id, wxT("Opt"), wxDefaultPosition, wxSize(30,20));

  m_tVars->SetValue(true); // default - show variables
  m_tFuns->SetValue(true); // default - show functions

  m_tVars->SetToolTip(_("Variables"));
  m_tFuns->SetToolTip(_("Functions and macros"));
  m_tLabs->SetToolTip(_("Labels"));
  m_tOpts->SetToolTip(_("Options"));

  buttonbox->Add(m_tVars,0,wxEXPAND,0);
  buttonbox->Add(m_tFuns,0,wxEXPAND,0);
  buttonbox->Add(m_tLabs,0,wxEXPAND,0);
  buttonbox->Add(m_tOpts,0,wxEXPAND,0);
  // left side
  vboxleft->Add(buttonbox, 0, wxEXPAND | wxTOP | wxLEFT | wxRIGHT, 5);
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

void Inspector::SetList(wxArrayString arrstr)
{
  // make sure the same items are selected!
  wxArrayInt selections;
  wxArrayString selnames;
  bool update = false;
  int numsel = m_listbox->GetSelections(selections);
  if (numsel > 0)
    for (int i = 0; i < numsel; i++) {
      selnames.Add(m_lbStrings[selections[i]]);
    }

  m_lbStrings = arrstr;
  m_listbox->Set(arrstr);

  if (numsel > 0)
    for (int i = 0; i < numsel; i++) {
      update |= m_listbox->SetStringSelection(selnames[i]);
  }

  m_listbox->Enable();

  if (update) {
    m_wantListUpdate = false;
    wxCommandEvent mev(wxEVT_COMMAND_BUTTON_CLICKED, inspector_update_id);
    (wxTheApp->GetTopWindow())->ProcessEvent(mev);
  }
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
    if ((m_tVars->GetValue()) ||
        (m_tFuns->GetValue()) ||
        (m_tLabs->GetValue())) {

      wxString ans = wxT(":lisp (inspector-list '(");
      if (m_tVars->GetValue())
        ans << wxT(" vars");
      if (m_tFuns->GetValue())
        ans << wxT(" funs");
      if (m_tLabs->GetValue())
        ans << wxT(" labs");
      if (m_tOpts->GetValue())
        ans << wxT(" opts");

      ans << wxT("));");

      return ans;
    }
    else
      return wxEmptyString;
  }
  else { // we want to update minimathctrl
    wxString ans = wxEmptyString;
    wxArrayInt selections;
    int numsel = m_listbox->GetSelections(selections);
    if (numsel < 1)
      return wxEmptyString;
    selections.Sort(&sortingfunc);

    ans = wxT(":lisp (inspector-get");

    for (int i = 0; i < numsel; i++) {
      wxString name = m_lbStrings[selections[i]];
      int bracket = name.Find(wxT("("));
      if (bracket > -1) { // we have a function
        name = name.SubString(0,bracket-1);
        ans << wxT(" '(") << LispSymbolString(name) << wxT(")");
      }
      else // we have a variable
        ans << wxT(" '") << LispSymbolString(name);
    }

    ans << wxT(");");

    return ans;
  }

  return wxEmptyString;
}

// Parses the string between <insp> </insp> tags in
// Maxima's output.
void Inspector::ParseMaximaResult(wxString result)
{
  if (result.Left(6) == wxT("<list>"))
  {
    result = result.SubString(6,
          result.Find(wxT("</list>")) - 1);

    wxArrayString list;
    wxStringTokenizer tokens(result, wxT(";"));
    while (tokens.HasMoreTokens()) {
      wxString token = tokens.GetNextToken();
      if (token.Length())
        list.Add(token);
    }
    m_wantListUpdate = false;
    SetList(list);
  }
  else if (result.Left(8) == wxT("<values>"))
  {
    result = result.SubString(8,
          result.Find(wxT("</values>")) - 1);
    if (result.Length() > 0)
    {
      MathParser mp;
      MathCell* res = mp.ParseLine(wxT("<mth>") + result + wxT("</mth>"), MC_TYPE_DEFAULT);
      m_minimathctrl->SetTree(res);
    }
  }

}

// handle toggle buttons
void Inspector::OnToggleButton(wxCommandEvent &ev)
{
  OutdateList();
  /*
  m_listbox->Disable();
  m_minimathctrl->ClearWindow();
  m_wantListUpdate = true;

  // send an event to wxmaxima
  wxCommandEvent mev(wxEVT_COMMAND_BUTTON_CLICKED, inspector_update_id);
  (wxTheApp->GetTopWindow())->ProcessEvent(mev);
  */
}

void Inspector::OnListBox(wxCommandEvent &ev)
{
  if (!ev.IsSelection())
    return;

  m_wantListUpdate = false;
  wxCommandEvent mev(wxEVT_COMMAND_BUTTON_CLICKED, inspector_update_id);
  (wxTheApp->GetTopWindow())->ProcessEvent(mev);
}

// this is called from wxMaxima, when evaluation queue is emptied
void Inspector::OutdateList()
{
  m_listbox->Disable();
  m_minimathctrl->ClearWindow();
  m_wantListUpdate = true;

  // send an event to wxmaxima
  wxCommandEvent ev(wxEVT_COMMAND_BUTTON_CLICKED, inspector_update_id);
  (wxTheApp->GetTopWindow())->ProcessEvent(ev);
}

// convert maxima string like "abc"
// into " '$abc"
wxString Inspector::LispSymbolString(wxString maximastring)
{
  // we have to obey strange lisp conventions
  // pass: 'abc' as $abc
  // 'ABC' as |$abc| and 'AbC' as |$AbC|
  wxString ans = wxEmptyString;
  if (maximastring == maximastring.Lower())
    ans << wxT("$") << maximastring;
  else if (maximastring == maximastring.Upper())
    ans << wxT("|$") << maximastring.Lower() << wxT("|");
  else
    ans << wxT("|$") << maximastring << wxT("|");
  return ans;
}

BEGIN_EVENT_TABLE(Inspector, wxPanel)
  EVT_LISTBOX(inspector_listbox_id, Inspector::OnListBox)
  EVT_TOGGLEBUTTON(inspector_vars_id, Inspector::OnToggleButton)
  EVT_TOGGLEBUTTON(inspector_funs_id, Inspector::OnToggleButton)
  EVT_TOGGLEBUTTON(inspector_labs_id, Inspector::OnToggleButton)
  EVT_TOGGLEBUTTON(inspector_opts_id, Inspector::OnToggleButton)
END_EVENT_TABLE()
