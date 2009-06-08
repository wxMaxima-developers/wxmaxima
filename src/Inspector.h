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

#ifndef INSPECTOR_H
#define INSPECTOR_H

#include <wx/wx.h>
#include <wx/tglbtn.h>
#include <wx/splitter.h>

#include "MiniMathCtrl.h"

enum {
  inspector_vars_id, //togglebuttons id-s
  inspector_funs_id,
  inspector_labs_id,
  inspector_opts_id,

  inspector_update_id,
  inspector_listbox_id,
  inspector_combo_id
};

class Inspector : public wxPanel
{
public:
  Inspector(wxWindow* parent, int id);
  ~Inspector();
  void SetList(wxArrayString arrstr);
  wxString GetMaximaCommand();
  void ParseMaximaResult(wxString result);
  void OutdateList();
private:
  wxString LispSymbolString(wxString maximastring);
  void OnToggleButton(wxCommandEvent &ev);
  void OnListBox(wxCommandEvent &ev);
  //wxSplitterWindow *m_splitter;

  // Togglebuttons
  wxToggleButton *m_tVars;
  wxToggleButton *m_tFuns;
  wxToggleButton *m_tLabs;
  wxToggleButton *m_tOpts;

  wxListBox *m_listbox;
  MiniMathCtrl *m_minimathctrl;

  wxArrayString m_lbStrings;
  bool m_wantListUpdate;

  DECLARE_EVENT_TABLE()
};

#endif // INSPECTOR_H
