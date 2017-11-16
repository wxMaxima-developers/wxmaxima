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

#ifndef LISTSORTWIZ_H
#define LISTSORTWIZ_H

#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/statline.h>
#include <wx/radiobut.h>
#include <wx/textctrl.h>

#include "BTextCtrl.h"

class ListSortWiz : public wxDialog
{
public:
  ListSortWiz(
    Configuration *cfg,
    wxWindow *parent, int id, const wxString &title,
    wxString list,
    bool eq = false,
    const wxPoint &pos = wxDefaultPosition,
    const wxSize &size = wxDefaultSize,
    long style = wxDEFAULT_DIALOG_STYLE);
  wxString GetValue();

private:
  void set_properties();

protected:
  wxChoice *m_choice;
  wxRadioButton *m_sortTraditional;
  wxRadioButton *m_sortFunction;
  wxRadioButton *m_sortLambda;
  wxTextCtrl *m_list;
  wxTextCtrl *m_CriterionFunc;
  wxTextCtrl *m_Criterion;
  wxButton *button_1;
  wxButton *button_2;
  void OnFunctionChange(wxGridEvent &unused){m_sortFunction->SetValue(true);}
  void OnLambdaChange(wxGridEvent &unused){m_sortLambda->SetValue(true);}
};

#endif // LISTSORTWIZ_H
