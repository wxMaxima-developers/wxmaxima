// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class FindReplacePane

  This dialog represents the insides of the "find" and "replace" sidebar/dialog.
*/

#ifndef FINDREPLACEPANE_H
#define FINDREPLACEPANE_H

#include "precomp.h"
#include <wx/fdrepdlg.h>
#include <wx/event.h>
#include <wx/panel.h>
#include <wx/radiobut.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>

/*! The find+replace pane
 */
class FindReplacePane : public wxPanel
{
public:
  class FindReplaceData: public wxFindReplaceData
  {
  public:
    FindReplaceData();
    bool GetRegexSearch() const {return m_regexSearch;}
    void SetRegexSearch(bool regexSearch) {m_regexSearch = regexSearch;}
  private:
    bool m_regexSearch;
  };

private:
  //! The storage the search strings and settings are kept in
  FindReplaceData *m_findReplaceData;
  //! Is this pane currently in focus?
  bool m_active;
  wxTextCtrl *m_searchText;
  wxTextCtrl *m_replaceText;
  wxButton *m_searchButton;
  wxButton *m_replaceButton;
  wxButton *m_replaceAllButton;
  wxRadioButton *m_forward;
  wxRadioButton *m_backwards;
  wxRadioButton *m_regexSearch;
  wxRadioButton *m_simpleSearch;
  wxCheckBox *m_matchCase;
  //! true means: The next Activation event is generated during construction
  bool m_activateDuringConstruction;
public:
  FindReplacePane(wxWindow *parent, FindReplaceData *data);

  bool GetRegexSearch() const {return m_findReplaceData->GetRegexSearch();}
  
  wxString GetFindString() const
    { return m_findReplaceData->GetFindString(); }

  void SetFindString(wxString strng);

  wxFindReplaceData *GetData() const
    { return m_findReplaceData; }

protected:
  void OnActivate(wxActivateEvent &event);

  void OnSearch(wxCommandEvent &event);

  void OnReplace(wxCommandEvent &event);

  void OnReplaceAll(wxCommandEvent &event);

  void OnReplaceStringChange(wxCommandEvent &event);

  void OnFindStringChange(wxCommandEvent &event);

  void OnDirectionChange(wxCommandEvent &event);

  void OnRegexSimpleChange(wxCommandEvent &event);

  void OnMatchCase(wxCommandEvent &event);

  void OnKeyDown(wxKeyEvent &event);
};

#endif // FINDREPLACEPANE_H
