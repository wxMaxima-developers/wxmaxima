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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef VARIABLESPANE_H
#define VARIABLESPANE_H

#include "precomp.h"
#include "EventIDs.h"
#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/panel.h>
#include <wx/arrstr.h>
#include <unordered_map>
#include <vector>

/*! \file
  The file that contains the "variables" sidepane

  This file contains the class Variablespane.
*/

/*! A "variables" sidepane

 */
class Variablespane : public wxPanel
{
public:
    wxWindowIDRef m_varID_values;
    wxWindowIDRef m_varID_functions;
    wxWindowIDRef m_varID_arrays;
    wxWindowIDRef m_varID_macros;
    wxWindowIDRef m_varID_labels;
    wxWindowIDRef m_varID_myoptions;
    wxWindowIDRef m_varID_rules;
    wxWindowIDRef m_varID_aliases;
    wxWindowIDRef m_varID_structs;
    wxWindowIDRef m_varID_dependencies;
    wxWindowIDRef m_varID_gradefs;
    wxWindowIDRef m_varID_prop;
    wxWindowIDRef m_varID_let_rule_packages;
    wxWindowIDRef m_varID_delete_row;
    wxWindowIDRef m_varID_clear;
    //! The constructor
    explicit Variablespane(wxWindow *parent, wxWindowID id = wxID_ANY);
    //! Called when a variable name was changed
    void OnTextChange(wxGridEvent &event);
    //! Called after the user has entered a variable name but before it is committed
    void OnTextChanging(wxGridEvent &event);
    //! Called on right-clicking the variables list
    void OnRightClick(wxGridEvent &event);
    //! Called if a right-click menu item was clicked at
    void InsertMenu(wxCommandEvent &event);
    //! Called on key press
    void OnKey(wxKeyEvent &event);
    //! Called if a printable char was entered
    void OnChar(wxKeyEvent &event);
    //! Add a variable whose name contains all the escapes maxima needs to the variables list
    void AddWatchCode(wxString code);
    //! Add a variable without escapes to the list.
    void AddWatch(wxString watch);
    //! Is this string a valid variable name?
    bool IsValidVariable(wxString var);
    //! Returns a list of all variable names in a format maxima understands
    std::vector<wxString> GetEscapedVarnames();
    //! Returns the variable list in a human-readable format
    std::vector<wxString> GetVarnames();
    //! Set all variable's contents to "unknown".
    void ResetValues();
    //! Remove all entries from the variables list
    void Clear();
    //! Convert a human-readable variable name to one maxima understands
    wxString EscapeVarname(wxString var);
    //! Convert a variable name maxima understands to human-readable
    wxString UnescapeVarname(wxString var);
    //! Tell the variables pane about a variable value
    void VariableValue(wxString var, wxString val);
    //! Sets the variable var to "undefined"
    void VariableUndefined(wxString var);
    void UpdateSize();
    //! The destructor
    ~Variablespane();
private:
    // The spreadsheet with the variable names
    wxGrid *m_grid;
    bool m_updateSizeNeeded = false;
    wxString InvertCase(wxString var);
#if wxCHECK_VERSION(3, 3, 0) || wxUSE_STL
    typedef std::unordered_map <wxString, int> IntHash;
#else
    WX_DECLARE_STRING_HASH_MAP(int, IntHash);
#endif

    //! A list of all symbols that can be entered using Esc-Codes
    IntHash m_vars;
    //! The row that was right-clicked at
    int m_rightClickRow;
    //! Compares two integers.
    static int CompareInt(int *int1, int *int2){return (*int1 < *int2);}
};

#endif // VARIABLESPANE_H
