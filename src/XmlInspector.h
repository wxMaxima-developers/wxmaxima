// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

  This file contains the definition of the class XmlInspector that handles the
  table of contents pane.
*/
#include "precomp.h"
#include <wx/wx.h>
#include <wx/richtext/richtextctrl.h>
#include <vector>

#ifndef XMLINSPECTOR_H
#define XMLINSPECTOR_H

/*! This class generates a pane displaying the communication between maxima and wxMaxima.

  The display of this data is only actually updated on calling XmlInspector::Update().
*/
class XmlInspector : public wxRichTextCtrl
{
public:
    XmlInspector(wxWindow *parent, int id);

    /*! The destructor
     */
    ~XmlInspector();

    //! Remove all text from the editor.
    virtual void Clear();

    //! Add some text we sent to maxima.
    void Add_ToMaxima(wxString text);
    //! Add some text we have received from maxima.
    void Add_FromMaxima(wxString text);
    //! Actually draw the updates
    void UpdateContents();
    //! Do we need to update the XmlInspector's display?
    bool UpdateNeeded(){return m_updateNeeded;}
private:
    bool m_updateNeeded;
    wxString m_fromMaxima;
    wxString m_toMaxima;
    bool m_clear;
    enum xmlInspectorIDs
    {
        XmlInspector_ctrl_id = 4,
        XmlInspector_regex_id
    };
    enum monitorState
    {
        clear,
        fromMaxima,
        toMaxima
    };
    monitorState m_state;

    wxChar m_lastChar = '\0';
    int m_indentLevel;

    wxString IndentString(int level);
};

#endif // XMLINSPECTOR_H
