// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//            (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file defines the contents of the class XmlInspector 

  XmlInspector is a sidebar allows to view the communication between maxima 
  and wxMaxima.
 */

#include "XmlInspector.h"

#include <wx/sizer.h>
#include <wx/regex.h>

XmlInspector::XmlInspector(wxWindow *parent, int id) : wxRichTextCtrl(parent, id,
                                                                      wxEmptyString,
                                                                      wxDefaultPosition,
                                                                      wxSize(wxSystemSettings::GetMetric ( wxSYS_SCREEN_X )/10,
                    wxSystemSettings::GetMetric ( wxSYS_SCREEN_Y )/10),
                                                                      wxTE_READONLY |
                                                                      wxTE_RICH |
                                                                      wxHSCROLL |
                                                                      wxTE_MULTILINE)
{
  m_state = clear;
  m_indentLevel = 0;
  BeginSuppressUndo();
  XmlInspector::Clear();
  wxRichTextCtrl::Clear();
}

XmlInspector::~XmlInspector()
{
}

void XmlInspector::Clear()
{
  m_clear = true;
  m_fromMaxima = wxEmptyString;
  m_toMaxima = wxEmptyString;
  m_updateNeeded = true;
}

void XmlInspector::UpdateContents()
{
  if(!m_updateNeeded)
    return;
  m_updateNeeded = false;
  
  if(m_clear)
  {
    wxRichTextCtrl::Clear();
    m_lastChar = wxChar(0);
    m_indentLevel = 0;
    m_state = clear;
    m_clear = false;
  }
  
  // Display all data we have sent to Maxima
  if(m_toMaxima != wxEmptyString)
  {
    if(m_state != toMaxima)
    {
      if(GetValue() != wxEmptyString)
      {
        Newline();Newline();
      }
      SetInsertionPointEnd();
      BeginTextColour(wxColour(0,0,0));
      WriteText(_("SENT TO MAXIMA:"));
      Newline();Newline();
      EndTextColour();
      m_state = toMaxima;
    }
    else
    {
      SetInsertionPointEnd();
      WriteText("\n");
    }
    
    SetInsertionPointEnd();
    BeginTextColour(wxColour(128,0,0));
    WriteText(m_toMaxima);
    EndTextColour();
    m_toMaxima = wxEmptyString;
  }

  // Display all data from Maxima
  if(m_fromMaxima != wxEmptyString)
  {
    if(m_state != fromMaxima)
    {
      if(GetValue() != wxEmptyString)
      {
        Newline();Newline();
      }
      SetInsertionPointEnd();
      BeginTextColour(wxColour(0,0,0));
      WriteText(_("MAXIMA RESPONSE:"));
      Newline();Newline();    
      EndTextColour();
      m_state = fromMaxima;
    }
    m_fromMaxima.Replace(wxT("$FUNCTION:"), wxT("\n$FUNCTION:"));

    // Indent the XML
    wxString textWithIndention;
    long length = 0;
    for (wxString::const_iterator it = m_fromMaxima.begin(); it!=m_fromMaxima.end(); ++it)
    {
      length++;
      // Assume that all tags add indentation
      if (*it == wxT('>'))
        m_indentLevel++;
      
      // A closing tag needs to remove the indentation of the opening tag 
      // plus the indentation of the closing tag
      if ((m_lastChar == wxT('<')) && (*it == wxT('/')))
        m_indentLevel -= 2;
    
      // Self-closing Tags remove their own indentation
      if ((m_lastChar == wxT('/')) && (*it == wxT('>')))
        m_indentLevel -= 1;
    
      // Add a linebreak and indent if we are at the space between 2 tags
      if ((m_lastChar == wxT('>')) && (*it == wxT('<')))
        textWithIndention += wxT ("\n") + IndentString(m_indentLevel);

      textWithIndention += *it;
      m_lastChar = *it;
    }
    SetInsertionPointEnd();
    BeginTextColour(wxColour(0,128,0));
    WriteText(textWithIndention);
    EndTextColour();
    m_fromMaxima = wxEmptyString;

    // Adding many chars to the XML inspector is slow => Warn our users if we do so.
    if(length > 100000)
      wxLogMessage(wxString::Format(_("Added much text (%li chars) to the XML inspector."), length));
  }
}

wxString XmlInspector::IndentString(int level)
{
  wxString result;
  for (int i = 0; i <= level; i++)
    result += wxT(" ");
  return result;
}

void XmlInspector::Add_ToMaxima(wxString text)
{
  if(m_toMaxima != wxEmptyString)
  {
    m_toMaxima += wxT("\n\u2015\n");
    if(m_fromMaxima != wxEmptyString)
      m_fromMaxima += wxT("\n\u2015\n");
  }
  m_toMaxima += text;
  m_updateNeeded = true;
}

void XmlInspector::Add_FromMaxima(wxString text)
{
  m_fromMaxima += text;
  m_updateNeeded = true;
}
