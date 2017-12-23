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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*!\file
  This file defines the contents of the class XmlInspector 

  XmlInspector is a sidebar allows to view the communication between maxima 
  and wxMaxima.
 */

#include "XmlInspector.h"

#include <wx/sizer.h>
#include <wx/regex.h>

XmlInspector::XmlInspector(wxWindow *parent, int id) : wxRichTextCtrl(parent, id, wxEmptyString, wxDefaultPosition,
                                                                  wxDefaultSize, wxTE_READONLY | wxTE_RICH | wxHSCROLL |
                                                                                 wxTE_MULTILINE)
{
  BeginSuppressUndo();
  Clear();
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

void XmlInspector::Update()
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
  }
  if(m_fromMaxima != wxEmptyString)
  {
    if(m_state != toMaxima)
    {
      if(GetValue() != wxEmptyString)
      {
        Newline();Newline();
      }
      BeginTextColour(wxColour(0,0,0));
      WriteText(_("SENT TO MAXIMA:"));
      Newline();Newline();
      EndTextColour();
      m_state = toMaxima;
    }
    else
      WriteText("\n");
    
    BeginTextColour(wxColour(128,0,0));
    WriteText(m_fromMaxima);
    EndTextColour();
    m_fromMaxima = wxEmptyString;
  }

  if(m_toMaxima != wxEmptyString)
  {
    if(m_state != fromMaxima)
    {
      if(GetValue() != wxEmptyString)
      {
        Newline();Newline();
      }
      BeginTextColour(wxColour(0,0,0));
      WriteText(_("MAXIMA RESPONSE:"));
      Newline();Newline();    
      EndTextColour();
      m_state = fromMaxima;
    }
    size_t index = 0;
    m_toMaxima.Replace(wxT("$FUNCTION:"), wxT("\n$FUNCTION:"));
    while (index < m_toMaxima.Length())
    {
      wxChar ch = m_toMaxima[index];
    
      // Assume that all tags add indentation
      if (ch == wxT('>'))
      {
        m_indentLevel++;
        if (m_toMaxima.Left(index + 1).EndsWith(wxT("</wxxml-symbols>")))
          m_indentLevel = 0;
      }
    
      // A closing tag needs to remove the indentation of the opening tag 
      // plus the indentation of the closing tag
      if ((m_lastChar == wxT('<')) && (ch == wxT('/')))
        m_indentLevel -= 2;
    
      // Self-closing Tags remove their own indentation
      if ((m_lastChar == wxT('/')) && (ch == wxT('>')))
        m_indentLevel -= 1;
    
      // Add a linebreak and indent if we are at the space between 2 tags
      if ((m_lastChar == wxT('>')) && (ch == wxT('<')))
      {
        m_toMaxima = m_toMaxima.Left(index) + wxT ("\n") + IndentString(m_indentLevel) + m_toMaxima.Right(m_toMaxima.Length() - index);
        index += 1 + m_indentLevel;
      }
    
      index++;
      m_lastChar = ch;
    }
    BeginTextColour(wxColour(0,128,0));
    WriteText(m_toMaxima);
    EndTextColour();
    m_toMaxima = wxEmptyString;
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
  m_fromMaxima += text;
  m_updateNeeded = true;
}

void XmlInspector::Add_FromMaxima(wxString text)
{
  m_toMaxima += text;
  m_updateNeeded = true;
}
