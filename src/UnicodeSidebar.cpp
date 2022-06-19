// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class UnicodeSidebar

  This file contains the definition of the class Unicodesidebar that allows to 
  select arbitrary unicode symbols.
 */


#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/wupdlock.h>
#include "../data/UnicodeData.h"
#include <memory>

#include "ErrorRedirector.h"
#include "UnicodeSidebar.h"
wxDEFINE_EVENT(SIDEBARKEYEVENT, SidebarKeyEvent);
wxDEFINE_EVENT(SYMBOLADDEVENT, SymboladdEvent);

UnicodeSidebar::UnicodeSidebar(wxWindow *parent, wxWindow *worksheet, Configuration *cfg) :
  wxPanel(parent),
  m_worksheet(worksheet)
{
  wxWindowUpdateLocker speedUp(this);
  wxBoxSizer *box = new wxBoxSizer(wxVERTICAL);
  m_initialized = false;
  m_regex = new RegexCtrl(this, wxID_ANY, cfg);
  m_regex->Connect(REGEX_EVENT, wxCommandEventHandler(UnicodeSidebar::OnRegExEvent), NULL, this);
  m_grid = new wxGrid(this, wxID_ANY);
  m_grid->CreateGrid(0,3);
  m_grid->BeginBatch();
  box->Add(m_regex, wxSizerFlags().Expand().Proportion(10));
  box->Add(m_grid, wxSizerFlags().Expand().Proportion(100));
  Connect(wxEVT_PAINT, wxPaintEventHandler(UnicodeSidebar::OnPaint), NULL, this);
  Connect(wxEVT_SIZE, wxSizeEventHandler(UnicodeSidebar::OnSize), NULL, this);
  m_grid->Connect(wxEVT_GRID_CELL_LEFT_DCLICK, wxGridEventHandler(UnicodeSidebar::OnDClick), NULL, this);
  m_grid->Connect(wxEVT_GRID_CELL_RIGHT_CLICK, wxGridEventHandler(UnicodeSidebar::OnRightClick), NULL, this);
  m_grid->Connect(wxEVT_GRID_CELL_CHANGING, wxGridEventHandler(UnicodeSidebar::OnChangeAttempt), NULL, this);
  m_grid->EndBatch();
  SetSizer(box);
  FitInside();
}

UnicodeSidebar::~UnicodeSidebar()
{
}

void UnicodeSidebar::OnDClick(wxGridEvent &event)
{
  wxString number;
  number = m_grid->GetCellValue(event.GetRow() ,0);
  long numVal;
  if(number.ToLong(&numVal, 16))
  {
    wxCommandEvent *ev = new wxCommandEvent(SIDEBARKEYEVENT, numVal);
    m_worksheet->GetEventHandler()->QueueEvent(ev);
  }
  m_worksheet->SetFocus();
}

void UnicodeSidebar::OnRightClick(wxGridEvent &event)
{
  wxString number;
  number = m_grid->GetCellValue(event.GetRow() ,0);
  if(number.ToLong(&m_charRightClickedOn, 16))
  {
    std::unique_ptr<wxMenu> popupMenu(new wxMenu());
    popupMenu->Append(popid_addToSymbols, _("Add to symbols Sidebar"));
    Connect(popid_addToSymbols, wxEVT_MENU,
            wxCommandEventHandler(UnicodeSidebar::OnMenu), NULL, this);
    PopupMenu(&*popupMenu);
  }
}

void UnicodeSidebar::OnMenu(wxCommandEvent &event)
{
  switch (event.GetId())
  {
  case popid_addToSymbols:
    wxWindow *toplevel = this;
    while(toplevel->GetParent() != NULL)
      toplevel = toplevel->GetParent();
    wxCommandEvent *ev = new wxCommandEvent(SYMBOLADDEVENT, m_charRightClickedOn);
    toplevel->GetEventHandler()->QueueEvent(ev);

    break;
  }
}

void UnicodeSidebar::OnChangeAttempt(wxGridEvent &event)
{
  event.Veto();
}

void UnicodeSidebar::UpdateDisplay()
{
  wxGridUpdateLocker speedUp(m_grid);
  int rows = m_grid->GetNumberRows() - 1;
  for(int i = 0; i<rows; i++)
  {
    wxString name = m_grid->GetCellValue(i,2).Lower();
    if(m_regex->Matches(name))
      m_grid->ShowRow(i);
    else
      m_grid->HideRow(i);
  }
}

void UnicodeSidebar::OnSize(wxSizeEvent &event)
{  
  int width = GetClientSize().x - m_grid->GetColSize(0) -
    m_grid->GetColSize(1) - 10;
  if(width < 0)
    width = 0;
  m_grid->SetColSize(2, width);
  event.Skip();
}

void UnicodeSidebar::OnPaint(wxPaintEvent &event)
{
  event.Skip();
  if(m_initialized)
    return;

  wxMemoryInputStream istream(UnicodeData_txt_gz, UnicodeData_txt_gz_len);
  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  wxString regex_string = m_regex->GetValue();
  wxRegEx regex(m_regex->GetValue());
  m_grid->HideRowLabels();
  m_grid->HideColLabels();
  while(!zstream.Eof())
  {
    wxString line = textIn.ReadLine();
    wxStringTokenizer items(line, wxT(";"), wxTOKEN_RET_EMPTY_ALL);
    wxString number = items.GetNextToken();
    wxString name   = items.GetNextToken();
    if(!name.IsEmpty() && (name != "<control>")&& (!name.StartsWith("<Plane")) &&
       (regex_string.IsEmpty())
      )
    {
      long numVal;
      if(number.ToLong(&numVal, 16))
      {
        m_grid->AppendRows();
        int row = m_grid->GetNumberRows() - 1;
        m_grid->SetCellValue(row,0,number);
        m_grid->SetCellValue(row,1,wxString(wxChar(numVal)));
        m_grid->SetCellValue(row,2,name);
        m_grid->EnableEditing(false);
      }
    }
  }  
  m_initialized = true;
}

void UnicodeSidebar::OnRegExEvent(wxCommandEvent &WXUNUSED(ev))
{
  UpdateDisplay();
}
