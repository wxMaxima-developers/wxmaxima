// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2011-2011 cw.ahbong <cwahbong@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class SymbolsSidebar

  SymbolsSidebar shows some miscellaneous unicode symbols the user might find useful.
*/
#include "SymbolsSidebar.h"
#include <wx/windowptr.h>
#include "wizards/Gen1Wiz.h"

void SymbolsSidebar::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}

SymbolsSidebar::SymbolsSidebar(wxWindow *parent,
                                        Configuration *configuration,
                                        wxWindow *worksheet, int ID)
  : wxScrolled<wxPanel>(parent, ID), m_configuration(configuration),
    m_worksheet(worksheet) {
  ShowScrollbars(wxSHOW_SB_NEVER, wxSHOW_SB_DEFAULT);
  EnableScrolling(false, true);
  SetScrollRate(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor());
  const CharButton::Definition symbolButtonDefinitions[] = {
    {L'\u00BD', _("1/2"), true},
    {L'\u00B2', _("to the power of 2"), true},
    {L'\u00B3', _("to the power of 3"), true},
    {L'\u221A',
     _("sqrt (needs parenthesis for its argument to work as a Maxima "
       "command)"),
     true},
    {L'\u2148'},
    {L'\u2147'},
    {L'\u210F'},
    {L'\u2208', _("in")},
    {L'\u2203', _("exists")},
    {L'\u2204', _("there is no")},
    {L'\u21D2', _("\"implies\" symbol"), true},
    {L'\u221E', _("Infinity"), true},
    {L'\u2205', _("empty")},
    {L'\u25b6'},
    {L'\u25b8'},
    {L'\u22C0', _("and"), true},
    {L'\u22C1', _("or"), true},
    {L'\u22BB', _("xor"), true},
    {L'\u22BC', _("nand"), true},
    {L'\u22BD', _("nor"), true},
    {L'\u21D4', _("equivalent"), true},
    {L'\u00b1', _("plus or minus")},
    {L'\u00AC', _("not"), true},
    {L'\u22C3', _("union")},
    {L'\u22C2', _("intersection")},
    {L'\u2286', _("subset or equal")},
    {L'\u2282', _("subset")},
    {L'\u2288', _("not subset or equal")},
    {L'\u2284', _("not subset")},
    {L'\u0127'},
    {L'\u0126'},
    {L'\u2202', _("partial sign")},
    {L'\u2207', _("nabla sign")},
    {L'\u222b', _("Integral sign")},
    {L'\u2245'},
    {L'\u221d', _("proportional to")},
    {L'\u2260', _("not bytewise identical"), true},
    {L'\u2264', _("less or equal"), true},
    {L'\u2265', _("greater than or equal"), true},
    {L'\u226A', _("much less than")},
    {L'\u226B', _("much greater than")},
    {L'\u2263', _("Identical to")},
    {L'\u2211', _("Sum sign")},
    {L'\u220F', _("Product sign")},
    {L'\u2225', _("Parallel to")},
    {L'\u27C2', _("Perpendicular to")},
    {L'\u219D', _("Leads to")},
    {L'\u2192', _("Right arrow")},
    {L'\u27F6', _("Long Right arrow")},
    {L'\u220e', _("End of proof")},
  };

  m_userSymbols = NULL;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  wxSizer *builtInSymbolsSizer = new Buttonwrapsizer(wxHORIZONTAL);
  wxPanel *builtInSymbols = new wxPanel(this);
  for (auto &def : symbolButtonDefinitions)
    {
      CharButton *button = new CharButton(builtInSymbols, m_worksheet, m_configuration, def);
      builtInSymbolsSizer->Add(button, wxSizerFlags().Expand());
      button->Connect(wxEVT_RIGHT_DOWN,
                      wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown), NULL, this);
      button->Connect(wxEVT_MENU,
                      wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL, this);
      button->GetTextObject()->Connect(wxEVT_RIGHT_DOWN,
                                       wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown),
                                       NULL, this);
      button->GetTextObject()->Connect(wxEVT_MENU,
                                       wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL, this);
    }
  
  builtInSymbols->SetSizer(builtInSymbolsSizer);
  vbox->Add(builtInSymbols, wxSizerFlags().Expand());

  m_userSymbols = new wxPanel(this);
  m_userSymbolsSizer = new Buttonwrapsizer(wxHORIZONTAL);
  AddUserSymbols();
  m_userSymbols->SetSizer(m_userSymbolsSizer);
  vbox->Add(m_userSymbols, wxSizerFlags().Expand());
  Connect(wxEVT_SIZE, wxSizeEventHandler(SymbolsSidebar::OnSize),
          NULL, this);
  Connect(wxEVT_MENU,
          wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL,
          this);
  GetTargetWindow()->Connect(wxEVT_MENU,
                             wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL,
                             this);
  Connect(wxEVT_RIGHT_DOWN,
          wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown));
  builtInSymbols->Connect(wxEVT_RIGHT_DOWN,
                          wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown));
  m_userSymbols->Connect(wxEVT_RIGHT_DOWN,
                         wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown));
  SetSizer(vbox);
  FitInside();
  SetMinSize(wxSize(GetContentScaleFactor() * 50, GetMinSize().y));
}

void SymbolsSidebar::OnMenu(wxCommandEvent &event) {
  std::unordered_map<int, std::function<void()>> m{
    {EventIDs::enable_unicodePane, [&](){
      wxWindow *mainWin = this;
      while (mainWin->GetParent() != NULL)
        mainWin = mainWin->GetParent();
      wxCommandEvent *ev = new wxCommandEvent(event);
      mainWin->GetEventHandler()->QueueEvent(ev);
    }},
    {EventIDs::menu_additionalSymbols, [&](){
      wxWindowPtr<Gen1Wiz>
        wiz(new
            Gen1Wiz(
                    this, -1, m_configuration, _("Non-builtin symbols"),
                                           _("Unicode symbols:"),
                    m_configuration->SymbolPaneAdditionalChars(),
                    _("Allows to specify which not-builtin unicode symbols should be "
                      "displayed in the symbols sidebar along with the built-in symbols.")));
      // wiz->Centre(wxBOTH);
      wiz->SetLabel1ToolTip(_("Drag-and-drop unicode symbols here"));
      wiz->ShowWindowModalThenDo([this, wiz](int retcode) {
        if (retcode == wxID_OK)
          m_configuration->SymbolPaneAdditionalChars(wiz->GetValue());
        UpdateUserSymbols();
      });
    }
    }};
  auto varFunc = m.find(event.GetId());
  if(varFunc == m.end())
    {
      wxASSERT(false);
    }
  else
    varFunc->second();
}

void SymbolsSidebar::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  popupMenu->Append(EventIDs::menu_additionalSymbols, _("Add more symbols"),
                    wxEmptyString, wxITEM_NORMAL);
  popupMenu->Append(EventIDs::enable_unicodePane, _("Show all unicode symbols"),
                    wxEmptyString, wxITEM_NORMAL);
  PopupMenu(&*popupMenu);
}

void SymbolsSidebar::UpdateUserSymbols() {
  if(m_userSymbols_Last == m_configuration->SymbolPaneAdditionalChars())
    return;
  
  if (m_userSymbols == NULL)
    return;

  wxLogNull blocker;

  // Clear the user symbols pane
  if(!m_userSymbols_Last.IsEmpty())
    m_userSymbols->DestroyChildren();

  AddUserSymbols();

  Layout();
}

void SymbolsSidebar::AddUserSymbols() {  
  if (m_userSymbols == NULL)
    return;

  wxLogNull blocker;
  
  // Populate the pane with a button per user symbol
  for (auto ch : m_configuration->SymbolPaneAdditionalChars()) {
    CharButton *button = new CharButton(
                                      m_userSymbols, m_worksheet, m_configuration,
                                      {ch, _("A symbol from the configuration dialogue")}, true);
    m_userSymbolButtons.push_back(button);
    button->Connect(wxEVT_RIGHT_DOWN,
                    wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown), NULL, this);
    button->Connect(wxEVT_MENU,
                    wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL, this);
    button->GetTextObject()->Connect(wxEVT_RIGHT_DOWN,
                                     wxMouseEventHandler(SymbolsSidebar::OnMouseRightDown),
                                     NULL, this);
    button->GetTextObject()->Connect(wxEVT_MENU,
                                     wxCommandEventHandler(SymbolsSidebar::OnMenu), NULL, this);
    m_userSymbolsSizer->Add(button, wxSizerFlags().Expand());
  }
  m_userSymbols_Last = m_configuration->SymbolPaneAdditionalChars();
}

