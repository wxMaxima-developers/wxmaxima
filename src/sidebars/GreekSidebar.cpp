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
  This file defines the class GreekSidebar

  GreekSidebar is a on-screen keyboard that allows non-greek users to type
  greek chars.
*/
#include "wxMaximaFrame.h"
#include "Dirstructure.h"
#include <string>
#include <memory>
#include "ButtonWrapSizer.h"

#include "GreekSidebar.h"

GreekSidebar::GreekSidebar(wxWindow *parent,
                                    Configuration *configuration,
                                    wxWindow *worksheet, int ID)
  : wxScrolled<wxPanel>(parent, ID), m_configuration(configuration),
    m_lowercaseSizer(new Buttonwrapsizer(wxHORIZONTAL)),
    m_uppercaseSizer(new Buttonwrapsizer(wxHORIZONTAL)), m_worksheet(worksheet) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  ShowScrollbars(wxSHOW_SB_NEVER, wxSHOW_SB_DEFAULT);
  EnableScrolling(false, true);
  SetScrollRate(5, 5);
  UpdateSymbols();

  vbox->Add(m_lowercaseSizer, wxSizerFlags().Expand());
  vbox->Add(m_uppercaseSizer, wxSizerFlags().Expand());

  Connect(wxEVT_SIZE, wxSizeEventHandler(GreekSidebar::OnSize),
          NULL, this);
  Connect(wxEVT_MENU,
          wxCommandEventHandler(GreekSidebar::OnMenu), NULL, this);
  Connect(wxEVT_RIGHT_DOWN,
          wxMouseEventHandler(GreekSidebar::OnMouseRightDown));

  SetSizer(vbox);
  FitInside();
  SetMinSize(wxSize(GetContentScaleFactor() * 50, GetMinSize().y));
}

void GreekSidebar::OnSize(wxSizeEvent &event) {
  // Shrink the width of the wxScrolled's virtual size if the wxScrolled is
  // shrinking
  SetVirtualSize(GetClientSize());
  event.Skip();
}

void GreekSidebar::OnMenu(wxCommandEvent &event) {
  std::unordered_map<int, std::function<void()>> m{
    {EventIDs::menu_showLatinGreekLookalikes, [&](){
      bool showLookalikes = !m_configuration->GreekSidebar_ShowLatinLookalikes();
      m_configuration->GreekSidebar_ShowLatinLookalikes(showLookalikes);
      UpdateSymbols();
      Layout();
    }},
    {EventIDs::menu_showGreekMu, [&](){
      bool show_mu = !m_configuration->GreekSidebar_Show_mu();
      m_configuration->GreekSidebar_Show_mu(show_mu);
      UpdateSymbols();
      Layout();
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

void GreekSidebar::UpdateSymbols() {
  //  wxWindowUpdateLocker drawBlocker(this);
  enum class Cond { None, Show_mu, ShowLatinLookalikes };
  struct EnabledDefinition : CharButton::Definition {
    Cond condition;
    EnabledDefinition(wchar_t sym, const wxString &descr,
                      Cond cond = Cond::None)
      : CharButton::Definition{sym, descr}, condition(cond) {}
    explicit EnabledDefinition(wchar_t sym)
      : EnabledDefinition(sym, wxm::emptyString) {}
  };

  static const EnabledDefinition lowerCaseDefs[] = {
    {L'\u03B1', _("alpha")},
    {L'\u03B2', _("beta")},
    {L'\u03B3', _("gamma")},
    {L'\u03B4', _("delta")},
    {L'\u03B5', _("epsilon")},
    {L'\u03B6', _("zeta")},
    {L'\u03B7', _("eta")},
    {L'\u03B8', _("theta")},
    {L'\u03B9', _("iota")},
    {L'\u03BA', _("kappa")},
    {L'\u03BB', _("lambda")},
    {L'\u03BC', _("mu"), Cond::Show_mu},
    {L'\u03BD', _("nu")},
    {L'\u03BE', _("xi")},
    {L'\u03BF', _("omicron"), Cond::ShowLatinLookalikes},
    {L'\u03C0', _("pi")},
    {L'\u03C1', _("rho")},
    {L'\u03C3', _("sigma")},
    {L'\u03C4', _("tau")},
    {L'\u03C5', _("upsilon")},
    {L'\u03C6', _("phi")},
    {L'\u03C7', _("chi")},
    {L'\u03C8', _("psi")},
    {L'\u03C9', _("omega")},
  };

  static const EnabledDefinition upperCaseDefs[] = {
    {L'\u0391', ("Alpha"), Cond::ShowLatinLookalikes},
    {L'\u0392', _("Beta"), Cond::ShowLatinLookalikes},
    {L'\u0393', _("Gamma")},
    {L'\u0394', _("Delta")},
    {L'\u0395', _("Epsilon"), Cond::ShowLatinLookalikes},
    {L'\u0396', _("Zeta"), Cond::ShowLatinLookalikes},
    {L'\u0397', _("Eta"), Cond::ShowLatinLookalikes},
    {L'\u0398', _("Theta")},
    {L'\u0399', _("Iota"), Cond::ShowLatinLookalikes},
    {L'\u039A', _("Kappa"), Cond::ShowLatinLookalikes},
    {L'\u039B', _("Lambda")},
    {L'\u039C', _("Mu"), Cond::ShowLatinLookalikes},
    {L'\u039D', _("Nu"), Cond::ShowLatinLookalikes},
    {L'\u039E', _("Xi")},
    {L'\u039F', _("Omicron"), Cond::ShowLatinLookalikes},
    {L'\u03A0', _("Pi")},
    {L'\u03A1', _("Rho"), Cond::ShowLatinLookalikes},
    {L'\u03A3', _("Sigma")},
    {L'\u03A4', _("Tau"), Cond::ShowLatinLookalikes},
    {L'\u03A5', _("Upsilon"), Cond::ShowLatinLookalikes},
    {L'\u03A6', _("Phi")},
    {L'\u03A7', _("Chi"), Cond::ShowLatinLookalikes},
    {L'\u03A8', _("Psi")},
    {L'\u03A9', _("Omega")},
  };

  bool const Show_mu = m_configuration->GreekSidebar_Show_mu();
  bool const ShowLatinLookalikes = m_configuration->GreekSidebar_ShowLatinLookalikes();

  m_lowercaseSizer->Clear(true);
  for (auto &def : lowerCaseDefs)
    if (def.condition == Cond::None ||
        (def.condition == Cond::Show_mu && Show_mu) ||
        (def.condition == Cond::ShowLatinLookalikes && ShowLatinLookalikes))
      {
        CharButton *button = new CharButton(this, m_worksheet, m_configuration, def);
        m_lowercaseSizer->Add(button, wxSizerFlags().Expand());
        button->Connect(wxEVT_RIGHT_DOWN,
                        wxMouseEventHandler(GreekSidebar::OnMouseRightDown), NULL, this);
        button->Connect(wxEVT_MENU,
                        wxCommandEventHandler(GreekSidebar::OnMenu), NULL, this);
        button->GetTextObject()->Connect(wxEVT_RIGHT_DOWN,
                                         wxMouseEventHandler(GreekSidebar::OnMouseRightDown),
                                         NULL, this);
        button->GetTextObject()->Connect(wxEVT_MENU,
                                         wxCommandEventHandler(GreekSidebar::OnMenu), NULL, this);
      }

  m_uppercaseSizer->Clear(true);
  for (auto &def : upperCaseDefs)
    if (def.condition == Cond::None ||
        (def.condition == Cond::Show_mu && Show_mu) ||
        (def.condition == Cond::ShowLatinLookalikes && ShowLatinLookalikes))
      {
        CharButton *button = new CharButton(this, m_worksheet, m_configuration, def);
        m_uppercaseSizer->Add(button, wxSizerFlags().Expand());
        button->Connect(wxEVT_RIGHT_DOWN,
                        wxMouseEventHandler(GreekSidebar::OnMouseRightDown), NULL, this);
        button->Connect(wxEVT_MENU,
                        wxCommandEventHandler(GreekSidebar::OnMenu), NULL, this);
        button->GetTextObject()->Connect(wxEVT_RIGHT_DOWN,
                                         wxMouseEventHandler(GreekSidebar::OnMouseRightDown),
                                         NULL, this);
        button->GetTextObject()->Connect(wxEVT_MENU,
                                         wxCommandEventHandler(GreekSidebar::OnMenu), NULL, this);
      }
}

void GreekSidebar::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  std::unique_ptr<wxMenu> popupMenu(new wxMenu());
  popupMenu->AppendCheckItem(EventIDs::menu_showLatinGreekLookalikes,
                             _(wxS("Show greek \u21D4 latin lookalikes")));
  popupMenu->Check(EventIDs::menu_showLatinGreekLookalikes,
                   m_configuration->GreekSidebar_ShowLatinLookalikes());
  popupMenu->AppendCheckItem(EventIDs::menu_showGreekMu,
                             _(wxS("Show lookalike for unit prefix µ")));
  popupMenu->Check(EventIDs::menu_showGreekMu, m_configuration->GreekSidebar_Show_mu());
  if(wxWindow::FindFocus())
    wxWindow::FindFocus()->PopupMenu(&*popupMenu);
}
