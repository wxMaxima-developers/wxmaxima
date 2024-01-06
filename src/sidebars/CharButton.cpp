// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class CharacterButton

  This file contains the definition of the class CharacterButton that allows to
  select arbitrary unicode symbols.
*/

#include "CharButton.h"
#include "UnicodeSidebar.h"
#include <wx/sizer.h>
#include <wx/dcbuffer.h>
#include <wx/settings.h>

void CharButton::MouseOverTextIs(bool mouseOver) {
  if (m_mouseOverText != mouseOver) {
    m_mouseOverText = mouseOver;
    m_backgroundColorChangeNeeded = true;
    Connect(wxEVT_IDLE, wxIdleEventHandler(CharButton::OnIdle), NULL, this);
  }
}

void CharButton::MouseOverPanelIs(bool mouseOver) {
  if (m_mouseOverPanel != mouseOver) {
    m_mouseOverPanel = mouseOver;
    m_backgroundColorChangeNeeded = true;
    Connect(wxEVT_IDLE, wxIdleEventHandler(CharButton::OnIdle), NULL, this);
  }
}

void CharButton::MouseOverPanel(wxMouseEvent &event) {
  MouseOverPanelIs();
  event.Skip();
}
void CharButton::MouseLeftPanel(wxMouseEvent &event) {
  MouseOverPanelIs(false);
  event.Skip();
}
void CharButton::MouseOverText(wxMouseEvent &event) {
  MouseOverTextIs();
  event.Skip();
}
void CharButton::MouseLeftText(wxMouseEvent &event) {
  MouseOverTextIs(false);
  event.Skip();
}

void CharButton::OnIdle(wxIdleEvent &event) {
  Disconnect(wxEVT_IDLE, wxIdleEventHandler(CharButton::OnIdle), NULL, this);
  if (!m_backgroundColorChangeNeeded)
    return;
  m_backgroundColorChangeNeeded = false;
  if ((m_mouseOverPanel) || (m_mouseOverText))
    SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT));
  else
    // An invalid color means "the default background"
    SetBackgroundColour(wxColour());
  event.Skip();
}

void CharButton::CharButtonPressed(wxCommandEvent &WXUNUSED(event)) {
  wxCommandEvent *ev = new wxCommandEvent(SIDEBARKEYEVENT, static_cast<wxWindowID>(m_char));
  m_worksheet->GetEventHandler()->QueueEvent(ev);
}

void CharButton::OnSize(wxSizeEvent &event) {
  wxFont fnt = GetFont();
  wxClientDC dc(this);
  dc.SetFont(fnt);
  auto size = dc.GetTextExtent(m_char);
  auto minSize = dc.GetTextExtent("M");
  minSize.x *= 1.5;
  minSize.y *= 1.5;
  size.x += 2 * GetContentScaleFactor();
  size.y += 2 * GetContentScaleFactor();
  if (minSize.x < minSize.y)
    minSize.x = minSize.y;
  if (minSize.x > size.x)
    size.x = minSize.x;
  if (minSize.y > size.y)
    size.y = minSize.y;
  //  SetSize(size);
  SetMinSize(size);
  event.Skip();
}

CharButton::CharButton(wxWindow *parent, wxWindow *worksheet,
                       Configuration *config, const Definition &def,
                       bool forceShow)
  : wxPanel(parent, wxID_ANY), m_char(def.symbol), m_configuration(config),
    m_description(def.description), m_worksheet(worksheet) {
  Connect(wxEVT_SIZE, wxSizeEventHandler(CharButton::OnSize));
  wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
  m_buttonText = new wxStaticText(this, -1, wxString(m_char));
  sizer->AddStretchSpacer(1);
  sizer->Add(m_buttonText, 0, wxALIGN_CENTER_VERTICAL);
  sizer->AddStretchSpacer(1);
  SetSizer(sizer);
  FitInside();
  SetToolTip(def.description);
  Connect(wxEVT_LEFT_UP, wxCommandEventHandler(CharButton::CharButtonPressed),
          NULL, this);
  Connect(wxEVT_IDLE, wxIdleEventHandler(CharButton::OnIdle), NULL, this);
  Connect(wxEVT_ENTER_WINDOW, wxMouseEventHandler(CharButton::MouseOverPanel),
          NULL, this);
  Connect(wxEVT_LEAVE_WINDOW, wxMouseEventHandler(CharButton::MouseLeftPanel),
          NULL, this);
  m_buttonText->Connect(wxEVT_ENTER_WINDOW,
                        wxMouseEventHandler(CharButton::MouseOverText), NULL,
                        this);
  m_buttonText->Connect(wxEVT_LEAVE_WINDOW,
                        wxMouseEventHandler(CharButton::MouseLeftText), NULL,
                        this);
  m_buttonText->Connect(wxEVT_LEFT_UP,
                        wxCommandEventHandler(CharButton::CharButtonPressed),
                        NULL, this);
  if (!(forceShow || m_configuration->FontRendersChar(m_char))) {
    Hide();
  }

  wxFont mathFont =
    m_configuration->GetStyle(TS_MATH)->GetFont();
  wxFont textFont =
    m_configuration->GetStyle(TS_CODE_DEFAULT)->GetFont();
  if (((!mathFont.IsOk()) ||
       m_configuration->FontRendersChar(m_char, mathFont)) ||
      ((!textFont.IsOk()) ||
       m_configuration->FontRendersChar(m_char, textFont))) {
    SetToolTip(m_description);
  } else {
    m_buttonText->SetForegroundColour(wxColor(128, 128, 128));
    SetToolTip(m_description + wxS("\n") +
               _("(Might not be displayed correctly in at least one of the "
                 "worksheet fonts)"));
  }
}
