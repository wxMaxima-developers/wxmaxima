// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  The dialog that records the keyboard shortcut for a symbol (GH #2578).
*/

#include "SymbolShortcutDialog.h"
#include <wx/button.h>
#include <wx/frame.h>
#include <wx/menu.h>
#include <wx/sizer.h>
#include <wx/stockitem.h>
#include <memory>

SymbolShortcutDialog::SymbolShortcutDialog(wxWindow *parent,
                                           const wxString &symbol,
                                           const wxString &currentShortcut,
                                           UsageLookup usage)
  : wxDialog(parent, wxID_ANY, _("Keyboard shortcut"), wxDefaultPosition,
             wxDefaultSize, wxDEFAULT_DIALOG_STYLE),
    m_shortcut(currentShortcut), m_usage(std::move(usage)) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  const int border = 8 * GetContentScaleFactor();

  wxStaticText *symbolText = new wxStaticText(this, wxID_ANY, symbol);
  symbolText->SetFont(symbolText->GetFont().Scaled(3));
  vbox->Add(symbolText, wxSizerFlags().Center().Border(wxALL, border));

  vbox->Add(new wxStaticText(
              this, wxID_ANY,
              _("Press the keys that should type this symbol into the worksheet.\n"
                "The combination needs Ctrl or Alt, or a function key.")),
            wxSizerFlags().Border(wxLEFT | wxRIGHT, border));

  m_shortcutText = new wxStaticText(this, wxID_ANY, wxEmptyString);
  m_shortcutText->SetFont(m_shortcutText->GetFont().Bold());
  vbox->Add(m_shortcutText, wxSizerFlags().Center().Border(wxALL, border));

  m_warning = new wxStaticText(this, wxID_ANY, wxEmptyString);
  vbox->Add(m_warning, wxSizerFlags().Expand().Border(wxLEFT | wxRIGHT, border));

  wxBoxSizer *buttons = new wxBoxSizer(wxHORIZONTAL);
  wxButton *remove = new wxButton(this, wxID_ANY, _("No shortcut"));
  remove->Bind(wxEVT_BUTTON, &SymbolShortcutDialog::OnRemove, this);
  buttons->Add(remove, wxSizerFlags().Border(wxRIGHT, border));
  buttons->AddStretchSpacer();
  wxStdDialogButtonSizer *okCancel = new wxStdDialogButtonSizer();
  wxButton *ok = new wxButton(this, wxID_OK);
  // Enter accepts the shortcut shown, whichever button has the focus.
  ok->SetDefault();
  okCancel->AddButton(ok);
  okCancel->AddButton(new wxButton(this, wxID_CANCEL));
  okCancel->Realize();
  buttons->Add(okCancel);
  vbox->Add(buttons, wxSizerFlags().Expand().Border(wxALL, border));

  // Char hook, not key down: it reaches the dialog before the button that
  // has the focus would act on the key.
  Bind(wxEVT_CHAR_HOOK, &SymbolShortcutDialog::OnCharHook, this);
  SetSizerAndFit(vbox);
  ShowShortcut();
  CentreOnParent();
  ok->SetFocus();
}

bool SymbolShortcutDialog::RecordKey(const wxKeyEvent &event) {
  const wxString shortcut = wxm::ShortcutFromKeyEvent(event);
  if (shortcut.IsEmpty())
    return false;
  m_shortcut = shortcut;
  ShowShortcut();
  return true;
}

void SymbolShortcutDialog::OnCharHook(wxKeyEvent &event) {
  if (!RecordKey(event))
    event.Skip();
}

void SymbolShortcutDialog::OnRemove(wxCommandEvent &WXUNUSED(event)) {
  m_shortcut.Clear();
  ShowShortcut();
}

void SymbolShortcutDialog::ShowShortcut() {
  if (m_shortcut.IsEmpty())
    m_shortcutText->SetLabel(_("(no shortcut)"));
  else
    m_shortcutText->SetLabel(wxm::ShortcutForDisplay(m_shortcut));
  wxString warning;
  if (!m_shortcut.IsEmpty() && m_usage)
    warning = m_usage(m_shortcut);
  m_warning->SetLabel(warning);
  m_warning->Wrap(GetClientSize().x > 0 ? GetClientSize().x : 300);
  Layout();
  Fit();
}

wxMenuBar *SymbolShortcutDialog::FindMenuBar(wxWindow *window) {
  // A floating sidebar lives in a frame of its own, whose parent is the main
  // window: walk up until a frame has a menu bar.
  while (window) {
    wxWindow *topLevel = wxGetTopLevelParent(window);
    if (wxFrame *frame = wxDynamicCast(topLevel, wxFrame))
      if (frame->GetMenuBar())
        return frame->GetMenuBar();
    window = topLevel ? topLevel->GetParent() : nullptr;
  }
  return nullptr;
}

namespace {
wxString MenuItemUsingShortcut(const wxMenu *menu, const wxString &rawShortcut) {
  for (const wxMenuItem *item : menu->GetMenuItems()) {
    if (item->GetSubMenu()) {
      const wxString label = MenuItemUsingShortcut(item->GetSubMenu(), rawShortcut);
      if (!label.IsEmpty())
        return label;
      continue;
    }
    // GetAccel() returns a fresh copy that we own.
    std::unique_ptr<wxAcceleratorEntry> accel(item->GetAccel());
    if (accel && wxm::SameShortcut(accel->ToRawString(), rawShortcut))
      return item->GetItemLabelText();
  }
  return {};
}
} // namespace

wxString SymbolShortcutDialog::MenuItemUsingShortcut(const wxMenuBar *menuBar,
                                                     const wxString &rawShortcut) {
  if (!menuBar || rawShortcut.IsEmpty())
    return {};
  for (size_t i = 0; i < menuBar->GetMenuCount(); ++i) {
    const wxString label =
      ::MenuItemUsingShortcut(menuBar->GetMenu(i), rawShortcut);
    if (!label.IsEmpty())
      return label;
  }
  return {};
}

SymbolShortcutDialog::UsageLookup
SymbolShortcutDialog::MakeUsageLookup(wxWindow *window,
                                      std::vector<wxm::SymbolShortcut> shortcuts,
                                      const wxString &symbol) {
  wxMenuBar *menuBar = FindMenuBar(window);
  return [menuBar, shortcuts = std::move(shortcuts), symbol](const wxString &raw) {
    const wxString menuItem = MenuItemUsingShortcut(menuBar, raw);
    if (!menuItem.IsEmpty())
      return wxString::Format(
        _("The menu entry \"%s\" has this shortcut, too. Which of the two "
          "it does depends on the operating system."), menuItem);
    for (const auto &entry : shortcuts)
      if ((entry.symbol != symbol) && wxm::SameShortcut(entry.shortcut, raw))
        return wxString::Format(
          _("This shortcut types %s now; it will type this symbol instead."),
          entry.symbol);
    return wxString();
  };
}
