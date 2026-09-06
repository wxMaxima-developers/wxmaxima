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

#include "AiChatSidebar.h"
#include "Configuration.h"

using json = nlohmann::json;

AiChatSidebar::AiChatSidebar(wxWindow *parent, Configuration *configuration,
                             Worksheet *worksheet, Variablespane *variablesPane,
                             wxWindowID id)
  : wxPanel(parent, id), m_configuration(configuration),
    m_tools(worksheet, variablesPane) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);

  m_statusText = new wxStaticText(this, wxID_ANY, wxEmptyString);
  vbox->Add(m_statusText,
           wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  // Only shown while no provider is configured (see ReloadProviderFromConfig())
  // -- there is no "log in" button that could get an API key automatically
  // (AiProviderApiKeyUrl()'s own comment explains why), so this is the
  // closest equivalent: one click to the exact place that both explains the
  // options and links to where to actually get a key.
  m_openOptionsButton = new wxButton(this, wxID_ANY, _("Open Options..."));
  vbox->Add(m_openOptionsButton,
           wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  m_historyCtrl =
    new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition,
                   wxDefaultSize,
                   wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH2);
  vbox->Add(m_historyCtrl,
           wxSizerFlags(1).Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  // Stacked vertically -- input above, buttons in a row below -- rather than
  // input-beside-buttons: this sidebar docks into a narrow column (shared
  // with e.g. Table of Contents), and a horizontal layout let the
  // fixed-width button column crowd the input box down to an invisible
  // sliver there (confirmed live: swapping each control's background to a
  // distinct debug colour showed the "input" area was really only a few
  // pixels wide). A vertical stack has no such competition for width.
  m_inputCtrl = new wxTextCtrl(
    this, wxID_ANY, wxEmptyString, wxDefaultPosition,
    wxSize(-1, 60 * GetContentScaleFactor()), wxTE_MULTILINE);
  m_inputCtrl->SetToolTip(
    _("Enter sends; Shift+Enter inserts a newline."));
  vbox->Add(m_inputCtrl,
           wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  // Stacked vertically too, each full-width, rather than side by side: even
  // two buttons side by side don't reliably both fit a docked sidebar this
  // narrow without the same width-squeeze problem the input box just had
  // (confirmed live: side by side, "Clear" was pushed down to an invisible
  // sliver at the panel's edge).
  m_sendButton = new wxButton(this, wxID_ANY, _("Send"));
  m_clearButton = new wxButton(this, wxID_ANY, _("Clear"));
  vbox->Add(m_sendButton,
           wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));
  vbox->Add(m_clearButton,
           wxSizerFlags().Expand().Border(wxALL, 5 * GetContentScaleFactor()));

  SetSizer(vbox);
  Layout();

  m_sendButton->Bind(wxEVT_BUTTON, &AiChatSidebar::OnSend, this);
  m_clearButton->Bind(wxEVT_BUTTON,
                      [this](wxCommandEvent &) { ClearConversation(); });
  m_openOptionsButton->Bind(wxEVT_BUTTON, &AiChatSidebar::OnOpenOptions, this);
  m_inputCtrl->Bind(wxEVT_KEY_DOWN, &AiChatSidebar::OnInputKeyDown, this);

  ReloadProviderFromConfig();
}

void AiChatSidebar::ReloadProviderFromConfig() {
  auto kind = static_cast<AiProviderKind>(m_configuration->AiChatProvider());
  wxString apiKey, model;
  switch (kind) {
  case AiProviderKind::Anthropic:
    apiKey = m_configuration->AiApiKeyAnthropic();
    model = m_configuration->AiModelAnthropic();
    break;
  case AiProviderKind::OpenAI:
    apiKey = m_configuration->AiApiKeyOpenAI();
    model = m_configuration->AiModelOpenAI();
    break;
  case AiProviderKind::Google:
    apiKey = m_configuration->AiApiKeyGoogle();
    model = m_configuration->AiModelGoogle();
    break;
  case AiProviderKind::Qwen:
    apiKey = m_configuration->AiApiKeyQwen();
    model = m_configuration->AiModelQwen();
    break;
  default:
    break;
  }
  m_provider = (apiKey.IsEmpty()) ? nullptr : MakeAiProvider(kind, apiKey, model);
  UpdateStatusText();
  m_sendButton->Enable(!m_requestInFlight && (m_provider != nullptr));
  m_openOptionsButton->Show(m_provider == nullptr);
  Layout();
}

void AiChatSidebar::OnOpenOptions(wxCommandEvent &WXUNUSED(event)) {
  wxCommandEvent openPreferences(wxEVT_MENU, wxID_PREFERENCES);
  GetParent()->GetEventHandler()->AddPendingEvent(openPreferences);
}

void AiChatSidebar::ClearConversation() {
  m_history.clear();
  m_historyCtrl->Clear();
}

void AiChatSidebar::OnInputKeyDown(wxKeyEvent &event) {
  if (((event.GetKeyCode() == WXK_RETURN) ||
      (event.GetKeyCode() == WXK_NUMPAD_ENTER)) &&
      !event.ShiftDown()) {
    wxCommandEvent dummy;
    OnSend(dummy);
    return; // Swallow the Enter -- don't Skip() -- so no newline is inserted.
  }
  event.Skip();
}

void AiChatSidebar::AppendToHistory(const wxString &speaker, const wxString &text) {
  m_historyCtrl->AppendText(speaker + wxS(":\n") + text + wxS("\n\n"));
}

void AiChatSidebar::SetBusy(bool busy) {
  m_requestInFlight = busy;
  m_inputCtrl->Enable(!busy);
  m_sendButton->Enable(!busy && (m_provider != nullptr));
  UpdateStatusText();
}

void AiChatSidebar::UpdateStatusText() {
  if (m_requestInFlight)
    m_statusText->SetLabel(
      wxString::Format(_("Waiting for %s..."),
                       m_provider ? m_provider->Name() : wxString()));
  else if (!AiProvider::NetworkingAvailable())
    m_statusText->SetLabel(
      _("This build of wxMaxima cannot make network requests."));
  else if (!m_provider)
    m_statusText->SetLabel(
      _("No AI provider configured -- add an API key in Options."));
  else
    m_statusText->SetLabel(
      wxString::Format(_("Chatting with %s"), m_provider->Name()));
}

wxString AiChatSidebar::BuildContextSnapshot() const {
  json worksheet = m_tools.ReadWorksheet();
  wxString text = wxString::FromUTF8(worksheet.value("text", std::string()).c_str());
  if (text.Length() > MAX_CONTEXT_LENGTH) {
    // A plain Left() truncation would silently cut off the "(CURRENT
    // CELL ...)" marker below for any worksheet long enough to need
    // truncating in the first place -- exactly the case where a user is
    // most likely to ask about "the current cell" or "the cell above the
    // cursor," since a short worksheet never needs truncating at all.
    // Center the kept window on that marker instead, when there is one.
    int markerPos = text.Find(wxS("(CURRENT CELL"));
    size_t start = 0;
    if (markerPos != wxNOT_FOUND) {
      size_t pos = static_cast<size_t>(markerPos);
      start = (pos > MAX_CONTEXT_LENGTH / 2) ? pos - MAX_CONTEXT_LENGTH / 2 : 0;
      if (start + MAX_CONTEXT_LENGTH > text.Length())
        start = text.Length() - MAX_CONTEXT_LENGTH;
    }
    text = text.Mid(start, MAX_CONTEXT_LENGTH);
    if (start > 0)
      text = wxS("[... earlier worksheet content omitted ...]\n") + text;
    text += wxS("\n... [truncated for the chat context]");
  }
  return _("You are an assistant embedded in wxMaxima, a GUI front-end for "
          "the Maxima computer algebra system. Below is a read-only "
          "snapshot of the user's current worksheet -- you cannot edit, "
          "evaluate or otherwise change it; only the user can do that "
          "through the wxMaxima UI. A cell marked \"(CURRENT CELL -- the "
          "user's cursor is here)\" is where the user's cursor currently "
          "is -- that is what they mean by \"this cell,\" \"the current "
          "cell,\" or \"the cell above/here.\" A cell marked \"(THIS CELL "
          "HAS AN ERROR)\" is one Maxima reported an error in. Use the "
          "snapshot only as context.\n\n"
          "--- Worksheet snapshot ---\n") +
    text;
}

void AiChatSidebar::OnSend(wxCommandEvent &) {
  if (m_requestInFlight)
    return;
  wxString text = m_inputCtrl->GetValue();
  text.Trim();
  text.Trim(false);
  if (text.IsEmpty())
    return;
  if (!m_provider) {
    AppendToHistory(_("wxMaxima"),
                    _("No AI provider is configured. Open Options to add an "
                      "API key for one."));
    return;
  }
  if (!AiProvider::NetworkingAvailable()) {
    AppendToHistory(_("wxMaxima"),
                    _("This build of wxMaxima cannot make network requests."));
    return;
  }

  m_inputCtrl->Clear();
  AppendToHistory(_("You"), text);
  m_history.push_back({wxS("user"), text});
  SetBusy(true);

  wxString context = BuildContextSnapshot();
  std::shared_ptr<AiProvider> provider = m_provider;
  AiProvider::SendChat(
    provider, this, context, m_history,
    [this, provider](bool ok, const wxString &replyOrError) {
      SetBusy(false);
      if (ok) {
        AppendToHistory(provider->Name(), replyOrError);
        m_history.push_back({wxS("assistant"), replyOrError});
      } else {
        AppendToHistory(_("Error"), replyOrError);
      }
    });
}
