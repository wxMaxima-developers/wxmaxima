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

#ifndef AICHATSIDEBAR_H
#define AICHATSIDEBAR_H

#include "precomp.h"
#include "ai/AiProvider.h"
#include "mcp/McpTools.h"
#include <wx/wx.h>
#include <memory>
#include <vector>

class Configuration;
class Worksheet;
class Variablespane;
class StatusBar;
class AiConnectionMonitor;

/*! A sidebar to chat with an external AI about the current worksheet
  (follow-up to the MCP server, GH request: "a de facto standard AI sidebar
  that ... gives it access to a worksheet").

  Safety scope, same as the MCP server's (see McpTools.h): every message
  sent to the AI is accompanied by a read-only worksheet snapshot (built via
  McpTools, the same tool logic the MCP server itself uses), refreshed
  before each turn -- but this first pass does NOT give the AI any tool-
  calling ability to actively request a specific cell/section/variable
  mid-conversation, and it cannot insert, edit, or evaluate anything in the
  worksheet. It is a read-only-context chat, not an agent. A real
  tool-calling loop (the AI deciding for itself to call read_cell/
  read_section/watch_variable) is a natural follow-up once this ships, since
  McpTools already implements every tool such a loop would need -- it just
  isn't wired up to a provider's function-calling API yet.

  Needs an internet connection and an API key the user pastes into Options
  for one of a handful of providers (AiProvider.h) -- unlike the MCP server,
  which is entirely local. Requires wxUSE_WEBREQUEST (wxWidgets >= 3.1.5
  with a working backend); says so plainly and refuses to send anything
  if that's not available in this build, rather than silently failing.
*/
class AiChatSidebar : public wxPanel {
public:
  //! \param statusBar The status bar to keep the AI status icon (4th field)
  //! in sync with, or NULL to skip that entirely -- see UpdateAiStatusIcon().
  //! \param monitor The AI connection monitor sidebar to mirror raw
  //! request/response traffic into, or NULL to skip that.
  AiChatSidebar(wxWindow *parent, Configuration *configuration,
               Worksheet *worksheet, Variablespane *variablesPane,
               StatusBar *statusBar = NULL, AiConnectionMonitor *monitor = NULL,
               wxWindowID id = wxID_ANY);

  //! Re-reads Configuration for the selected provider/API key/model and
  //! rebuilds this sidebar's provider accordingly. Call after the Options
  //! dialog closes, in case the AI settings changed. Safe to call even
  //! while a request is in flight -- see AiProvider::SendChat()'s own
  //! shared_ptr-based lifetime handling for why that's safe.
  void ReloadProviderFromConfig();

  //! Clears the conversation history (but not the provider/API key setup).
  void ClearConversation();

  //! Gives keyboard focus to the input box -- called when the AI status
  //! icon is single-clicked to bring this sidebar to the user's attention.
  void FocusInput() { m_inputCtrl->SetFocus(); }

private:
  void OnSend(wxCommandEvent &event);
  void OnInputKeyDown(wxKeyEvent &event);
  //! Opens the Options dialog's AI Chat tab -- shown only while no provider
  //! is configured, since there is no "log in" button that could get an API
  //! key automatically (see AiProviderApiKeyUrl()'s own comment). Re-posts
  //! wxID_PREFERENCES to the top-level window rather than constructing
  //! ConfigDialogue itself, so this reuses the exact same handling
  //! (MaximaCommandMenus.cpp) the Edit -> Configure menu item already does
  //! (re-reading the config file first, applying settings afterwards, ...)
  //! instead of duplicating any of it.
  void OnOpenOptions(wxCommandEvent &event);
  void AppendToHistory(const wxString &speaker, const wxString &text);
  void SetBusy(bool busy);
  //! A compact, size-capped plain-text snapshot of the worksheet (table of
  //! contents + a capped full-text dump), refreshed on every send -- see
  //! the class comment for why this is a snapshot, not live tool-calling.
  wxString BuildContextSnapshot() const;
  void UpdateStatusText();
  //! Keeps m_statusBar's AI status icon (if any) in sync with our own
  //! None/Active/Busy/Error state -- called from SetBusy() and
  //! ReloadProviderFromConfig().
  void UpdateAiStatusIcon();
  //! Fires once a request has been in flight for a while, so the user gets
  //! the same "this is taking a while" signal a long Maxima calculation
  //! already gives via the status bar, rather than a plain unchanging
  //! "Waiting for X..." forever.
  void OnLongWaitTimer(wxTimerEvent &event);

  Configuration *m_configuration;
  McpTools m_tools;
  std::shared_ptr<AiProvider> m_provider;
  std::vector<AiChatMessage> m_history;
  bool m_requestInFlight = false;
  //! Whether the most recently completed request failed -- feeds
  //! UpdateAiStatusIcon()'s AiStatus::Error/Active choice once a request
  //! isn't in flight any more. Never true before the first request.
  bool m_lastRequestFailed = false;
  //! The error detail from the most recent failed request, shown in the
  //! status bar icon's tooltip -- see m_lastRequestFailed.
  wxString m_lastErrorDetail;
  StatusBar *m_statusBar = NULL;
  AiConnectionMonitor *m_monitor = NULL;
  //! One-shot; started on SetBusy(true), stopped on SetBusy(false). See
  //! OnLongWaitTimer().
  wxTimer m_longWaitTimer;
  //! How long a request has to be in flight before OnLongWaitTimer() fires.
  static constexpr int LONG_WAIT_MS = 10000;

  wxTextCtrl *m_historyCtrl;
  wxTextCtrl *m_inputCtrl;
  wxButton *m_sendButton;
  wxButton *m_clearButton;
  wxStaticText *m_statusText;
  wxButton *m_openOptionsButton;

  //! A cap on the worksheet snapshot's size distinct from (and much
  //! smaller than) McpTools::MAX_TEXT_LENGTH: that limit exists to bound a
  //! single MCP tool response, not to size something that gets resent as
  //! context on every single chat turn and counts against the provider's
  //! own context window and the user's token bill.
  static constexpr std::size_t MAX_CONTEXT_LENGTH = 8000;
};

#endif // AICHATSIDEBAR_H
