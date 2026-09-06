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

#ifndef AIPROVIDER_H
#define AIPROVIDER_H

#include "precomp.h"
#include <wx/wx.h>
#include <functional>
#include <memory>
#include <vector>

/*! \file
  Talks to one of a handful of external AI chat APIs for the AI chat sidebar
  (AiChatSidebar). See that class for the safety scope (read-only worksheet
  context, no tool-calling in this first pass).

  Every provider here needs an internet connection to a third-party service
  and an API key the user pastes into Options -- unlike the MCP server
  (src/mcp/), which is entirely local and needs neither. The two features
  are otherwise unrelated beyond both ultimately drawing on McpTools for
  worksheet context.
*/

//! One turn of a chat conversation.
struct AiChatMessage {
  //! "user" or "assistant" -- never "system"/"model"/etc.; each provider's
  //! own request-building maps this pair to whatever vocabulary its own API
  //! actually expects (e.g. Google's Gemini calls the AI's own turn "model").
  wxString role;
  wxString content;
};

//! Which AI service to talk to. Persisted as a plain int in Configuration;
//! keep existing values stable across releases (append only).
enum class AiProviderKind { None = 0, Anthropic = 1, OpenAI = 2, Google = 3, Qwen = 4 };

//! Human-readable name for Options/the sidebar's status line.
wxString AiProviderKindName(AiProviderKind kind);

//! This provider's built-in default model id, shown as Options' initial
//! value for a not-yet-configured model field. Users can override it --
//! model catalogs change far more often than this code does.
wxString AiProviderDefaultModel(AiProviderKind kind);

//! Where to go to create/find an API key for this provider -- shown as a
//! link next to that provider's key field in Options, since there is no
//! "log in" button that could get one automatically: none of these four
//! providers offer a legitimate third-party OAuth flow a desktop app could
//! use, so pasting a key from the provider's own site is the only option.
//! Best-effort: a provider's console is free to move its own pages, same
//! caveat as AiProviderDefaultModel()'s model ids going stale over time.
wxString AiProviderApiKeyUrl(AiProviderKind kind);

/*! Talks to one external AI provider's chat completion HTTP API.

  Deliberately split into a stateless, directly-testable half (BuildRequestBody()/
  ParseReply(), pure string/JSON in and out, no networking -- see
  test/unit_tests/test_AiProvider.cpp) and the actual network call
  (SendChat()), which needs a live wxWebRequest and can only really be
  verified against a real or fake HTTP server (done live, see AGENTS.md).
*/
class AiProvider {
public:
  AiProvider(wxString baseUrl, wxString apiKey, wxString model)
    : m_baseUrl(std::move(baseUrl)), m_apiKey(std::move(apiKey)),
      m_model(std::move(model)) {}
  virtual ~AiProvider() = default;

  virtual AiProviderKind Kind() const = 0;
  wxString Name() const { return AiProviderKindName(Kind()); }

  //! The full URL SendChat() will POST to. Virtual so Google's provider can
  //! append the model id (its endpoint is per-model, unlike the others).
  virtual wxString RequestUrl() const { return m_baseUrl; }

  //! Any additional headers this provider's auth scheme needs, name/value
  //! pairs, beyond the "POST JSON" content-type SendChat() always sets.
  virtual std::vector<std::pair<wxString, wxString>> AuthHeaders() const = 0;

  /*! Builds the JSON request body for one chat turn. `context` is a
    read-only worksheet snapshot (from McpTools), sent as a system/context
    message ahead of the actual conversation; `history` is every prior turn
    plus the new user message as its last entry. Pure function, no I/O --
    unit-tested directly. */
  virtual wxString BuildRequestBody(const wxString &context,
                                    const std::vector<AiChatMessage> &history) const = 0;

  /*! Extracts the assistant's reply text from a successful (2xx) response
    body. Throws AiProviderError (see below) if the body doesn't parse or
    doesn't have the shape this provider expects. Pure function, no I/O. */
  virtual wxString ParseReply(const wxString &responseBody) const = 0;

  /*! Sends one chat turn asynchronously via wxWebRequest and calls
    `callback(ok, replyOrError)` exactly once, on the GUI thread, once the
    HTTP request completes, fails, or errors out. `owner` is the
    wxEvtHandler the completion event is delivered to -- must outlive the
    request; the sidebar itself is the intended owner. No-op (calls back
    with ok=false immediately) if wxUSE_WEBREQUEST is off in this build.

    Static, taking `self` as an explicit shared_ptr rather than being a
    plain instance method: the completion lambda needs the provider object
    (for ParseReply()/Name(), both virtual) to still exist whenever the
    response actually arrives, which can outlast the caller doing something
    else entirely -- e.g. the user changing the configured provider/model in
    Options while a request is still in flight. Capturing a shared_ptr
    keeps the exact instance that sent the request alive for the callback
    regardless of what AiChatSidebar does to its own current-provider
    pointer in the meantime; capturing a raw `this` would not. */
  static void SendChat(std::shared_ptr<const AiProvider> self, wxEvtHandler *owner,
                       const wxString &context,
                       const std::vector<AiChatMessage> &history,
                       std::function<void(bool ok, const wxString &replyOrError)> callback);

  //! True if this build of wxWidgets has wxWebRequest at all (>= 3.1.5,
  //! built with a working backend) -- if false, the whole chat sidebar
  //! feature has nothing to talk to and should say so rather than silently
  //! failing every request.
  static bool NetworkingAvailable();

protected:
  wxString m_baseUrl;
  wxString m_apiKey;
  wxString m_model;
};

//! Thrown by ParseReply() for a response that doesn't parse or doesn't have
//! the expected shape -- distinct from a non-2xx HTTP status, which
//! SendChat() reports directly from the response body/status line instead.
class AiProviderError : public std::runtime_error {
public:
  explicit AiProviderError(const std::string &msg) : std::runtime_error(msg) {}
};

//! Builds the provider for this kind, or nullptr for AiProviderKind::None.
std::shared_ptr<AiProvider> MakeAiProvider(AiProviderKind kind, const wxString &apiKey,
                                           const wxString &model);

#endif // AIPROVIDER_H
