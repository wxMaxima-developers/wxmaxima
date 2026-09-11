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
//! Custom is what every user-added provider (see AiCustomProviderConfig)
//! reports from AiProvider::Kind() -- Configuration::AiActiveCustomProviderId()
//! says which one, since a plain kind alone can't distinguish two custom
//! entries from each other.
enum class AiProviderKind { None = 0, Anthropic = 1, OpenAI = 2, Google = 3, Qwen = 4, Custom = 5 };

//! Which request/response wire format a provider uses. The four built-in
//! AiProviderKinds each hardcode one of these; a user-added custom provider
//! (AiCustomProviderConfig) picks one explicitly, since these three cover
//! every shape actually implemented -- Anthropic's and Google's own APIs,
//! plus the "OpenAI-compatible" shape a large number of real third-party
//! and self-hosted endpoints (OpenRouter, Groq, Ollama, ...) already speak
//! verbatim, precisely because it's become a de facto standard. None of the
//! three auth conventions that go with these shapes (a plain x-api-key
//! header, "Authorization: Bearer <key>", or x-goog-api-key) are exotic
//! enough to need a fourth, freeform "custom auth" option -- picking a
//! shape already picks its auth convention too.
enum class AiProviderShape { Anthropic, OpenAiCompatible, Google };

//! Human-readable name for the shape picker in the "Add custom provider"
//! dialog.
wxString AiProviderShapeName(AiProviderShape shape);

//! One user-added custom provider, as persisted in Configuration's
//! AiCustomProvidersJson() (everything except the API key, which lives in
//! the OS secret store, keyed by CustomProviderSecretService(id) -- see
//! AiProvider::SaveApiKey()/LoadApiKey()).
struct AiCustomProviderConfig {
  //! Stable, internally-generated identifier -- never shown in the UI and
  //! never reused, so renaming a provider or two providers sharing a
  //! display name can't collide in the secret store or in
  //! Configuration::AiActiveCustomProviderId(). See NewAiCustomProviderId().
  wxString id;
  //! User-chosen display name, shown in the provider dropdown.
  wxString name;
  AiProviderShape shape = AiProviderShape::OpenAiCompatible;
  //! Full request URL for OpenAI-compatible/Anthropic shapes; for Google's
  //! shape, the part before "<model>:generateContent" (mirrors
  //! GoogleProvider::RequestUrl()).
  wxString baseUrl;
  wxString model;
};

//! One well-known local AI server's connection defaults, offered as a
//! quick-fill preset in the "Add custom provider" dialog (Options -> AI
//! Chat) -- picking one just pre-fills that dialog's own editable fields,
//! it's a shortcut for not typing e.g. Ollama's default port from memory,
//! not a fifth built-in AiProviderKind. All of these speak the
//! OpenAI-compatible shape and normally don't check the API key at all
//! (it can be left blank), since a local server has no third party to
//! authenticate to.
struct AiLocalServerPreset {
  wxString name;
  wxString baseUrl;
  wxString model;
};

//! A short, hand-picked list of common local LLM servers -- not
//! exhaustive, just the ones popular enough to be worth a one-click
//! shortcut; anything else is still just as reachable via a plain custom
//! entry with a hand-typed URL.
std::vector<AiLocalServerPreset> AiKnownLocalServerPresets();

//! A fresh id for a new custom provider (see AiCustomProviderConfig::id) --
//! never shown to the user, just needs to not collide with any existing one
//! for the lifetime of this installation.
wxString NewAiCustomProviderId();

//! Parses Configuration::AiCustomProvidersJson() back into a list; returns
//! an empty list for an empty or malformed string rather than throwing --
//! a corrupted/hand-edited config file should degrade to "no custom
//! providers configured", not crash Options on open.
std::vector<AiCustomProviderConfig> ParseAiCustomProviders(const wxString &json);

//! The inverse of ParseAiCustomProviders(), for Configuration::
//! AiCustomProvidersJson()'s setter.
wxString SerializeAiCustomProviders(const std::vector<AiCustomProviderConfig> &providers);

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

//! A built-in provider's fixed request URL (Google's own, without the
//! "<model>:generateContent" suffix its real RequestUrl() appends -- see
//! GoogleProvider), shown read-only in Options next to a Custom entry's
//! editable one, so both look like the same kind of field even though only
//! one of them can actually be changed. Must be kept in sync with
//! MakeAiProvider()'s own hardcoded URLs; there is deliberately no single
//! shared source for both, since MakeAiProvider() builds a real (kind,
//! shape)-bound provider object while this is a pure display string with
//! no such object to read it back from.
wxString AiProviderBaseUrl(AiProviderKind kind);

//! Where to see this provider's current list of available model ids --
//! shown as a link next to Options' model field. AiProviderDefaultModel()
//! uses each provider's own "rolling" alias where one exists, but a model
//! *line* still eventually gets superseded by a new one, something a
//! plain string constant in this codebase cannot track by itself; a link
//! to the provider's own list is the durable fix, not a fancier
//! auto-detection mechanism that would itself need to keep up with each
//! provider's API just to answer the same question this link answers
//! directly. Same best-effort/link-rot caveat as AiProviderApiKeyUrl().
wxString AiProviderModelListUrl(AiProviderKind kind);

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
  //! AiProviderKindName(Kind()) for a built-in provider, or the user's own
  //! chosen name for a custom one (Kind() == Custom doesn't carry a name by
  //! itself) -- SetDisplayName() is called by MakeAiProviderForShape() for
  //! exactly this reason.
  wxString Name() const {
    return m_displayName.IsEmpty() ? AiProviderKindName(Kind()) : m_displayName;
  }
  void SetDisplayName(const wxString &name) { m_displayName = name; }

  //! True if this build of wxWidgets has a working wxSecretStore backend
  //! (>= 3.1.1, compiled with wxUSE_SECRETSTORE, AND an actual OS keyring
  //! service reachable at runtime -- e.g. gnome-keyring/kwallet on Linux,
  //! always true on Windows/macOS). API keys are only ever stored here,
  //! never in plain Configuration/wxConfig, so the whole AI Chat feature
  //! (Options tab, sidebar, menu entry) stays hidden whenever this is
  //! false rather than falling back to storing a key in plain text.
  static bool SecretStoreAvailable();

  //! Saves (or, for an empty key, deletes) an API key for `service` -- a
  //! stable identifier: AiProviderKindName(kind) for a built-in provider,
  //! or CustomProviderSecretService(id) for a custom one. No-op (and never
  //! called by any code path that matters) if !SecretStoreAvailable().
  static void SaveApiKey(const wxString &service, const wxString &apiKey);
  //! Returns the stored key for `service`, or an empty string if none is
  //! stored (also the result if !SecretStoreAvailable() -- there is nowhere
  //! a key could have been saved to).
  static wxString LoadApiKey(const wxString &service);
  static void DeleteApiKey(const wxString &service);
  //! The secret-store service name for a custom provider's own id -- kept
  //! distinct from a bare built-in kind name so a custom provider a user
  //! happens to name e.g. "OpenAI" can never collide with the real
  //! built-in OpenAI entry's stored key.
  static wxString CustomProviderSecretService(const wxString &id);
  //! The secret-store service name for one of the four built-in kinds --
  //! just AiProviderKindName(kind), given its own name so every call site
  //! that needs it (Configuration's four AiApiKeyX() accessors, the
  //! plain-text-key migration in Configuration::ReadConfig()) shares one
  //! definition instead of separately hardcoding the same string.
  static wxString BuiltinProviderSecretService(AiProviderKind kind);

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
                       std::function<void(bool ok, const wxString &replyOrError)> callback,
                       std::function<void(const wxString &requestBody)> onRequest = nullptr,
                       std::function<void(bool ok, const wxString &responseBodyOrDetail)> onResponse = nullptr);

  //! True if this build of wxWidgets has wxWebRequest at all (>= 3.1.5,
  //! built with a working backend) -- if false, the whole chat sidebar
  //! feature has nothing to talk to and should say so rather than silently
  //! failing every request.
  static bool NetworkingAvailable();

protected:
  wxString m_baseUrl;
  wxString m_apiKey;
  wxString m_model;
  wxString m_displayName;
};

//! Thrown by ParseReply() for a response that doesn't parse or doesn't have
//! the expected shape -- distinct from a non-2xx HTTP status, which
//! SendChat() reports directly from the response body/status line instead.
class AiProviderError : public std::runtime_error {
public:
  explicit AiProviderError(const std::string &msg) : std::runtime_error(msg) {}
};

//! Builds the provider for this kind, or nullptr for AiProviderKind::None.
//! Never called with AiProviderKind::Custom -- see MakeAiProviderForShape()
//! for that case, since a custom provider's URL/shape isn't implied by the
//! kind alone.
std::shared_ptr<AiProvider> MakeAiProvider(AiProviderKind kind, const wxString &apiKey,
                                           const wxString &model);

//! Builds a provider for a user-added custom entry: reuses the exact same
//! request/response-shape implementation a built-in provider of that shape
//! uses (AnthropicProvider/OpenAiCompatibleProvider/GoogleProvider), just
//! pointed at an arbitrary URL with the user's own display name and model
//! instead of one of the four hardcoded built-ins. Kind() on the result is
//! always AiProviderKind::Custom.
std::shared_ptr<AiProvider> MakeAiProviderForShape(AiProviderShape shape,
                                                   const wxString &displayName,
                                                   const wxString &baseUrl,
                                                   const wxString &apiKey,
                                                   const wxString &model);

#endif // AIPROVIDER_H
