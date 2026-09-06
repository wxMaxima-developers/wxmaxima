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

#include "AiProvider.h"
#include <nlohmann/json.hpp>
#if wxUSE_WEBREQUEST
#include <wx/webrequest.h>
#endif

using json = nlohmann::json;

namespace {
std::string U8(const wxString &s) { return s.ToUTF8().data(); }
wxString FromU8(const std::string &s) { return wxString::FromUTF8(s.c_str()); }

//! Anthropic Messages API (https://api.anthropic.com/v1/messages).
class AnthropicProvider : public AiProvider {
public:
  AnthropicProvider(const wxString &apiKey, const wxString &model)
    : AiProvider("https://api.anthropic.com/v1/messages", apiKey, model) {}

  AiProviderKind Kind() const override { return AiProviderKind::Anthropic; }

  std::vector<std::pair<wxString, wxString>> AuthHeaders() const override {
    return {{wxS("x-api-key"), m_apiKey}, {wxS("anthropic-version"), wxS("2023-06-01")}};
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json body;
    body["model"] = U8(m_model);
    body["max_tokens"] = 2048;
    if (!context.IsEmpty())
      body["system"] = U8(context);
    json messages = json::array();
    for (const auto &m : history)
      messages.push_back({{"role", U8(m.role)}, {"content", U8(m.content)}});
    body["messages"] = messages;
    return FromU8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(U8(responseBody));
      wxString reply;
      for (const auto &block : j.at("content"))
        if (block.value("type", std::string()) == "text")
          reply += FromU8(block.value("text", std::string()));
      if (reply.IsEmpty())
        throw AiProviderError("Anthropic response had no text content block");
      return reply;
    } catch (const json::exception &e) {
      throw AiProviderError(std::string("Could not parse Anthropic's response: ") + e.what());
    }
  }
};

//! The OpenAI Chat Completions request/response shape -- shared verbatim by
//! OpenAI itself and by Alibaba's DashScope "OpenAI-compatible mode"
//! endpoint for Qwen, per DashScope's own documented compatibility layer.
//! Only the base URL, auth header and default model id actually differ.
class OpenAiCompatibleProvider : public AiProvider {
public:
  OpenAiCompatibleProvider(AiProviderKind kind, wxString baseUrl,
                           const wxString &apiKey, const wxString &model)
    : AiProvider(std::move(baseUrl), apiKey, model), m_kind(kind) {}

  AiProviderKind Kind() const override { return m_kind; }

  std::vector<std::pair<wxString, wxString>> AuthHeaders() const override {
    return {{wxS("Authorization"), wxS("Bearer ") + m_apiKey}};
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json messages = json::array();
    if (!context.IsEmpty())
      messages.push_back({{"role", "system"}, {"content", U8(context)}});
    for (const auto &m : history)
      messages.push_back({{"role", U8(m.role)}, {"content", U8(m.content)}});
    json body;
    body["model"] = U8(m_model);
    body["messages"] = messages;
    return FromU8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(U8(responseBody));
      wxString reply = FromU8(
        j.at("choices").at(0).at("message").value("content", std::string()));
      if (reply.IsEmpty())
        throw AiProviderError("Response had an empty message content");
      return reply;
    } catch (const json::exception &e) {
      throw AiProviderError(std::string("Could not parse the response: ") + e.what());
    }
  }

private:
  AiProviderKind m_kind;
};

//! Google Gemini's generateContent REST API.
class GoogleProvider : public AiProvider {
public:
  GoogleProvider(const wxString &apiKey, const wxString &model)
    : AiProvider("https://generativelanguage.googleapis.com/v1beta/models/",
                apiKey, model) {}

  AiProviderKind Kind() const override { return AiProviderKind::Google; }

  wxString RequestUrl() const override {
    return m_baseUrl + m_model + wxS(":generateContent");
  }

  std::vector<std::pair<wxString, wxString>> AuthHeaders() const override {
    return {{wxS("x-goog-api-key"), m_apiKey}};
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json body;
    if (!context.IsEmpty())
      body["systemInstruction"] = {{"parts", json::array({{{"text", U8(context)}}})}};
    json contents = json::array();
    for (const auto &m : history) {
      // Gemini calls the AI's own turn "model", not "assistant".
      wxString role = (m.role == wxS("assistant")) ? wxString(wxS("model")) : m.role;
      contents.push_back(
        {{"role", U8(role)}, {"parts", json::array({{{"text", U8(m.content)}}})}});
    }
    body["contents"] = contents;
    return FromU8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(U8(responseBody));
      wxString reply;
      for (const auto &part : j.at("candidates").at(0).at("content").at("parts"))
        reply += FromU8(part.value("text", std::string()));
      if (reply.IsEmpty())
        throw AiProviderError("Gemini response had no text part");
      return reply;
    } catch (const json::exception &e) {
      throw AiProviderError(std::string("Could not parse Gemini's response: ") + e.what());
    }
  }
};
} // namespace

wxString AiProviderKindName(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("Anthropic (Claude)");
  case AiProviderKind::OpenAI: return wxS("OpenAI");
  case AiProviderKind::Google: return wxS("Google (Gemini)");
  case AiProviderKind::Qwen: return wxS("Qwen (Alibaba)");
  default: return wxS("None");
  }
}

wxString AiProviderDefaultModel(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("claude-3-5-sonnet-20241022");
  case AiProviderKind::OpenAI: return wxS("gpt-4o-mini");
  case AiProviderKind::Google: return wxS("gemini-1.5-flash");
  case AiProviderKind::Qwen: return wxS("qwen-plus");
  default: return wxEmptyString;
  }
}

std::shared_ptr<AiProvider> MakeAiProvider(AiProviderKind kind, const wxString &apiKey,
                                           const wxString &model) {
  switch (kind) {
  case AiProviderKind::Anthropic:
    return std::make_shared<AnthropicProvider>(apiKey, model);
  case AiProviderKind::OpenAI:
    return std::make_shared<OpenAiCompatibleProvider>(
      AiProviderKind::OpenAI, wxS("https://api.openai.com/v1/chat/completions"),
      apiKey, model);
  case AiProviderKind::Qwen:
    return std::make_shared<OpenAiCompatibleProvider>(
      AiProviderKind::Qwen,
      wxS("https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"),
      apiKey, model);
  case AiProviderKind::Google:
    return std::make_shared<GoogleProvider>(apiKey, model);
  default:
    return nullptr;
  }
}

bool AiProvider::NetworkingAvailable() {
#if wxUSE_WEBREQUEST
  return true;
#else
  return false;
#endif
}

void AiProvider::SendChat(
  std::shared_ptr<const AiProvider> self, wxEvtHandler *owner, const wxString &context,
  const std::vector<AiChatMessage> &history,
  std::function<void(bool ok, const wxString &replyOrError)> callback) {
#if wxUSE_WEBREQUEST
  wxString body = self->BuildRequestBody(context, history);
  // A real, unique id is essential here, not just the default wxID_ANY:
  // every wxWebRequestEvent this handler ever receives -- from any past or
  // future request -- carries whatever id its own request was given, and
  // the id-filtered Bind() below (plus the redundant in-lambda check) both
  // rely on it to tell "my request completed" apart from "some other
  // request completed." With the default wxID_ANY every request would
  // share the same id and every stale lambda would refire on every new
  // request's completion.
  // A plain int here would permanently reserve an id from wx's finite
  // auto-id pool (NewControlId()'s own contract: reserved until assigned to
  // a wxWindowIDRef or explicitly unreserved) every single chat turn, for
  // the lifetime of the app -- wxWindowIDRef is what releases it once
  // nothing references it any more.
  wxWindowIDRef requestId = wxWindow::NewControlId();
  wxWebRequest request =
    wxWebSession::GetDefault().CreateRequest(owner, self->RequestUrl(), requestId);
  if (!request.IsOk()) {
    callback(false, _("Could not create the HTTP request."));
    return;
  }
  for (const auto &header : self->AuthHeaders())
    request.SetHeader(header.first, header.second);
  request.SetMethod(wxS("POST"));
  request.SetData(body, wxS("application/json"), wxConvUTF8);

  // Capture `self` (a shared_ptr) by value so the exact provider instance
  // that built this request -- needed for ParseReply()/Name(), both
  // virtual -- stays alive for the callback even if AiChatSidebar replaces
  // its own current-provider pointer before the response arrives (e.g. the
  // user changes provider/model in Options mid-request). Filtered to this
  // request's own id so only its own event(s) reach this particular
  // lambda; every other in-flight/future request's events are cheap
  // early-returns here. Deliberately never unbound: wx's functor-based
  // Bind() has no reliable matching Unbind() for a lambda (no identity to
  // compare against), and the existing wxWebRequest use in this codebase
  // (wxMaxima::CheckForUpdates()) has the same one-lambda-per-call,
  // never-unbound shape -- one small permanently-bound no-op-after-firing
  // lambda per chat turn is not a real leak for how few requests a chat
  // session sends in practice.
  // `done` guards against the callback firing twice: State_Unauthorized
  // (see below) responds by calling request.Cancel(), which itself
  // raises a *second* event (State_Cancelled) for this same requestId --
  // without this guard that would invoke `callback` a second time for one
  // chat turn.
  auto done = std::make_shared<bool>(false);
  owner->Bind(
    wxEVT_WEBREQUEST_STATE,
    [requestId, self, callback, done](wxWebRequestEvent &evt) {
      if (evt.GetId() != requestId)
        return;
      if (*done)
        return;
      switch (evt.GetState()) {
      case wxWebRequest::State_Completed: {
        wxWebResponse response = evt.GetResponse();
        int status = response.GetStatus();
        wxString responseBody = response.AsString();
        if ((status < 200) || (status >= 300)) {
          *done = true;
          callback(false, wxString::Format(
                            _("%s returned HTTP %d: %s"), self->Name(), status,
                            responseBody.Left(500)));
          return;
        }
        *done = true;
        try {
          callback(true, self->ParseReply(responseBody));
        } catch (const AiProviderError &e) {
          callback(false, FromU8(e.what()));
        }
        break;
      }
      // A wrong/expired API key surfaces as an HTTP 401 -- every provider
      // here authenticates via a plain header (x-api-key/Authorization/
      // x-goog-api-key), not real HTTP Basic/Digest auth, so there is no
      // wxWebAuthChallenge credential this app could ever supply in
      // response. Left unhandled, the request just sits in this state
      // forever (confirmed live: no further wxEVT_WEBREQUEST_STATE event
      // arrives on its own) since nothing ever answers the challenge --
      // so this must be treated as a terminal failure, the same as a
      // non-2xx State_Completed, and the request explicitly cancelled so
      // it doesn't dangle. GetResponse() is still valid here: the server's
      // full 401 response (status/body) already arrived before wx
      // reinterpreted it as an auth challenge.
      case wxWebRequest::State_Unauthorized: {
        wxWebResponse response = evt.GetResponse();
        wxString detail = response.IsOk() ? response.AsString().Left(500) : wxString();
        *done = true;
        callback(false, wxString::Format(
                          _("%s rejected the API key (HTTP 401): %s"),
                          self->Name(), detail));
        // GetRequest() returns a const reference; wxWebRequest's handle is
        // ref-counted (like wxWebResponse/wxWebSession), so a plain copy
        // shares the same underlying request and Cancel() on it still
        // cancels the one this event is about.
        wxWebRequest req = evt.GetRequest();
        req.Cancel();
        break;
      }
      case wxWebRequest::State_Failed:
        *done = true;
        callback(false, wxString::Format(_("Could not reach %s (network error)."),
                                         self->Name()));
        break;
      case wxWebRequest::State_Cancelled:
        *done = true;
        callback(false, _("Request cancelled."));
        break;
      default:
        break;
      }
    },
    requestId);
  request.Start();
#else
  callback(false, _("This build of wxMaxima was compiled without wxWebRequest "
                    "support, so it cannot talk to any AI provider."));
#endif
}
