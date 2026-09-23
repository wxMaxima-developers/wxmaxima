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
#include <wx/base64.h>
#include <wx/webrequest.h>
#endif
#if wxUSE_SECRETSTORE
#include <wx/secretstore.h>
#endif
#include <random>

#include "StringUtils.h"

using json = nlohmann::json;

namespace {

//! Drops `suffix` from the end of `url` if it is there, then any trailing
//! slashes. Shared by the ModelsRequestUrl() implementations, which all
//! amount to "take the chat endpoint and swap its last segment".
wxString UrlWithoutSuffix(const wxString &url, const wxString &suffix) {
  wxString result = url;
  result.Trim(true).Trim(false);
  if (!suffix.IsEmpty() && result.EndsWith(suffix))
    result = result.Left(result.Length() - suffix.Length());
  while (result.EndsWith(wxS("/")))
    result = result.Left(result.Length() - 1);
  return result;
}

/*! The model ids in an OpenAI-shaped {"data":[{"id":...}]} response.

  Anthropic's own model list happens to use this exact shape too, so both
  of those providers share this and only Google needs its own. */
std::vector<wxString> ParseOpenAiShapedModelList(const wxString &responseBody) {
  try {
    json j = json::parse(wxm::ToUtf8(responseBody));
    if (!j.contains("data") || !j["data"].is_array())
      throw AiProviderError("The model list had no \"data\" array in it.");
    std::vector<wxString> ids;
    for (const auto &entry : j.at("data")) {
      if (!entry.is_object())
        continue;
      const std::string id = entry.value("id", std::string());
      if (!id.empty())
        ids.push_back(wxm::FromUtf8(id));
    }
    return ids;
  } catch (const AiProviderError &) {
    throw;
  } catch (const std::exception &e) {
    throw AiProviderError(std::string("Could not read the model list: ") + e.what());
  }
}

//! Anthropic Messages API (https://api.anthropic.com/v1/messages).
class AnthropicProvider : public AiProvider {
public:
  AnthropicProvider(const wxString &apiKey, const wxString &model,
                    wxString baseUrl = wxS("https://api.anthropic.com/v1/messages"),
                    AiProviderKind kind = AiProviderKind::Anthropic)
    : AiProvider(std::move(baseUrl), apiKey, model), m_kind(kind) {}

  AiProviderKind Kind() const override { return m_kind; }

  std::vector<std::pair<wxString, wxString>> AuthHeaders() const override {
    std::vector<std::pair<wxString, wxString>> headers = {
      {wxS("anthropic-version"), wxS("2023-06-01")}};
    // A Basic-auth proxy in front of this endpoint takes the one stored
    // secret as its password, so there is none left to send as an API key
    // -- see AiCustomProviderConfig::username for why that tradeoff is
    // deliberate rather than an oversight.
    if (UsesBasicAuth()) {
      headers.push_back(BasicAuthHeader());
      return headers;
    }
    // Omitted entirely rather than sent empty: a custom provider using
    // this shape may be a local proxy that authenticates nothing at all,
    // and an empty credential header is likelier to be rejected outright
    // than simply ignored.
    if (!m_apiKey.IsEmpty())
      headers.push_back({wxS("x-api-key"), m_apiKey});
    return headers;
  }

  //! ".../v1/messages" -> ".../v1/models". Confirmed against the real
  //! endpoint: an unauthenticated GET to https://api.anthropic.com/v1/models
  //! answers with Anthropic's own 401 ("x-api-key header is required"),
  //! i.e. the route exists and only wants a key.
  wxString ModelsRequestUrl() const override {
    const wxString stripped = UrlWithoutSuffix(m_baseUrl, wxS("/messages"));
    if (stripped.IsEmpty())
      return wxEmptyString;
    return stripped + wxS("/models");
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json body;
    body["model"] = wxm::ToUtf8(m_model);
    body["max_tokens"] = 2048;
    if (!context.IsEmpty())
      body["system"] = wxm::ToUtf8(context);
    json messages = json::array();
    for (const auto &m : history)
      messages.push_back({{"role", wxm::ToUtf8(m.role)},
                          {"content", wxm::ToUtf8(m.content)}});
    body["messages"] = messages;
    return wxm::FromUtf8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(wxm::ToUtf8(responseBody));
      wxString reply;
      for (const auto &block : j.at("content"))
        if (block.value("type", std::string()) == "text")
          reply += wxm::FromUtf8(block.value("text", std::string()));
      if (reply.IsEmpty())
        throw AiProviderError("Anthropic response had no text content block");
      return reply;
    } catch (const json::exception &e) {
      throw AiProviderError(std::string("Could not parse Anthropic's response: ") + e.what());
    }
  }

private:
  AiProviderKind m_kind;
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
    // A Basic-auth reverse proxy (what Ollama's own docs recommend for an
    // instance reachable beyond localhost) owns the Authorization header,
    // so Bearer cannot also be sent -- there is exactly one of that header
    // and one stored secret, which becomes the Basic password.
    if (UsesBasicAuth())
      return {BasicAuthHeader()};
    // A local OpenAI-compatible server (Ollama, LM Studio, llama.cpp
    // server, ...) has no third party to authenticate to and normally has
    // no API key at all -- send no Authorization header rather than a bare,
    // malformed "Bearer " with nothing after it.
    if (m_apiKey.IsEmpty())
      return {};
    return {{wxS("Authorization"), wxS("Bearer ") + m_apiKey}};
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json messages = json::array();
    if (!context.IsEmpty())
      messages.push_back({{"role", "system"}, {"content", wxm::ToUtf8(context)}});
    for (const auto &m : history)
      messages.push_back({{"role", wxm::ToUtf8(m.role)},
                          {"content", wxm::ToUtf8(m.content)}});
    json body;
    body["model"] = wxm::ToUtf8(m_model);
    body["messages"] = messages;
    return wxm::FromUtf8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(wxm::ToUtf8(responseBody));
      wxString reply = wxm::FromUtf8(
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
  GoogleProvider(const wxString &apiKey, const wxString &model,
                wxString baseUrl = wxS("https://generativelanguage.googleapis.com/v1beta/models/"),
                AiProviderKind kind = AiProviderKind::Google)
    : AiProvider(std::move(baseUrl), apiKey, model), m_kind(kind) {}

  AiProviderKind Kind() const override { return m_kind; }

  wxString RequestUrl() const override {
    return m_baseUrl + m_model + wxS(":generateContent");
  }

  //! Google's list lives at the same ".../v1beta/models" this provider's
  //! base URL already is, minus the trailing slash RequestUrl() needs in
  //! order to append the model id. Confirmed against the real endpoint:
  //! an unauthenticated GET answers 403 "Please use API Key", i.e. the
  //! route exists and only wants a key.
  wxString ModelsRequestUrl() const override {
    return UrlWithoutSuffix(m_baseUrl, wxEmptyString);
  }

  /*! Google answers {"models":[{"name":"models/gemini-...",
    "supportedGenerationMethods":[...]}]} -- a different shape from
    everyone else's, and one that lists models this provider cannot
    actually use (embedding-only ones, for instance), so entries that
    don't advertise generateContent are dropped. An entry that doesn't say
    either way is kept: an unexpected omission should not silently hide a
    model the user may well be able to select. */
  std::vector<wxString> ParseModelList(const wxString &responseBody) const override {
    try {
      json j = json::parse(wxm::ToUtf8(responseBody));
      if (!j.contains("models") || !j["models"].is_array())
        throw AiProviderError("The model list had no \"models\" array in it.");
      std::vector<wxString> ids;
      for (const auto &entry : j.at("models")) {
        if (!entry.is_object())
          continue;
        std::string name = entry.value("name", std::string());
        if (name.empty())
          continue;
        if (entry.contains("supportedGenerationMethods") &&
            entry["supportedGenerationMethods"].is_array()) {
          bool canGenerate = false;
          for (const auto &method : entry.at("supportedGenerationMethods"))
            if (method.is_string() && (method.get<std::string>() == "generateContent"))
              canGenerate = true;
          if (!canGenerate)
            continue;
        }
        // The chat endpoint is built as <base>/<id>:generateContent, and
        // <base> already ends in "models/" -- so the id this UI needs is
        // the bare "gemini-2.0-flash", not the "models/gemini-2.0-flash"
        // Google reports. Leaving the prefix on would build a URL with
        // "models/models/" in it.
        const std::string prefix = "models/";
        if (name.compare(0, prefix.size(), prefix) == 0)
          name = name.substr(prefix.size());
        if (!name.empty())
          ids.push_back(wxm::FromUtf8(name));
      }
      return ids;
    } catch (const AiProviderError &) {
      throw;
    } catch (const std::exception &e) {
      throw AiProviderError(std::string("Could not read the model list: ") + e.what());
    }
  }

  std::vector<std::pair<wxString, wxString>> AuthHeaders() const override {
    // See AnthropicProvider::AuthHeaders(): the proxy's password is the one
    // secret this provider has, so its own key header goes unsent.
    if (UsesBasicAuth())
      return {BasicAuthHeader()};
    if (m_apiKey.IsEmpty())
      return {};
    return {{wxS("x-goog-api-key"), m_apiKey}};
  }

  wxString BuildRequestBody(const wxString &context,
                            const std::vector<AiChatMessage> &history) const override {
    json body;
    if (!context.IsEmpty())
      body["systemInstruction"] =
        {{"parts", json::array({{{"text", wxm::ToUtf8(context)}}})}};
    json contents = json::array();
    for (const auto &m : history) {
      // Gemini calls the AI's own turn "model", not "assistant".
      wxString role = (m.role == wxS("assistant")) ? wxString(wxS("model")) : m.role;
      contents.push_back({{"role", wxm::ToUtf8(role)},
                          {"parts", json::array(
                              {{{"text", wxm::ToUtf8(m.content)}}})}});
    }
    body["contents"] = contents;
    return wxm::FromUtf8(body.dump());
  }

  wxString ParseReply(const wxString &responseBody) const override {
    try {
      json j = json::parse(wxm::ToUtf8(responseBody));
      wxString reply;
      for (const auto &part : j.at("candidates").at(0).at("content").at("parts"))
        reply += wxm::FromUtf8(part.value("text", std::string()));
      if (reply.IsEmpty())
        throw AiProviderError("Gemini response had no text part");
      return reply;
    } catch (const json::exception &e) {
      throw AiProviderError(std::string("Could not parse Gemini's response: ") + e.what());
    }
  }

private:
  AiProviderKind m_kind;
};
} // namespace

wxString AiProviderKindName(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("Anthropic (Claude)");
  case AiProviderKind::OpenAI: return wxS("OpenAI");
  case AiProviderKind::Google: return wxS("Google (Gemini)");
  case AiProviderKind::Qwen: return wxS("Qwen (Alibaba)");
  case AiProviderKind::GitHubModels: return wxS("GitHub Models");
  case AiProviderKind::DeepSeek: return wxS("DeepSeek");
  case AiProviderKind::OpenRouter: return wxS("OpenRouter");
  // A Custom provider's own display name is carried on the AiProvider
  // instance itself (AiProvider::SetDisplayName()/Name()), not derivable
  // from the kind alone -- this generic fallback is only ever seen if
  // something asks for a bare kind name without going through an actual
  // instance (e.g. a log message), so it says what it is.
  case AiProviderKind::Custom: return wxS("Custom provider");
  default: return wxS("None");
  }
}

wxString AiProviderShapeName(AiProviderShape shape) {
  switch (shape) {
  case AiProviderShape::Anthropic: return _("Anthropic (Messages API)");
  case AiProviderShape::Google: return _("Google Gemini (generateContent)");
  case AiProviderShape::OpenAiCompatible:
  default: return _("OpenAI-compatible (most third-party/self-hosted APIs)");
  }
}

// Keep these two in sync with each other AND with the order the shape
// picker's items are actually added in ConfigDialogue (both places:
// CreateAiChatPanel()'s detail box and AddCustomAiProviderDialog()).
int AiProviderShapeToChoiceIndex(AiProviderShape shape) {
  switch (shape) {
  case AiProviderShape::Anthropic: return 1;
  case AiProviderShape::Google: return 2;
  case AiProviderShape::OpenAiCompatible:
  default: return 0;
  }
}

AiProviderShape AiProviderShapeFromChoiceIndex(int index) {
  switch (index) {
  case 1: return AiProviderShape::Anthropic;
  case 2: return AiProviderShape::Google;
  case 0:
  default: return AiProviderShape::OpenAiCompatible;
  }
}

wxString AiProviderRequestUrlProblem(const wxString &url) {
  wxString trimmed = url;
  trimmed.Trim(true).Trim(false);
  if (trimmed.IsEmpty())
    return _("Please enter the full request URL this provider expects.");
  // Compared case-insensitively: a scheme is case-insensitive per RFC 3986,
  // and "HTTP://..." pasted from somewhere would otherwise be rejected for
  // no reason.
  wxString lower = trimmed.Lower();
  if (!lower.StartsWith(wxS("http://")) && !lower.StartsWith(wxS("https://")))
    return _("The request URL has to start with \"http://\" or \"https://\" "
            "-- a bare host and port such as \"127.0.0.1:11434\" is not a "
            "complete URL. For a local Ollama server the full URL is "
            "\"http://localhost:11434/v1/chat/completions\"; the \"Quick "
            "fill\" list above fills this in for the most common local "
            "servers.");
  // Anything after the scheme is a host: "http://" alone is not.
  if (lower == wxS("http://") || lower == wxS("https://"))
    return _("The request URL is missing everything after the \"://\".");
  return wxEmptyString;
}

wxString AiProviderDefaultModel(AiProviderKind kind) {
  // A model id going stale is not a cosmetic problem: Anthropic's API
  // answers an id it does not recognise with a bare HTTP 404, which is
  // what an out-of-date default here looked like to a user -- a "could
  // not reach Anthropic" that had in fact reached it perfectly well.
  // Each of these is that provider's own "rolling" alias -- it always
  // resolves to the current snapshot of that model line, rather than a
  // pinned dated snapshot (e.g. Anthropic's own "claude-3-5-sonnet-
  // 20241022", which this used to be) that provider eventually retires on
  // its own schedule regardless of what this code does. This only pushes
  // the staleness problem up one level, from "this specific snapshot got
  // retired" to "this whole model line got superseded" -- a real, slower-
  // moving case a plain string constant genuinely cannot track by itself
  // (see AiProviderModelListUrl() -- the actual fix for that is a link to
  // the provider's own current list, not a fancier detection mechanism
  // trying to chase a moving target with another moving target).
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("claude-sonnet-5");
  case AiProviderKind::OpenAI: return wxS("gpt-4o-mini");
  case AiProviderKind::Google: return wxS("gemini-1.5-flash");
  case AiProviderKind::Qwen: return wxS("qwen-plus");
  // GitHub Models' catalog names models "<publisher>/<model>"; this one is
  // consistently free-tier-available, unlike some of the larger catalog
  // entries which need a paid Models plan.
  case AiProviderKind::GitHubModels: return wxS("openai/gpt-4o-mini");
  // DeepSeek's own alias for its current general chat model (the other one,
  // "deepseek-reasoner", thinks before answering: slower, and billed for
  // the thinking).
  case AiProviderKind::DeepSeek: return wxS("deepseek-chat");
  // OpenRouter's own router, which picks a model per request. The one
  // default here that cannot go stale, since it names no model at all.
  case AiProviderKind::OpenRouter: return wxS("openrouter/auto");
  default: return wxEmptyString;
  }
}

wxString AiProviderApiKeyUrl(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("https://console.anthropic.com/settings/keys");
  case AiProviderKind::OpenAI: return wxS("https://platform.openai.com/api-keys");
  case AiProviderKind::Google: return wxS("https://aistudio.google.com/apikey");
  case AiProviderKind::Qwen: return wxS("https://dashscope.console.aliyun.com/apiKey");
  // A fine-grained personal access token with "Models" (read-only)
  // permission -- GitHub Models is the one built-in provider here whose
  // "API key" is a real GitHub credential, not a service-specific one; see
  // AiProviderKind's own doc comment for why this is GitHub Models, not
  // GitHub Copilot Chat.
  case AiProviderKind::GitHubModels:
    return wxS("https://github.com/settings/personal-access-tokens/new");
  case AiProviderKind::DeepSeek: return wxS("https://platform.deepseek.com/api_keys");
  case AiProviderKind::OpenRouter: return wxS("https://openrouter.ai/settings/keys");
  default: return wxEmptyString;
  }
}

wxString AiProviderModelListUrl(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic:
    return wxS("https://docs.anthropic.com/en/docs/about-claude/models");
  case AiProviderKind::OpenAI: return wxS("https://platform.openai.com/docs/models");
  case AiProviderKind::Google: return wxS("https://ai.google.dev/gemini-api/docs/models");
  case AiProviderKind::Qwen: return wxS("https://www.alibabacloud.com/help/en/model-studio/models");
  case AiProviderKind::GitHubModels: return wxS("https://github.com/marketplace/models");
  case AiProviderKind::DeepSeek:
    return wxS("https://api-docs.deepseek.com/quick_start/pricing");
  case AiProviderKind::OpenRouter: return wxS("https://openrouter.ai/models");
  default: return wxEmptyString;
  }
}

wxString AiProviderBaseUrl(AiProviderKind kind) {
  switch (kind) {
  case AiProviderKind::Anthropic: return wxS("https://api.anthropic.com/v1/messages");
  case AiProviderKind::OpenAI: return wxS("https://api.openai.com/v1/chat/completions");
  case AiProviderKind::Qwen:
    return wxS("https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions");
  case AiProviderKind::Google:
    return wxS("https://generativelanguage.googleapis.com/v1beta/models/");
  case AiProviderKind::GitHubModels:
    return wxS("https://models.github.ai/inference/chat/completions");
  case AiProviderKind::DeepSeek: return wxS("https://api.deepseek.com/chat/completions");
  case AiProviderKind::OpenRouter:
    return wxS("https://openrouter.ai/api/v1/chat/completions");
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
  case AiProviderKind::GitHubModels:
    // Officially documented, OpenAI-compatible-shaped endpoint
    // (https://docs.github.com/en/github-models), authenticated with a
    // plain GitHub personal access token via "Authorization: Bearer" --
    // exactly what OpenAiCompatibleProvider already sends, so no new
    // provider class is needed.
    return std::make_shared<OpenAiCompatibleProvider>(
      AiProviderKind::GitHubModels,
      wxS("https://models.github.ai/inference/chat/completions"), apiKey, model);
  case AiProviderKind::DeepSeek:
  case AiProviderKind::OpenRouter:
    // Both speak OpenAI's chat/completions shape with a Bearer key, so like
    // GitHub Models they need nothing but their own URL. Taken from
    // AiProviderBaseUrl() rather than written out a second time: a second
    // copy of a constant is how Configuration's model defaults once drifted
    // away from AiProviderDefaultModel() without anyone noticing.
    return std::make_shared<OpenAiCompatibleProvider>(kind, AiProviderBaseUrl(kind),
                                                      apiKey, model);
  default:
    return nullptr;
  }
}

std::shared_ptr<AiProvider> MakeAiProviderForShape(AiProviderShape shape,
                                                   const wxString &displayName,
                                                   const wxString &baseUrl,
                                                   const wxString &apiKey,
                                                   const wxString &model,
                                                   const wxString &basicAuthUser) {
  std::shared_ptr<AiProvider> provider;
  switch (shape) {
  case AiProviderShape::Anthropic:
    provider = std::make_shared<AnthropicProvider>(apiKey, model, baseUrl,
                                                    AiProviderKind::Custom);
    break;
  case AiProviderShape::Google:
    provider = std::make_shared<GoogleProvider>(apiKey, model, baseUrl,
                                                 AiProviderKind::Custom);
    break;
  case AiProviderShape::OpenAiCompatible:
  default:
    provider = std::make_shared<OpenAiCompatibleProvider>(AiProviderKind::Custom, baseUrl,
                                                           apiKey, model);
    break;
  }
  provider->SetDisplayName(displayName);
  provider->SetBasicAuthUser(basicAuthUser);
  return provider;
}

std::vector<AiLocalServerPreset> AiKnownLocalServerPresets() {
  return {
    // Ollama's OpenAI-compatible endpoint: http://localhost:11434/v1/chat/completions
    // (its native /api/chat endpoint uses a different, non-OpenAI-shaped
    // wire format, so the /v1/ prefix specifically is what OpenAiCompatibleProvider needs).
    {wxS("Ollama"), wxS("http://localhost:11434/v1/chat/completions"), wxS("llama3.2")},
    // LM Studio's built-in local server, OpenAI-compatible by design.
    {wxS("LM Studio"), wxS("http://localhost:1234/v1/chat/completions"), wxS("local-model")},
    // llama.cpp's own `llama-server` also speaks the OpenAI-compatible shape.
    {wxS("llama.cpp server"), wxS("http://localhost:8080/v1/chat/completions"), wxS("local-model")},
  };
}

wxString NewAiCustomProviderId() {
  // Never shown to the user and never needs to survive comparison with
  // anything outside this installation -- a short random hex string is
  // enough to make collisions between two providers added on the same
  // machine practically impossible, without pulling in a real UUID
  // library for something this low-stakes (a collision just means two
  // custom providers would share one stored API key, not a crash or data
  // loss, and even that has never been observed across normal use).
  std::random_device rd;
  std::mt19937_64 gen(rd());
  std::uniform_int_distribution<uint64_t> dist;
  return wxString::Format(wxS("%016" wxLongLongFmtSpec "x"),
                          static_cast<wxLongLong_t>(dist(gen)));
}

std::vector<AiCustomProviderConfig> ParseAiCustomProviders(const wxString &jsonText) {
  std::vector<AiCustomProviderConfig> result;
  if (jsonText.IsEmpty())
    return result;
  try {
    json j = json::parse(wxm::ToUtf8(jsonText));
    if (!j.is_array())
      return result;
    for (const auto &entry : j) {
      AiCustomProviderConfig cfg;
      cfg.id = wxm::FromUtf8(entry.value("id", std::string()));
      cfg.name = wxm::FromUtf8(entry.value("name", std::string()));
      cfg.baseUrl = wxm::FromUtf8(entry.value("baseUrl", std::string()));
      cfg.model = wxm::FromUtf8(entry.value("model", std::string()));
      // Absent in every entry written before Basic auth existed, which
      // value()'s default handles: no username means no Basic auth, i.e.
      // exactly the behaviour those entries already had.
      cfg.username = wxm::FromUtf8(entry.value("username", std::string()));
      std::string shape = entry.value("shape", std::string("openai"));
      if (shape == "anthropic")
        cfg.shape = AiProviderShape::Anthropic;
      else if (shape == "google")
        cfg.shape = AiProviderShape::Google;
      else
        cfg.shape = AiProviderShape::OpenAiCompatible;
      // An entry with no id is unusable (it could never be matched back to
      // a saved API key or to Configuration::AiActiveCustomProviderId()) --
      // silently drop it rather than fabricating one, since it can only
      // come from a corrupted/hand-edited config file, not normal use.
      if (!cfg.id.IsEmpty())
        result.push_back(cfg);
    }
  } catch (const json::exception &) {
    // Malformed JSON (e.g. a hand-edited or truncated config file) -- fall
    // back to "no custom providers configured" rather than crashing Options
    // on open, same reasoning as the empty-string early return above.
    return {};
  }
  return result;
}

wxString SerializeAiCustomProviders(const std::vector<AiCustomProviderConfig> &providers) {
  json arr = json::array();
  for (const auto &cfg : providers) {
    std::string shape;
    switch (cfg.shape) {
    case AiProviderShape::Anthropic: shape = "anthropic"; break;
    case AiProviderShape::Google: shape = "google"; break;
    case AiProviderShape::OpenAiCompatible:
    default: shape = "openai"; break;
    }
    arr.push_back({{"id", wxm::ToUtf8(cfg.id)},
                   {"name", wxm::ToUtf8(cfg.name)},
                   {"shape", shape},
                   {"baseUrl", wxm::ToUtf8(cfg.baseUrl)},
                   {"model", wxm::ToUtf8(cfg.model)},
                   {"username", wxm::ToUtf8(cfg.username)}});
  }
  return wxm::FromUtf8(arr.dump());
}

std::pair<wxString, wxString> AiProvider::BasicAuthHeader() const {
  // RFC 7617: base64 of "user:password", over the UTF-8 bytes. The colon
  // separates at the *first* occurrence, so a password containing one is
  // fine while a username containing one is not encodable at all -- that
  // is the spec's own limitation, not something to work around here.
  const std::string credentials =
    wxm::ToUtf8(m_basicAuthUser) + ":" + wxm::ToUtf8(m_apiKey);
  return {wxS("Authorization"),
          wxS("Basic ") + wxBase64Encode(credentials.data(), credentials.length())};
}

wxString AiProvider::CustomProviderSecretService(const wxString &id) {
  return wxS("custom:") + id;
}

wxString AiProvider::BuiltinProviderSecretService(AiProviderKind kind) {
  return AiProviderKindName(kind);
}

bool AiProvider::SecretStoreAvailable() {
#if wxUSE_SECRETSTORE
  return wxSecretStore::GetDefault().IsOk();
#else
  return false;
#endif
}

void AiProvider::SaveApiKey(const wxString &service, const wxString &apiKey) {
#if wxUSE_SECRETSTORE
  wxSecretStore store = wxSecretStore::GetDefault();
  if (!store.IsOk())
    return;
  // An empty key means "cleared in Options" -- delete rather than saving an
  // empty secret, so LoadApiKey() and "is a key configured at all" checks
  // elsewhere don't need to special-case an empty-but-present entry.
  if (apiKey.IsEmpty()) {
    store.Delete(wxS("wxMaxima/AI/") + service);
    return;
  }
  store.Save(wxS("wxMaxima/AI/") + service, wxS("apikey"),
            wxSecretValue(apiKey));
#else
  wxUnusedVar(service);
  wxUnusedVar(apiKey);
#endif
}

wxString AiProvider::LoadApiKey(const wxString &service) {
#if wxUSE_SECRETSTORE
  wxSecretStore store = wxSecretStore::GetDefault();
  if (!store.IsOk())
    return wxEmptyString;
  wxString user;
  wxSecretValue secret;
  if (!store.Load(wxS("wxMaxima/AI/") + service, user, secret))
    return wxEmptyString;
  return secret.GetAsString();
#else
  wxUnusedVar(service);
  return wxEmptyString;
#endif
}

void AiProvider::DeleteApiKey(const wxString &service) {
#if wxUSE_SECRETSTORE
  wxSecretStore store = wxSecretStore::GetDefault();
  if (store.IsOk())
    store.Delete(wxS("wxMaxima/AI/") + service);
#else
  wxUnusedVar(service);
#endif
}

bool AiProvider::NetworkingAvailable() {
#if wxUSE_WEBREQUEST
  return true;
#else
  return false;
#endif
}

wxString AiProvider::ModelsRequestUrl() const {
  const wxString stripped = UrlWithoutSuffix(m_baseUrl, wxS("/chat/completions"));
  if (stripped.IsEmpty())
    return wxEmptyString;
  return stripped + wxS("/models");
}

std::vector<wxString> AiProvider::ParseModelList(const wxString &responseBody) const {
  return ParseOpenAiShapedModelList(responseBody);
}

void AiProvider::FetchModels(
  std::shared_ptr<const AiProvider> self, wxEvtHandler *owner,
  std::function<void(bool ok, const std::vector<wxString> &models,
                     const wxString &errorIfAny)> callback) {
  const std::vector<wxString> none;
  if (!self || !owner) {
    callback(false, none, _("No provider to ask."));
    return;
  }
  const wxString url = self->ModelsRequestUrl();
  if (url.IsEmpty()) {
    callback(false, none,
             wxString::Format(_("%s does not offer a list of models."), self->Name()));
    return;
  }
#if wxUSE_WEBREQUEST
  // Everything below mirrors SendChat() deliberately -- the unique
  // wxWindowIDRef, the shared_ptr capture, the `done` guard and the
  // State_Unauthorized case are all load-bearing for the same reasons
  // documented there at length, and an "it's only a GET, it can be
  // simpler" version of this would reintroduce every one of those bugs.
  wxWindowIDRef requestId = wxWindow::NewControlId();
  wxWebRequest request = wxWebSession::GetDefault().CreateRequest(owner, url, requestId);
  if (!request.IsOk()) {
    callback(false, none, _("Could not create the HTTP request."));
    return;
  }
  for (const auto &header : self->AuthHeaders())
    request.SetHeader(header.first, header.second);
  request.SetMethod(wxS("GET"));

  // `done` is not optional, for the same reason SendChat() needs one: the
  // State_Unauthorized case answers by cancelling the request, and that
  // cancellation raises a *second* terminal event (State_Cancelled) for
  // this same id. Without the guard the caller's callback would fire
  // twice for one fetch -- once with the 401 and once with "cancelled",
  // the second overwriting the first and hiding the real reason.
  auto done = std::make_shared<bool>(false);
  owner->Bind(
    wxEVT_WEBREQUEST_STATE,
    [requestId, self, callback, done](wxWebRequestEvent &evt) {
      static const std::vector<wxString> empty;
      if (evt.GetId() != requestId)
        return;
      if (*done)
        return;
      switch (evt.GetState()) {
      case wxWebRequest::State_Completed: {
        wxWebResponse response = evt.GetResponse();
        const int status = response.GetStatus();
        const wxString responseBody = response.AsString();
        *done = true;
        if ((status < 200) || (status >= 300)) {
          callback(false, empty,
                   wxString::Format(_("%s returned HTTP %d: %s"), self->Name(), status,
                                    responseBody.Left(300)));
          return;
        }
        try {
          callback(true, self->ParseModelList(responseBody), wxEmptyString);
        } catch (const AiProviderError &e) {
          callback(false, empty, wxm::FromUtf8(e.what()));
        }
        break;
      }
      // See SendChat()'s own State_Unauthorized case: a 401 is diverted
      // here rather than reported as a non-2xx completion, and a request
      // left sitting in this state never produces another event on its
      // own, so it has to be treated as terminal and cancelled explicitly.
      case wxWebRequest::State_Unauthorized: {
        wxWebResponse response = evt.GetResponse();
        const wxString detail = response.IsOk() ? response.AsString().Left(300) : wxString();
        *done = true;
        callback(false, empty,
                 wxString::Format(_("%s rejected the API key (HTTP 401): %s"),
                                  self->Name(), detail));
        wxWebRequest req = evt.GetRequest();
        req.Cancel();
        break;
      }
      case wxWebRequest::State_Failed:
        *done = true;
        callback(false, empty, evt.GetErrorDescription());
        break;
      case wxWebRequest::State_Cancelled:
        *done = true;
        callback(false, empty, _("The request for the model list was cancelled."));
        break;
      default:
        // Active/Idle: not terminal, nothing to report yet.
        return;
      }
    });
  request.Start();
#else
  callback(false, none,
           _("This build of wxMaxima has no network support compiled in."));
#endif
}

AiRequestCanceller AiProvider::SendChat(
  std::shared_ptr<const AiProvider> self, wxEvtHandler *owner, const wxString &context,
  const std::vector<AiChatMessage> &history,
  std::function<void(bool ok, const wxString &replyOrError)> callback,
  std::function<void(const wxString &requestBody)> onRequest,
  std::function<void(bool ok, const wxString &responseBodyOrDetail)> onResponse) {
#if wxUSE_WEBREQUEST
  wxString body = self->BuildRequestBody(context, history);
  if (onRequest)
    onRequest(body);
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
    wxString msg = _("Could not create the HTTP request.");
    if (onResponse)
      onResponse(false, msg);
    callback(false, msg);
    // Nothing was started, so there is nothing to cancel -- an empty
    // canceller is how the caller is told that.
    return nullptr;
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
    [requestId, self, callback, onResponse, done](wxWebRequestEvent &evt) {
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
          if (onResponse)
            onResponse(false, responseBody);
          callback(false, wxString::Format(
                            _("%s returned HTTP %d: %s"), self->Name(), status,
                            responseBody.Left(500)));
          return;
        }
        *done = true;
        try {
          wxString reply = self->ParseReply(responseBody);
          if (onResponse)
            onResponse(true, responseBody);
          callback(true, reply);
        } catch (const AiProviderError &e) {
          if (onResponse)
            onResponse(false, responseBody);
          callback(false, wxm::FromUtf8(e.what()));
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
        if (onResponse)
          onResponse(false, detail);
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
      // The bare "network error" this used to report was useless in
      // practice: "could not resolve the host name", "connection refused"
      // (a local LLM server that isn't running, or listening on a
      // different port), "the TLS certificate could not be verified" and
      // "the connection died mid-transfer" are four completely different
      // problems with four completely different fixes, and the backend
      // already distinguishes them for us. GetErrorDescription() is what
      // carries that text (for the curl backend, libcurl's own error
      // string) -- pass it on verbatim rather than throwing it away.
      case wxWebRequest::State_Failed: {
        *done = true;
        wxString detail = evt.GetErrorDescription();
        // A failed request can still have received a perfectly real HTTP
        // response: not every backend routes an error status through
        // State_Completed, and the ones that don't leave GetErrorDescription()
        // saying little more than the status number. The response body is
        // where the provider actually explains itself -- an unknown model id,
        // for instance, is an otherwise-unexplained HTTP 404 whose body names
        // the model. Reporting the description alone threw that away.
        wxWebResponse response = evt.GetResponse();
        if (response.IsOk()) {
          // Only when the backend told us nothing: its description normally
          // names the status already, and repeating it reads as two separate
          // failures rather than one.
          const int status = response.GetStatus();
          if (detail.IsEmpty() && (status > 0))
            detail = wxString::Format(wxS("HTTP %d"), status);
          const wxString body = response.AsString().Left(500);
          if (!body.IsEmpty())
            detail += wxS(": ") + body;
        }
        detail.Trim().Trim(false);
        wxString msg =
          detail.IsEmpty()
          ? wxString::Format(_("Could not reach %s (network error)."), self->Name())
          : wxString::Format(_("Could not reach %s: %s"), self->Name(), detail);
        if (onResponse)
          onResponse(false, msg);
        callback(false, msg);
        break;
      }
      case wxWebRequest::State_Cancelled:
        *done = true;
        if (onResponse)
          onResponse(false, _("Request cancelled."));
        callback(false, _("Request cancelled."));
        break;
      default:
        break;
      }
    },
    requestId);
  request.Start();
  // `request` is a ref-counted handle, so this copy refers to the same
  // request and keeps it alive for as long as the caller holds on to the
  // canceller. The same `done` flag the handler above uses guards it:
  // once any terminal state has been reported, cancelling is a no-op
  // rather than a call into a request that has already finished.
  return [request, done]() mutable {
    if (*done)
      return;
    request.Cancel();
  };
#else
  wxString msg = _("This build of wxMaxima was compiled without wxWebRequest "
                   "support, so it cannot talk to any AI provider.");
  if (onResponse)
    onResponse(false, msg);
  callback(false, msg);
  return nullptr;
#endif
}
