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
  Pins each AiProvider's request/response shape -- the stateless half of
  AiProvider (BuildRequestBody()/ParseReply(), pure JSON in and out, see
  AiProvider.h for why this split exists). The actual network call
  (SendChat(), wxWebRequest-based) is not exercised here -- no live network
  access or API keys are available in this sandbox; verified live instead
  in a real Xvfb session (see AGENTS.md's AI chat sidebar entry).
*/

#include "ai/AiProvider.cpp"

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

namespace {
std::vector<AiChatMessage> OneUserTurn(const wxString &text) {
  return {{wxS("user"), text}};
}
} // namespace

SCENARIO("Anthropic's Messages API request/response shape") {
  auto provider = MakeAiProvider(AiProviderKind::Anthropic, wxS("sk-ant-test"),
                                 wxS("claude-3-5-sonnet-20241022"));
  REQUIRE(provider);

  GIVEN("a system context and one user message") {
    wxString body = provider->BuildRequestBody(wxS("worksheet context"),
                                               OneUserTurn(wxS("What is 2+2?")));
    json parsed = json::parse(std::string(body.ToUTF8()));
    THEN("the body has Anthropic's model/system/messages shape") {
      CHECK(parsed.at("model") == "claude-3-5-sonnet-20241022");
      CHECK(parsed.at("system") == "worksheet context");
      REQUIRE(parsed.at("messages").size() == 1);
      CHECK(parsed.at("messages")[0].at("role") == "user");
      CHECK(parsed.at("messages")[0].at("content") == "What is 2+2?");
    }
  }

  GIVEN("the x-api-key/anthropic-version headers") {
    THEN("both are present with the right values") {
      auto headers = provider->AuthHeaders();
      bool sawKey = false, sawVersion = false;
      for (const auto &h : headers) {
        if (h.first == wxS("x-api-key")) {
          CHECK(h.second == wxS("sk-ant-test"));
          sawKey = true;
        }
        if (h.first == wxS("anthropic-version")) {
          CHECK(h.second == wxS("2023-06-01"));
          sawVersion = true;
        }
      }
      CHECK(sawKey);
      CHECK(sawVersion);
    }
  }

  GIVEN("a realistic successful response") {
    wxString response = wxS(
      R"({"content":[{"type":"text","text":"4"}],"id":"msg_1","model":"claude-3-5-sonnet-20241022"})");
    THEN("ParseReply() extracts the text block") {
      CHECK(provider->ParseReply(response) == wxS("4"));
    }
  }

  GIVEN("a response with no text content block") {
    wxString response = wxS(R"({"content":[]})");
    THEN("ParseReply() throws AiProviderError, not a crash or an empty reply") {
      CHECK_THROWS_AS(provider->ParseReply(response), AiProviderError);
    }
  }

  GIVEN("a response that isn't even valid JSON") {
    THEN("ParseReply() throws AiProviderError") {
      CHECK_THROWS_AS(provider->ParseReply(wxS("not json at all")), AiProviderError);
    }
  }
}

SCENARIO("OpenAI's (and Qwen's identical) chat/completions shape") {
  auto provider = MakeAiProvider(AiProviderKind::OpenAI, wxS("sk-test"),
                                 wxS("gpt-4o-mini"));
  REQUIRE(provider);

  GIVEN("a system context and one user message") {
    wxString body = provider->BuildRequestBody(wxS("worksheet context"),
                                               OneUserTurn(wxS("Hello")));
    json parsed = json::parse(std::string(body.ToUTF8()));
    THEN("context becomes the first, system-role message") {
      CHECK(parsed.at("model") == "gpt-4o-mini");
      REQUIRE(parsed.at("messages").size() == 2);
      CHECK(parsed.at("messages")[0].at("role") == "system");
      CHECK(parsed.at("messages")[0].at("content") == "worksheet context");
      CHECK(parsed.at("messages")[1].at("role") == "user");
    }
  }

  GIVEN("no context at all") {
    wxString body = provider->BuildRequestBody(wxEmptyString, OneUserTurn(wxS("Hi")));
    json parsed = json::parse(std::string(body.ToUTF8()));
    THEN("no system message is added") {
      REQUIRE(parsed.at("messages").size() == 1);
      CHECK(parsed.at("messages")[0].at("role") == "user");
    }
  }

  GIVEN("the Bearer auth header") {
    THEN("it carries the API key") {
      auto headers = provider->AuthHeaders();
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
      CHECK(headers[0].second == wxS("Bearer sk-test"));
    }
  }

  GIVEN("a realistic successful response") {
    wxString response = wxS(
      R"({"choices":[{"message":{"role":"assistant","content":"Hi there!"}}]})");
    THEN("ParseReply() extracts choices[0].message.content") {
      CHECK(provider->ParseReply(response) == wxS("Hi there!"));
    }
  }

  GIVEN("Qwen, via DashScope's OpenAI-compatible endpoint") {
    auto qwen = MakeAiProvider(AiProviderKind::Qwen, wxS("qwen-key"), wxS("qwen-plus"));
    THEN("it points at DashScope's compatible-mode URL, not OpenAI's") {
      CHECK(qwen->RequestUrl() ==
           wxS("https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"));
    }
    AND_THEN("its request/response shape is identical to OpenAI's") {
      wxString body = qwen->BuildRequestBody(wxEmptyString, OneUserTurn(wxS("Hi")));
      json parsed = json::parse(std::string(body.ToUTF8()));
      CHECK(parsed.at("model") == "qwen-plus");
      CHECK(qwen->ParseReply(
              wxS(R"({"choices":[{"message":{"content":"ok"}}]})")) == wxS("ok"));
    }
  }
}

SCENARIO("GitHub Models -- GitHub's own official, OpenAI-compatible model API") {
  auto provider = MakeAiProvider(AiProviderKind::GitHubModels, wxS("github_pat_test"),
                                 wxS("openai/gpt-4o-mini"));
  REQUIRE(provider);

  GIVEN("its request URL") {
    THEN("it points at GitHub's own Models inference endpoint, not Copilot's") {
      CHECK(provider->RequestUrl() ==
           wxS("https://models.github.ai/inference/chat/completions"));
    }
  }

  GIVEN("the Bearer auth header") {
    THEN("it carries the personal access token, the same way OpenAI's does") {
      auto headers = provider->AuthHeaders();
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
      CHECK(headers[0].second == wxS("Bearer github_pat_test"));
    }
  }

  GIVEN("a system context and one user message") {
    wxString body = provider->BuildRequestBody(wxS("worksheet context"),
                                               OneUserTurn(wxS("Hello")));
    json parsed = json::parse(std::string(body.ToUTF8()));
    THEN("its request/response shape is identical to OpenAI's own") {
      CHECK(parsed.at("model") == "openai/gpt-4o-mini");
      REQUIRE(parsed.at("messages").size() == 2);
      CHECK(parsed.at("messages")[0].at("role") == "system");
      CHECK(provider->ParseReply(
              wxS(R"({"choices":[{"message":{"content":"ok"}}]})")) == wxS("ok"));
    }
  }
}

SCENARIO("Google Gemini's generateContent shape") {
  auto provider = MakeAiProvider(AiProviderKind::Google, wxS("goog-key"),
                                 wxS("gemini-1.5-flash"));
  REQUIRE(provider);

  GIVEN("the model-specific request URL") {
    THEN("it embeds the model id and :generateContent") {
      CHECK(provider->RequestUrl() ==
           wxS("https://generativelanguage.googleapis.com/v1beta/models/"
               "gemini-1.5-flash:generateContent"));
    }
  }

  GIVEN("the x-goog-api-key header") {
    THEN("it carries the API key") {
      auto headers = provider->AuthHeaders();
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("x-goog-api-key"));
      CHECK(headers[0].second == wxS("goog-key"));
    }
  }

  GIVEN("a system context and a two-turn conversation") {
    std::vector<AiChatMessage> history = {{wxS("user"), wxS("Hi")},
                                          {wxS("assistant"), wxS("Hello!")},
                                          {wxS("user"), wxS("What's 2+2?")}};
    wxString body = provider->BuildRequestBody(wxS("worksheet context"), history);
    json parsed = json::parse(std::string(body.ToUTF8()));
    THEN("context becomes systemInstruction, and 'assistant' becomes 'model'") {
      CHECK(parsed.at("systemInstruction").at("parts")[0].at("text") ==
           "worksheet context");
      REQUIRE(parsed.at("contents").size() == 3);
      CHECK(parsed.at("contents")[0].at("role") == "user");
      CHECK(parsed.at("contents")[1].at("role") == "model");
      CHECK(parsed.at("contents")[2].at("parts")[0].at("text") == "What's 2+2?");
    }
  }

  GIVEN("a realistic successful response") {
    wxString response = wxS(
      R"({"candidates":[{"content":{"role":"model","parts":[{"text":"4"}]}}]})");
    THEN("ParseReply() extracts candidates[0].content.parts[0].text") {
      CHECK(provider->ParseReply(response) == wxS("4"));
    }
  }

  GIVEN("multiple text parts in one candidate") {
    wxString response = wxS(
      R"({"candidates":[{"content":{"parts":[{"text":"foo"},{"text":"bar"}]}}]})");
    THEN("ParseReply() concatenates every part's text") {
      CHECK(provider->ParseReply(response) == wxS("foobar"));
    }
  }
}

SCENARIO("MakeAiProvider(AiProviderKind::None, ...) returns nullptr") {
  THEN("no provider object is created for the disabled/unconfigured state") {
    CHECK(MakeAiProvider(AiProviderKind::None, wxEmptyString, wxEmptyString) == nullptr);
  }
}

SCENARIO("MakeAiProviderForShape() reuses each shape's real implementation "
        "against an arbitrary URL") {
  GIVEN("an OpenAI-compatible custom provider (e.g. a self-hosted Ollama/OpenRouter endpoint)") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::OpenAiCompatible, wxS("My Local Ollama"),
      wxS("http://localhost:11434/v1/chat/completions"), wxS("unused"), wxS("llama3"));
    REQUIRE(provider);
    THEN("Kind() is Custom, but Name() is the user's own display name") {
      CHECK(provider->Kind() == AiProviderKind::Custom);
      CHECK(provider->Name() == wxS("My Local Ollama"));
    }
    THEN("RequestUrl() is exactly the URL given, and auth is Bearer") {
      CHECK(provider->RequestUrl() == wxS("http://localhost:11434/v1/chat/completions"));
      auto headers = provider->AuthHeaders();
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
    }
    THEN("the request body is the same OpenAI chat-completions shape") {
      wxString body = provider->BuildRequestBody(wxEmptyString, OneUserTurn(wxS("hi")));
      json parsed = json::parse(std::string(body.ToUTF8()));
      CHECK(parsed.at("model") == "llama3");
      CHECK(parsed.at("messages")[0].at("role") == "user");
    }
  }

  GIVEN("an Anthropic-shaped custom provider") {
    auto provider = MakeAiProviderForShape(AiProviderShape::Anthropic, wxS("My Anthropic Proxy"),
                                           wxS("https://proxy.example.com/v1/messages"),
                                           wxS("sk-ant-test"), wxS("claude-3-5-sonnet-latest"));
    REQUIRE(provider);
    THEN("it uses the real Anthropic request shape and auth header, at the custom URL") {
      CHECK(provider->RequestUrl() == wxS("https://proxy.example.com/v1/messages"));
      auto headers = provider->AuthHeaders();
      bool hasApiKeyHeader = false;
      for (const auto &h : headers)
        if (h.first == wxS("x-api-key"))
          hasApiKeyHeader = true;
      CHECK(hasApiKeyHeader);
      wxString body = provider->BuildRequestBody(wxEmptyString, OneUserTurn(wxS("hi")));
      json parsed = json::parse(std::string(body.ToUTF8()));
      CHECK(parsed.at("model") == "claude-3-5-sonnet-latest");
      CHECK(parsed.contains("max_tokens"));
    }
  }

  GIVEN("a Google-shaped custom provider") {
    auto provider = MakeAiProviderForShape(AiProviderShape::Google, wxS("My Gemini Proxy"),
                                           wxS("https://proxy.example.com/models/"),
                                           wxS("goog-key"), wxS("gemini-1.5-flash"));
    REQUIRE(provider);
    THEN("RequestUrl() still appends <model>:generateContent, at the custom base") {
      CHECK(provider->RequestUrl() ==
           wxS("https://proxy.example.com/models/gemini-1.5-flash:generateContent"));
      auto headers = provider->AuthHeaders();
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("x-goog-api-key"));
    }
  }
}

SCENARIO("AiCustomProviderConfig JSON round-trips through Serialize/Parse") {
  GIVEN("a list of two custom providers, one of each non-default shape") {
    std::vector<AiCustomProviderConfig> providers = {
      {wxS("id1"), wxS("Groq"), AiProviderShape::OpenAiCompatible,
       wxS("https://api.groq.com/openai/v1/chat/completions"), wxS("llama-3.3-70b"),
       wxS("")},
      {wxS("id2"), wxS("My Anthropic Proxy"), AiProviderShape::Anthropic,
       wxS("https://proxy.example.com/v1/messages"), wxS("claude-3-5-sonnet-latest"),
       wxS("")},
      {wxS("id3"), wxS("My Gemini Proxy"), AiProviderShape::Google,
       wxS("https://proxy.example.com/models/"), wxS("gemini-1.5-flash"),
       wxS("")},
    };
    wxString json_ = SerializeAiCustomProviders(providers);
    auto roundTripped = ParseAiCustomProviders(json_);
    THEN("every field survives, in order, including which shape each one is") {
      REQUIRE(roundTripped.size() == 3);
      for (size_t i = 0; i < providers.size(); ++i) {
        CHECK(roundTripped[i].id == providers[i].id);
        CHECK(roundTripped[i].name == providers[i].name);
        CHECK(roundTripped[i].shape == providers[i].shape);
        CHECK(roundTripped[i].baseUrl == providers[i].baseUrl);
        CHECK(roundTripped[i].model == providers[i].model);
      }
    }
  }

  GIVEN("an empty list") {
    THEN("it serializes to something ParseAiCustomProviders() reads back as empty") {
      CHECK(ParseAiCustomProviders(SerializeAiCustomProviders({})).empty());
    }
  }

  GIVEN("an empty string (never configured) or malformed JSON (a hand-edited config file)") {
    THEN("both degrade to an empty list rather than throwing") {
      CHECK(ParseAiCustomProviders(wxEmptyString).empty());
      CHECK(ParseAiCustomProviders(wxS("{not valid json")).empty());
      CHECK(ParseAiCustomProviders(wxS(R"({"not": "an array"})")).empty());
    }
  }

  GIVEN("an entry with no id (can never be matched back to a stored key)") {
    THEN("ParseAiCustomProviders() drops it rather than fabricating an id") {
      wxString json_ = wxS(R"([{"name":"no id","shape":"openai","baseUrl":"x","model":"y"}])");
      CHECK(ParseAiCustomProviders(json_).empty());
    }
  }
}

SCENARIO("NewAiCustomProviderId() produces distinct, non-empty ids") {
  THEN("two calls in a row don't collide") {
    wxString id1 = NewAiCustomProviderId();
    wxString id2 = NewAiCustomProviderId();
    CHECK(!id1.IsEmpty());
    CHECK(!id2.IsEmpty());
    CHECK(id1 != id2);
  }
}

SCENARIO("AiKnownLocalServerPresets() lists usable OpenAI-compatible presets") {
  auto presets = AiKnownLocalServerPresets();
  THEN("the list is non-empty and every entry is fully filled in") {
    CHECK(!presets.empty());
    for (const auto &preset : presets) {
      CHECK(!preset.name.IsEmpty());
      CHECK(!preset.baseUrl.IsEmpty());
      CHECK(!preset.model.IsEmpty());
    }
  }
  THEN("each preset's URL round-trips through the OpenAI-compatible shape") {
    for (const auto &preset : presets) {
      auto provider = MakeAiProviderForShape(AiProviderShape::OpenAiCompatible,
                                             preset.name, preset.baseUrl,
                                             wxS(""), preset.model);
      REQUIRE(provider != nullptr);
      CHECK(provider->RequestUrl() == preset.baseUrl);
      CHECK(provider->Name() == preset.name);
    }
  }
}

SCENARIO("The API-style picker's index <-> shape mapping round-trips") {
  // Options lists OpenAiCompatible *first* (the overwhelmingly common
  // choice for a hand-added endpoint), which is not the order
  // AiProviderShape declares -- so the two conversions cannot be plain
  // casts. Getting this wrong silently rewrote a custom provider's wire
  // format: a freshly added OpenAI-compatible entry was shown as
  // "Anthropic (Messages API)", and Options' own save-before-switch
  // write-back then really did turn it into one, so a local Ollama server
  // was sent Anthropic-shaped requests it never asked for.
  THEN("OpenAI-compatible is the picker's first item") {
    CHECK(AiProviderShapeToChoiceIndex(AiProviderShape::OpenAiCompatible) == 0);
    CHECK(AiProviderShapeFromChoiceIndex(0) == AiProviderShape::OpenAiCompatible);
  }
  THEN("every shape survives a shape -> index -> shape round trip") {
    for (auto shape : {AiProviderShape::Anthropic, AiProviderShape::OpenAiCompatible,
                       AiProviderShape::Google})
      CHECK(AiProviderShapeFromChoiceIndex(AiProviderShapeToChoiceIndex(shape)) == shape);
  }
  THEN("every picker index survives an index -> shape -> index round trip") {
    for (int index = 0; index < 3; index++)
      CHECK(AiProviderShapeToChoiceIndex(AiProviderShapeFromChoiceIndex(index)) == index);
  }
  THEN("an out-of-range index degrades to the default shape rather than "
      "producing an invalid enum value") {
    CHECK(AiProviderShapeFromChoiceIndex(-1) == AiProviderShape::OpenAiCompatible);
    CHECK(AiProviderShapeFromChoiceIndex(99) == AiProviderShape::OpenAiCompatible);
  }
}

SCENARIO("A provider with no API key sends no credential header at all") {
  // A local AI server (Ollama, LM Studio, llama.cpp server -- see
  // AiKnownLocalServerPresets()) has no third party to authenticate to and
  // normally has no key. Sending a bare "Authorization: Bearer " with
  // nothing after it is likelier to be rejected outright than ignored, so
  // the header is omitted entirely instead.
  GIVEN("a keyless OpenAI-compatible provider") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::OpenAiCompatible, wxS("Local Ollama"),
      wxS("http://localhost:11434/v1/chat/completions"), wxEmptyString, wxS("llama3.2"));
    REQUIRE(provider);
    THEN("no Authorization header is sent") {
      CHECK(provider->AuthHeaders().empty());
    }
  }
  GIVEN("a keyless Anthropic-shaped provider") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::Anthropic, wxS("Local Anthropic proxy"),
      wxS("http://localhost:8080/v1/messages"), wxEmptyString, wxS("some-model"));
    REQUIRE(provider);
    THEN("no x-api-key header is sent, but the version header still is") {
      bool hasApiKeyHeader = false, hasVersionHeader = false;
      for (const auto &h : provider->AuthHeaders()) {
        if (h.first == wxS("x-api-key"))
          hasApiKeyHeader = true;
        if (h.first == wxS("anthropic-version"))
          hasVersionHeader = true;
      }
      CHECK_FALSE(hasApiKeyHeader);
      CHECK(hasVersionHeader);
    }
  }
  GIVEN("a keyless Google-shaped provider") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::Google, wxS("Local Gemini proxy"),
      wxS("http://localhost:8080/v1beta/models/"), wxEmptyString, wxS("some-model"));
    REQUIRE(provider);
    THEN("no x-goog-api-key header is sent") {
      CHECK(provider->AuthHeaders().empty());
    }
  }
  GIVEN("the same providers with a key") {
    THEN("the credential header is sent as before") {
      auto openai = MakeAiProviderForShape(
        AiProviderShape::OpenAiCompatible, wxS("x"), wxS("http://x/"), wxS("k"), wxS("m"));
      REQUIRE(openai->AuthHeaders().size() == 1);
      CHECK(openai->AuthHeaders()[0].second == wxS("Bearer k"));
      auto anthropic = MakeAiProviderForShape(
        AiProviderShape::Anthropic, wxS("x"), wxS("http://x/"), wxS("k"), wxS("m"));
      bool hasApiKeyHeader = false;
      for (const auto &h : anthropic->AuthHeaders())
        if ((h.first == wxS("x-api-key")) && (h.second == wxS("k")))
          hasApiKeyHeader = true;
      CHECK(hasApiKeyHeader);
      auto google = MakeAiProviderForShape(
        AiProviderShape::Google, wxS("x"), wxS("http://x/"), wxS("k"), wxS("m"));
      REQUIRE(google->AuthHeaders().size() == 1);
      CHECK(google->AuthHeaders()[0].first == wxS("x-goog-api-key"));
    }
  }
}

SCENARIO("AiProviderRequestUrlProblem() rejects what actually gets typed") {
  THEN("a complete URL is accepted, http and https alike, any case") {
    CHECK(AiProviderRequestUrlProblem(
            wxS("http://localhost:11434/v1/chat/completions")).IsEmpty());
    CHECK(AiProviderRequestUrlProblem(
            wxS("https://api.example.com/v1/chat/completions")).IsEmpty());
    CHECK(AiProviderRequestUrlProblem(wxS("HTTP://localhost:11434/v1/")).IsEmpty());
    CHECK(AiProviderRequestUrlProblem(wxS("  http://localhost:8080/  ")).IsEmpty());
  }
  THEN("a bare host:port is rejected -- the mistake this exists to catch") {
    // Exactly what a real config file contained after a user set up a
    // local Ollama server by hand: no scheme, no path. libcurl reads
    // everything before the first colon as the scheme, so this reached the
    // user as an unexplained network failure.
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("127.0.0.1:11434")).IsEmpty());
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("localhost:11434")).IsEmpty());
    CHECK_FALSE(
      AiProviderRequestUrlProblem(wxS("localhost:11434/v1/chat/completions")).IsEmpty());
  }
  THEN("an empty or scheme-only URL is rejected too") {
    CHECK_FALSE(AiProviderRequestUrlProblem(wxEmptyString).IsEmpty());
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("   ")).IsEmpty());
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("http://")).IsEmpty());
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("https://")).IsEmpty());
  }
  THEN("a non-HTTP scheme is rejected") {
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("ftp://example.com/")).IsEmpty());
    CHECK_FALSE(AiProviderRequestUrlProblem(wxS("file:///tmp/x")).IsEmpty());
  }
  THEN("every built-in local-server preset passes its own check") {
    for (const auto &preset : AiKnownLocalServerPresets())
      CHECK(AiProviderRequestUrlProblem(preset.baseUrl).IsEmpty());
  }
  THEN("every built-in provider's own base URL passes it too") {
    for (auto kind : {AiProviderKind::Anthropic, AiProviderKind::OpenAI,
                      AiProviderKind::Google, AiProviderKind::Qwen,
                      AiProviderKind::GitHubModels})
      CHECK(AiProviderRequestUrlProblem(AiProviderBaseUrl(kind)).IsEmpty());
  }
}

SCENARIO("A custom provider with a username authenticates with HTTP Basic") {
  // The case this exists for: Ollama's own docs recommend putting an
  // instance reachable beyond localhost behind an authenticating reverse
  // proxy, and those speak HTTP Basic, not Bearer tokens.
  GIVEN("an OpenAI-compatible endpoint behind a Basic-auth proxy") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::OpenAiCompatible, wxS("Ollama behind nginx"),
      wxS("http://ollama.example.org/v1/chat/completions"), wxS("s3cret"),
      wxS("llama3.2"), wxS("alice"));
    REQUIRE(provider);
    auto headers = provider->AuthHeaders();
    THEN("exactly one Authorization header is sent, and it is Basic") {
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
      // base64("alice:s3cret"), checked against the literal encoding
      // rather than recomputed the same way the code does it.
      CHECK(headers[0].second == wxS("Basic YWxpY2U6czNjcmV0"));
    }
    AND_THEN("no Bearer token is sent alongside it") {
      for (const auto &header : headers)
        CHECK(header.second.Find(wxS("Bearer")) == wxNOT_FOUND);
    }
  }

  GIVEN("the Anthropic shape behind a Basic-auth proxy") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::Anthropic, wxS("Proxied Anthropic"),
      wxS("http://proxy.example.org/v1/messages"), wxS("pw"), wxS("some-model"),
      wxS("bob"));
    REQUIRE(provider);
    auto headers = provider->AuthHeaders();
    THEN("Basic replaces x-api-key, and anthropic-version still goes") {
      bool haveBasic = false, haveVersion = false;
      for (const auto &header : headers) {
        CHECK(header.first != wxS("x-api-key"));
        if ((header.first == wxS("Authorization")) &&
            header.second.StartsWith(wxS("Basic ")))
          haveBasic = true;
        if (header.first == wxS("anthropic-version"))
          haveVersion = true;
      }
      CHECK(haveBasic);
      CHECK(haveVersion);
    }
  }

  GIVEN("the Google shape behind a Basic-auth proxy") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::Google, wxS("Proxied Gemini"),
      wxS("http://proxy.example.org/v1beta/models/"), wxS("pw"), wxS("gemini-2.0-flash"),
      wxS("bob"));
    REQUIRE(provider);
    auto headers = provider->AuthHeaders();
    THEN("Basic replaces x-goog-api-key") {
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
      CHECK(headers[0].second.StartsWith(wxS("Basic ")));
    }
  }

  GIVEN("no username, which is what every pre-existing entry has") {
    auto provider = MakeAiProviderForShape(
      AiProviderShape::OpenAiCompatible, wxS("Plain"),
      wxS("http://localhost:11434/v1/chat/completions"), wxS("k"), wxS("m"));
    REQUIRE(provider);
    auto headers = provider->AuthHeaders();
    THEN("the Bearer behaviour is exactly as before") {
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].first == wxS("Authorization"));
      CHECK(headers[0].second == wxS("Bearer k"));
    }
  }

  GIVEN("a password containing a colon") {
    // RFC 7617 splits on the FIRST colon, so this is legal and must not be
    // mangled -- a proxy password is exactly the kind of string that has
    // one.
    auto provider = MakeAiProviderForShape(
      AiProviderShape::OpenAiCompatible, wxS("Colon"),
      wxS("http://localhost:11434/v1/chat/completions"), wxS("a:b"), wxS("m"),
      wxS("u"));
    REQUIRE(provider);
    auto headers = provider->AuthHeaders();
    THEN("it is encoded verbatim") {
      REQUIRE(headers.size() == 1);
      CHECK(headers[0].second == wxS("Basic dTphOmI="));  // base64("u:a:b")
    }
  }
}

SCENARIO("A custom provider's username survives the config round trip") {
  GIVEN("an entry with a username and one without") {
    std::vector<AiCustomProviderConfig> providers;
    AiCustomProviderConfig withAuth;
    withAuth.id = wxS("id-1");
    withAuth.name = wxS("Proxied Ollama");
    withAuth.shape = AiProviderShape::OpenAiCompatible;
    withAuth.baseUrl = wxS("http://ollama.example.org/v1/chat/completions");
    withAuth.model = wxS("llama3.2");
    withAuth.username = wxS("alice");
    providers.push_back(withAuth);
    AiCustomProviderConfig without;
    without.id = wxS("id-2");
    without.name = wxS("Local Ollama");
    without.baseUrl = wxS("http://localhost:11434/v1/chat/completions");
    without.model = wxS("llama3.2");
    providers.push_back(without);

    WHEN("they are serialized and parsed back") {
      auto parsed = ParseAiCustomProviders(SerializeAiCustomProviders(providers));
      THEN("both usernames come back unchanged") {
        REQUIRE(parsed.size() == 2);
        CHECK(parsed[0].username == wxS("alice"));
        CHECK(parsed[1].username.IsEmpty());
      }
    }
  }

  GIVEN("an entry saved before Basic auth existed, i.e. with no username key") {
    auto parsed = ParseAiCustomProviders(
      wxS("[{\"id\":\"old\",\"name\":\"Old\",\"shape\":\"openai\","
          "\"baseUrl\":\"http://localhost:11434/v1/chat/completions\","
          "\"model\":\"llama3.2\"}]"));
    THEN("it parses with an empty username, i.e. unchanged behaviour") {
      REQUIRE(parsed.size() == 1);
      CHECK(parsed[0].username.IsEmpty());
      CHECK(parsed[0].model == wxS("llama3.2"));
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
