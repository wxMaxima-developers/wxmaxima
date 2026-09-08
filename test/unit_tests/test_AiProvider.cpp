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
       wxS("https://api.groq.com/openai/v1/chat/completions"), wxS("llama-3.3-70b")},
      {wxS("id2"), wxS("My Anthropic Proxy"), AiProviderShape::Anthropic,
       wxS("https://proxy.example.com/v1/messages"), wxS("claude-3-5-sonnet-latest")},
      {wxS("id3"), wxS("My Gemini Proxy"), AiProviderShape::Google,
       wxS("https://proxy.example.com/models/"), wxS("gemini-1.5-flash")},
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

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
