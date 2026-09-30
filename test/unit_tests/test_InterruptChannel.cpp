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
  Tests the channel wxMaxima interrupts Maxima through (GH #2289): the
  handshake that decides whether a connection may interrupt Maxima, and the
  channel itself over a real local socket, with the test playing Maxima's
  part.
*/

#include "InterruptChannelHandshake.h"
#include "MaximaInterruptChannel.h"
#include <wx/app.h>
#include <wx/evtloop.h>
#include <wx/log.h>
#include <wx/socket.h>
#include <wx/stopwatch.h>
#include <cstdio>
#include <cstring>
#include <functional>
#include <memory>
#include <string>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using State = InterruptChannelHandshake::State;

static State FeedString(InterruptChannelHandshake &handshake, const std::string &text) {
  return handshake.Feed(text.data(), text.size());
}

SCENARIO("The interrupt channel handshake only accepts the secret") {
  GIVEN("a handshake expecting a token") {
    InterruptChannelHandshake handshake("s3cr3t+/=");
    THEN("it is pending before anything arrived") {
      REQUIRE(handshake.GetState() == State::pending);
    }
    THEN("the token followed by a newline is accepted") {
      REQUIRE(FeedString(handshake, "s3cr3t+/=\n") == State::accepted);
    }
    THEN("a CR LF line end, as a Lisp on MS Windows may send, is accepted") {
      REQUIRE(FeedString(handshake, "s3cr3t+/=\r\n") == State::accepted);
    }
    THEN("the token arriving in pieces is accepted") {
      REQUIRE(FeedString(handshake, "s3c") == State::pending);
      REQUIRE(FeedString(handshake, "r3t+") == State::pending);
      REQUIRE(FeedString(handshake, "/=\n") == State::accepted);
    }
    THEN("a wrong token of the right length is rejected") {
      REQUIRE(FeedString(handshake, "s3cr3t+/X\n") == State::rejected);
    }
    THEN("a prefix of the token is rejected") {
      REQUIRE(FeedString(handshake, "s3cr3t\n") == State::rejected);
    }
    THEN("an empty line is rejected") {
      REQUIRE(FeedString(handshake, "\n") == State::rejected);
    }
    THEN("a long line is rejected before its newline arrives") {
      REQUIRE(FeedString(handshake, std::string(1000, 'x')) == State::rejected);
    }
    THEN("the decision is final") {
      REQUIRE(FeedString(handshake, "wrong\n") == State::rejected);
      REQUIRE(FeedString(handshake, "s3cr3t+/=\n") == State::rejected);
    }
    THEN("what follows an accepted token doesn't change anything") {
      REQUIRE(FeedString(handshake, "s3cr3t+/=\nsomething else\n") == State::accepted);
    }
  }
  GIVEN("a handshake without a token") {
    InterruptChannelHandshake handshake("");
    THEN("nothing, not even an empty line, gets in") {
      REQUIRE(handshake.GetState() == State::rejected);
      REQUIRE(FeedString(handshake, "\n") == State::rejected);
    }
  }
}

//! Runs the event loop until \p done returns true or a few seconds passed.
static bool WaitFor(const std::function<bool()> &done) {
  // Without an active event loop wxYield() does nothing on MS Windows: see
  // main(). Fail here, on every platform, rather than by a timeout on one.
  REQUIRE(wxEventLoopBase::GetActive() != nullptr);
  wxStopWatch watch;
  while (!done() && (watch.Time() < 5000)) {
    wxTheApp->Yield(true);
    wxMilliSleep(5);
  }
  return done();
}

//! Plays Maxima's part: connects to the server and sends \p firstLine.
static std::unique_ptr<wxSocketClient> ConnectAsMaxima(unsigned short port,
                                                       const std::string &firstLine) {
  auto client = std::make_unique<wxSocketClient>(wxSOCKET_BLOCK);
  wxIPV4address address;
  address.LocalHost();
  address.Service(port);
  REQUIRE(client->Connect(address, true));
  client->Write(firstLine.data(), firstLine.size());
  return client;
}

SCENARIO("The interrupt channel carries interrupts to Maxima") {
  wxIPV4address serverAddress;
  serverAddress.LocalHost();
  serverAddress.Service(0);
  wxSocketServer server(serverAddress, wxSOCKET_BLOCK);
  REQUIRE(server.IsOk());
  wxIPV4address bound;
  server.GetLocal(bound);
  const unsigned short port = bound.Service();

  GIVEN("a Lisp that connects with the right token") {
    auto maxima = ConnectAsMaxima(port, "token\n");
    MaximaInterruptChannel channel(server.Accept(true), wxS("token"));
    THEN("the channel becomes ready") {
      REQUIRE(WaitFor([&] { return channel.IsReady(); }));
      REQUIRE_FALSE(channel.IsDead());
      AND_THEN("an interrupt reaches the Lisp as one line") {
        REQUIRE(channel.SendInterrupt());
        char buffer[64] = {};
        maxima->SetTimeout(5);
        maxima->Read(buffer, std::strlen("interrupt\n"));
        REQUIRE(std::string(buffer, maxima->LastReadCount()) == "interrupt\n");
      }
      AND_THEN("the channel knows when the Lisp went away") {
        maxima->Close();
        REQUIRE(WaitFor([&] { return channel.IsDead(); }));
        REQUIRE_FALSE(channel.IsReady());
        REQUIRE_FALSE(channel.SendInterrupt());
      }
    }
  }
  GIVEN("a connection with a wrong token") {
    auto impostor = ConnectAsMaxima(port, "guess\n");
    MaximaInterruptChannel channel(server.Accept(true), wxS("token"));
    THEN("it never becomes ready and can't be used to interrupt") {
      REQUIRE(WaitFor([&] { return channel.IsDead(); }));
      REQUIRE_FALSE(channel.IsReady());
      REQUIRE_FALSE(channel.SendInterrupt());
    }
  }
  GIVEN("a connection that hasn't sent anything yet") {
    auto silent = std::make_unique<wxSocketClient>(wxSOCKET_BLOCK);
    wxIPV4address address;
    address.LocalHost();
    address.Service(port);
    REQUIRE(silent->Connect(address, true));
    MaximaInterruptChannel channel(server.Accept(true), wxS("token"));
    THEN("an interrupt isn't sent to it") {
      REQUIRE_FALSE(channel.IsReady());
      REQUIRE_FALSE(channel.SendInterrupt());
    }
  }
  GIVEN("no connection at all") {
    MaximaInterruptChannel channel(nullptr, wxS("token"));
    THEN("the channel is dead and refuses to interrupt") {
      REQUIRE(channel.IsDead());
      REQUIRE_FALSE(channel.SendInterrupt());
    }
  }
}

class TestApp : public wxApp {
public:
  bool OnInit() override { return true; }
};
wxDECLARE_APP(TestApp);
wxIMPLEMENT_APP_NO_MAIN(TestApp);

//! How many wxWidgets assertions failed. The shared test setup only prints
//! them; here one is a failure, since the channel runs in release builds
//! where nobody would see it.
static int g_failedAssertions = 0;

static void CountingAssertHandler(const wxString &file, int line, const wxString &func,
                                  const wxString &cond, const wxString &msg) {
  ++g_failedAssertions;
  fprintf(stderr, "wxASSERT failed: %s:%d [%s]: %s %s\n",
          static_cast<const char *>(file.utf8_str()), line,
          static_cast<const char *>(func.utf8_str()),
          static_cast<const char *>(cond.utf8_str()),
          static_cast<const char *>(msg.utf8_str()));
}

int main(int argc, char **argv) {
  wxLog::EnableLogging(false);
  wxSetAssertHandler(CountingAssertHandler);
  wxApp::SetInstance(new TestApp());
  wxEntryStart(argc, argv);
  wxTheApp->CallOnInit();
  int result;
  {
    // The socket events the channel waits for have to be dispatched by an
    // event loop, and wxMaxima has one running. This test never runs one, so
    // it only yields -- and on MS Windows wxYield() needs an *active* loop to
    // do even that: without one it peeks at the first message, finds nothing
    // to hand it to (wxApp::Dispatch() returns false) and gives up. The
    // messages WSAAsyncSelect() posts for a socket then are never delivered,
    // and the channel never sees its token. GTK's wxYield() iterates the
    // GLib main context directly, which is why only MSW noticed.
    wxEventLoop loop;
    wxEventLoopActivator activate(&loop);
    result = Catch::Session().run(argc, argv);
  }
  wxEntryCleanup();
  if (g_failedAssertions > 0) {
    fprintf(stderr, "%i wxWidgets assertion(s) failed.\n", g_failedAssertions);
    result = 1;
  }
  return result;
}
