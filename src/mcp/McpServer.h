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

#ifndef MCPSERVER_H
#define MCPSERVER_H

#include "precomp.h"
#include "McpTools.h"
#include <wx/socket.h>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>

class Configuration;
class Worksheet;
class Variablespane;

/*! A minimal, opt-in MCP (Model Context Protocol) server exposing the
  current worksheet as read-only context an external AI tool can query
  (GH request: "a de facto standard [for] an AI sidebar that ... gives it
  access to a worksheet"). Off by default -- see
  Configuration::McpServerEnabled().

  Transport: MCP's "Streamable HTTP", the subset of it a purely
  request/response (no server-initiated push) server needs -- a single HTTP
  endpoint that accepts a POST'd JSON-RPC 2.0 request and answers with one
  JSON-RPC response, closing the connection afterwards (no keep-alive, no
  chunked transfer, no SSE stream: a GET on the endpoint gets a plain 405,
  which the spec allows for a server with nothing to push). No
  `Mcp-Session-Id` bookkeeping either -- the spec marks that optional for the
  server to assign, and every tool call here is independently answerable
  from the live worksheet state with no cross-call session to track.

  Implemented directly on wxSocketServer/wxSocketBase (the same primitive
  Maxima.cpp already uses for the Maxima<->wxMaxima protocol, just
  event-driven here instead of on a worker thread) rather than pulling in an
  HTTP library: the request shape this needs to parse is deliberately tiny
  (one method, one path, a couple of headers, a Content-Length-delimited
  body), and every request is handled synchronously by McpTools before the
  response is written, so there is no benefit to a general-purpose HTTP
  server for a case this narrow.

  Runs entirely on the GUI thread's own event loop (the wxSocketServer/
  wxSocketBase objects are event-driven, `Notify()`-based, not blocking calls
  on a worker thread) so it can call directly into Worksheet/GroupCell/
  Variablespane -- none of which are thread-safe -- with no marshaling. Keep
  it that way: never make this transport threaded without adding the
  CallAfter()-based marshaling every cross-thread touch of a worksheet
  cell needs elsewhere in this codebase (see AGENTS.md's `CellPtr` /
  "Asynchronous Sidebars & Safety" notes).

  Safety: binds to 127.0.0.1 only (refuses to listen on any other address),
  double-checks each accepted connection's peer address is loopback too, and
  validates the `Origin` header (when a client sends one -- browsers always
  do, most non-browser MCP clients don't) against localhost/127.0.0.1 to
  block the DNS-rebinding attack the MCP spec calls out: a malicious web page
  resolving an attacker-controlled hostname to 127.0.0.1 so a victim's
  browser fetch reaches this server as if it were same-origin. None of this
  can ever let a request *write* to the worksheet -- see McpTools.h for the
  exact, deliberately narrow read (plus watch/unwatch) surface this offers.
*/
class McpServer : public wxEvtHandler {
public:
  McpServer(Worksheet *worksheet, Variablespane *variablesPane);
  ~McpServer() override;

  //! Starts/stops listening to match the current configuration. Call this
  //! once after construction and again whenever the option or port could
  //! have changed (the Options dialog closing).
  void ReconcileWithConfig(const Configuration &config);

  //! Plugs in a live "is Maxima actually connected right now" query --
  //! see McpTools::SetConnectionCheck() for why this can't be answered
  //! from a Worksheet/Variablespane alone and has to be threaded in from
  //! outside. Call once, from wherever this McpServer is constructed
  //! (wxMaximaFrame doesn't itself know this; wxMaxima, which owns the
  //! actual Maxima process/socket, does).
  void SetConnectionCheck(std::function<bool()> isConnected)
    {
      m_tools.SetConnectionCheck(std::move(isConnected));
    }

private:
  //! One accepted-but-not-yet-fully-handled HTTP connection.
  struct Connection {
    //! Owned via wxSocketBase::Destroy(), never plain delete -- see the
    //! class comment on why this can't be a unique_ptr with a default
    //! deleter (this is destroyed from inside the socket's own event
    //! handler call stack).
    wxSocketBase *socket = nullptr;
    std::string buffer;
    long contentLength = -1;
    std::size_t bodyStart = 0;
    bool badRequest = false;
  };

  void Start(int port);
  void Stop();
  void OnServerEvent(wxSocketEvent &event);
  void OnClientEvent(wxSocketEvent &event);
  //! Reads whatever is currently available and, once a full request has
  //! arrived, handles it and closes the connection.
  void PumpConnection(wxSocketBase *socket, Connection &conn);
  //! True once conn.buffer holds the complete request headers, having
  //! parsed conn.contentLength/bodyStart as a side effect.
  bool TryParseHeaders(Connection &conn, std::string &method,
                       std::string &path,
                       std::unordered_map<std::string, std::string> &headers);
  void HandleRequest(Connection &conn, const std::string &method,
                     const std::string &path,
                     const std::unordered_map<std::string, std::string> &headers,
                     const std::string &body);
  //! Runs one JSON-RPC 2.0 request/notification through McpTools; returns
  //! the response body to write (empty for a notification -- no id).
  std::string HandleJsonRpc(const std::string &requestBody);
  void SendResponse(Connection &conn, int status, const char *statusText,
                    const std::string &contentType, const std::string &body);
  void CloseConnection(wxSocketBase *socket);
  //! Is this Origin header value (if any) allowed to reach a localhost-only
  //! server? Empty (no Origin header at all -- most non-browser clients)
  //! is allowed; only a *present-but-not-localhost* Origin is rejected.
  static bool OriginAllowed(const std::string &origin);

  McpTools m_tools;
  std::unique_ptr<wxSocketServer> m_server;
  std::unordered_map<wxSocketBase *, std::unique_ptr<Connection>> m_connections;
  int m_listeningPort = -1;
};

#endif // MCPSERVER_H
