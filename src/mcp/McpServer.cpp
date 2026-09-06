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

#include "McpServer.h"
#include "Configuration.h"
#include <wx/log.h>
#include <algorithm>
#include <cctype>

using json = nlohmann::json;

namespace {
enum { ID_MCP_SERVER_SOCKET = 1, ID_MCP_CLIENT_SOCKET = 2 };

//! A request this small in practice; refuse to buffer past this much
//! (headers+body) so a misbehaving/hostile connection can't grow this
//! process's memory unbounded.
constexpr std::size_t MAX_REQUEST_BYTES = 4 * 1024 * 1024;

std::string ToLower(std::string s) {
  std::transform(s.begin(), s.end(), s.begin(),
                 [](unsigned char c) { return std::tolower(c); });
  return s;
}

std::string Trim(const std::string &s) {
  std::size_t begin = s.find_first_not_of(" \t\r\n");
  if (begin == std::string::npos)
    return {};
  std::size_t end = s.find_last_not_of(" \t\r\n");
  return s.substr(begin, end - begin + 1);
}
} // namespace

McpServer::McpServer(Worksheet *worksheet, Variablespane *variablesPane)
  : m_tools(worksheet, variablesPane) {
  Bind(wxEVT_SOCKET, &McpServer::OnServerEvent, this, ID_MCP_SERVER_SOCKET);
  Bind(wxEVT_SOCKET, &McpServer::OnClientEvent, this, ID_MCP_CLIENT_SOCKET);
}

McpServer::~McpServer() { Stop(); }

void McpServer::ReconcileWithConfig(const Configuration &config) {
  if (!config.McpServerEnabled()) {
    Stop();
    return;
  }
  if (m_server && (m_listeningPort == config.McpServerPort()))
    return; // already listening on the right port
  Stop();
  Start(config.McpServerPort());
}

void McpServer::Start(int port) {
  wxIPV4address addr;
  addr.LocalHost(); // 127.0.0.1 -- never listen on any externally-reachable address.
  addr.Service(port);
  m_server = std::make_unique<wxSocketServer>(addr, wxSOCKET_REUSEADDR);
  if (!m_server->IsOk()) {
    wxLogMessage(
      _("MCP server: could not listen on 127.0.0.1:%d (port already in "
        "use?) -- an AI tool will not be able to connect."),
      port);
    m_server.reset();
    return;
  }
  m_listeningPort = port;
  m_server->SetEventHandler(*this, ID_MCP_SERVER_SOCKET);
  m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
  m_server->Notify(true);
  wxLogMessage(_("MCP server: listening on 127.0.0.1:%d"), port);
}

void McpServer::Stop() {
  for (auto &entry : m_connections)
    entry.second->socket->Destroy();
  m_connections.clear();
  m_server.reset();
  m_listeningPort = -1;
}

void McpServer::OnServerEvent(wxSocketEvent &event) {
  if (event.GetSocketEvent() != wxSOCKET_CONNECTION)
    return;
  wxSocketBase *client = m_server->Accept(false);
  if (!client)
    return;

  // Defense in depth: binding to 127.0.0.1 should already make this
  // unreachable from another machine, but double-check the peer address
  // rather than trusting that alone.
  wxIPV4address peer;
  if (!client->GetPeer(peer) || (peer.IPAddress() != wxS("127.0.0.1"))) {
    client->Destroy();
    return;
  }

  auto conn = std::make_unique<Connection>();
  conn->socket = client;
  client->SetEventHandler(*this, ID_MCP_CLIENT_SOCKET);
  client->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_LOST_FLAG);
  client->Notify(true);
  client->SetFlags(wxSOCKET_NOWAIT);
  m_connections[client] = std::move(conn);
}

void McpServer::OnClientEvent(wxSocketEvent &event) {
  wxSocketBase *socket = event.GetSocket();
  auto it = m_connections.find(socket);
  if (it == m_connections.end())
    return;
  switch (event.GetSocketEvent()) {
  case wxSOCKET_INPUT:
    PumpConnection(socket, *it->second);
    break;
  case wxSOCKET_LOST:
    CloseConnection(socket);
    break;
  default:
    break;
  }
}

void McpServer::CloseConnection(wxSocketBase *socket) {
  auto it = m_connections.find(socket);
  if (it == m_connections.end())
    return;
  it->second->socket->Destroy();
  m_connections.erase(it);
}

void McpServer::PumpConnection(wxSocketBase *socket, Connection &conn) {
  char buf[4096];
  // wxSOCKET_NOWAIT means Read() never blocks; drain whatever the OS
  // currently has buffered in a bounded number of chunks per event.
  for (int i = 0; i < 256; ++i) {
    socket->Read(buf, sizeof(buf));
    std::size_t got = socket->LastCount();
    if (got == 0)
      break;
    conn.buffer.append(buf, got);
    if (conn.buffer.size() > MAX_REQUEST_BYTES) {
      conn.badRequest = true;
      break;
    }
  }

  if (conn.badRequest) {
    SendResponse(conn, 413, "Payload Too Large", "text/plain",
                "Request too large");
    return;
  }

  std::string method, path;
  std::unordered_map<std::string, std::string> headers;
  if (conn.contentLength < 0) {
    if (!TryParseHeaders(conn, method, path, headers)) {
      if (conn.badRequest)
        SendResponse(conn, 400, "Bad Request", "text/plain", "Bad Request");
      return; // otherwise: headers not fully received yet
    }
  }

  std::size_t haveBody = conn.buffer.size() - conn.bodyStart;
  if (static_cast<long>(haveBody) < conn.contentLength)
    return; // body not fully received yet

  // Headers were already consumed into `method`/`path`/`headers` the first
  // time TryParseHeaders() succeeded; on a later call (more INPUT events
  // needed to finish the body) they're gone from conn.buffer's start
  // logically but still physically there -- reparse is cheap and simpler
  // than caching them separately.
  TryParseHeaders(conn, method, path, headers);
  std::string body = conn.buffer.substr(
    conn.bodyStart, static_cast<std::size_t>(std::max<long>(conn.contentLength, 0)));
  HandleRequest(conn, method, path, headers, body);
}

bool McpServer::TryParseHeaders(
  Connection &conn, std::string &method, std::string &path,
  std::unordered_map<std::string, std::string> &headers) {
  std::size_t headerEnd = conn.buffer.find("\r\n\r\n");
  if (headerEnd == std::string::npos)
    return false;

  std::size_t lineStart = 0;
  std::size_t lineEnd = conn.buffer.find("\r\n", lineStart);
  std::string requestLine = conn.buffer.substr(lineStart, lineEnd - lineStart);
  std::size_t sp1 = requestLine.find(' ');
  std::size_t sp2 = (sp1 == std::string::npos)
    ? std::string::npos
    : requestLine.find(' ', sp1 + 1);
  if ((sp1 == std::string::npos) || (sp2 == std::string::npos)) {
    conn.badRequest = true;
    return false;
  }
  method = requestLine.substr(0, sp1);
  path = requestLine.substr(sp1 + 1, sp2 - sp1 - 1);

  lineStart = lineEnd + 2;
  while (lineStart < headerEnd) {
    lineEnd = conn.buffer.find("\r\n", lineStart);
    if ((lineEnd == std::string::npos) || (lineEnd > headerEnd))
      lineEnd = headerEnd;
    std::string line = conn.buffer.substr(lineStart, lineEnd - lineStart);
    std::size_t colon = line.find(':');
    if (colon != std::string::npos)
      headers[ToLower(Trim(line.substr(0, colon)))] = Trim(line.substr(colon + 1));
    lineStart = lineEnd + 2;
  }

  conn.bodyStart = headerEnd + 4;
  conn.contentLength = 0;
  auto lengthIt = headers.find("content-length");
  if (lengthIt != headers.end())
    conn.contentLength = std::strtol(lengthIt->second.c_str(), nullptr, 10);
  return true;
}

bool McpServer::OriginAllowed(const std::string &origin) {
  if (origin.empty())
    return true; // no Origin header at all -- most non-browser MCP clients
  std::string lower = ToLower(origin);
  return (lower.rfind("http://localhost", 0) == 0) ||
    (lower.rfind("http://127.0.0.1", 0) == 0);
}

void McpServer::HandleRequest(
  Connection &conn, const std::string &method, const std::string &path,
  const std::unordered_map<std::string, std::string> &headers,
  const std::string &body) {
  auto originIt = headers.find("origin");
  std::string origin = (originIt == headers.end()) ? "" : originIt->second;
  if (!OriginAllowed(origin)) {
    SendResponse(conn, 403, "Forbidden", "text/plain",
                "Origin not allowed");
    return;
  }

  if (path != "/mcp") {
    SendResponse(conn, 404, "Not Found", "text/plain", "Not Found");
    return;
  }
  if (method == "GET") {
    // Streamable HTTP allows a server with no server-initiated push to
    // simply refuse GET (no SSE stream offered here).
    SendResponse(conn, 405, "Method Not Allowed", "text/plain",
                "This MCP server does not offer a server-push stream; POST "
                "a JSON-RPC request instead.");
    return;
  }
  if (method != "POST") {
    SendResponse(conn, 405, "Method Not Allowed", "text/plain",
                "Method Not Allowed");
    return;
  }

  std::string responseBody = HandleJsonRpc(body);
  if (responseBody.empty())
    // A JSON-RPC *notification* (no "id") gets no response body -- the
    // spec's Streamable HTTP transport still expects an HTTP status though.
    SendResponse(conn, 202, "Accepted", "text/plain", "");
  else
    SendResponse(conn, 200, "OK", "application/json", responseBody);
}

std::string McpServer::HandleJsonRpc(const std::string &requestBody) {
  json request;
  json id = nullptr;
  bool hasId = false;
  try {
    request = json::parse(requestBody);
    if (request.contains("id") && !request["id"].is_null()) {
      id = request["id"];
      hasId = true;
    }
  } catch (const json::exception &) {
    json error;
    error["jsonrpc"] = "2.0";
    error["id"] = nullptr;
    error["error"] = {{"code", -32700}, {"message", "Parse error"}};
    return error.dump();
  }

  std::string method = request.value("method", std::string());
  json params = request.value("params", json::object());

  json response;
  response["jsonrpc"] = "2.0";
  if (hasId)
    response["id"] = id;

  try {
    if (method == "initialize") {
      json result;
      result["protocolVersion"] = "2025-06-18";
      result["capabilities"] = {{"tools", json::object()}};
      result["serverInfo"] = {{"name", "wxmaxima-worksheet"},
                              {"version", "1.0"}};
      response["result"] = result;
    } else if (method == "notifications/initialized") {
      return {}; // notification: no response
    } else if (method == "tools/list") {
      response["result"] = m_tools.ListTools();
    } else if (method == "tools/call") {
      wxString name = wxString::FromUTF8(
        params.value("name", std::string()).c_str());
      json arguments = params.value("arguments", json::object());
      response["result"] = m_tools.CallTool(name, arguments);
    } else if (!hasId) {
      return {}; // an unknown notification is simply ignored, per JSON-RPC
    } else {
      response["error"] = {{"code", -32601}, {"message", "Method not found"}};
      response.erase("result");
    }
  } catch (const McpToolError &e) {
    response["error"] = {{"code", -32602}, {"message", e.what()}};
  } catch (const std::exception &e) {
    response["error"] = {{"code", -32603}, {"message", e.what()}};
  }

  if (!hasId)
    return {}; // notification: JSON-RPC defines no response for these
  return response.dump();
}

void McpServer::SendResponse(Connection &conn, int status,
                             const char *statusText,
                             const std::string &contentType,
                             const std::string &body) {
  std::string response = "HTTP/1.1 " + std::to_string(status) + " " +
    statusText + "\r\n";
  response += "Content-Type: " + contentType + "\r\n";
  response += "Content-Length: " + std::to_string(body.size()) + "\r\n";
  response += "Connection: close\r\n\r\n";
  response += body;
  conn.socket->Write(response.data(), response.size());
  CloseConnection(conn.socket);
}
