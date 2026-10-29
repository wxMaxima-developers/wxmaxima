// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "Maxima.h"
#include <wx/socket.h>
#include <wx/sckstrm.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/tokenzr.h>
#include <wx/app.h>
#include <wx/xml/xml.h>
#include <wx/sstream.h>
#include <wx/stopwatch.h>
#include <iostream>
#include <iterator>
#include <utility>
#include <vector>
#include <cstdio>
#include <wx/utils.h>
#ifndef __WINDOWS__
#include <sys/socket.h>
#endif

wxDEFINE_EVENT(EVT_MAXIMA, wxThreadEvent);

#define INPUT_RESTART_PERIOD 100

Maxima::Maxima(wxSocketBase *socket, Configuration *config) :
  m_configuration(config),
  m_socket(socket),
  m_socketInput(*m_socket),
  m_textInput(m_socketInput, wxS("\n"), wxConvUTF8)
{
  wxASSERT(socket);
  
  // COMPLETELY isolate the socket from the main thread event loop.
  // This is crucial to avoid GLib assertion failures related to FD monitoring.
  m_socket->SetNotify(0);
  m_socket->Notify(false);
  m_socket->SetFlags(wxSOCKET_BLOCK); // We will use WaitForRead with timeout in thread.
  m_socket->SetTimeout(1);

#ifdef SO_RCVBUF
  // Keeps the operating system from buffering megabytes of Maxima's output
  // on our behalf (on Linux the receive buffer grows to 6 MB by itself).
  // Our own backlog is limited (MaximaItemQueue::BackPressure()), but
  // whatever waits in the socket has to be displayed, too, before the
  // reaction to Ctrl+G shows up (GH #2594).
  int receiveBufferSize = 64 * 1024;
  m_socket->SetOption(SOL_SOCKET, SO_RCVBUF, &receiveBufferSize,
                      sizeof(receiveBufferSize));
#endif

  m_workerThread = jthread([this](stop_token stopToken) { WorkerThread(stopToken); });
}

Maxima::~Maxima() {
  m_workerThread.request_stop();

  // Defensively drop any handlers that might still target this object before it
  // is torn down. These stay as 1-arg Disconnect() rather than Unbind(): they
  // disconnect *every* handler for the event type (Unbind would need the exact
  // handler+sink, which we don't have here), and ~wxEvtHandler would clear them
  // anyway. wxEVT_TIMER/wxEVT_SOCKET are effectively no-ops (the socket runs with
  // Notify(false) and is read by the worker thread); EVT_MAXIMA is the one wired
  // up from wxMaxima (m_client->Bind(EVT_MAXIMA, &MaximaProcessManager::MaximaEvent, ...)).
  Disconnect(wxEVT_TIMER);
  Disconnect(wxEVT_SOCKET);
  Disconnect(EVT_MAXIMA);

  // The worker thread may be blocked on the socket (on FreeBSD it can wait
  // there indefinitely), so it has to be woken up before it can be joined.
  // Close() is not the way to do that: it frees the file descriptor while the
  // worker is still using it, and the worker's next WaitForRead() then hands
  // -1 to FD_SET(), which glibc's fortified version answers by aborting the
  // whole program ("bit out of range 0 - FD_SETSIZE on fd_set"). shutdown()
  // instead makes every pending and future read or write on the socket return
  // at once, but keeps the descriptor valid until we close it after the join.
  // MS Windows doesn't need this: a blocked read there ends with the socket's
  // timeout.
#ifndef __WINDOWS__
  if (m_socket && m_socket->IsOk()) {
    wxSOCKET_T fd = m_socket->GetSocket();
    if (fd >= 0)
      ::shutdown(fd, SHUT_RDWR);
  }
#endif

  if(m_workerThread.joinable()) {
    m_workerThread.join();
  }

  if (m_socket) {
      m_socket->Close();
  }

  m_items.Clear();

  wxEvtHandler::DeletePendingEvents();
}

bool Maxima::Write(const void *buffer, std::size_t length) {
  if (!buffer || !length) return false;
  
  std::vector<char> data(static_cast<const char*>(buffer), static_cast<const char*>(buffer) + length);
  {
      std::lock_guard<std::mutex> lock(m_outputQueueMutex);
      m_outputQueue.push_back(std::move(data));
  }
  return true;
}

void Maxima::SocketEvent(wxSocketEvent &WXUNUSED(event)) {
}

void Maxima::TimerEvent(wxTimerEvent &WXUNUSED(event)) {
}

void Maxima::PushItems(std::vector<MaximaItemQueue::Item> &&items) {
  if (m_items.Push(std::move(items))) {
    wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
    event->SetInt(DATA_AVAILABLE);
    QueueEvent(event);
  }
}

void Maxima::WorkerThread(const stop_token& stopToken) {
  // How much to read in one go before the data is split into items
  constexpr std::size_t maxCharsPerRead = 65536;
  wxMilliClock_t lastProcessTime = wxGetLocalTimeMillis();
  while (!stopToken.stop_requested()) {
    bool activity = false;

    if (!m_socket->IsConnected()) {
        wxThreadEvent *evt = new wxThreadEvent(EVT_MAXIMA);
        evt->SetInt(DISCONNECTED);
        QueueEvent(evt);
        break;
    }

    // 1. Handle Output Queue
    std::vector<std::vector<char>> toWrite;
    {
        std::lock_guard<std::mutex> lock(m_outputQueueMutex);
        if (!m_outputQueue.empty()) {
            toWrite.swap(m_outputQueue);
        }
    }
    
    for (const auto& data : toWrite) {
        m_socket->Write(data.data(), data.size());
        activity = true;
        if (m_socket->Error()) {
            wxThreadEvent *sendevent = new wxThreadEvent(EVT_MAXIMA);
            sendevent->SetInt(WRITE_ERROR);
            QueueEvent(sendevent);
        }
    }

    // 2. Handle Input -- unless the GUI thread still has a big backlog
    // (GH #2594). Not reading lets the socket's buffers fill up, which makes
    // Maxima wait in its next write: a command that prints faster than we
    // can display its output then runs at the speed of the display instead
    // of filling our memory with output nobody can see yet.
    if (!m_items.BackPressure() && m_socket->WaitForRead(0, 50)) {
        activity = true;
        wxString incomingData;
        // Bounded, so that a Maxima writing as fast as we read can't keep
        // us in here, growing m_processingBuffer, without ever handing a
        // single item to the GUI.
        std::size_t charsRead = 0;
        while ((charsRead++ < maxCharsPerRead) && m_socket->IsData()) {
            wxUniChar ch = m_textInput.GetChar();
            if (ch == wxS('\0')) break;
            if (ch == wxS('\r')) continue;
            m_processingBuffer += ch;
            if (m_xmlInspector) incomingData += ch;
        }
        if (m_xmlInspector && !incomingData.IsEmpty()) {
            std::vector<MaximaItemQueue::Item> inspectorItem;
            inspectorItem.push_back({STRING_FOR_XMLINSPECTOR, std::move(incomingData)});
            PushItems(std::move(inspectorItem));
        }
    }

    wxMilliClock_t now = wxGetLocalTimeMillis();
    if (!m_processingBuffer.IsEmpty() && (now - lastProcessTime > 100 || m_processingBuffer.Length() > 10000)) {
        ProcessData(stopToken);
        lastProcessTime = now;
        activity = true;
    }

    if (!activity) {
        if (!m_processingBuffer.IsEmpty()) {
            ProcessData(stopToken);
        }
        wxMilliSleep(20);
    }
  }
}

void Maxima::ProcessData(const stop_token& stopToken)
{
    if (m_processingBuffer.IsEmpty())
        return;

    std::vector<MaximaItemQueue::Item> newItems;
    const size_t consumed = SplitIntoItems(
      m_processingBuffer, KnownTags(), READ_MISC_TEXT, newItems,
      [&stopToken]{ return stopToken.stop_requested(); });
    m_processingBuffer.erase(0, consumed);

    PushItems(std::move(newItems));
}

wxString Maxima::InvertCase(const wxString &var) {
  wxString retval;
  for (auto const &i: var) {
    if (wxIsupper(i))
      retval += wxString(i).Lower();
    else {
      if (wxIslower(i))
        retval += wxString(i).Upper();
      else
        retval += i;
    }
  }
  return retval;
}

wxString Maxima::EscapeVarnameForMaxima(wxString var) {
  wxString result;
  result.reserve(var.length() * 2);

  bool isLispVar = false;
  if (var.StartsWith(wxS("?"))) {
    isLispVar = true;
    var = var.Mid(1);
  }

  bool first = true;
  for (wxUniChar c : var) {
    switch (c.GetValue()) {
    case '\\':
    case '+':
    case '#':
    case '\'':
    case '\"':
    case '!':
    case '-':
    case '*':
    case '/':
    case '^':
    case '$':
    case ';':
    case ',':
    case '<':
    case '>':
    case '@':
    case '~':
    case '`':
    case '?':
    case '(':
    case ')':
    case '{':
    case '}':
    case '[':
    case ']':
    case ' ':
    case ':':
    case '.':
    case '=':
    case '|':
    case '&':
    case 0x00B0: // '°'
      result += wxS('\\');
      break;
    default:
      if (c > 0x7F || (first && c >= '0' && c <= '9'))
        result += wxS('\\');
      break;
    }
    result += c;
    first = false;
  }

  if (isLispVar)
    return (wxS("?") + result);
  else
    return result;
}

wxString Maxima::MaximaVarnameToLisp(const wxString& var)
{
  if (var.StartsWith(wxS("?"))) 
    return(InvertCase(var.Mid(1)));
  else
    return InvertCase("$"+var);
}

const std::unordered_map<wxString, Maxima::EventCause, wxStringHash> &Maxima::KnownTags()
{
  // A function-local static is initialized exactly once, thread-safely, and
  // is never written to afterwards, so the worker thread can read it
  // without a lock.
  static const std::unordered_map<wxString, EventCause, wxStringHash> knownTags = {
    {wxS("PROMPT"), XML_PROMPT},
    {wxS("suppressOutput"), XML_SUPPRESSOUTPUT},
    {wxS("wxxml-symbols"), XML_WXXMLSYMBOLS},
    {wxS("variables"), XML_VARIABLES},
    {wxS("watch_variables_add"), XML_WATCH_VARIABLES_ADD},
    {wxS("statusbar"), XML_STATUSBAR},
    {wxS("worksheetexport"), XML_WORKSHEET_EXPORT},
    {wxS("html-manual-keywords"), XML_HTML_MANUAL_KEYWORDS},
    {wxS("mth"), XML_MATHS},
    {wxS("math"), XML_MATHS},
    {wxS("wxxml-key"), XML_WXXML_KEY},
    {wxS("wxxml-asciimath"), XML_ASCIIMATH},
    // Deliberately attribute-free: ProcessData() matches a bare "<tag>" at
    // the start of the buffer, so a tag carrying the cell id as an attribute
    // would never be recognised at all. The id travels in the body instead,
    // as <id>...</id>.
    {wxS("wxasync"), XML_ASYNC_OUTPUT}
  };
  return knownTags;
}
bool Maxima::m_pipeToStderr = false;
