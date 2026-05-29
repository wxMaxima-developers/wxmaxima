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
#include <vector>

wxDEFINE_EVENT(EVT_MAXIMA, wxThreadEvent);

#define INPUT_RESTART_PERIOD 100

Maxima::Maxima(wxSocketBase *socket, Configuration *config) :
  m_configuration(config),
  m_readPendingQueued(false),
  m_socket(socket),
  m_socketInput(*m_socket),
  m_textInput(m_socketInput, wxS("\n"), wxConvUTF8)
{
  {
      std::lock_guard<std::mutex> lock(m_knownTagsMutex);
      if(m_knownTags.empty())
        {
          m_knownTags[wxS("PROMPT")] = XML_PROMPT;
          m_knownTags[wxS("suppressOutput")] = XML_SUPPRESSOUTPUT;
          m_knownTags[wxS("wxxml-symbols")] = XML_WXXMLSYMBOLS;
          m_knownTags[wxS("variables")] = XML_VARIABLES;
          m_knownTags[wxS("watch_variables_add")] = XML_WATCH_VARIABLES_ADD;
          m_knownTags[wxS("statusbar")] = XML_STATUSBAR;
          m_knownTags[wxS("html-manual-keywords")] = XML_HTML_MANUAL_KEYWORDS;
          m_knownTags[wxS("mth")] = XML_MATHS;
          m_knownTags[wxS("math")] = XML_MATHS;
          m_knownTags[wxS("wxxml-key")] = XML_WXXML_KEY;
        }
  }
  wxASSERT(socket);
  
  // COMPLETELY isolate the socket from the main thread event loop.
  // This is crucial to avoid GLib assertion failures related to FD monitoring.
  m_socket->SetNotify(0);
  m_socket->Notify(false);
  m_socket->SetFlags(wxSOCKET_BLOCK); // We will use WaitForRead with timeout in thread.
  m_socket->SetTimeout(1);

  m_workerThread = jthread(&Maxima::WorkerThread, this);
}

Maxima::~Maxima() {
  m_workerThread.request_stop();
  if(m_workerThread.joinable())
    m_workerThread.join();

  Disconnect(wxEVT_TIMER);
  Disconnect(wxEVT_SOCKET);
  Disconnect(EVT_MAXIMA);

  if (m_socket) {
      m_socket->Close();
  }
  
  {
      std::lock_guard<std::mutex> lock(m_interpretedQueueMutex);
      m_interpretedQueue.clear();
  }
  
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

void Maxima::ReadSocket() {
    std::vector<InterpretedItem> items;
    {
        std::lock_guard<std::mutex> lock(m_interpretedQueueMutex);
        if (m_interpretedQueue.empty()) return;
        items.swap(m_interpretedQueue);
        m_readPendingQueued = false;
    }
    
    wxString combinedText;
    auto fireText = [&combinedText, this]() {
        if (!combinedText.IsEmpty()) {
            wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
            event->SetInt(READ_MISC_TEXT);
            event->SetString(combinedText);
            QueueEvent(event);
            combinedText.Clear();
        }
    };

    for (const auto& item : items) {
        if (item.cause == READ_MISC_TEXT) {
            combinedText += item.data;
        } else {
            fireText();
            wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
            event->SetInt(item.cause);
            event->SetString(item.data);
            QueueEvent(event);
        }
    }
    fireText();
}

void Maxima::WorkerThread(stop_token stopToken) {
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

    // 2. Handle Input
    if (m_socket->WaitForRead(0, 50)) {
        activity = true;
        while (m_socket->IsData()) {
            wxUniChar ch = m_textInput.GetChar();
            if (ch == wxS('\0')) break;
            if (ch == wxS('\r')) continue;
            m_processingBuffer += ch;
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

void Maxima::ProcessData(stop_token stopToken)
{
    if (m_processingBuffer.IsEmpty())
        return;

    bool progress;
    wxString batchedText;
    std::vector<InterpretedItem> newItems;

    do {
        progress = false;
        int tagPos = m_processingBuffer.Find(wxS('<'));
        wxString dataToSend;
        if (tagPos == wxNOT_FOUND) {
            dataToSend = m_processingBuffer;
            m_processingBuffer.Clear();
            progress = true;
        } else if (tagPos > 0) {
            dataToSend = m_processingBuffer.Left(tagPos);
            m_processingBuffer = m_processingBuffer.Mid(tagPos);
            progress = true;
        } else {
            std::unordered_map<wxString, EventCause, wxStringHash>::const_iterator tag;
            {
                std::lock_guard<std::mutex> lock(m_knownTagsMutex);
                for(tag = m_knownTags.begin(); tag != m_knownTags.end(); ++tag)
                {
                    if (stopToken.stop_requested()) break;
                    wxString tagstartname = wxS("<") + tag->first + wxS(">");
                    if (m_processingBuffer.StartsWith(tagstartname))
                        break;
                }
            }
            
            if (tag == m_knownTags.end()) {
                dataToSend = m_processingBuffer.Left(1);
                m_processingBuffer = m_processingBuffer.Mid(1);
                progress = true;
            } else {
                wxString tagEndName = wxS("</") + tag->first + wxS(">");
                int end = m_processingBuffer.Find(tagEndName);
                if (end != wxNOT_FOUND) {
                    if (!batchedText.IsEmpty()) {
                        newItems.push_back({.cause = READ_MISC_TEXT, .data = batchedText});
                        batchedText.Clear();
                    }
                    if (stopToken.stop_requested()) break;
                    size_t tagFullLength = end + tagEndName.Length();                    wxString fullTag = m_processingBuffer.Left(tagFullLength);
                    m_processingBuffer = m_processingBuffer.Mid(tagFullLength);
                    if (m_processingBuffer.StartsWith(wxS("\n")))
                        m_processingBuffer = m_processingBuffer.Mid(1);
                    
                    newItems.push_back({.cause = tag->second, .data = fullTag});
                    progress = true;
                }
            }
        }
        
        if (!dataToSend.IsEmpty()) {
            batchedText += dataToSend;
        }
        
        if (stopToken.stop_requested()) break;
    } while (progress && !m_processingBuffer.IsEmpty());

    if (!batchedText.IsEmpty()) {
        newItems.push_back({READ_MISC_TEXT, batchedText});
    }

    if (!newItems.empty()) {
        std::lock_guard<std::mutex> lock(m_interpretedQueueMutex);
        m_interpretedQueue.insert(m_interpretedQueue.end(), newItems.begin(), newItems.end());
        if (!m_readPendingQueued.exchange(true)) {
            CallAfter([this]{ ReadSocket(); });
        }
    }
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

wxString Maxima::MaximaVarnameToLisp(wxString var)
{
  if (var.StartsWith(wxS("?"))) 
    return(InvertCase(var.Mid(1)));
  else
    return InvertCase("$"+var);
}

std::unordered_map<wxString, Maxima::EventCause, wxStringHash> Maxima::m_knownTags;
std::mutex Maxima::m_knownTagsMutex;
bool Maxima::m_pipeToStderr = false;
