// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include <utility>
#include "Maxima.h"
#include <wx/xml/xml.h>
#include <iostream>
#include <wx/app.h>
#include <wx/debug.h>
#include <wx/sstream.h>
#include <wx/tokenzr.h>

//! The time, in ms, we'll wait for an end of string to arrive from maxima after
//! the input was first read.
static constexpr int STRING_END_TIMEOUT = 5000;
//! The period, in ms, with which we may force a read from Maxima in case we
//! expect some input but got no notifications for it.
#ifdef __WINDOWS__
static constexpr int INPUT_RESTART_PERIOD = 3000;
#else
static constexpr int INPUT_RESTART_PERIOD = -1;
#endif

wxDEFINE_EVENT(EVT_MAXIMA, wxThreadEvent);

// Attention: 'wxS("\n"), wxConvUTF8' should be the default and one might think, one can omit these
// parameters, but don't do it. wxWidgets (at least older versions, which are still used in Linux
// distributions) had a bug, so that without these parameters one get wrong characters.
// See: https://github.com/wxWidgets/wxWidgets/issues/14720#issuecomment-1010968576
Maxima::Maxima(wxSocketBase *socket, Configuration *config) :
  m_configuration(config),
  m_socket(socket),
  m_socketInput(*m_socket),
  m_textInput(m_socketInput, wxS("\n"), wxConvUTF8),
  m_abortParserThread(false)
{
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
  m_socketInputData.Alloc(1000000);
  wxASSERT(socket);
  Bind(wxEVT_TIMER, wxTimerEventHandler(Maxima::TimerEvent), this);
  Bind(wxEVT_SOCKET, wxSocketEventHandler(Maxima::SocketEvent), this);
  m_socket->SetEventHandler(*this);
  m_socket->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_OUTPUT_FLAG |
                      wxSOCKET_LOST_FLAG);
  m_socket->Notify(true);
  m_socket->SetFlags(wxSOCKET_NOWAIT_READ);
  m_socket->SetTimeout(120);

  // There are some hints in the code history that wxSOCKET_INPUT
  // event may be "flaky". We don't want wxMaxima to get stuck, but we don't
  // want to be forcing idle events for nothing. So this is a "way out" - we
  // monitor the progress of input, and only act if we expected some but none
  // came.
  if (INPUT_RESTART_PERIOD > 0)
    m_readIdleTimer.Start(INPUT_RESTART_PERIOD);
}

Maxima::~Maxima() {
  Disconnect(wxEVT_TIMER);
  Disconnect(wxEVT_SOCKET);
  Disconnect(EVT_MAXIMA);
  m_abortParserThread = true;

  // Exit all threads before the program ends
  if(m_parserTask.joinable())
    m_parserTask.join();
  if(IsConnected())
    {
      // Make wxWidgets close the connection only after we have sent the close
      // command.
      m_socket->SetFlags(wxSOCKET_WAITALL);
      // Try to gracefully close maxima.
      wxString closeCommand;
      if (m_configuration->InLispMode())
        closeCommand = wxS("($quit)");
      else
        closeCommand = wxS("quit();");
      wxCharBuffer buf = closeCommand.ToUTF8();
      m_socket->Write(buf.data(), buf.length());
    }
  m_socket->Close();
  wxEvtHandler::DeletePendingEvents();
}

bool Maxima::Write(const void *buffer, std::size_t length) {
  if(!buffer)
    return false;
  if (length == 0)
    return false;
  m_socket->Write(buffer, length);
  if (m_socket->Error() && m_socket->LastError() != wxSOCKET_WOULDBLOCK) {
    wxThreadEvent *sendevent = new wxThreadEvent(EVT_MAXIMA);
    sendevent->SetInt(WRITE_ERROR);
    QueueEvent(sendevent);
    return false;
  }
  return true;
}

void Maxima::SocketEvent(wxSocketEvent &event) {
  switch (event.GetSocketEvent()) {
  case wxSOCKET_INPUT:
    ReadSocket();
    break;
  case wxSOCKET_OUTPUT:
    break;
  case wxSOCKET_LOST: {
      wxThreadEvent *evt = new wxThreadEvent(EVT_MAXIMA);
      evt->SetInt(DISCONNECTED);
    QueueEvent(evt);
    break;
  }
  case wxSOCKET_CONNECTION:
    // We don't get these events, as we only deal with connected sockets.
    break;
  }
}

void Maxima::TimerEvent(wxTimerEvent &event) {
  if (&event.GetTimer() == &m_readIdleTimer) {
    // Let's keep the input from Maxima flowing in. This is a platform-specific
    // workaround, so this timer is not guaranteed to fire at all.
    ReadSocket();
  }
}

void Maxima::ReadSocket() {  
  // It is theoretically possible that the client has exited after sending us
  // data and before we had been able to process it.
  if (!m_socket->IsConnected() || !m_socket->IsData())
    return;

  // Avoid frequent (slow) malloc() calls.
  m_socketInputData.Alloc(1000000);


  {
    wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
    event->SetInt(READ_PENDING);
    QueueEvent(event);
  }
  m_abortParserThread = true;
  wxString line;
  wxString rawData;
  wxUniChar ch;
  wxUniChar lastch = '\0';
  const bool collectRawData = m_xmlInspector || GetPipeToStdErr();
  if(collectRawData)
    rawData.Alloc(1000000);
  m_socketInputData.Alloc(1000000);

  // We want to modify m_socketInputData, which is the variable we share with the
  // background thread. In order not to modify it while the background thread
  // accesses it we wait for the backgroundthread to finish.
  if(m_parserTask.joinable())
    m_parserTask.join();
  do
    {
      ch = m_textInput.GetChar();
      if(ch == wxS('\0'))
        continue;
      if(collectRawData)
        rawData.Append(ch);
      if(ch == wxS('\r'))
        {
          if(lastch != wxS('\n'))
            m_socketInputData.Append(wxS('\n'));
          lastch = ch;
          continue;
        }
      m_socketInputData.Append(ch);
      lastch = ch;
    }  while (m_socket->LastReadCount() > 0);

  if(collectRawData)
    {
      wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
      event->SetInt(STRING_FOR_XMLINSPECTOR);
      event->SetString(rawData);
      QueueEvent(event);
    }
  // The string we have received now is broken into tags by a background task before sending
  // it to wxMaxima. As the main task no more accesses the string while that thread is running
  // we don't need any locks or similar for that.
  m_abortParserThread = false;
  if(m_configuration->UseThreads())
    m_parserTask = jthread(&Maxima::SendToWxMaxima, this);
  else
    SendToWxMaxima();
}

void Maxima::SendToWxMaxima()
{
  // This thread shares m_socketInputData with the main thread. But it accesses
  // that variable only when the main thread doesn't and vice versa, therefore
  // that doesn't cause a race condition
  if(m_socketInputData.IsEmpty())
    return;
  size_t size_before;
  do{
    size_before = m_socketInputData.Length();
    wxString dataToSend;
    wxString rest;
    std::unordered_map<wxString, EventCause, wxStringHash>::const_iterator tag =
      m_knownTags.end();
    wxString::const_iterator it;
    for(it = m_socketInputData.begin(); it < m_socketInputData.end(); ++it)
      {
        if(*it == wxS('<'))
          {
            // Extract the starting tag, if this is one.
            wxString::const_iterator it2 = it;
            size_t i = 0;
            wxString tagStart;
            while((i < 20) && (it2 != m_socketInputData.end()))
              {
                tagStart += *it2;
                i++; ++it2;
              }
            for(tag = m_knownTags.begin(); tag != m_knownTags.end(); ++tag)
              {
                wxString tagstartname = wxS("<") + tag->first + wxS(">");
                if(tagStart.StartsWith(tagstartname))
                  break;
              }
            if(tag != m_knownTags.end())
              break;
            if(m_abortParserThread)
              return;
          }
        dataToSend += *it;
        if(*it == wxS('\n'))
          {
            wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
            event->SetInt(READ_MISC_TEXT);
            event->SetString(dataToSend);
            QueueEvent(event);
            dataToSend.Clear();
          }
      }
    if(!dataToSend.IsEmpty())
      {
        wxStringTokenizer lines(dataToSend, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
        while (lines.HasMoreTokens()) {
          wxString line = lines.GetNextToken();
          if(lines.HasMoreTokens())
            line += wxS("\n");
          wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
          event->SetInt(READ_MISC_TEXT);
          event->SetString(line);
          QueueEvent(event);
        }
      }
    for(; it < m_socketInputData.end(); ++it)
        rest += *it;
    m_socketInputData = rest;
    if(m_abortParserThread)
      return;
    
    if(tag != m_knownTags.end())
      {
        // Send the tag we found, but only once it is closed
        wxString tagEndName = wxS("</") + tag->first + wxS(">");
        auto end = m_socketInputData.Find(tagEndName);
        if(end != wxNOT_FOUND)
          {
            long charsInTag = tag->first.Length() + 3 + end;
            dataToSend.Clear();
            rest.Clear();
            for(const auto &i : m_socketInputData)
              {
                if(charsInTag > 0)
                  {
                    --charsInTag;
                    dataToSend += i;
                  }
                else
                  {
                    if((charsInTag < 0) || (i != '\n'))
                      rest += i;
                    if(charsInTag == 0)
                      --charsInTag;
                  }
                
              }

            wxThreadEvent *event = new wxThreadEvent(EVT_MAXIMA);
              event->SetInt(tag->second);
              // XML_PROMPT contains fake XML and XML_SUPPRESSOUTPUT contains any kind of
              // text including XML. XML_MATHS should support adding real maths, but
              // currently still doesn't
              if((tag->second != XML_PROMPT) && (tag->second != XML_SUPPRESSOUTPUT))
              {
                if((tag->second == XML_MATHS) &&
                   ((m_configuration->ShowLength_Bytes() != 0) && (dataToSend.Length() > m_configuration->ShowLength_Bytes())
                    ))
                  {
                    event->SetInt(XML_TOOLONGMATHS);
                  }
                else
                  {
                    event->SetInt(tag->second);
                    wxXmlDocument xmldoc;
                    wxStringInputStream xmlStream(dataToSend);
                    wxLogNull suppressErrorDialogs;
                    xmldoc.Load(xmlStream);
                    event->SetPayload(xmldoc);
                  }
              }
            else
              event->SetString(dataToSend);
            QueueEvent(event);
            m_socketInputData = rest;
          }
      }
  } while ((m_socketInputData.Length() != size_before) && (!m_socketInputData.IsEmpty()));
}

std::unordered_map<wxString, Maxima::EventCause, wxStringHash> Maxima::m_knownTags;
bool Maxima::m_pipeToStderr = false;
