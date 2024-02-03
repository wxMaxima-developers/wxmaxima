// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#ifndef WXMAXIMA_MAXIMA_H
#define WXMAXIMA_MAXIMA_H

/*! \file
 *
 * Declares the interface to the Maxima process.
 */

#include <wx/defs.h>
#include <wx/buffer.h>
#include <wx/event.h>
#include <wx/sckstrm.h>
#include <wx/txtstrm.h>
#include <wx/socket.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <memory>
#include <thread>
#include <atomic>
#include <mutex>
#include "Configuration.h"

/*! Interface to the Maxima process
 *
 * Eventually this class will be the entire stand-alone Maxima
 * interface, factored out from wxMaxima. For now, it only provides
 * socket I/O.
 *
 * It is a source of EVT_MAXIMA events, used to asynchronously
 * decouple the I/O from the front-end. In the future, more of this class
 * could run on a worker thread perhaps.
 *
 * What it already does do is that on incoming data it creates a worker
 * thread that splits the incoming data into known XML tags maxima sends
 * and misc text and sends each of these items in a separate
 * EVT_MAXIMA to the wxMaxima main class
 */
class Maxima : public wxEvtHandler
{
public:
  //! Construct this object when a connection is received from Maxima.
  //! The argument should be socketServer.Accept();
  explicit Maxima(wxSocketBase *socket, Configuration *config);
  virtual ~Maxima() override;

  wxSocketBase *Socket() const { return m_socket.get(); }

  //! Are we connected to Maxima?
  bool IsConnected() const { return m_socket->IsConnected(); }

  //! Tells us if the user wants all data to maxima to be copied to StdErr
  static void SetPipeToStdErr(bool pipe) { m_pipeToStderr = pipe; }
  static bool GetPipeToStdErr() { return m_pipeToStderr; }

  /*! Write more data to be sent to maxima.
   *
   * \param buffer is the data's location, can be null if length is zero
   * \param length is number of bytes to be written, can be zero to resume sending
   *        any data still in the transmit buffer but unsent. This is done from
   *        the event handler and doesn't have to be cared for by the user.
   * \returns true if writing was attempted, false if there's nothing else left to send.
   */
  bool Write(const void *buffer, std::size_t length);

  //! Read whatever data is in the socket. This is normally handled by the event handler,
  //! but can be called manually to poll for data. Ideally, this should be private.
  void ReadSocket();

  //! Clear the first prompt state, based on what was read from maxima.
  //! This is called from prompt recognizer code in the wxMaxima class.
  void ClearFirstPrompt() { m_first = false; }

  enum EventCause {
    //! There's still pending data coming from Maxima. The Data member is empty at the moment.
    READ_PENDING,
    //! Maxima has sent non-XML text
    READ_MISC_TEXT,
    XML_PROMPT,
    XML_SUPPRESSOUTPUT,
    XML_WXXMLSYMBOLS,
    XML_VARIABLES,
    XML_WATCH_VARIABLES_ADD,
    XML_STATUSBAR,
    XML_HTML_MANUAL_KEYWORDS,
    XML_MATHS,
    XML_WXXML_KEY,
    //! Maxima has disconnected (possibly because the process had died).
    DISCONNECTED,
    //! A write to Maxima is still ongoing. We use this event to keep the traffic indicator alive.
    WRITE_PENDING,
    //! The transmission has failed - this is an unrecoverable error, most likely.
    WRITE_ERROR,
    STRING_FOR_XMLINSPECTOR,
  };
  void XmlInspectorActive(bool active){m_xmlInspector = active;}
private:
  //! If this is set to true by XmlInspectorActive we send all data we get to the XML inspector
  bool m_xmlInspector = false;
  /*! Send still-unsent data to wxMaxima

    \todo As we tell wxWidgets to send all data in one go at the end of a write command
    there should no more be unsent data.
   */
  void SendDataTowxMaxima();
  //! The configuration of our wxMaxima process
  Configuration *m_configuration;
  //! The thread handler for SendDataTowxMaxima, the thread that parses the data from maxima.
  std::thread m_readerTask;
  //! Handles events on the open client socket
  void SocketEvent(wxSocketEvent &event);
  //! Handles timer events
  void TimerEvent(wxTimerEvent &event);

  std::unique_ptr<wxSocketBase> m_socket;
  //! The data we receive from Maxima
  wxSocketInputStream m_socketInput;
  /*! Splits data we receive from Maxima to UTF-8

    We cannot create this as a local variable in the function that interprets
    data from Maxima as that data might end in the middle of an Unicode
    codepoint (which means the next call to SocketEvent() starts in the middle
    of a Unicode codepoint, as well). Both cases aren't handled well if we abandon
    our TextInputStream inbetween and create a new one.
   */
  wxTextInputStream m_textInput;

  /*! The data we received from Maxima

    Used by the main thread and by the thread SendDataTowxMaxima() runs in.
    We still don't need a mutex to protect it, though, as the main thread
    waits for the other to exit before writing new data to this variable.
   */
  wxString m_socketInputData;
  /*! Data we didn't manage to send to wxMaxima until now

    \todo Do we still need this variable? We tell wxWidgets to send all data in
    one go, so there should be no data be left at the end of a write command.
   */
  wxMemoryBuffer m_socketOutputData;

  //! true = Maxima still has to send us its first prompt
  bool m_first = true;
  //! true = copy all data we receive to StdErr.
  static bool m_pipeToStderr;

  /*! Search m_socketInputData for complete commands and send them to wxMaxima

    This is a restartable process that is meant to be run as a background thread
    that interprets the data maxima has sent us and sends it to wxMaxima one
    item at a time.

    Items that this task recognizes:
     - All XML tags registered in m_knownTags are sent as a whole before
       sending them to wxMaxima and in most cases this background task even
       parses the XML data beforehand.
     - All text between such commands is left as it is and sent to wxMaxima
       as a string.
     .

    If m_abortReaderThread = true this process exits as fast as possible in
    order to allow the main thread to append data to m_socketInputData, as fast
    as possible: If maxima hasn't finished sending data it is highly probable
    that m_socketInputData will contain the beginning of an XML tag, but not
    its end and therefore cannot do anything, anyway.
   */
  void SendToWxMaxima();

  /*! A timer that triggers reading data from maxima

    On MM Windows sometimes when we receive the signal that maxima has sent us
    data, but don't receive data. In those cases the data seems to arrive only after
    the next idle event. This timer wakes up the reader again, a while after we got
    the signal that data is available and long after the idle event. Hopefully the
    actual data has arrived until then.
  */
  wxTimer m_readIdleTimer{this};
  //! The names of maxima tags we want to send to wxMaxima in whole
  static std::unordered_map<wxString, EventCause, wxStringHash> m_knownTags;
  /*! True = abort SendToWxMaxima() thread as fast as possible since new data has arrived.

    If new data has arrived the probability is high that m_socketInputData does contain
    the start of a command, but not its end.
   */
  std::atomic_bool m_abortReaderThread;
};

wxDECLARE_EVENT(EVT_MAXIMA, wxThreadEvent);
#endif
