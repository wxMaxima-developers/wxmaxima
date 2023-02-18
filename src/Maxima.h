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

/*! Interface to the Maxima process
 *
 * Eventually this class will be the entire stand-alone Maxima
 * interface, factored out from wxMaxima. For now, it only provides
 * socket I/O.
 *
 * It is a source of EVT_MAXIMA events, used to asynchronously
 * decouple the I/O from the front-end. In the future, this class could run
 * on a worker thread perhaps (after it'd do more work in string processing).
 */
class Maxima : public wxEvtHandler
{
public:
  //! Construct this object when a connection is received from Maxima.
  //! The argument should be socketServer.Accept();
  explicit Maxima(wxSocketBase *socket);
  ~Maxima() override;

  wxSocketBase *Socket() const { return m_socket.get(); }

  bool IsConnected() const { return m_socket->IsConnected(); }

  void SetPipeToStdOut(bool pipe) { m_pipeToStderr = pipe; }

  /*! Write more data to be sent to maxima.
   *
   * \param buffer is the data's location, can be null if length is zero
   * \param length is number of bytes to be written, can be zero to resume sending
   *        any data still in the transmit buffer but unsent. This is done from
   *        the event handler and doesn't have to be cared for by the user.
   * \returns true if writing was attempted, false if there's nothing else left to send.
   */
  bool Write(const void *buffer, size_t length);

  //! Read whatever data is in the socket. This is normally handled by the event handler,
  //! but can be called manually to poll for data. Ideally, this should be private.
  void ReadSocket();

  //! Clear the first prompt state, based on what was read from maxima.
  //! This is called from prompt recognizer code in the wxMaxima class.
  void ClearFirstPrompt() { m_first = false; }

private:
  //! Handles events on the open client socket
  void SocketEvent(wxSocketEvent &event);
  //! Handles timer events
  void TimerEvent(wxTimerEvent &event);

  std::unique_ptr<wxSocketBase> m_socket;
  wxSocketInputStream m_socketInput;
  wxTextInputStream m_textInput;

  wxString m_socketInputData;
  wxMemoryBuffer m_socketOutputData;

  bool m_first = true;
  bool m_pipeToStderr = false;

  wxTimer m_stringEndTimer{this};
  wxTimer m_readIdleTimer{this};
  static wxChar m_nullChar;
  static wxChar m_ascii10;
  static wxChar m_ascii13;
};

class MaximaEvent : public wxEvent
{
public:
  enum Cause {
    //! There's still pending data coming from Maxima. The Data member is empty at the moment.
    READ_PENDING,
    //! A complete response has been read from Maxima.
    READ_DATA,
    //! Reading from Maxima had timed out while awaiting an end marker - partial response is provided.
    READ_TIMEOUT,
    //! Maxima has disconnected (possibly because the process had died).
    DISCONNECTED,
    //! A write to Maxima is still ongoing. We use this event to keep the traffic indicator alive.
    WRITE_PENDING,
    //! The transmission has failed - this is an unrecoverable error, most likely.
    WRITE_ERROR,
  };
  MaximaEvent(Cause cause, Maxima *source);
  MaximaEvent(Cause cause, Maxima *source, wxString &&data);
  wxEvent *Clone() const override;
  Cause GetCause() const { return m_cause; }
  Maxima *GetSource() const { return m_source; }
  const wxString &GetData() const { return m_data; }
  wxString &GetData() { return m_data; }
  void SetData(const wxString &data) { m_data = data; }
private:
  Cause m_cause;
  Maxima *m_source;
  wxString m_data;
};

wxDECLARE_EVENT(EVT_MAXIMA, MaximaEvent);

#endif
