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
  Declares InterruptChannelHandshake, which decides whether a connection may
  interrupt Maxima.
*/

#ifndef INTERRUPTCHANNELHANDSHAKE_H
#define INTERRUPTCHANNELHANDSHAKE_H

#include <cstddef>
#include <string>

/*! Checks the first line a would-be interrupt channel sends.

  Maxima's Lisp opens a second connection to wxMaxima's server that
  wxMaxima writes "interrupt" to (see MaximaInterruptChannel and
  wx-open-interrupt-channel in wxMathML.lisp). wxMaxima's server listens on
  a local port anybody on this machine can connect to, so a connection only
  becomes the interrupt channel after its first line is the secret wxMaxima
  handed to Maxima in MAXIMA_AUTH_CODE.

  Kept free of sockets and of wxWidgets so it can be tested on its own:
  bytes go in through Feed() in whatever pieces the network delivers them.
*/
class InterruptChannelHandshake
{
public:
  enum class State { pending, accepted, rejected };

  explicit InterruptChannelHandshake(std::string token);

  /*! Processes the next \p length bytes the connection sent.

    Returns the state afterwards. Once the state is accepted or rejected it
    no longer changes: whatever a channel sends after its token is ignored.
  */
  State Feed(const char *data, std::size_t length);
  State GetState() const { return m_state; }

private:
  //! Compares in a time that doesn't depend on where the first difference is.
  static bool ConstantTimeEqual(const std::string &a, const std::string &b);

  std::string m_token;
  std::string m_received;
  State m_state = State::pending;
};

#endif // INTERRUPTCHANNELHANDSHAKE_H
