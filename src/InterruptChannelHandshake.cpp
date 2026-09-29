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
  Implements InterruptChannelHandshake.
*/

#include "InterruptChannelHandshake.h"
#include <utility>

InterruptChannelHandshake::InterruptChannelHandshake(std::string token)
  : m_token(std::move(token))
{
  // An empty secret would let anyone in by sending an empty line.
  if (m_token.empty())
    m_state = State::rejected;
}

InterruptChannelHandshake::State
InterruptChannelHandshake::Feed(const char *data, std::size_t length)
{
  for (std::size_t i = 0; (i < length) && (m_state == State::pending); ++i) {
    const char ch = data[i];
    if (ch == '\n') {
      // Lisps on MS Windows may end the line with CR LF.
      if (!m_received.empty() && (m_received.back() == '\r'))
        m_received.pop_back();
      m_state = ConstantTimeEqual(m_received, m_token) ? State::accepted
                                                       : State::rejected;
    } else {
      m_received += ch;
      // Whatever is longer than the token plus a CR cannot be the token, and
      // must not be allowed to fill our memory until it sends a newline.
      if (m_received.size() > m_token.size() + 1)
        m_state = State::rejected;
    }
  }
  return m_state;
}

bool InterruptChannelHandshake::ConstantTimeEqual(const std::string &a,
                                                  const std::string &b)
{
  if (a.size() != b.size())
    return false;
  unsigned char difference = 0;
  for (std::size_t i = 0; i < a.size(); ++i)
    difference |= static_cast<unsigned char>(a[i] ^ b[i]);
  return difference == 0;
}
