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
  Implements TakeMaximaAuthLine().
*/

#include "MaximaAuthLine.h"
#include "InterruptChannelHandshake.h"

MaximaAuthLine TakeMaximaAuthLine(std::string &text, const std::string &secret) {
  static const std::string prefix = "auth=";

  // The line has to start a line: "auth=" in the middle of a line is part of
  // something else. Maxima sends it first, but a Lisp may put out a blank
  // line or a warning before Maxima's own code runs.
  std::size_t start = 0;
  while ((start < text.size()) && (text.compare(start, prefix.size(), prefix) != 0)) {
    start = text.find('\n', start);
    if (start == std::string::npos)
      return MaximaAuthLine::absent;
    ++start;
  }
  if (start >= text.size())
    return MaximaAuthLine::absent;

  std::size_t end = text.find('\n', start);
  // A line that isn't complete cannot be checked; and the caller only has
  // the text once the first prompt that follows it has arrived.
  const bool complete = (end != std::string::npos);
  if (!complete)
    end = text.size();
  else
    ++end;

  // InterruptChannelHandshake compares in constant time, accepts a CR LF
  // line end and rejects an empty secret.
  InterruptChannelHandshake handshake(secret);
  const std::string line = text.substr(start + prefix.size(), end - start - prefix.size());
  const bool accepted = complete &&
    (handshake.Feed(line.data(), line.size()) == InterruptChannelHandshake::State::accepted);

  text.erase(start, end - start);
  return accepted ? MaximaAuthLine::accepted : MaximaAuthLine::rejected;
}
