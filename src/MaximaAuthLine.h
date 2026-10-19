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
  Declares TakeMaximaAuthLine(), which checks the auth= line a Maxima may
  start its connection to wxMaxima with.
*/

#ifndef MAXIMAAUTHLINE_H
#define MAXIMAAUTHLINE_H

#include <string>

//! What TakeMaximaAuthLine() found.
enum class MaximaAuthLine {
  //! No auth= line: a Maxima that predates it. wxMathML.lisp's key decides.
  absent,
  //! The line carries the secret wxMaxima passed in MAXIMA_AUTH_CODE.
  accepted,
  //! There is an auth= line, but with something else than the secret.
  rejected
};

/*! Checks and removes the auth=<secret> line from what Maxima sent first.

  A Maxima started with -s and MAXIMA_AUTH_CODE set sends "auth=" followed
  by the variable's value as the first line on its connection to wxMaxima,
  before its banner, its pid= line and its first prompt. That lets
  wxMaxima know whom it talks to before it trusts any of these.

  The line is removed from \p text whatever its outcome, so the secret never
  reaches the log the banner is written to.

  Kept free of wxWidgets, so it can be tested on its own.

  \param text   What Maxima sent up to and including its first prompt.
  \param secret The value of MAXIMA_AUTH_CODE.
*/
MaximaAuthLine TakeMaximaAuthLine(std::string &text, const std::string &secret);

#endif // MAXIMAAUTHLINE_H
