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
  Appends Maxima's (and wxMaxima's own) output to the worksheet.

  Turning a chunk of text or a parsed MathML-like XML fragment into cells and
  inserting them into the worksheet used to be part of the wxMaxima god class.
  That output-appending is peeled off into this class to shrink wxMaxima.cpp.
  The appenders still drive the frame (its worksheet, parser, status bar,
  evaluator) through the m_wxMaxima reference, so MaximaOutputAppender is a
  friend of wxMaxima; the wxMaxima frame owns this object by value.
*/

#ifndef MAXIMAOUTPUTAPPENDER_H
#define MAXIMAOUTPUTAPPENDER_H

#include <wx/string.h>
#include <wx/xml/xml.h>
#include "cells/Cell.h" // CellType

class wxMaxima;
class TextCell;

class MaximaOutputAppender {
public:
  explicit MaximaOutputAppender(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Options controlling how an appended cell is laid out. A bit set.
  enum AppendOpt {
    NewLine = 1,
    BigSkip = 2,
    PromptToolTip = 4,
    DefaultOpt = NewLine | BigSkip
  };

  //! Append one line \p s of the given \p type to the console, dispatching on
  //! the type (plain text goes verbatim, prompts/other run through the parser).
  TextCell *ConsoleAppend(wxString s, CellType type);

  //! Append a parsed 2D-maths XML fragment to the console.
  void ConsoleAppend(wxXmlDocument xml, CellType type,
                     const wxString &userLabel = {});

  //! Parse \p s as MathML-like XML into a cell and insert it.
  void DoConsoleAppend(wxString s, CellType type, AppendOpt opts = DefaultOpt,
                       const wxString &userLabel = {});

  /*! Append one or more lines of ordinary unicode text to the console.

    \return A pointer to the last line that was appended or NULL if there is
    no such line.
  */
  TextCell *DoRawConsoleAppend(wxString s, CellType type, AppendOpt opts = {});

private:
  //! The wxMaxima frame whose services the appenders drive. Not owned; the
  //! frame owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMAOUTPUTAPPENDER_H
