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
  Declares the parser for the <variables> XML documents Maxima sends.

  First step of extracting wxMaxima's Maxima-XML-response handling (the
  Read* method cluster) out of the wxMaxima god class: the pure
  XML -> data-structure parsing lives here, GUI-free and unit-testable;
  wxMaxima::ReadVariables() only iterates the parsed updates and dispatches
  them to the GUI and the variable-action handlers.
*/

#ifndef MAXIMAVARIABLEUPDATES_H
#define MAXIMAVARIABLEUPDATES_H

#include <wx/string.h>
#include <wx/xml/xml.h>
#include <vector>

/*! One variable update from a <variables> document Maxima sent.

  Represents one <variable> element: its <name> and, if the variable is
  bound, the raw <value> content (exactly as sent - if stringdisp:true made
  Maxima quote a string value, the quotes are still present here; stripping
  them is the consumer's decision).
*/
struct MaximaVariableUpdate
{
  //! The variable's name
  wxString m_name;
  //! The variable's value, verbatim as Maxima sent it. Only valid if m_bound.
  wxString m_value;
  //! True = the variable is bound to a value; false = it is undefined.
  bool m_bound = false;
};

/*! Parses a <variables> XML document Maxima sent into a list of updates.

  Returns one entry per <variable> element that contains a <name>. An entry
  is bound if the element also contains a non-empty <value> node.

  The document's validity (wxXmlDocument::IsOk()) is the caller's
  responsibility; an invalid or empty document yields an empty list.
*/
std::vector<MaximaVariableUpdate>
ParseMaximaVariableUpdates(const wxXmlDocument &xmldoc);

/*! Parses the <watch_variables_add> XML document Maxima sends into the list
  of variable names to add to the watch list.

  Unlike the <variables> document above, each <variable> element here holds
  the variable's name directly as its text content.

  The document's validity (wxXmlDocument::IsOk()) is the caller's
  responsibility; an invalid or empty document yields an empty list.
*/
std::vector<wxString>
ParseWatchVariableAdditions(const wxXmlDocument &xmldoc);

#endif // MAXIMAVARIABLEUPDATES_H
