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
  Implements the parser for the <variables> XML documents Maxima sends.
*/

#include "MaximaVariableUpdates.h"

std::vector<MaximaVariableUpdate>
ParseMaximaVariableUpdates(const wxXmlDocument &xmldoc) {
  std::vector<MaximaVariableUpdate> updates;

  wxXmlNode *node = xmldoc.GetRoot();
  if (node == NULL)
    return updates;

  for (wxXmlNode *vars = node->GetChildren(); vars != NULL;
       vars = vars->GetNext()) {
    MaximaVariableUpdate update;
    bool haveName = false;

    for (wxXmlNode *var = vars->GetChildren(); var != NULL;
         var = var->GetNext()) {
      if (var->GetName() == wxS("name")) {
        wxXmlNode *namenode = var->GetChildren();
        if (namenode) {
          update.m_name = namenode->GetContent();
          haveName = true;
        }
      }
      if (var->GetName() == wxS("value")) {
        wxXmlNode *valnode = var->GetChildren();
        if (valnode) {
          update.m_value = valnode->GetContent();
          update.m_bound = true;
        }
      }
    }

    if (haveName)
      updates.push_back(std::move(update));
  }
  return updates;
}

std::vector<wxString>
ParseWatchVariableAdditions(const wxXmlDocument &xmldoc) {
  std::vector<wxString> names;

  wxXmlNode *node = xmldoc.GetRoot();
  if (node == NULL)
    return names;

  for (wxXmlNode *var = node->GetChildren(); var != NULL;
       var = var->GetNext()) {
    if (var->GetName() == wxS("variable")) {
      wxXmlNode *valnode = var->GetChildren();
      if (valnode)
        names.push_back(valnode->GetContent());
    }
  }
  return names;
}
