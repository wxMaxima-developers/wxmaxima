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
  Implements the Maxima-response handlers extracted from the wxMaxima god class.
*/

#include "MaximaResponseReader.h"
#include "wxMaxima.h"
#include "EventIDs.h"

void MaximaResponseReader::ReadStatusBar(const wxXmlDocument &xmldoc) {
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);
  if(!xmldoc.IsOk())
    {
      m_wxMaxima.DoRawConsoleAppend(_("There was an error in the XML that should describe the status bar message.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      m_wxMaxima.AbortOnError();
    }
  else
    {
      wxXmlNode *node = xmldoc.GetRoot();
      if (node != NULL) {
        wxXmlNode *contents = node->GetChildren();
        if (contents)
          m_wxMaxima.StatusText(contents->GetContent(), false);
      }
    }
}

void MaximaResponseReader::ReadManualTopicNames(const wxXmlDocument &xmldoc) {
  if(xmldoc.IsOk())
    {
      std::vector<wxString> topics;
      wxXmlNode *node = xmldoc.GetRoot();
      while ((node) && (node->GetName() != wxS("html-manual-keywords")))
        node = node->GetNext();

      if (node == NULL) {
        wxLogMessage(_("No topics found in topic tag"));
      } else {
        for (wxXmlNode *entry = node->GetChildren(); entry != NULL;
             entry = entry->GetNext())
          {
            if (entry->GetName() == wxS("keyword")) {
              wxXmlNode *topic = entry->GetChildren();
              if (topic) {
                wxLogMessage(_("Received manual topic request: %s"),
                             topic->GetContent().ToUTF8().data());
                topics.push_back(topic->GetContent());
              }
            }
          }
        if (topics.empty())
          wxLogMessage(_("No topics found in topic flag"));
        else
          {
#ifdef USE_WEBVIEW
            m_wxMaxima.m_helpPane->SelectKeywords(topics);
            m_wxMaxima.ShowPane(EventIDs::menu_pane_help);
#else
            m_wxMaxima.ShowMaximaHelp(topics.front());
#endif
          }
      }
    }
  else
    {
      m_wxMaxima.DoRawConsoleAppend(_("There was an error in the XML that should describe the manual topics.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      m_wxMaxima.AbortOnError();
    }
}


/***
 * Checks if maxima displayed a new chunk of math
 */
void MaximaResponseReader::ReadMath(const wxXmlDocument &xml) {
  if(!m_wxMaxima.GetWorksheet())
    return;

  m_wxMaxima.GetWorksheet()->SetCurrentTextCell(nullptr);

  // Append everything from the "beginning of math" to the "end of math" marker
  // to the console
  if (m_wxMaxima.m_configuration.UseUserLabels()) {
    m_wxMaxima.ConsoleAppend(xml, MC_TYPE_DEFAULT,
                  m_wxMaxima.GetWorksheet()->GetEvaluationQueue().GetUserLabel());
  } else {
    m_wxMaxima.ConsoleAppend(xml, MC_TYPE_DEFAULT);
  }
}

void MaximaResponseReader::ReadLoadSymbols(const wxXmlDocument &data) {
  m_wxMaxima.GetWorksheet()->AddSymbols(data);
}
