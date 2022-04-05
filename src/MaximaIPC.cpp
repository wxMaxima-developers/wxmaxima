// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#define wxNO_UNSAFE_WXSTRING_CONV 1
#include "MaximaIPC.h"
#include "wxMaxima.h"
#include <wx/event.h>
#include <wx/hashmap.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>
#include <memory>
#include <vector>


bool MaximaIPC::m_enabled;

MaximaIPC::MaximaIPC(wxMaxima *wxm) :
    m_wxMaxima(wxm),
    m_eventTargets{
      {"wxMaxima", m_wxMaxima},
      {"Worksheet", m_wxMaxima->m_worksheet},
    }
{
}

static wxString const ipcPrefix = "<ipc>";
static wxString const ipcSuffix = "</ipc>";
static wxString const tag_ipc = "ipc";
static wxString const tag_event = "event";
static wxString const attr_target = "tgt";    // target object for the event
static wxString const attr_type = "type";     // wxEvent::EventType
static wxString const attr_id = "id";         // wxEvent::Id
static wxString const attr_int = "int";       // wxCommandEvent::Int
static wxString const attr_extralong = "el";  // wxCommandEvent::ExtraLong
static wxString const attr_string = "str";    // wxCommandEvent::String
static wxString const attr_keycode = "key";   // wxKeyCode::keyCode

#define ID_(id) {#id, wxEVT_##id}
#define ID2_(id, name) {name, wxEVT_##id}
static const std::unordered_map<wxString, wxEventType, wxStringHash> EVENT_TYPE_NAMES =
{
  ID_(MENU), ID_(KEY_DOWN), ID2_(KEY_DOWN, "KEY"),
};
#undef ID_
#undef ID2_

#define ID_(id) {#id, wxID_##id}
static const std::unordered_map<wxString, int, wxStringHash> EVENT_ID_NAMES =
{
  ID_(CUT), ID_(COPY), ID_(PASTE), ID_(SELECTALL),
};
#undef ID_

#define ID_(id) {#id, WXK_##id}
static const std::unordered_map<wxString, wxKeyCode, wxStringHash> KEY_NAMES =
{
  ID_(HOME),
};
#undef ID_

template <typename T, typename Val>
static void LookupName(Val &val, const wxString &name, const T& names)
{
  auto it = names.find(name);
  if (it != names.end())
    val = it->second;
}

void MaximaIPC::ReadInputData(wxString &data)
{
  if (!m_enabled)
    return;
  if (!data.StartsWith(ipcPrefix))
    return;
  int end = m_wxMaxima->FindTagEnd(data, ipcSuffix);
  if (end == wxNOT_FOUND)
    return;
  auto const ipcSize = end + ipcSuffix.size();
  wxString xml = data.Left(ipcSize);
  data.Remove(0, ipcSize);

  wxXmlDocument xmldoc;
  wxStringInputStream xmlStream(xml);
  xmldoc.Load(xmlStream, wxT("UTF-8"));
  wxXmlNode *node = xmldoc.GetRoot();
  if (!node)
    return;
  if (node->GetName() != tag_ipc)
    return;
  for (node = node->GetChildren(); node; node = node->GetNext())
  {
    if (node->GetType() != wxXML_ELEMENT_NODE)
      continue;
    if (node->GetName() == tag_event)
    {
      wxEvtHandler *target = m_wxMaxima;
      std::unique_ptr<wxEvent> baseEvent = {};
      wxEventType type = wxEVT_NULL;
      int id = {};
      long lval;
      wxString val;
      if (node->GetAttribute(attr_target, &val))
        LookupName(target, val, m_eventTargets);
      if (node->GetAttribute(attr_type, &val))
      {
        if (val.ToLong(&lval))
          type = wxEventType(lval);
        else
          LookupName(type, val, EVENT_TYPE_NAMES);
      }
      if (node->GetAttribute(attr_id, &val))
      {
        if (val.ToLong(&lval))
          id = lval;
        else {
          LookupName(id, val, EVENT_ID_NAMES);
        }
      }
      if (type == wxEVT_MENU)
      {
        auto event = std::make_unique<wxCommandEvent>(type, id);
        if (node->GetAttribute(attr_int, &val))
          if (val.ToLong(&lval)) event->SetInt(lval);
        if (node->GetAttribute(attr_extralong, &val))
          if (val.ToLong(&lval)) event->SetExtraLong(lval);
        if (node->GetAttribute(attr_string, &val))
          event->SetString(val);
        baseEvent = std::move(event);
      }
      else if (type == wxEVT_KEY_DOWN)
      {
        auto event = std::make_unique<wxKeyEvent>(wxEVT_KEY_DOWN);
        if (node->GetAttribute(attr_keycode, &val))
        {
          if (val.ToLong(&lval))
            event->m_keyCode = lval;
          else
            LookupName(event->m_keyCode, val, KEY_NAMES);
        }
        baseEvent = std::move(event);
      }

      if (baseEvent)
        m_queue.emplace_back(target, std::move(baseEvent));
    }
  }
}

bool MaximaIPC::DrainQueue()
{
  if (!m_enabled || m_queue.empty())
    return false;
  bool drained = false;
  if (m_queueTail < m_queue.size())
  {
    auto &entry = m_queue[m_queueTail++];
    entry.target->ProcessEvent(*entry.event);
    entry.event.reset();
    drained = true;
  }
  if (m_queueTail >= m_queue.size())
  {
    m_queueTail = 0;
    m_queue.clear();
  }
  return drained;
}
