// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class LabelCell

  Labels are TextCells that scale down automatically if they need more space
  than we got.
*/

#include "LabelCell.h"
#include "CellImpl.h"
#include "StringUtils.h"

LabelCell::LabelCell(GroupCell *group, Configuration *config,
                     wxString automaticLabel, TextStyle style)
  : TextCell(group, config, automaticLabel, style),
    m_labelChoice_Last(config->GetLabelChoice()) {
  InitBitFields();
  m_width = Scale_Px(m_configuration->GetLabelWidth());
}

// cppcheck-suppress uninitMemberVar symbolName=LabelCell::m_alt
// cppcheck-suppress uninitMemberVar symbolName=LabelCell::m_altJs
// cppcheck-suppress uninitMemberVar symbolName=LabelCell::m_initialToolTip
LabelCell::LabelCell(GroupCell *group, const LabelCell &cell)
  : TextCell(group, cell.m_configuration),
    m_userDefinedLabel(cell.m_userDefinedLabel) {}

DEFINE_CELL(LabelCell)

void LabelCell::Draw(wxPoint point) {
  Cell::Draw(point);
  if (DrawThisCell(point) &&
      !(IsHidden() ||
        (GetHidableMultSign() && m_configuration->HidemultiplicationSign()))) {

    wxDC *dc = m_configuration->GetDC();
    int padding = 0;
    if (GetStyle() != TS_ASCIIMATHS)
      padding = MC_TEXT_PADDING;

    SetForeground();
    SetFont(m_fontSize_Scaled);
    dc->DrawText(m_displayedText, point.x + padding,
                 point.y - m_center + MC_TEXT_PADDING);
  }
}

TextCell::TextIndex LabelCell::GetLabelIndex() const {
  if (GetTextStyle() == TS_USERLABEL)
    return userLabelText;
  else
    return cellText;
}

void LabelCell::SetUserDefinedLabel(const wxString &userDefinedLabel) {
  m_userDefinedLabel = userDefinedLabel;
  UpdateDisplayedText();
}

bool LabelCell::NeedsRecalculation(AFontSize fontSize) const {
  return TextCell::NeedsRecalculation(fontSize) ||
    ((GetTextStyle() == TS_USERLABEL) &&
     (!m_configuration->UseUserLabels())) ||
    ((GetTextStyle() == TS_LABEL) && (m_configuration->UseUserLabels()) &&
     (!m_userDefinedLabel.empty())) ||
    (m_configuration->GetLabelChoice() != m_labelChoice_Last);
}

void LabelCell::UpdateDisplayedText() {
  m_displayedText = m_text;

  if ((GetTextStyle() == TS_USERLABEL) || (GetTextStyle() == TS_LABEL)) {
    if (!m_configuration->ShowLabels())
      m_displayedText = wxEmptyString;
    else {
      if (m_configuration->UseUserLabels()) {
        if (m_userDefinedLabel.empty()) {
          if (m_configuration->ShowAutomaticLabels())
            m_displayedText = m_text;
          else
            m_displayedText = wxEmptyString;
        } else
          m_displayedText = m_userDefinedLabel;
      }
    }
  }
  m_displayedText.Replace(wxT("\xDCB6"),
                          wxT("\u00A0")); // A non-breakable space
  m_displayedText.Replace(wxT("\n"), wxEmptyString);
  m_displayedText.Replace(wxT("-->"), wxT("\u2794"));
  m_displayedText.Replace(wxT(" -->"), wxT("\u2794"));
  m_displayedText.Replace(wxT(" \u2212\u2192 "), wxT("\u2794"));
  m_displayedText.Replace(wxT("->"), wxT("\u2192"));
  m_displayedText.Replace(wxT("\u2212>"), wxT("\u2192"));
}

wxString LabelCell::ToXML() const {
  wxString tag;
  if (IsHidden() || GetHidableMultSign())
    tag = _T("h");
  else
    switch (GetStyle()) {
    case TS_GREEK_CONSTANT:
      tag = _T("g");
      break;
    case TS_SPECIAL_CONSTANT:
      tag = _T("s");
      break;
    case TS_VARIABLE:
      tag = _T("v");
      break;
    case TS_FUNCTION:
      tag = _T("fnm");
      break;
    case TS_NUMBER:
      tag = _T("n");
      break;
    case TS_STRING:
      tag = _T("st");
      break;
    case TS_LABEL:
      tag = _T("lbl");
      break;
    case TS_USERLABEL:
      tag = _T("lbl");
      break;
    case TS_MAIN_PROMPT:
      tag = _T("lbl");
      break;
    case TS_OTHER_PROMPT:
      tag = _T("lbl");
      break;
    default:
      tag = _T("t");
    }

  wxString xmlstring = XMLescape(m_text);
  // convert it, so that the XML configuration doesn't fail

  return wxT("<") + tag + GetXMLFlags() + wxT(">") + xmlstring + wxT("</") +
    tag + wxT(">");
}

void LabelCell::SetStyle(TextStyle style) {
  wxASSERT((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL) ||
           (GetTextStyle() == TS_MAIN_PROMPT));

  TextCell::SetStyle(style);
}

wxString LabelCell::ToString() const { return GetAltCopyText(); }

wxString LabelCell::GetXMLFlags() const {
  wxString flags = TextCell::GetXMLFlags();
  if (!m_userDefinedLabel.empty())
    flags +=
      wxT(" userdefinedlabel=\"") + XMLescape(m_userDefinedLabel) + wxT("\"");
  return flags;
}

void LabelCell::Recalculate(AFontSize fontsize) {
  TextCell::Recalculate(fontsize);
  m_width = wxMax(m_width, Scale_Px(m_configuration->GetLabelWidth())  +
    MC_TEXT_PADDING);
}

const wxString &LabelCell::GetAltCopyText() const {
  auto &text = m_altCopyText;
  text = m_text;
  if (m_configuration->UseUserLabels() && !m_userDefinedLabel.empty())
    text = wxT("(") + m_userDefinedLabel + wxT(")");
  text.Replace(wxT("\u2794"), wxT("-->"));
  text.Replace(wxT("\u2192"), wxT("->"));
  text.Trim();
  text += wxT("\t");

  return text;
}

void LabelCell::SetAltCopyText(const wxString &WXUNUSED(text)) {
  // LabelCell generates its own AltCopyText, so there's no need to set it.
  // It's a hack of sorts, but it works.
}
