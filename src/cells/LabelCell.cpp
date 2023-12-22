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

DEFINE_CELL_TYPEINFO(LabelCell)

std::unique_ptr<Cell> LabelCell::Copy(GroupCell *group) const
{
  std::unique_ptr<LabelCell> rr = std::make_unique<LabelCell>(group, m_configuration, m_displayedText, GetTextStyle() );
  return rr;
}

void LabelCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point) &&
      !(IsHidden() ||
        (GetHidableMultSign() && m_configuration->HidemultiplicationSign()))) {

    int padding = 0;
    if (GetTextStyle() != TS_ASCIIMATHS)
      padding = MC_TEXT_PADDING;

    // Hide input labels if the user wants to
    if((GetType() != MC_TYPE_MAIN_PROMPT) || (m_configuration->ShowInputLabels()))
      {
        SetTextColor(dc);
        SetFont(dc, m_fontSize_Scaled);
        dc->DrawText(m_displayedText, point.x + padding,
                     point.y - m_center + MC_TEXT_PADDING);
      }
  }
}

void LabelCell::SetUserDefinedLabel(const wxString &userDefinedLabel) {
  m_userDefinedLabel = userDefinedLabel;
  UpdateDisplayedText();
}

bool LabelCell::NeedsRecalculation(AFontSize fontSize) const {
  return TextCell::NeedsRecalculation(fontSize) ||
    (m_configuration->GetLabelChoice() != m_labelChoice_Last) ||
    (m_zoomFactor_old != m_configuration->GetZoomFactor());
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
  m_displayedText.Replace(wxS("\xDCB6"),
                          wxS("\u00A0")); // A non-breakable space
  m_displayedText.Replace(wxS("\n"), wxEmptyString);
  m_displayedText.Replace(wxS("-->"), wxS("\u2794"));
  m_displayedText.Replace(wxS(" -->"), wxS("\u2794"));
  m_displayedText.Replace(wxS(" \u2212\u2192 "), wxS("\u2794"));
  m_displayedText.Replace(wxS("->"), wxS("\u2192"));
  m_displayedText.Replace(wxS("\u2212>"), wxS("\u2192"));
}

wxString LabelCell::ToXML() const {
  wxString tag;
  if (IsHidden() || GetHidableMultSign())
    tag = wxS("h");
  else
    switch (GetTextStyle()) {
    case TS_GREEK_CONSTANT:
      tag = wxS("g");
      break;
    case TS_SPECIAL_CONSTANT:
      tag = wxS("s");
      break;
    case TS_VARIABLE:
      tag = wxS("v");
      break;
    case TS_FUNCTION:
      tag = wxS("fnm");
      break;
    case TS_NUMBER:
      tag = wxS("n");
      break;
    case TS_STRING:
      tag = wxS("st");
      break;
    case TS_LABEL:
      tag = wxS("lbl");
      break;
    case TS_USERLABEL:
      tag = wxS("lbl");
      break;
    case TS_MAIN_PROMPT:
      tag = wxS("lbl");
      break;
    case TS_OTHER_PROMPT:
      tag = wxS("lbl");
      break;
    default:
      tag = wxS("t");
    }

  wxString xmlstring = XMLescape(m_text);
  // convert it, so that the XML configuration doesn't fail

  return wxS("<") + tag + GetXMLFlags() + wxS(">") + xmlstring + wxS("</") +
    tag + wxS(">");
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
      wxS(" userdefinedlabel=\"") + XMLescape(m_userDefinedLabel) + wxS("\"");
  return flags;
}

void LabelCell::Recalculate(AFontSize fontsize) {
  TextCell::Recalculate(fontsize);
  m_width = std::max(m_width, Scale_Px(m_configuration->GetLabelWidth())  +
                  MC_TEXT_PADDING);
  m_height = std::max(m_height, static_cast<wxCoord>(m_fontSize_Scaled.Get() + Scale_Px(2)));
  m_center = m_height / 2;
  if(m_labelChoice_Last != m_configuration->GetLabelChoice())
    UpdateDisplayedText();

  m_labelChoice_Last = m_configuration->GetLabelChoice();
  m_zoomFactor_old = m_configuration->GetZoomFactor();
}

const wxString &LabelCell::GetAltCopyText() const {
  auto &text = m_altCopyText;
  text = m_text;
  if (m_configuration->UseUserLabels() && !m_userDefinedLabel.empty())
    text = wxS("(") + m_userDefinedLabel + wxS(")");
  text.Replace(wxS("\u2794"), wxS("-->"));
  text.Replace(wxS("\u2192"), wxS("->"));
  text.Trim();
  text += wxS("\t");

  return text;
}

void LabelCell::SetAltCopyText(const wxString &WXUNUSED(text)) {
  // LabelCell generates its own AltCopyText, so there's no need to set it.
  // It's a hack of sorts, but it works.
}
