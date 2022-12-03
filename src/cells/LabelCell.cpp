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
  if (!m_configuration->ShowLabels())
    return;
  if (InUpdateRegion()) {
    SetForeground();
    wxDC *dc = m_configuration->GetDC();

    auto const index = GetLabelIndex();
    if (index != noText) {
      auto const style = m_configuration->GetStyle(
						   m_textStyle, Scale_Px(m_fontSize_scaledToFit));
      dc->SetFont(style.GetFont());
      SetToolTip(&m_userDefinedLabel);
      if (m_textStyle == TS_USERLABEL) {
        auto text = wxT("(") + m_userDefinedLabel + wxT(")");
        m_unescapeRegEx.ReplaceAll(&text, wxT("\\1"));
        dc->DrawText(text, point.x + MC_TEXT_PADDING,
                     point.y - m_center + MC_TEXT_PADDING);
      } else
        dc->DrawText(m_displayedText, point.x + MC_TEXT_PADDING,
                     point.y - m_center + MC_TEXT_PADDING);
    }
  }
}

TextCell::TextIndex LabelCell::GetLabelIndex() const {
  if (m_textStyle == TS_USERLABEL)
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
    ((m_textStyle == TS_USERLABEL) &&
     (!m_configuration->UseUserLabels())) ||
    ((m_textStyle == TS_LABEL) && (m_configuration->UseUserLabels()) &&
     (!m_userDefinedLabel.empty())) ||
    (m_configuration->GetLabelChoice() != m_labelChoice_Last);
}

void LabelCell::UpdateDisplayedText() {
  m_displayedText = m_text;

  if ((m_textStyle == TS_USERLABEL) || (m_textStyle == TS_LABEL)) {
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
  wxASSERT((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) ||
           (m_textStyle == TS_MAIN_PROMPT));

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
  m_fontSize_scaledToFit = fontsize;
  // If the config settings about how many digits to display has changed we
  // need to regenerate the info which number to show.
  if (NeedsRecalculation(fontsize)) {
    if (GetPrevious()) {
      ForceBreakLine(true);
      SetBigSkip(true);
    }
    if (m_configuration->GetLabelChoice() != m_labelChoice_Last) {
      m_labelChoice_Last = m_configuration->GetLabelChoice();
      UpdateDisplayedText();
    }

    Cell::Recalculate(fontsize);

    // If the setting has changed and we want to show a user-defined label
    // instead of an automatic one or vice versa we decide that here.
    if ((m_textStyle == TS_USERLABEL) && (!m_configuration->UseUserLabels()))
      m_textStyle = TS_LABEL;
    if ((m_textStyle == TS_LABEL) && (m_configuration->UseUserLabels()) &&
        (!m_userDefinedLabel.empty()))
      m_textStyle = TS_USERLABEL;

    // Labels and prompts are fixed width - adjust font size so that
    // they fit in
    auto const index = GetLabelIndex();
    if (index != noText) {
      Style style = m_configuration->GetStyle(
					      m_textStyle, m_configuration->GetDefaultFontSize());
      wxDC *dc = m_configuration->GetDC();
      dc->SetFont(style.GetFont());

      style.SetFontSize(Scale_Px(fontsize));
      wxSize labelSize_unscaled =
	CalculateTextSize(m_configuration->GetDC(), m_displayedText, index);
      style.SetFontSize(Scale_Px(m_fontSize_scaledToFit));
      wxSize labelSize = labelSize_unscaled;
      m_height = labelSize.GetHeight();
      m_center = m_height / 2;

      wxASSERT_MSG((labelSize.GetWidth() > 0) || (m_displayedText.IsEmpty()),
                   _("Seems like something is broken with the maths font."));

      while ((labelSize.GetWidth() + Scale_Px(fontsize) >=
              Scale_Px(m_configuration->GetLabelWidth())) &&
             (!m_fontSize_scaledToFit.IsMinimal())) {
#if wxCHECK_VERSION(3, 1, 2)
        m_fontSize_scaledToFit -= .3 + 3 * (m_width - labelSize.GetWidth()) /
	  labelSize.GetWidth() / 4;
#else
        m_fontSize_scaledToFit -=
	  1 + 3 * (m_width - labelSize.GetWidth()) / labelSize.GetWidth() / 4;
#endif
        style.SetFontSize(Scale_Px(m_fontSize_scaledToFit));
        dc->SetFont(style.GetFont());
        labelSize =
	  CalculateTextSize(m_configuration->GetDC(), m_displayedText, index);
      }
      m_width = labelSize.GetWidth() + Scale_Px(2);
      m_center = (labelSize.GetHeight()) / 2.0;
    } else {
      m_width = m_height = m_center = 0;
    }
    m_width = wxMax(m_width, Scale_Px(m_configuration->GetLabelWidth())) +
      MC_TEXT_PADDING;
  }
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
