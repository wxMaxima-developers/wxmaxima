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
  This file defines the class LongNumberCell

  LongNumberCell is the Cell that is used as a placeholder for items that
  should be overridden, before they are displayed.
*/

#include "LongNumberCell.h"
#include "CellImpl.h"
#include "CellList.h"
#include "DigitCell.h"
#include "StringUtils.h"
#include <clocale>

LongNumberCell::LongNumberCell(GroupCell *group, Configuration *config,
                               const wxString &number)
  : TextCell(group, config, number, TS_NUMBER) {
  InitBitFields_LongNumberCell();
}

LongNumberCell::LongNumberCell(GroupCell *group, const LongNumberCell &cell)
  : LongNumberCell(group, cell.m_configuration, cell.m_text) {}

DEFINE_CELL(LongNumberCell)

void LongNumberCell::UpdateDisplayedText() const {
  unsigned int displayedDigits = m_configuration->GetDisplayedDigits();
  if ((m_displayedText.Length() > displayedDigits) &&
      (!m_configuration->ShowAllDigits())) {
    int left = displayedDigits / 3;
    if (left > 30)
      left = 30;
    m_numStart = m_displayedText.Left(left);
    m_ellipsis = wxString::Format(_(wxS("\u2026[%li digits]")),
                                  static_cast<long>(m_displayedText.Length()) - 2 * left);
  } else {
    m_numStart.clear();
    m_ellipsis.clear();
  }
  m_sizeCache.clear();
}

Cell *LongNumberCell::GetInnerCell(size_t index) const
{
  if(index != 0)
    return NULL;
  else
    return m_innerCell.get();
}

void LongNumberCell::Recalculate(AFontSize fontsize) const {
  bool changed = false;
  if (IsBrokenIntoLines() && m_innerCell)
    changed |= m_innerCell->RecalculateList(fontsize);

  if (changed || NeedsRecalculation(fontsize)) {
    // If the config settings about how many digits to display has changed we
    // need to regenerate the info which number to show.
    if (ConfigChanged())
      UpdateDisplayedText();
    if (IsBrokenIntoLines()) {
      Cell::Recalculate(fontsize);
      m_width = 0;
      m_height = 0;
      m_center = 0;
      m_numStart.clear();
      m_ellipsis.clear();
    } else {
      if (m_numStart.IsEmpty())
        TextCell::Recalculate(fontsize);
      else {
        Cell::Recalculate(fontsize);
        wxDC *dc = m_configuration->GetRecalcDC();
        SetFont(dc, m_fontSize_Scaled);
        auto numStartSize = CalculateTextSize(dc, m_numStart, numberStart);
        auto ellipsisSize = CalculateTextSize(dc, m_ellipsis, ellipsis);
        m_numStartWidth = numStartSize.GetWidth();
        m_ellipsisWidth = ellipsisSize.GetWidth();
        m_width = m_numStartWidth + m_ellipsisWidth + 2 * MC_TEXT_PADDING;
        m_height = std::max(numStartSize.GetHeight(), ellipsisSize.GetHeight()) +
                   2 * MC_TEXT_PADDING;
        m_center = m_height / 2;
      }
    }
  }
}

void LongNumberCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
  if (IsBrokenIntoLines() && m_innerCell)
    m_innerCell->SetCurrentPointList(point);
}

void LongNumberCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  if (InUpdateRegion()) {
    wxPoint point = m_currentPoint;
    if (m_numStart == wxEmptyString)
      TextCell::Draw(dc, antialiassingDC);
    else {
      Cell::Draw(dc, antialiassingDC);
      if (IsBrokenIntoLines())
        return;
      SetTextColor(dc);
      SetFont(dc, m_fontSize_Scaled);
      dc->DrawText(m_numStart, point.x + MC_TEXT_PADDING,
                   point.y - m_center + MC_TEXT_PADDING);
      wxColor textColor = dc->GetTextForeground();
      wxColor backgroundColor = dc->GetTextBackground();
      dc->SetTextForeground(
                            wxColor((textColor.Red() + backgroundColor.Red()) / 2,
                                    (textColor.Green() + backgroundColor.Green()) / 2,
                                    (textColor.Blue() + backgroundColor.Blue()) / 2));
      dc->DrawText(m_ellipsis, point.x + MC_TEXT_PADDING + m_numStartWidth,
                   point.y - m_center + MC_TEXT_PADDING);
    }
  }
}

bool LongNumberCell::BreakUp() const {
  if (IsBrokenIntoLines())
    return false;

  if (!m_configuration->ShowAllDigits())
    return false;
  if (!m_configuration->LineBreaksInLongNums())
    return false;
  if (m_text.IsEmpty())
    return false;

  if (!m_innerCell) {
    int groupSize = 3;
    struct lconv *lc = localeconv();
    if (lc && lc->grouping && lc->grouping[0] > 0 && lc->grouping[0] < 127) {
      int size = lc->grouping[0];
      if (size == 3 || size == 4) {
        groupSize = size;
      }
    }

    wxString prefix = "";
    wxString suffix = "";
    wxString middle = "";

    int len = m_text.Length();
    int start = 0;
    while (start < len) {
      wxChar c = m_text[start];
      if (wxIsdigit(c) || c == '.') {
        break;
      }
      prefix += c;
      start++;
    }

    int end = len - 1;
    while (end >= start) {
      wxChar c = m_text[end];
      if (wxIsdigit(c) || c == '.') {
        break;
      }
      suffix = c + suffix;
      end--;
    }

    if (start <= end) {
      middle = m_text.Mid(start, end - start + 1);
    }

    wxString integerPart = "";
    wxString fractionalPart = "";
    int dotPos = middle.Find('.');
    bool hasDot = (dotPos != wxNOT_FOUND);
    if (hasDot) {
      integerPart = middle.Left(dotPos);
      fractionalPart = middle.Mid(dotPos + 1);
    } else {
      integerPart = middle;
    }

    std::vector<wxString> intGroups;
    int L_int = integerPart.Length();
    int firstSize = L_int % groupSize;
    if (firstSize == 0 && L_int > 0) firstSize = groupSize;
    if (firstSize > 0) {
      intGroups.push_back(integerPart.Left(firstSize));
    }
    for (int i = firstSize; i < L_int; i += groupSize) {
      intGroups.push_back(integerPart.Mid(i, groupSize));
    }

    std::vector<wxString> fracGroups;
    int L_frac = fractionalPart.Length();
    for (int i = 0; i < L_frac; i += groupSize) {
      fracGroups.push_back(fractionalPart.Mid(i, groupSize));
    }

    std::vector<wxString> finalGroups;
    if (!intGroups.empty()) {
      finalGroups.push_back(prefix + intGroups[0]);
      for (size_t i = 1; i < intGroups.size(); ++i) {
        finalGroups.push_back(intGroups[i]);
      }
      if (hasDot) {
        finalGroups.back() += wxS(".");
        for (const auto &g : fracGroups) {
          finalGroups.push_back(g);
        }
      }
    } else {
      if (hasDot) {
        if (!fracGroups.empty()) {
          finalGroups.push_back(prefix + wxS(".") + fracGroups[0]);
          for (size_t i = 1; i < fracGroups.size(); ++i) {
            finalGroups.push_back(fracGroups[i]);
          }
        } else {
          finalGroups.push_back(prefix + wxS("."));
        }
      } else {
        if (!prefix.IsEmpty()) {
          finalGroups.push_back(prefix);
        }
      }
    }

    if (!finalGroups.empty()) {
      finalGroups.back() += suffix;
    } else {
      finalGroups.push_back(prefix + suffix);
    }

    Cell *last = NULL;
    for (const auto &groupStr : finalGroups) {
      if (!last) {
        m_innerCell = std::make_unique<DigitCell>(GetGroup(), m_configuration,
                                                  groupStr, TS_NUMBER);
        last = m_innerCell.get();
      } else {
        CellList::AppendCell(last, std::make_unique<DigitCell>(GetGroup(),
                                                               m_configuration,
                                                               groupStr, TS_NUMBER));
        last = last->GetNext();
      }
    }
  }
  m_innerCell->last()->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_innerCell;
  Cell::BreakUpAndMark();
  return true;
}

void LongNumberCell::SetNextToDraw(Cell *next) const {
  if (IsBrokenIntoLines())
    m_innerCell->last()->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
