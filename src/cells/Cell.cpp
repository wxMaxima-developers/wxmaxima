// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class Cell

  Cell is the base class for all cell- or list-type elements.
*/

#include "Cell.h"
#include "CellList.h"
#include "GroupCell.h"
#include "TextCell.h"
#include "VisiblyInvalidCell.h"
#include "stx/unique_cast.hpp"
#include <utility>
#include <wx/regex.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>

const wxString &Cell::GetLocalToolTip() const { return *m_toolTip; }

const wxString &Cell::GetToolTip(const wxPoint point) const {
  if (!ContainsPoint(point))
    return wxm::emptyString;

  for (const Cell &cell : OnInner(this))
    for (const Cell &tmp : OnList(&cell)) {
      auto &toolTip = tmp.GetToolTip(point);
      if (!toolTip.empty())
        return toolTip;
    }

  return *m_toolTip;
}

Cell::Cell(GroupCell *group, Configuration *config)
  : m_group(group), m_configuration(config), m_toolTip(&wxm::emptyString),
    m_fontSize_Scaled(-1) {
  wxASSERT(group && (group->GetType() == MC_TYPE_GROUP || group == this));
  InitBitFields();
  ResetSize();
}

Cell::~Cell() {
  if (m_ownsToolTip)
    wxDELETE(m_toolTip);
  m_ownsToolTip = false;

  CellList::DeleteList(this);
}

void Cell::SetType(CellType type) {
  m_type = type;

  switch (m_type) {
  case MC_TYPE_MAIN_PROMPT:
    m_textStyle = TS_MAIN_PROMPT;
    break;
  case MC_TYPE_PROMPT:
    m_textStyle = TS_OTHER_PROMPT;
    break;
  case MC_TYPE_LABEL:
    m_textStyle = TS_LABEL;
    // FIXME - this code probably intended to line-break here
    // HardLineBreak();
    break;
  case MC_TYPE_INPUT:
    m_textStyle = TS_INPUT;
    break;
  case MC_TYPE_ERROR:
    m_textStyle = TS_ERROR;
    break;
  case MC_TYPE_WARNING:
    m_textStyle = TS_WARNING;
    break;
  case MC_TYPE_TEXT:
    m_textStyle = TS_TEXT;
    break;
  case MC_TYPE_ASCIIMATHS:
    m_textStyle = TS_ASCIIMATHS;
    break;
  case MC_TYPE_SUBSUBSECTION:
    m_textStyle = TS_SUBSUBSECTION;
    break;
  case MC_TYPE_HEADING5:
    m_textStyle = TS_HEADING5;
    break;
  case MC_TYPE_HEADING6:
    m_textStyle = TS_HEADING6;
    break;
  case MC_TYPE_SUBSECTION:
    m_textStyle = TS_SUBSECTION;
    break;
  case MC_TYPE_SECTION:
    m_textStyle = TS_SECTION;
    break;
  case MC_TYPE_TITLE:
    m_textStyle = TS_TITLE;
    break;
  default:
    m_textStyle = TS_DEFAULT;
    break;
  }
  ResetSize();
  if ((m_group) && (m_group != this))
    GetGroup()->ResetSize();
}

void Cell::CopyCommonData(const Cell &cell) {
  wxASSERT(m_toolTip && !m_ownsToolTip);
  wxASSERT(cell.m_toolTip);
  if (cell.m_ownsToolTip) {
    m_ownsToolTip = true;
    m_toolTip = new wxString(*cell.m_toolTip);
  } else
    m_toolTip = cell.m_toolTip;

  m_forceBreakLine = cell.m_forceBreakLine;
  m_type = cell.m_type;
  m_textStyle = cell.m_textStyle;
  Hide(cell.m_isHidden);
  m_isHidableMultSign = cell.m_isHidableMultSign;
  if (cell.IsBrokenIntoLines())
    BreakUp();
}

std::unique_ptr<Cell> Cell::CopyList(GroupCell *group) const {
  CellListBuilder<> copy;
  for (auto &src : OnList(this))
    copy.Append(src.Copy(group));
  return std::move(copy);
}

std::unique_ptr<Cell> Cell::CopyList(GroupCell *group, const Cell *cell) {
  return cell ? cell->CopyList(group) : nullptr;
}

void Cell::ClearCacheList() {
  for (Cell &tmp : OnList(this))
    tmp.ClearCache();
}

int Cell::CellsInListRecursive() const {
  //! The number of cells the current group contains (-1, if no GroupCell)
  int cells = 0;

  for (const Cell &tmp : OnList(this)) {
    ++cells;
    for (const Cell &cell : OnInner(&tmp))
      cells += cell.CellsInListRecursive();
  }
  return cells;
}

wxRect Cell::CropToUpdateRegion(wxRect rect) const {
  if (!m_configuration->ClipToDrawRegion())
    return rect;
  else
    return rect.Intersect(m_configuration->GetUpdateRegion());
}

void Cell::FontsChangedList() {
  for (Cell &tmp : OnList(this)) {
    tmp.FontsChanged();
    for (Cell &cell : OnInner(&tmp))
      cell.FontsChangedList();
  }
}

GroupCell *Cell::GetGroup() const {
  GroupCell *group = m_group;
  wxASSERT_MSG(
	       group,
	       _("Bug: Math Cell that claims to have no group Cell it belongs to"));
  return group;
}

bool Cell::NeedsRecalculation(AFontSize fontSize) const {
  if (!HasValidSize())
    return true;
  if (GetType() == MC_TYPE_GROUP)
    return false;
  return !EqualToWithin(Scale_Px(fontSize), m_fontSize_Scaled, 0.1f);
}

int Cell::GetCenterList() const {
  if (m_maxCenter.IsInvalid()) {
    int maxCenter = 0;
    for (const Cell &tmp : OnDrawList(this)) {
      if ((&tmp != this) && (tmp.m_breakLine))
        break;
      if (!tmp.m_isBrokenIntoLines)
        maxCenter = wxMax(maxCenter, tmp.m_center);
    }
    m_maxCenter.SetCached(maxCenter);
  }
  return m_maxCenter;
}

int Cell::GetMaxDrop() const {
  if (m_maxDrop.IsInvalid()) {
    int maxDrop = 0;
    for (const Cell &tmp : OnDrawList(this)) {
      if ((&tmp != this) && (tmp.m_breakLine))
        break;
      if (!tmp.m_isBrokenIntoLines)
        maxDrop = wxMax(maxDrop, tmp.m_height - tmp.m_center);
    }
    m_maxDrop.SetCached(maxDrop);
  }
  return m_maxDrop;
}

int Cell::GetHeightList() const { return GetCenterList() + GetMaxDrop(); }

int Cell::GetFullWidth() const {
  if (m_fullWidth.IsInvalid()) {
    // We begin this calculation with a negative offset since the full width of
    // only a single cell doesn't contain the space that separates two cells -
    // that is automatically added to every cell in the next step.
    int fullWidth = 0;
    for (const Cell &tmp : OnDrawList(this)) {
      fullWidth += tmp.m_width;
    }
    m_fullWidth.SetCached(fullWidth);
  }
  return m_fullWidth;
}

int Cell::GetLineWidth() const {
  if (m_lineWidth.IsInvalid()) {
    int width = m_width;
    for (const Cell &tmp : OnDrawList(this)) {
      if (&tmp != this)
        if (tmp.m_isBrokenIntoLines || tmp.m_breakLine ||
            (tmp.m_type == MC_TYPE_MAIN_PROMPT))
          break;

      width += tmp.m_width;
    }
    m_lineWidth.SetCached(width);
  }
  return m_lineWidth;
}

/*! Draw this cell to dc

  To make this work each derived class must draw the content of the cell
  and then call MathCall::Draw(...).
*/
void Cell::Draw(wxPoint point) {
  m_configuration->NotifyOfCellRedraw(this);

  if ((point.x >= 0) && (point.y >= 0))
    SetCurrentPoint(point);

  // Mark all cells that contain tooltips
  if (!m_toolTip->empty() && (GetStyle() != TS_LABEL) &&
      (GetStyle() != TS_USERLABEL) && m_configuration->ClipToDrawRegion() &&
      !m_configuration->GetPrinting() && !m_group->GetSuppressTooltipMarker() &&
      (!m_configuration->HideMarkerForThisMessage(*m_toolTip))) {
    wxRect rect = Cell::CropToUpdateRegion(GetRect());
    if (m_configuration->InUpdateRegion(rect) && !rect.IsEmpty()) {
      wxDC *dc = m_configuration->GetDC();
      dc->SetPen(*wxTRANSPARENT_PEN);
      dc->SetBrush(m_configuration->GetTooltipBrush());
      dc->DrawRectangle(rect);
    }
  }

  // Tell the screen reader that this cell's contents might have changed.
#if wxUSE_ACCESSIBILITY
  if (m_configuration->GetWorkSheet())
    wxAccessible::NotifyEvent(0, m_configuration->GetWorkSheet(),
                              wxOBJID_CLIENT, wxOBJID_CLIENT);
#endif
}

void Cell::ClearToolTip() {
  if (m_ownsToolTip)
    const_cast<wxString *>(m_toolTip)->Truncate(0); //-V575
  else
    m_toolTip = &wxm::emptyString;
}

void Cell::SetToolTip(wxString &&tooltip) {
  if (m_ownsToolTip)
    const_cast<wxString &>(*m_toolTip) = std::move(tooltip);
  else {
    m_toolTip = nullptr;
    m_ownsToolTip = true;
    m_toolTip = new wxString(std::move(tooltip));
  }
}

void Cell::SetToolTip(const wxString *toolTip) {
  if (!toolTip)
    toolTip = &wxm::emptyString;
  if (m_ownsToolTip) {
    m_ownsToolTip = false;
    wxDELETE(m_toolTip);
  }
  m_toolTip = toolTip;
}

void Cell::AddToolTip(const wxString &tip) {
  if (tip.empty())
    return;
  if (m_ownsToolTip) {
    auto &wrToolTip = const_cast<wxString &>(*m_toolTip);
    if (!m_toolTip->empty() && !wxm::EndsWithChar(*m_toolTip, '\n'))
      wrToolTip << '\n';
    wrToolTip << tip;
  } else
    SetToolTip(wxString(tip)); // this will move from the temporary copy
}

void Cell::SetAltCopyText(const wxString &text) {
  wxASSERT_MSG(
	       text.empty(),
	       wxString::Format(_("Bug: AltCopyTexts not implemented for %s cell"),
				GetInfo().GetName()));
}

void Cell::DrawList(wxPoint point) {
  for (Cell &tmp : OnDrawList(this)) {
    tmp.Draw(point);
    point.x += tmp.m_width;
  }
}

void Cell::RecalculateList(AFontSize fontsize) {
  for (Cell &tmp : OnList(this))
    tmp.Recalculate(fontsize);
}

void Cell::ResetSizeList() {
  for (Cell &tmp : OnList(this))
    tmp.ResetSize();
}

void Cell::Recalculate(AFontSize fontsize) {
  m_fontSize_Scaled = Scale_Px(fontsize);
  ResetCellListSizes();
  m_recalculateWidths = false;
}

/*! Is this cell currently visible in the window?.
 */
bool Cell::DrawThisCell(wxPoint point) {
  SetCurrentPoint(point);

  // If the cell isn't on the worksheet we don't draw it.
  if (!HasValidPosition())
    return false;

  // If a cell is broken into lines the cells it contains are displayed but
  // not the cell itself (example: Denominator and Numerator are displayed
  // but not the horizontal line with denominator above and numerator below.
  if (m_isBrokenIntoLines)
    return false;

  return (InUpdateRegion());
}

bool Cell::HasValidSize() const {
  return !m_recalculateWidths && m_width >= 0 && m_height >= 0 && m_center >= 0;
}

bool Cell::HasStaleSize() const {
  return m_width >= 0 && m_height >= 0 && m_center >= 0;
}

bool Cell::HasValidPosition() const {
  return (m_currentPoint.x >= 0) & (m_currentPoint.y >= 0);
}

void Cell::SetConfigurationList(Configuration *config) {
  for (Cell &tmp : OnList(this))
    tmp.SetConfiguration(config);
}

void Cell::SetConfiguration(Configuration *config) {
  m_configuration = config;
  for (Cell &cell : OnInner(this))
    cell.SetConfigurationList(config);
}

wxRect Cell::GetRect(bool wholeList) const {
  if (wholeList)
    return wxRect(m_currentPoint.x, m_currentPoint.y - GetCenterList(),
                  GetLineWidth(), GetHeightList());
  else
    return wxRect(m_currentPoint.x, m_currentPoint.y - m_center, m_width,
                  m_height);
}

bool Cell::InUpdateRegion() const {
  if (!m_configuration->ClipToDrawRegion())
    return true;
  if (HasStaleSize())
    return m_configuration->InUpdateRegion(GetRect());
  if (HasValidPosition()) {
    // The cell hasn't been recalculated yet: we perform a best-attempt
    // guess at its extents. This case may happen when the canvas is being
    // scrolled by the user, or when maxima is updating it.
    wxRect cellRect;
    cellRect.SetPosition(m_currentPoint);
    cellRect.SetWidth(m_configuration->GetClientWidth() - m_currentPoint.x);
    if (m_next && m_next->HasValidPosition())
      cellRect.SetHeight(m_next->m_currentPoint.y - m_currentPoint.y);
    else
      cellRect.SetHeight(1);
    return m_configuration->InUpdateRegion(cellRect);
  }
  return false;
}

void Cell::DrawBoundingBox(wxDC &dc, bool all) {
  wxRect rect = GetRect(all);
  if (InUpdateRegion()) {
    dc.DrawRectangle(CropToUpdateRegion(rect));
  }
}

bool Cell::IsCompound() const {
  for (const Cell &tmp : OnList(this))
    if (tmp.IsOperator())
      return true;
  return false;
}

wxString Cell::ToString() const { return {}; }

static const wxString space = wxT(" ");

wxString Cell::VariablesAndFunctionsList() const {
  wxString retval;
  for (const Cell &tmp : OnDrawList(this)) {
    if ((tmp.GetStyle() == TS_LABEL) || (tmp.GetStyle() == TS_USERLABEL) ||
        (tmp.GetStyle() == TS_MAIN_PROMPT) || (tmp.GetStyle() == TS_VARIABLE) ||
        (tmp.GetStyle() == TS_FUNCTION)) {
      retval << tmp.ToString() << space;
    }
  }
  return retval;
}

wxString Cell::ListToString() const {
  wxString retval;
  bool firstline = true;

  for (const Cell &tmp : OnList(this)) {
    if ((!firstline) && (tmp.m_forceBreakLine)) {
      if (!retval.EndsWith(wxT('\n')))
        retval += wxT("\n");
      // if(
      //    (tmp.GetStyle() != TS_LABEL) &&
      //    (tmp.GetStyle() != TS_USERLABEL) &&
      //    (tmp.GetStyle() != TS_MAIN_PROMPT) &&
      //    (tmp.GetStyle() != TS_OTHER_PROMPT))
      //   retval += wxT("\t");
    }
    // if(firstline)
    // {
    //   if((tmp.GetStyle() != TS_LABEL) &&
    //      (tmp.GetStyle() != TS_USERLABEL) &&
    //      (tmp.GetStyle() != TS_MAIN_PROMPT) &&
    //      (tmp.GetStyle() != TS_OTHER_PROMPT))
    //     retval += wxT("\t");
    // }
    retval += tmp.ToString();

    firstline = false;
  }
  return retval;
}

wxString Cell::ToMatlab() const { return {}; }

wxString Cell::ListToMatlab() const {
  wxString retval;
  bool firstline = true;

  for (const Cell &tmp : OnDrawList(this)) {
    if ((!firstline) && (tmp.m_forceBreakLine)) {
      if (!retval.EndsWith(wxT('\n')))
        retval += wxT("\n");
      // if(
      //    (tmp.GetStyle() != TS_LABEL) &&
      //    (tmp.GetStyle() != TS_USERLABEL) &&
      //    (tmp.GetStyle() != TS_MAIN_PROMPT) &&
      //    (tmp.GetStyle() != TS_OTHER_PROMPT))
      //   retval += wxT("\t");
    }
    // if(firstline)
    // {
    //   if((tmp.GetStyle() != TS_LABEL) &&
    //      (tmp.GetStyle() != TS_USERLABEL) &&
    //      (tmp.GetStyle() != TS_MAIN_PROMPT) &&
    //      (tmp.GetStyle() != TS_OTHER_PROMPT))
    //     retval += wxT("\t");
    // }
    retval += tmp.ToMatlab();

    firstline = false;
  }

  return retval;
}

wxString Cell::ToTeX() const { return {}; }

wxString Cell::ListToTeX() const {
  wxString retval;
  for (const Cell &tmp : OnList(this)) {
    if (((!retval.IsEmpty()) && (tmp.m_textStyle == TS_LABEL)) ||
        (tmp.BreakLineHere()))
      retval += wxT("\\]\\[");
    retval += tmp.ToTeX();
  }
  return retval;
}

wxString Cell::ToXML() const { return {}; }

wxString Cell::ToMathML() const { return {}; }

wxString Cell::ListToMathML(bool startofline) const {
  bool highlight = false;
  wxString retval;

  // If the region to export contains linebreaks or labels we put it into a
  // table.
  bool needsTable = false;
  for (const Cell &tmp : OnList(this)) {
    if (tmp.HasHardLineBreak() || tmp.GetType() == MC_TYPE_LABEL) {
      needsTable = true;
      break;
    }
  }

  // If the list contains multiple cells we wrap them in a <mrow> in order to
  // group them into a single object.
  bool const multiCell = m_next.get();

  // Export all cells
  for (const Cell &tmp : OnList(this)) {
    // Do we need to end a highlighting region?
    if ((!tmp.m_highlight) && (highlight))
      retval += wxT("</mrow>");

    // Handle linebreaks
    if ((&tmp != this) && (tmp.HasHardLineBreak()))
      retval +=
	wxT("</mtd></mlabeledtr>\n<mlabeledtr columnalign=\"left\"><mtd>");

    // If a linebreak isn't followed by a label we need to introduce an empty
    // one.
    if ((((tmp.HasHardLineBreak()) || (startofline && (this == &tmp))) &&
         ((tmp.GetStyle() != TS_LABEL) && (tmp.GetStyle() != TS_USERLABEL))) &&
        (needsTable))
      retval += wxT("<mtext></mtext></mtd><mtd>");

    // Do we need to start a highlighting region?
    if ((tmp.m_highlight) && (!highlight))
      retval += wxT("<mrow mathcolor=\"red\">");
    highlight = tmp.m_highlight;

    retval += tmp.ToMathML();
  }

  // If the region we converted to MathML ended within a highlighted region
  // we need to close this region now.
  if (highlight)
    retval += wxT("</mrow>");

  // If we grouped multiple cells as a single object we need to cose this group
  // now
  if ((multiCell) && (!needsTable))
    retval = wxT("<mrow>") + retval + wxT("</mrow>\n");

  // If we put the region we exported into a table we need to end this table now
  if (needsTable)
    retval = wxT("<mtable>\n<mlabeledtr columnalign=\"left\"><mtd>") + retval +
      wxT("</mtd></mlabeledtr>\n</mtable>");
  return retval;
}

wxString Cell::OMML2RTF(wxXmlNode *node) {
  wxString result;

  while (node != NULL) {
    if (node->GetType() == wxXML_ELEMENT_NODE) {
      wxString ommlname = node->GetName();
      result += wxT("{\\m") + ommlname.Right(ommlname.Length() - 2);

      // Convert the attributes
      wxXmlAttribute *attributes = node->GetAttributes();
      while (attributes != NULL) {
        wxString ommlatt = attributes->GetName();
        result += wxT("{\\m") + ommlatt.Right(ommlatt.Length() - 2) + wxT(" ") +
	  attributes->GetValue() + wxT("}");
        attributes = attributes->GetNext();
      }

      // Convert all child nodes
      if (node->GetChildren() != NULL) {
        result += OMML2RTF(node->GetChildren());
      }
      result += wxT("}");
    } else
      result += wxT(" ") + RTFescape(node->GetContent());

    node = node->GetNext();
  }
  return result;
}

wxString Cell::OMML2RTF(wxString ommltext) {
  if (ommltext.empty())
    return {};

  wxString result;
  wxXmlDocument ommldoc;
  ommltext = wxT("<m:r>") + ommltext + wxT("</m:r>");

  wxStringInputStream ommlStream(ommltext);

  ommldoc.Load(ommlStream, wxT("UTF-8"));

  wxXmlNode *node = ommldoc.GetRoot();
  result += OMML2RTF(node);

  if (!result.empty() && (result != wxT("\\mr"))) {
    result = wxT("{\\mmath {\\*\\moMath") + result + wxT("}}");
  }
  return result;
}

wxString Cell::XMLescape(wxString input) {
  input.Replace(wxT("&"), wxT("&amp;"));
  input.Replace(wxT("<"), wxT("&lt;"));
  input.Replace(wxT(">"), wxT("&gt;"));
  input.Replace(wxT("'"), wxT("&apos;"));
  input.Replace(wxT("\""), wxT("&quot;"));
  return input;
}

wxString Cell::RTFescape(wxString input, bool MarkDown) {
  // Characters with a special meaning in RTF
  input.Replace("\\", "\\\\");
  input.Replace("{", "\\{");
  input.Replace("}", "\\}");
  input.Replace(wxT("\r"), "\n");

  // The Character we will use as a soft line break
  input.Replace("\r", wxm::emptyString);

  // Encode unicode characters in a rather mind-boggling way
  wxString output;
  for (size_t i = 0; i < input.Length(); i++) {
    wxChar ch = input[i];
    if (ch == wxT('\n')) {
      if (((i > 0) && (input[i - 1] == wxT('\n'))) || !MarkDown)
        output += wxT("\\par}\n{\\pard ");
      else
        output += wxT("\n");
    } else {
      if ((ch < 128) && (ch > 0)) {
        output += ch;
      } else {
        if (ch < 32768) {
          output += wxString::Format("\\u%i?", static_cast<int>(ch));
        } else {
          output += wxString::Format("\\u%i?", static_cast<int>(ch) - 65536);
        }
      }
    }
  }
  return (output);
}

wxString Cell::ToOMML() const { return {}; }

wxString Cell::ListToOMML(bool WXUNUSED(startofline)) const {
  bool multiCell = (m_next != NULL);

  wxString retval;

  // If the region to export contains linebreaks or labels we put it into a
  // table. Export all cells

  for (const Cell &tmp : OnList(this)) {
    wxString token = tmp.ToOMML();

    // End exporting the equation if we reached the end of the equation.
    if (token.empty())
      break;

    retval += token;

    // Hard linebreaks aren't supported by OMML and therefore need a new
    // equation object
    if (tmp.HasHardLineBreak())
      break;
  }

  if (multiCell && !retval.empty())
    return wxT("<m:r>") + retval + wxT("</m:r>");
  else
    return retval;
}

wxString Cell::ListToRTF(bool startofline) const {
  wxString retval;

  for (const Cell *tmp = this; tmp != NULL;) {
    wxString rtf = tmp->ToRTF();
    if (!rtf.empty()) {
      if ((GetStyle() == TS_LABEL) || ((GetStyle() == TS_USERLABEL))) {
        retval +=
	  wxT("\\par}\n{\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ") +
	  rtf + wxT("\\tab");
        startofline = false;
      } else {
        if (startofline)
          retval += wxT("\\par}\n{\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ") +
	    rtf + wxT("\\n");
        startofline = true;
      }
      tmp = tmp->GetNext();
    } else {
      if (!tmp->ListToOMML().empty()) {
        // Math!

        // set the style for this line.
        if (startofline)
          retval += wxT("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");

        retval += OMML2RTF(tmp->ListToOMML());

        startofline = true;

        // Skip the rest of this equation
        while (tmp != NULL) {
          // A non-equation item starts a new rtf item
          if (tmp->ToOMML().empty())
            break;

          // A newline starts a new equation
          if (tmp->HasHardLineBreak()) {
            tmp = tmp->GetNext();
            break;
          }

          tmp = tmp->GetNext();
        }
      } else {
        tmp = tmp->GetNext();
      }
    }
  }
  return retval;
}

void Cell::SelectPointText(wxPoint WXUNUSED(point)) {}

void Cell::SelectRectText(wxPoint WXUNUSED(one), wxPoint WXUNUSED(two)) {}

void Cell::PasteFromClipboard(bool WXUNUSED(primary)) {}

wxString Cell::ListToXML() const {
  bool highlight = false;
  wxString retval;

  for (const Cell &tmp : OnList(this)) {
    if ((tmp.GetHighlight()) && (!highlight)) {
      retval += wxT("<hl boxname=\"highlight\">\n");
      highlight = true;
    }

    if ((!tmp.GetHighlight()) && (highlight)) {
      retval += wxT("</hl>\n");
      highlight = false;
    }

    retval += tmp.ToXML();
  }

  if (highlight) {
    retval += wxT("</hl>\n");
  }

  return retval;
}

/***
 * Get the part for diff tag support - only ExptCell and SubSupCell override
 * this.
 */
wxString Cell::GetDiffPart() const {
  wxString s = ToString();
  if (s == wxEmptyString)
    return s;

  return wxT(",") + s + wxT(",1");
}

Cell::Range Cell::GetCellsInRect(const wxRect &rect) const {
  Range r = GetListCellsInRect(rect);
  if (r.first && r.last == r.first)
    return r.first->GetInnerCellsInRect(rect);
  return r;
}

Cell::Range Cell::GetListCellsInRect(const wxRect &rect) const {
  Range r = {};
  for (Cell const &tmp : OnDrawList(this))
    if (rect.Intersects(tmp.GetRect(false))) {
      auto *const cell = const_cast<Cell *>(&tmp);
      if (!r.first)
        r.first = cell;
      r.last = cell;
    }
  return r;
}

Cell::Range Cell::GetInnerCellsInRect(const wxRect &rect) const {
  Range retval = {const_cast<Cell *>(this), const_cast<Cell *>(this)};
  for (Cell const &cell : OnInner(this))
    for (Cell const &tmp : OnList(&cell))
      if (tmp.ContainsRect(rect)) {
        auto r = tmp.GetCellsInRect(rect);
        if (r.first)
          retval = r;
      }

  return retval;
}

bool Cell::ContainsRect(const wxRect &sm, bool all) const {
  wxRect big = GetRect(all);
  if (big.x <= sm.x && big.y <= sm.y && big.x + big.width >= sm.x + sm.width &&
      big.y + big.height >= sm.y + sm.height)
    return true;
  return false;
}

/*! Resets remembered size and position info for this cell and all cells inside
  it

  Resets cached data like width and the height of the current cell
  as well as the vertical position of the center. Then repeats this
  with
*/
void Cell::ResetData() {
  ResetSize();
  for (Cell &cell : OnInner(this))
    for (Cell &tmp : OnList(&cell))
      tmp.ResetData();
}

void Cell::ResetDataList() {
  for (Cell &tmp : OnList(this))
    tmp.ResetData();
}

void Cell::ResetCellListSizesList() {
  for (Cell &cell : OnList(this)) {
    cell.ResetCellListSizes();
    for (Cell &tmp : OnList(&cell))
      tmp.ResetCellListSizes();
  }
}

void Cell::ResetSize() {
  m_recalculateWidths = true;
  m_fullWidth.Invalidate();
  m_lineWidth.Invalidate();
  m_maxCenter.Invalidate();
  m_maxDrop.Invalidate();
}

void Cell::ResetCellListSizes() {
  m_fullWidth.Invalidate();
  m_lineWidth.Invalidate();
  m_maxCenter.Invalidate();
  m_maxDrop.Invalidate();
}

void Cell::SetZeroSize() {
  m_width = 0;
  m_height = 0;
  m_center = 0;
}

Cell *Cell::first() const {
  const Cell *tmp = this;
  while (tmp->m_previous)
    tmp = tmp->m_previous;

  wxASSERT(tmp);
  return const_cast<Cell *>(tmp);
}

Cell *Cell::last() const {
  const Cell *tmp = this;
  while (tmp->m_next)
    tmp = tmp->GetNext();

  wxASSERT(tmp);
  return const_cast<Cell *>(tmp);
}

bool Cell::BreakUp() { return false; }

void Cell::BreakUpAndMark() {
  wxASSERT(!m_isBrokenIntoLines);
  Cell::BreakUp();
  if (!m_isBrokenIntoLines)
    m_recalculateWidths = true;
  m_isBrokenIntoLines = true;
}

void Cell::Unbreak() {
  if (m_isBrokenIntoLines) {
    ResetData();
    m_recalculateWidths = true;
  }

  m_isBrokenIntoLines = false;
  SetNextToDraw(m_next);

  // Unbreak the inner cells, too
  for (Cell &cell : OnInner(this))
    for (Cell &tmp : OnList(&cell))
      tmp.Unbreak();
}

void Cell::UnbreakList() {
  for (Cell &tmp : OnList(this))
    tmp.Unbreak();
}

wxColour Cell::GetForegroundColor() const {
  wxColour color;
  if (m_highlight)
    color = m_configuration->GetColor(TS_HIGHLIGHT);
  else if (m_type == MC_TYPE_PROMPT)
    color = m_configuration->GetColor(TS_OTHER_PROMPT);
  else if (m_type == MC_TYPE_INPUT)
    color = m_configuration->GetColor(TS_INPUT);
  else
    color = m_configuration->GetColor(TS_DEFAULT);

  return color;
}

// cppcheck-suppress functionStatic
// cppcheck-suppress functionConst
// Set the pen in device context according to the style of the cell.
void Cell::SetPen(double lineWidth) const {
  wxDC *dc = m_configuration->GetDC();

  wxPen pen = *(wxThePenList->FindOrCreatePen(
					      GetForegroundColor(), lineWidth * m_configuration->GetDefaultLineWidth(),
					      wxPENSTYLE_SOLID));
  dc->SetPen(pen);

  if (m_configuration->GetAntialiassingDC() != dc)
    m_configuration->GetAntialiassingDC()->SetPen(pen);
}

void Cell::SetBrush() const {
  wxDC *dc = m_configuration->GetDC();

  wxBrush brush = *(wxTheBrushList->FindOrCreateBrush(GetForegroundColor()));
  dc->SetBrush(brush);

  if (m_configuration->GetAntialiassingDC() != dc)
    m_configuration->GetAntialiassingDC()->SetBrush(brush);
}

const wxString &Cell::GetValue() const { return wxm::emptyString; }

void Cell::SetForeground() {
  wxColour color;
  wxDC *dc = m_configuration->GetDC();
  if (m_highlight) {
    color = m_configuration->GetColor(TS_HIGHLIGHT);
  } else {
    switch (m_type) {
    case MC_TYPE_PROMPT:
      color = m_configuration->GetColor(TS_OTHER_PROMPT);
      break;
    case MC_TYPE_MAIN_PROMPT:
      color = m_configuration->GetColor(TS_MAIN_PROMPT);
      break;
    case MC_TYPE_ERROR:
      color = wxColour(wxT("red"));
      break;
    case MC_TYPE_WARNING:
      color = m_configuration->GetColor(TS_WARNING);
      break;
    case MC_TYPE_LABEL:
      color = m_configuration->GetColor(TS_LABEL);
      break;
    default:
      color = m_configuration->GetColor(m_textStyle);
      break;
    }
  }

  dc->SetTextForeground(color);
}

bool Cell::IsMath() const {
  return !(m_textStyle == TS_LABEL || m_textStyle == TS_USERLABEL ||
           m_textStyle == TS_INPUT);
}

#if wxUSE_ACCESSIBILITY

CellAccessible *Cell::GetAccessible() {
  if (!m_accessible)
    m_accessible = std::make_unique<CellAccessible>(this);
  return m_accessible.get();
}

wxAccStatus CellAccessible::GetDescription(int childId, wxString *description) {
  return m_cell->GetDescription(childId, description);
}

wxAccStatus Cell::GetDescription(int childId, wxString *description) const {
  if (!description)
    return wxACC_FAIL;

  if (childId == 0)
    return (*description = _("Math output")), wxACC_OK;

  Cell *childCell = {};
  if (GetChild(childId, &childCell) == wxACC_OK && childCell)
    return childCell->GetDescription(0, description);

  return (description->clear()), wxACC_FAIL;
}

wxAccStatus CellAccessible::GetParent(wxAccessible **parent) {
  Cell *parentCell = nullptr;
  auto rc = m_cell->GetParent(&parentCell);
  if (rc == wxACC_OK) {
    if (!parentCell && m_cell->GetConfiguration()->GetWorkSheet())
      return (*parent =
	      m_cell->GetConfiguration()->GetWorkSheet()->GetAccessible()),
	wxACC_OK;

    return parentCell ? (*parent = parentCell->GetAccessible()),
      wxACC_OK   : wxACC_FAIL;
  }
  return rc;
}

wxAccStatus Cell::GetParent(Cell **parent) const {
  if (!parent)
    return wxACC_FAIL;

  if (*parent != this)
    return (*parent = GetGroup()), wxACC_OK;

  *parent = nullptr; // This means the worksheet
  return wxACC_OK;
}

wxAccStatus CellAccessible::GetValue(int childId, wxString *strValue) {
  return m_cell->GetValue(childId, strValue);
}

wxAccStatus Cell::GetValue(int childId, wxString *strValue) const {
  if (!strValue)
    return wxACC_FAIL;

  Cell *childCell = nullptr;
  if (GetChild(childId, &childCell) == wxACC_OK)
    return (*strValue = childCell->ToString()), wxACC_OK;

  return (strValue->clear()), wxACC_FAIL;
}

wxAccStatus CellAccessible::GetChildCount(int *childCount) {
  if (!childCount)
    return wxACC_FAIL;

  int count = 0;
  for (Cell &cell : OnInner(m_cell))
    ++count;

  return (*childCount = count), wxACC_OK;
}

static wxAccStatus ReturnCell(wxAccStatus rc, Cell *cell,
                              wxAccessible **accCell) {
  if (accCell)
    *accCell = cell ? cell->GetAccessible() : nullptr;
  return rc;
}

wxAccStatus CellAccessible::HitTest(const wxPoint &pt, int *childId,
                                    wxAccessible **child) {
  Cell *childCell = nullptr;
  auto rc = m_cell->HitTest(pt, childId, &childCell);
  return ReturnCell(rc, childCell, child);
}

wxAccStatus Cell::HitTest(const wxPoint &pt, int *childId, Cell **child) {
  wxRect rect;
  GetLocation(rect, 0);
  // If this cell doesn't contain the point none of the sub-cells does.
  if (!rect.Contains(pt))
    return (childId && (*childId = 0)), (child && (*child = NULL)), //-V560
      wxACC_FAIL;

  int id = 0; // Child #0 is this very cell
  for (Cell &cell : OnInner(this)) {
    // GetChildCount(), GetChild(), and this loop all skip null children - thus
    // the child identifiers present the same via the accessibility API.
    // This is facilitated by the inner cell iterators not ever returning a null
    // cell :)
    ++id; // The first valid inner cell will have id #1, and so on.

    cell.GetLocation(rect, 0);
    if (rect.Contains(pt))
      return (childId && (*childId = id)), (child && (*child = &cell)),
	wxACC_OK;
  }
  return (childId && (*childId = 0)), (child && (*child = this)), //-V560
    wxACC_OK;
}

wxAccStatus CellAccessible::GetChild(int childId, wxAccessible **child) {
  Cell *childCell = nullptr;
  auto rc = m_cell->GetChild(childId, &childCell);
  return ReturnCell(rc, childCell, child);
}

wxAccStatus Cell::GetChild(int childId, Cell **child) const {
  if (!child)
    return wxACC_FAIL;

  if (childId == 0)
    return (*child = const_cast<Cell *>(this)), wxACC_OK;

  if (childId > 0)
    for (Cell &cell : OnInner(this))
      if (--childId == 0)
        return (*child = &cell), wxACC_OK;

  return wxACC_FAIL;
}

wxAccStatus CellAccessible::GetFocus(int *childId, wxAccessible **child) {
  Cell *childCell = nullptr;
  auto rc = m_cell->GetFocus(childId, &childCell);
  return ReturnCell(rc, childCell, child);
}

wxAccStatus Cell::GetFocus(int *childId, Cell **child) const {
  int id = 0;
  for (Cell &cell : OnInner(this)) {
    ++id;

    int dummy;
    if (cell.GetFocus(&dummy, child) == wxACC_OK)
      return (childId && (*childId = id)), (child && (*child = &cell)),
	wxACC_OK;
  }

  return (childId && (*childId = 0)), (child && (*child = nullptr)), //-V560
    wxACC_FAIL;
}

wxAccStatus CellAccessible::GetDefaultAction(int childId,
                                             wxString *actionName) {
  return m_cell->GetDefaultAction(childId, actionName);
}

wxAccStatus Cell::GetDefaultAction(int childId, wxString *actionName) const {
  if (!actionName)
    return wxACC_FAIL;

  if (childId == 0)
    return actionName->Clear(), wxACC_OK;

  Cell *childCell = nullptr;
  if (GetChild(childId, &childCell) == wxACC_OK && childCell)
    return childCell->GetDefaultAction(0, actionName);

  actionName->clear();
  return wxACC_OK;
}

wxAccStatus CellAccessible::GetLocation(wxRect &rect, int elementId) {
  return m_cell->GetLocation(rect, elementId);
}

wxAccStatus Cell::GetLocation(wxRect &rect, int elementId) {
  if (elementId == 0) {
    rect = wxRect(GetRect().GetTopLeft() +
		  m_configuration->GetVisibleRegion().GetTopLeft(),
                  GetRect().GetBottomRight() +
		  m_configuration->GetVisibleRegion().GetTopLeft());
    if (rect.GetTop() < 0)
      rect.SetTop(0);
    if (rect.GetLeft() < 0)
      rect.SetLeft(0);
    if (rect.GetBottom() > m_configuration->GetVisibleRegion().GetWidth())
      rect.SetBottom(m_configuration->GetVisibleRegion().GetWidth());
    if (rect.GetRight() > m_configuration->GetVisibleRegion().GetHeight())
      rect.SetRight(m_configuration->GetVisibleRegion().GetHeight());
    rect =
      wxRect(rect.GetTopLeft() + m_configuration->GetWorksheetPosition(),
	     rect.GetBottomRight() + m_configuration->GetWorksheetPosition());
    return wxACC_OK;
  }

  Cell *childCell = nullptr;
  if (GetChild(elementId, &childCell) == wxACC_OK && childCell)
    return childCell->GetLocation(rect, 0);

  return wxACC_FAIL;
}

wxAccStatus CellAccessible::GetRole(int childId, wxAccRole *role) {
  return m_cell->GetRole(childId, role);
}

wxAccStatus Cell::GetRole(int WXUNUSED(childId), wxAccRole *role) const {
  if (!role)
    return wxACC_FAIL;

  return (*role = wxROLE_SYSTEM_STATICTEXT), wxACC_OK;
}

#endif

int Cell::GetInnerCellCount() const { return 0; }

Cell *Cell::GetInnerCell(int) const {
  // This method should never have been called since there are no inner cells
  // in this class.
  wxASSERT_MSG(false, "Invalid use of GetInnerCell with no inner cells");
  return nullptr;
}
