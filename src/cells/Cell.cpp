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
#include <algorithm>
#include <utility>
#include <wx/regex.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>
#include "SvgBitmap.h"
#if wxCHECK_VERSION(3, 1, 6)
#include <wx/bmpbndl.h>
#endif

const wxString &Cell::GetLocalToolTip() const { return *m_toolTip; }

const wxString Cell::GetToolTip(const wxPoint point) const {
  if (!ContainsPoint(point))
    return wxm::emptyString;

  for (const Cell &cell : OnInner(this))
    for (const Cell &tmp : OnList(&cell)) {
      auto &toolTip = tmp.GetToolTip(point);
      if (!toolTip.empty())
        return toolTip;
    }

  return GetLocalToolTip();
}

Cell::Cell(GroupCell *group, Configuration *config)
  : m_group(group), m_configuration(config), m_toolTip(&wxm::emptyString),
    m_fontSize_Scaled(-1) {
  wxASSERT((!group) || ((group->GetType() == MC_TYPE_GROUP || group == this)));
  InitBitFields_Cell();
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
    SetStyle(TS_MAIN_PROMPT);
    break;
  case MC_TYPE_PROMPT:
    SetStyle(TS_OTHER_PROMPT);
    break;
  case MC_TYPE_LABEL:
    SetStyle(TS_LABEL);
    // FIXME - this code probably intended to line-break here
    // HardLineBreak();
    break;
  case MC_TYPE_INPUT:
    SetStyle(TS_CODE_DEFAULT);
    break;
  case MC_TYPE_ERROR:
    SetStyle(TS_ERROR);
    break;
  case MC_TYPE_WARNING:
    SetStyle(TS_WARNING);
    break;
  case MC_TYPE_TEXT:
    SetStyle(TS_TEXT);
    break;
  case MC_TYPE_ASCIIMATHS:
    SetStyle(TS_ASCIIMATHS);
    break;
  case MC_TYPE_SUBSUBSECTION:
    SetStyle(TS_SUBSUBSECTION);
    break;
  case MC_TYPE_HEADING5:
    SetStyle(TS_HEADING5);
    break;
  case MC_TYPE_HEADING6:
    SetStyle(TS_HEADING6);
    break;
  case MC_TYPE_SUBSECTION:
    SetStyle(TS_SUBSECTION);
    break;
  case MC_TYPE_SECTION:
    SetStyle(TS_SECTION);
    break;
  case MC_TYPE_TITLE:
    SetStyle(TS_TITLE);
    break;
  default:
    SetStyle(TS_MATH);
    break;
  }
  if ((m_group) && (m_group != this))
    GetGroup()->ResetSize();
}

wxBitmap Cell::BitmapFromSVG(wxString svgData, wxSize size)
{
  svgData.Replace("\"currentColor\"",
                  "\"#" + wxColor2HtmlString(GetForegroundColor()) + "\"");
  svgData.Replace("\"#FFFFFF\"",
                  "\"#" + wxColor2HtmlString(m_configuration->DefaultBackgroundColor()) + "\"");
#if wxCHECK_VERSION(3, 1, 6)
  wxBitmapBundle sumbitmap = wxBitmapBundle::FromSVG(svgData.c_str(),
                                                     size);
  // Make the bitmap hi-res, if the OS supports and needs that
  const wxWindow *worksheet = m_configuration->GetWorkSheet();
  if(worksheet)
    sumbitmap.GetPreferredBitmapSizeFor(worksheet);
  wxBitmap bmp(sumbitmap.GetBitmap(size));
#else
  SvgBitmap bmp(m_configuration->GetWorkSheet(),
                svgData,
                size);
#endif
  return bmp;
}

bool Cell::FirstLineOnlyEditor()
{
  wxASSERT(GetGroup()->GetType() == MC_TYPE_GROUP);
  return GetGroup()->FirstLineOnlyEditor();
}

wxString Cell::wxColor2HtmlString(wxColor col)
{
  return wxString::Format("%02X%02X%02X%02X",
                          (unsigned int) col.Red(),
                          (unsigned int) col.Green(),
                          (unsigned int) col.Blue(),
                          (unsigned int) col.Alpha());
}

void Cell::CopyCommonData(const Cell &cell) {
  wxASSERT(m_toolTip && !m_ownsToolTip);
  wxASSERT(cell.m_toolTip);
  if (cell.m_ownsToolTip) {
    m_ownsToolTip = true;
    if(cell.m_toolTip)
      m_toolTip = new wxString(*cell.m_toolTip);
  } else
    m_toolTip = cell.m_toolTip;

  m_forceBreakLine = cell.m_forceBreakLine;
  m_type = cell.m_type;
  SetStyle(cell.GetTextStyle());
  Hide(cell.m_isHidden);
  m_isHidableMultSign = cell.m_isHidableMultSign;
  if (cell.IsBrokenIntoLines())
    BreakUp();
}

std::unique_ptr<Cell> Cell::CopyList(GroupCell *group) const {
  CellListBuilder<> copy;
  for (auto &src : OnList(this))
    copy.Append(src.Copy(group));
  /* The warning from gcc is correct. But an old MacOs compiler errors out
     on correct code, here. */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-move"
#endif
  return std::move(copy);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
}

std::unique_ptr<Cell> Cell::CopyList(GroupCell *group, const Cell *cell) {
  return cell ? cell->CopyList(group) : nullptr;
}

void Cell::ClearCacheList() {
  for (Cell &tmp : OnList(this))
    tmp.ClearCache();
}

unsigned long Cell::CellsInListRecursive() const {
  //! The number of cells the current group contains (-1, if no GroupCell)
  unsigned cells = 0;

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
  if(!EqualToWithin(Scale_Px(fontSize), m_fontSize_Scaled, 0.1f))
    return true;
  return(ConfigChanged());
}

int Cell::GetCenterList() const {
  int maxCenter = 0;
  for (const Cell &tmp : OnDrawList(this)) {
    if ((&tmp != this) && (tmp.m_breakLine))
      break;
    if (!tmp.m_isBrokenIntoLines)
      maxCenter = std::max(maxCenter, tmp.m_center);
  }
  return maxCenter;
}

int Cell::GetMaxDrop() const {
  int maxDrop = 0;
  for (const Cell &tmp : OnDrawList(this)) {
    if ((&tmp != this) && (tmp.m_breakLine))
      break;
    if (!tmp.m_isBrokenIntoLines)
      maxDrop = std::max(maxDrop, tmp.m_height - tmp.m_center);
  }
  return maxDrop;
}

int Cell::GetHeightList() const { return GetCenterList() + GetMaxDrop(); }

int Cell::GetFullWidth() const {
  // We begin this calculation with a negative offset since the full width of
  // only a single cell doesn't contain the space that separates two cells -
  // that is automatically added to every cell in the next step.
  int fullWidth = 0;
  for (const Cell &tmp : OnDrawList(this)) {
    fullWidth += tmp.m_width;
  }
  return fullWidth;
}

int Cell::GetLineWidth() const {
  int width = m_width;
  for (const Cell &tmp : OnDrawList(this)) {
    if (&tmp != this)
      if (tmp.m_isBrokenIntoLines || tmp.m_breakLine ||
          (tmp.m_type == MC_TYPE_MAIN_PROMPT))
        break;
    width += tmp.m_width;
  }
  return width;
}

/*! Draw this cell to dc

  To make this work each derived class must draw the content of the cell
  and then call MathCell::Draw(...).
*/
void Cell::Draw(wxPoint point, wxDC *dc, wxDC *WXUNUSED(antialiassingDC)) {
  if(m_configuration->GetDebugmode())
    {
      if(!m_isHidden)
        {
          wxASSERT(m_width  >= 0);
          wxASSERT(m_height >= 0);
        }
    }
  m_configuration->NotifyOfCellRedraw(this);

  if ((point.x >= 0) && (point.y >= 0))
    SetCurrentPoint(point);

  // Mark all cells that contain tooltips
  if (!m_toolTip->empty() && (GetTextStyle() != TS_LABEL) &&
      (GetTextStyle() != TS_USERLABEL) && m_configuration->ClipToDrawRegion() &&
      !m_configuration->GetPrinting() && !m_group->GetSuppressTooltipMarker() &&
      (!m_configuration->HideMarkerForThisMessage(*m_toolTip))) {
    wxRect rect = Cell::CropToUpdateRegion(GetRect());
    if (m_configuration->InUpdateRegion(rect) && !rect.IsEmpty()) {
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

void Cell::SetToolTip(const wxString &tooltip) {
  if (m_ownsToolTip)
    const_cast<wxString &>(*m_toolTip) = tooltip;
  else {
    m_toolTip = nullptr;
    m_ownsToolTip = true;
    m_toolTip = new wxString(tooltip);
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
    if (!m_toolTip->empty() && !m_toolTip->EndsWith(wxS("\n")))
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

void Cell::SetIsExponentList() {
  for (Cell &tmp : OnList(this)) {
    tmp.SetIsExponent();
  }
}

void Cell::SetIsExponent() {
  for (Cell &tmp : OnInner(this)) {
    tmp.SetIsExponentList();
  }
}

void Cell::DrawList(wxPoint point, wxDC *dc, wxDC *adc) {
  for (Cell &tmp : OnDrawList(this)) {
    tmp.Draw(point, dc, adc);
    point.x += tmp.m_width;
  }
}

void Cell::RecalculateList(AFontSize fontsize) {
  for (Cell &tmp : OnList(this))
    tmp.Recalculate(fontsize);
}

void Cell::ResetSizeList() {
  for (Cell &tmp : OnList(this))
    {
      tmp.ResetSize();
      for (Cell &cell : OnInner(&tmp))
        cell.ResetSize();
    }
}

void Cell::Recalculate(AFontSize fontsize) {
  if(NeedsRecalculation(fontsize))
    {
      m_cellCfgCnt_last = m_configuration->CellCfgCnt();
      m_fontSize_Scaled = Scale_Px(fontsize);
    }
}

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
  return m_width >= 0 && m_height >= 0 && m_center >= 0;
}

bool Cell::HasStaleSize() const {
  return m_width >= 0 && m_height >= 0 && m_center >= 0;
}

bool Cell::HasValidPosition() const {
  return (m_currentPoint.x >= 0) && (m_currentPoint.y >= 0);
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

int Cell::GetLineIndent() const {
  if ((GetTextStyle() != TS_LABEL) &&
      (GetTextStyle() != TS_USERLABEL) &&
      (GetTextStyle() != TS_MAIN_PROMPT) &&
      (GetTextStyle() != TS_OTHER_PROMPT) &&
      (GetTextStyle() != TS_ASCIIMATHS) && m_configuration->IndentMaths())
    return Scale_Px(m_configuration->GetLabelWidth()) + 2 * MC_TEXT_PADDING;
  return 0;
}

void Cell::BreakLines_List()
{
  // 1st step: Tell all cells to display as beautiful 2d object, if that is
  // possible.
  UnBreakUpCells();
  RecalculateList(m_configuration->GetMathFontSize());

  // 2nd step: Convert all objects that are wider than a line to 1D objects that
  // (hopefully) can be broken into lines
  if(BreakUpCells())
    RecalculateList(m_configuration->GetMathFontSize());

  // 3rd step: Determine a sane maximum line width
  int fullWidth = m_configuration->GetCanvasSize().x - m_configuration->GetIndent();
  int currentWidth = GetLineIndent();
  //  if ((this->GetTextStyle() != TS_LABEL) && (this->GetTextStyle() != TS_USERLABEL))
  //  fullWidth -= m_configuration->GetIndent();

  // We don't want the text go exactly to the right border.
  fullWidth -= Scale_Px(5);

  // Don't let the layout degenerate for small window widths
  if (fullWidth < Scale_Px(150))
    fullWidth = Scale_Px(150);

  // 4th step: break the output into lines.
  if (!IsHidden()) {
    bool prevBroken = false;
    for (Cell &tmp : OnDrawList(this)) {
      if (prevBroken) {
        currentWidth += tmp.GetLineIndent();
        prevBroken = false;
      }
      wxCoord const cellWidth = tmp.GetWidth();
      tmp.SoftLineBreak(false);
      if (tmp.HasHardLineBreak() || (currentWidth + cellWidth >= fullWidth)) {
        tmp.SoftLineBreak(true);
        currentWidth = tmp.GetLineIndent();
        prevBroken = true;
      }
      currentWidth += cellWidth;
    }
  }
  ResetSize_RecursivelyList();
}

bool Cell::BreakUpCells() {
  int clientWidth =
    .8 * m_configuration->GetCanvasSize().x - m_configuration->GetIndent();
  if (clientWidth < Scale_Px(50))
    clientWidth = Scale_Px(50);

  bool lineHeightsChanged = false;
  if (!IsHidden())
    for (Cell &tmp : OnDrawList(this)) {
        if (tmp.GetWidth() < 0)
          tmp.Recalculate(m_configuration->GetMathFontSize());
        if (tmp.GetWidth() > clientWidth)
          lineHeightsChanged |= tmp.BreakUp();
    }
  return lineHeightsChanged;
}

bool Cell::UnBreakUpCells() {

  bool retval = false;
  for (Cell &tmp : OnDrawList(this)) {
    if (tmp.IsBrokenIntoLines()) {
      tmp.Unbreak();
      retval = true;
    }
  }
  if(retval)
    ResetSize_RecursivelyList();

  return retval;
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
    cellRect.SetWidth(m_configuration->GetCanvasSize().x);
    if(GetHeight() > 0)
      {
        cellRect.SetHeight(GetHeight());
      }
    else
      {
        if (m_next && m_next->HasValidPosition())
          cellRect.SetHeight(m_next->m_currentPoint.y - m_currentPoint.y);
        else
          cellRect.SetHeight(1);
      }
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

static const wxString space = wxS(" ");

wxString Cell::VariablesAndFunctionsList() const {
  wxString retval;
  for (const Cell &tmp : OnDrawList(this)) {
    if ((tmp.GetTextStyle() == TS_LABEL) || (tmp.GetTextStyle() == TS_USERLABEL) ||
        (tmp.GetTextStyle() == TS_MAIN_PROMPT) || (tmp.GetTextStyle() == TS_VARIABLE) ||
        (tmp.GetTextStyle() == TS_FUNCTION)) {
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
      if (!retval.EndsWith(wxS('\n')))
        retval += wxS("\n");
      // if(
      //    (tmp.GetTextStyle() != TS_LABEL) &&
      //    (tmp.GetTextStyle() != TS_USERLABEL) &&
      //    (tmp.GetTextStyle() != TS_MAIN_PROMPT) &&
      //    (tmp.GetTextStyle() != TS_OTHER_PROMPT))
      //   retval += wxS("\t");
    }
    // if(firstline)
    // {
    //   if((tmp.GetTextStyle() != TS_LABEL) &&
    //      (tmp.GetTextStyle() != TS_USERLABEL) &&
    //      (tmp.GetTextStyle() != TS_MAIN_PROMPT) &&
    //      (tmp.GetTextStyle() != TS_OTHER_PROMPT))
    //     retval += wxS("\t");
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
      if (!retval.EndsWith(wxS('\n')))
        retval += wxS("\n");
      // if(
      //    (tmp.GetTextStyle() != TS_LABEL) &&
      //    (tmp.GetTextStyle() != TS_USERLABEL) &&
      //    (tmp.GetTextStyle() != TS_MAIN_PROMPT) &&
      //    (tmp.GetTextStyle() != TS_OTHER_PROMPT))
      //   retval += wxS("\t");
    }
    // if(firstline)
    // {
    //   if((tmp.GetTextStyle() != TS_LABEL) &&
    //      (tmp.GetTextStyle() != TS_USERLABEL) &&
    //      (tmp.GetTextStyle() != TS_MAIN_PROMPT) &&
    //      (tmp.GetTextStyle() != TS_OTHER_PROMPT))
    //     retval += wxS("\t");
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
    if (((!retval.IsEmpty()) && (tmp.GetTextStyle() == TS_LABEL)) ||
        (tmp.BreakLineHere()))
      retval += wxS("\\]\\[");
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
      retval += wxS("</mrow>");

    // Handle linebreaks
    if ((&tmp != this) && (tmp.HasHardLineBreak()))
      retval +=
        wxS("</mtd></mlabeledtr>\n<mlabeledtr columnalign=\"left\"><mtd>");

    // If a linebreak isn't followed by a label we need to introduce an empty
    // one.
    if ((((tmp.HasHardLineBreak()) || (startofline && (this == &tmp))) &&
         ((tmp.GetTextStyle() != TS_LABEL) && (tmp.GetTextStyle() != TS_USERLABEL))) &&
        (needsTable))
      retval += wxS("<mtext></mtext></mtd><mtd>");

    // Do we need to start a highlighting region?
    if ((tmp.m_highlight) && (!highlight))
      retval += wxS("<mrow mathcolor=\"red\">");
    highlight = tmp.m_highlight;

    retval += tmp.ToMathML();
  }

  // If the region we converted to MathML ended within a highlighted region
  // we need to close this region now.
  if (highlight)
    retval += wxS("</mrow>");

  // If we grouped multiple cells as a single object we need to close this group
  // now
  if ((multiCell) && (!needsTable))
    retval = wxS("<mrow>") + retval + wxS("</mrow>\n");

  // If we put the region we exported into a table we need to end this table now
  if (needsTable)
    retval = wxS("<mtable side=\"left\">\n<mlabeledtr><mtd>") + retval +
      wxS("</mtd></mlabeledtr>\n</mtable>");
  return retval;
}

wxString Cell::OMML2RTF(wxXmlNode *node) {
  wxString result;

  while (node != NULL) {
    if (node->GetType() == wxXML_ELEMENT_NODE) {
      wxString ommlname = node->GetName();
      result += wxS("{\\m") + ommlname.Right(ommlname.Length() - 2);

      // Convert the attributes
      wxXmlAttribute *attributes = node->GetAttributes();
      while (attributes != NULL) {
        wxString ommlatt = attributes->GetName();
        result += wxS("{\\m") + ommlatt.Right(ommlatt.Length() - 2) + wxS(" ") +
          attributes->GetValue() + wxS("}");
        attributes = attributes->GetNext();
      }

      // Convert all child nodes
      if (node->GetChildren() != NULL) {
        result += OMML2RTF(node->GetChildren());
      }
      result += wxS("}");
    } else
      result += wxS(" ") + RTFescape(node->GetContent());

    node = node->GetNext();
  }
  return result;
}

wxString Cell::OMML2RTF(wxString ommltext) {
  if (ommltext.empty())
    return {};

  wxString result;
  wxXmlDocument ommldoc;
  ommltext = wxS("<m:r>") + ommltext + wxS("</m:r>");

  wxStringInputStream ommlStream(ommltext);

  ommldoc.Load(ommlStream);

  wxXmlNode *node = ommldoc.GetRoot();
  result += OMML2RTF(node);

  if (!result.empty() && (result != wxS("\\mr"))) {
    result = wxS("{\\mmath {\\*\\moMath") + result + wxS("}}");
  }
  return result;
}

wxString Cell::XMLescape(wxString input) {
  input.Replace(wxS("&"), wxS("&amp;"));
  input.Replace(wxS("<"), wxS("&lt;"));
  input.Replace(wxS(">"), wxS("&gt;"));
  input.Replace(wxS("'"), wxS("&apos;"));
  input.Replace(wxS("\""), wxS("&quot;"));
  wxString output;
  output.reserve(input.Length());
  for (const auto &i: input)
    {
      if((i < wxS('\u001F')) ||
         ((i >= wxS('\u007F')) && (i <= wxS('\u009F'))))
        output += wxString::Format("&#%03i;", static_cast<int>(i));
      else
        output += i;
    }
  return output;
}

wxString Cell::RTFescape(wxString input, bool MarkDown) {
  // Characters with a special meaning in RTF
  input.Replace(wxS("\\"), wxS("\\\\"));
  input.Replace(wxS("{"), wxS("\\{"));
  input.Replace(wxS("}"), wxS("\\}"));
  input.Replace(wxS("\r"), wxS("\n"));

  // The Character we will use as a soft line break
  input.Replace(wxS("\r"), wxm::emptyString);

  // Encode unicode characters in a rather mind-boggling way
  wxString output;
  for (size_t i = 0; i < input.Length(); i++) {
    wxChar ch = input.at(i);
    if (ch == wxS('\n')) {
      if (((i > 0) && (input.at(i - 1) == wxS('\n'))) || !MarkDown)
        output += wxS("\\par}\n{\\pard ");
      else
        output += wxS("\n");
    } else {
      if ((ch < 128) && (ch > 0)) {
        output += ch;
      } else {
        if (ch < 32768) {
          output += wxString::Format("\\u%li?", static_cast<long>(ch));
        } else {
          output += wxString::Format("\\u%li?", static_cast<long>(ch) - 65536);
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
    return wxS("<m:r>") + retval + wxS("</m:r>");
  else
    return retval;
}

wxString Cell::ListToRTF(bool startofline) const {
  wxString retval;

  for (const Cell *tmp = this; tmp != NULL;) {
    wxString rtf = tmp->ToRTF();
    if (!rtf.empty()) {
      if ((GetTextStyle() == TS_LABEL) || ((GetTextStyle() == TS_USERLABEL))) {
        retval +=
          wxS("\\par}\n{\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ") +
          rtf + wxS("\\tab");
        startofline = false;
      } else {
        if (startofline)
          retval += wxS("\\par}\n{\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ") +
            rtf + wxS("\\n");
        startofline = true;
      }
      tmp = tmp->GetNext();
    } else {
      if (!tmp->ListToOMML().empty()) {
        // Math!

        // set the style for this line.
        if (startofline)
          retval += wxS("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");

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
      retval += wxS("<hl boxname=\"highlight\">\n");
      highlight = true;
    }

    if ((!tmp.GetHighlight()) && (highlight)) {
      retval += wxS("</hl>\n");
      highlight = false;
    }

    retval += tmp.ToXML();
  }

  if (highlight) {
    retval += wxS("</hl>\n");
  }

  return retval;
}

wxString Cell::GetDiffPart() const {
  wxString s = ToString();
  if (s == wxEmptyString)
    return s;

  return wxS(",") + s + wxS(",1");
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
void Cell::ResetSize_Recursively() {
  ResetSize();
  for (Cell &cell : OnInner(this))
    for (Cell &tmp : OnList(&cell))
      tmp.ResetSize_Recursively();
}

void Cell::ResetSize_RecursivelyList() {
  for (Cell &tmp : OnList(this))
    tmp.ResetSize_Recursively();
}

void Cell::ResetSize() {
  m_cellCfgCnt_last--;
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
  if (!m_isBrokenIntoLines)
    {
      Cell::BreakUp();
      ResetSize_Recursively();
      m_isBrokenIntoLines = true;
    }
  m_height = 0;
  m_width = 0;
  m_center = 0;
  
}

void Cell::Unbreak() {
  if (m_isBrokenIntoLines) {
    ResetSize_Recursively();
    // Unbreak the inner cells, too
    for (Cell &cell : OnInner(this))
      cell.UnbreakList();
    m_isBrokenIntoLines = false;
  }
  SetNextToDraw(GetNext());
}

void Cell::UnbreakList() {
  for (Cell &tmp : OnList(this))
    tmp.Unbreak();
}

wxColour Cell::GetForegroundColor() const {
  wxColour color;
  if (m_highlight)
    color = m_configuration->GetColor(TS_HIGHLIGHT);
  else
    color = m_configuration->GetColor(GetTextStyle());

  return color;
}

// Set the pen in device context according to the style of the cell.
void Cell::SetPen(wxDC *dc, double lineWidth) const {
  wxPen pen = *(wxThePenList->FindOrCreatePen(
                                              GetForegroundColor(),
                                              lineWidth * m_configuration->GetDefaultLineWidth(),
                                              wxPENSTYLE_SOLID));
  dc->SetPen(pen);
}

void Cell::SetBrush(wxDC *dc) const {
  wxBrush brush = *(wxTheBrushList->FindOrCreateBrush(GetForegroundColor()));
  dc->SetBrush(brush);
}

const wxString &Cell::GetValue() const { return wxm::emptyString; }

void Cell::SetTextColor(wxDC *dc) {
  wxColour color;
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
      color = wxColour(wxS("red"));
      break;
    case MC_TYPE_WARNING:
      color = m_configuration->GetColor(TS_WARNING);
      break;
    case MC_TYPE_LABEL:
      color = m_configuration->GetColor(TS_LABEL);
      break;
    default:
      color = m_configuration->GetColor(GetTextStyle());
      break;
    }
  }
  dc->SetTextForeground(color);
}

bool Cell::IsMath() const {
  return !(GetTextStyle() == TS_LABEL || GetTextStyle() == TS_USERLABEL ||
           GetTextStyle() == TS_CODE_DEFAULT);
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

  description->clear();
  return wxACC_FAIL;
}

wxAccStatus CellAccessible::GetParent(wxAccessible **parent) {
  if(!parent)
    return wxACC_FAIL;

  Cell *parentCell = nullptr;
  auto rc = m_cell->GetParent(&parentCell);
  if (rc == wxACC_OK) {
    if (!parentCell && m_cell->GetConfiguration()->GetWorkSheet())
      {
        *parent =
          m_cell->GetConfiguration()->GetWorkSheet()->GetAccessible();
        return wxACC_OK;
      }
    if(parentCell)
      {
        *parent = parentCell->GetAccessible();
        return wxACC_OK;
      }
    else
      {
        *parent = NULL;
        return wxACC_OK;
      }
  }
  return rc;
}

wxAccStatus Cell::GetParent(Cell **parent) const {
  if (!parent)
    return wxACC_FAIL;

  if (*parent != this)
    {
      *parent = GetGroup();
      return wxACC_OK;
    }

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
    {
      *strValue = childCell->ToString();
      return wxACC_OK;
    }

  strValue->clear();
  return wxACC_FAIL;
}

wxAccStatus CellAccessible::GetChildCount(int *childCount) {
  if (!childCount)
    return wxACC_FAIL;

  int count = 0;
  for (const Cell &cell : OnInner(m_cell))
    ++count;

  *childCount = count;
  return wxACC_OK;
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
    {
      if(childId)
        *childId = 0;
      if(child)
        *child = NULL;
      return wxACC_FAIL;
    }

  int id = 0; // Child #0 is this very cell
  for (Cell &cell : OnInner(this)) {
    // GetChildCount(), GetChild(), and this loop all skip null children - thus
    // the child identifiers present the same via the accessibility API.
    // This is facilitated by the inner cell iterators not ever returning a null
    // cell :)
    ++id; // The first valid inner cell will have id #1, and so on.

    cell.GetLocation(rect, 0);
    if (rect.Contains(pt))
      {
        if(childId)
          *childId = id;
        if(child)
          *child = &cell;
        return wxACC_OK;
      }
  }
  if(childId)
    *childId = 0;
  if(child)
    *child = this;
  return wxACC_OK;
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
    {
      *child = const_cast<Cell *>(this);
      return wxACC_OK;
    }

  if (childId > 0)
    for (Cell &cell : OnInner(this))
      if (--childId == 0)
        {
          *child = &cell;
          return wxACC_OK;
        }

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
      {
        if(childId)
          *childId = id;
        if(child)
          *child = &cell;
        return wxACC_OK;
      }
  }

  if (childId)
    *childId = 0;
  if(child)
    *child = nullptr;
  return wxACC_FAIL;
}

wxAccStatus CellAccessible::GetDefaultAction(int childId,
                                             wxString *actionName) {
  return m_cell->GetDefaultAction(childId, actionName);
}

wxAccStatus Cell::GetDefaultAction(int childId, wxString *actionName) const {
  if (!actionName)
    return wxACC_FAIL;

  if (childId == 0)
    {
      actionName->Clear();
      return wxACC_OK;
    }

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

size_t Cell::GetInnerCellCount() const { return 0; }

Cell *Cell::GetInnerCell(size_t) const {
  // This method should never have been called since there are no inner cells
  // in this class.
  wxASSERT_MSG(false, "Invalid use of GetInnerCell with no inner cells");
  return nullptr;
}

std::ostream& operator<<(std::ostream& out, const CellType celltype){
  std::string result;
  switch(celltype){
  case MC_TYPE_DEFAULT: result = "MC_TYPE_DEFAULT"; break;
  case MC_TYPE_MAIN_PROMPT: result = "MC_TYPE_MAIN_PROMPT"; break;
  case MC_TYPE_PROMPT: result = "MC_TYPE_PROMPT"; break;
  case MC_TYPE_LABEL: result = "MC_TYPE_LABEL"; break;
  case   MC_TYPE_INPUT: result = "MC_TYPE_INPUT"; break;
  case MC_TYPE_WARNING: result = "MC_TYPE_WARNING"; break;
  case MC_TYPE_ERROR: result = "MC_TYPE_ERROR"; break;
  case MC_TYPE_TEXT: result = "MC_TYPE_TEXT"; break;
  case MC_TYPE_ASCIIMATHS: result = "MC_TYPE_ASCIIMATHS"; break;
  case MC_TYPE_SUBSECTION: result = "MC_TYPE_SUBSECTION"; break;
  case MC_TYPE_SUBSUBSECTION: result = "MC_TYPE_SUBSUBSECTION"; break;
  case MC_TYPE_HEADING5: result = "MC_TYPE_HEADING5"; break;
  case MC_TYPE_HEADING6: result = "MC_TYPE_HEADING6"; break;
  case MC_TYPE_SECTION: result = "MC_TYPE_SECTION"; break;
  case MC_TYPE_TITLE: result = "MC_TYPE_TITLE"; break;
  case MC_TYPE_IMAGE: result = "MC_TYPE_IMAGE"; break;
  case MC_TYPE_SLIDE: result = "MC_TYPE_SLIDE"; break;
  case MC_TYPE_GROUP: result = "MC_TYPE_GROUP"; break;
  default: result = "!!!Bug: Unknown cell type!!!";
  }
  return out << result;
}
