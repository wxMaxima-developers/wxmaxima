// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class MathPrintOut

  MathPrintOut is the class that handles printing.
*/

//! Bitmaps are scaled down if the resolution of the DC is too low.
#define DPI_REFERENCE 96

#include "Printout.h"
#include "PageBreaks.h"
#include "cells/CellList.h"
#include "worksheet/Worksheet.h"

#include <wx/busyinfo.h>
#include <wx/config.h>
#include <wx/log.h>

#include <algorithm>


Printout::Printout(const wxString& title, GroupCell *tree, double scaleFactor)
  : wxPrintout(title), m_configuration(GetDC(), Configuration::temporary),
    m_configPointer(&m_configuration),
    m_scaleFactor(scaleFactor),
    m_printing(&m_configuration)
{
  // We don't want EditorCells to have linebreaks after a certain number of
  // chars. Instead we want linebreaks at the end of the physical place
  m_configuration.LineWidth_em(10000);

  // Don't take the ppi rate from the worksheet
  m_configuration.SetWorkSheet(nullptr);

  // Settings that apply to printing, but not to displaying
  m_configuration.ClipToDrawRegion(false);

  // Create our own copy of the worksheet that uses our private configuration
  if (tree) {
    m_configuration.ShowCodeCells(tree->GetConfiguration()->ShowCodeCells());
    m_configuration.ShowBrackets(tree->GetConfiguration()->PrintBrackets());
    auto copy = tree->CopyList();
    copy->SetConfigurationList(m_configPointer);
    m_tree = std::move(copy);
  }
}

bool Printout::HasPage(int num) {
  // Num starts counting with 1, m_pageStarts[n] starts counting with n=0
  return (num > 0) && (static_cast<std::size_t>(num) <= m_pageStarts.size());
}

bool Printout::OnPrintPage(int num) {
  wxLogMessage(_("Printout: Request to print page %li"), static_cast<long>(num));
  if (!HasPage(num) || !m_tree)
    return false;
  wxDC *dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();

  // Set the canvas size
  wxCoord pageWidth, pageHeight;
  dc->GetSize(&pageWidth, &pageHeight);
  pageWidth -= m_configuration.PrintMargin_Left() +
    m_configuration.PrintMargin_Right();
  pageHeight -= m_configuration.PrintMargin_Top() +
    m_configuration.PrintMargin_Bot();
  m_configuration.SetCanvasSize(wxSize(pageWidth,pageHeight));
  wxLogMessage(_("printOut: Setting the page size to (%li,%li)"),
               static_cast<long>(pageWidth),
               static_cast<long>(pageHeight));

  // The part of the worksheet this page shows: up to where the next page
  // starts. wxm::BreakIntoPages() only breaks between lines, so the
  // clipping never cuts a line in two.
  const auto page = static_cast<std::size_t>(num) - 1;
  const wxCoord startpoint = m_pageStarts.at(page);
  const wxCoord endpoint = (page + 1 < m_pageStarts.size()) ?
    m_pageStarts.at(page + 1) : startpoint + pageHeight;

  // Move the origin so the top of the page's part of the worksheet appears
  // at the top margin
  wxPoint deviceOrigin(m_configuration.PrintMargin_Left(),
                       m_configuration.PrintMargin_Top() - startpoint);
  wxLogMessage(_("Printout: Setting the device origin to %lix%li"),
               static_cast<long>(deviceOrigin.x),
               static_cast<long>(deviceOrigin.y)
               );
  dc->SetDeviceOrigin(deviceOrigin.x, deviceOrigin.y);

  dc->DestroyClippingRegion();
  wxLogMessage(_("Printout: Printing the region %li-%li"),
               static_cast<long>(startpoint),
               static_cast<long>(endpoint));
  dc->SetClippingRegion(0, startpoint, pageWidth, endpoint - startpoint);

  // Every group that reaches into this page. A page break cell draws
  // nothing; what follows it is on the next page.
  for (GroupCell &group : OnList(m_tree.get())) {
    const wxRect rect = group.GetRect();
    if (rect.GetTop() >= endpoint)
      break;
    if (group.GetGroupType() == GC_TYPE_PAGEBREAK)
      continue;
    const wxCoord bottom = std::max(rect.GetBottom(), group.GetOutputRect().GetBottom());
    if (bottom < startpoint)
      continue;
    group.Draw(dc, dc);
  }
  dc->DestroyClippingRegion();
  return true;
}

bool Printout::OnBeginDocument(int startPage, int endPage) {
  m_configuration.SetRecalcContext(*GetDC());
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return false;
  return true;
}

void Printout::BreakPages() {
  m_pageStarts = wxm::BreakIntoPages(m_tree.get(), m_configuration.GetCanvasSize().y);
  wxLogMessage(wxS("Printout: %li pages"), static_cast<long>(m_pageStarts.size()));
}

void Printout::GetPageInfo(int *minPage, int *maxPage, int *fromPage,
                           int *toPage) {
  *minPage = 1;
  *maxPage = static_cast<int>(m_pageStarts.size());
  *fromPage = 1;
  *toPage = static_cast<int>(m_pageStarts.size());
}

void Printout::OnPreparePrinting() {
  m_configuration.SetRecalcContext(*GetDC());
  // on MSW according to
  // https://groups.google.com/forum/#!topic/wx-users/QF_W4g3Oe98 the
  // wxFont::SetPointSize is scaled relative to the screen DPI rate in order to
  // get the right font size in pixels. Unfortunately this is true for printing,
  // too, which might employ an entirely different DPI rate.
  //
  // Also it could be shown that on a 600dpi printer the font is only half the
  // size one would get on an 300dpi printer => we need to correct the scale
  // factor for the DPI rate, too. It seems that for a 75dpi and a 300dpi
  // printer the scaling factor is 1.0.
  wxSize printPPI;
  printPPI = GetDC()->GetPPI();
  wxLogMessage(_("Printout: Print ppi: %lix%li"),
               static_cast<long>(printPPI.x), static_cast<long>(printPPI.y));

  // A floating-point division: an integer one rounded 300dpi down to a
  // scale of 3 instead of 3.125, and anything below 96dpi down to 0.
  double scaleFactor = static_cast<double>(printPPI.x) / DPI_REFERENCE *
    m_configuration.PrintScale();
  wxLogMessage(_("Printout: Scalefactor: %e"), scaleFactor);
  m_configuration.SetZoomFactor_temporarily(scaleFactor);

  // wxSize screenPPI;
  // screenPPI = m_configuration.GetDC()->GetPPI();
  // double oldZoomFactor = m_configuration.GetZoomFactor();
  // wxMessageDialog dialog(nullptr,
  //   wxString::Format(wxS("screenPPI.x=%i,\nprintPPI.x=%i\nzoomFactor=%f\nUserScale.x=%f"),
  //     screenPPI.x, printPPI.x, oldZoomFactor, userScale_x),
  //   wxString("Printer Parameters"));
  // dialog.ShowModal();

  int pageWidth, pageHeight;
  GetDC()->GetSize(&pageWidth, &pageHeight);
  pageWidth -= m_configuration.PrintMargin_Left() +
    m_configuration.PrintMargin_Right();
  pageHeight -= m_configuration.PrintMargin_Top() +
    m_configuration.PrintMargin_Bot();
  m_configuration.SetCanvasSize(wxSize(pageWidth,pageHeight));

  //  if (m_configuration.PrintBrackets()) {
  //  if (marginX < m_configuration.Scale_Px(1 + m_configuration.GetBaseIndent()))
  //    marginX = m_configuration.Scale_Px(1 + m_configuration.GetBaseIndent());
  //}
  if(m_configuration.PrintBrackets())
    m_configuration.SetIndent(0);
  else
    m_configuration.SetIndent(-m_configuration.GetCellBracketWidth());
  m_configuration.LineWidth_em(10000);
  Recalculate();
  BreakPages();
}

void Printout::Recalculate() {
  if (!m_tree)
    return;

  wxLogMessage(_("Printout: Layouting the whole worksheet as a endless scroll of the width of the paper"));
  // Don't take the ppi rate from the worksheet but use a fixed one instead
  m_configuration.SetWorkSheet(nullptr);
  m_configuration.SetRecalcContext(*GetDC());
  m_tree->ResetSize();

  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);

  //  marginX += m_configuration.Scale_Px(m_configuration.GetBaseIndent());

  m_configuration.RecalculateForce();
  for (GroupCell &group : OnList(m_tree.get()))
    group.Recalculate();

  m_configuration.RecalculateForce();
  for (GroupCell &group : OnList(m_tree.get()))
    group.Recalculate();

  m_tree->UpdateYPositionList();
}

