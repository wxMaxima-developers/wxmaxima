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
#include "Worksheet.h"

#include <wx/busyinfo.h>
#include <wx/config.h>
#include <wx/log.h>


Printout::Printout(wxString title, GroupCell *tree, double scaleFactor)
  : wxPrintout(title), m_configuration(GetDC(), Configuration::temporary),
    m_configPointer(&m_configuration),
    m_printing(&m_configuration)
{
  m_configuration.LineWidth_em(10000);

  // Don't take the ppi rate from the worksheet
  m_configuration.SetWorkSheet(NULL);
  m_configuration.ClipToDrawRegion(false);

  if (tree) {
    m_configuration.ShowCodeCells(tree->GetConfiguration()->ShowCodeCells());
    m_configuration.ShowBrackets(tree->GetConfiguration()->PrintBrackets());
    auto copy = tree->CopyList();
    m_tree = std::move(copy);
    m_tree->SetConfigurationList(m_configPointer);
    m_scaleFactor = scaleFactor;
  }
}

bool Printout::HasPage(int num) {
  // Num starts counting with 1, m_pages[n] starts counting with n=0 
  if ((num > 0) && (static_cast<unsigned int>(num) <= m_pages.size()))
    return true;
  return false;
}

bool Printout::OnPrintPage(int num) {
  wxLogMessage(_("Printout: Request to print page %li"), (long) num);
  // Num starts counting with 1, m_pages[n] starts counting with n=0 
  if ((unsigned)num > m_pages.size())
    return false;
  if (num <= 0)
    return false;
  //  wxBusyInfo busyInfo(wxString::Format(_("Printing page %i..."),num));
  wxDC *dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();

  // Set the canvas size
  int pageWidth, pageHeight;
  dc->GetSize(&pageWidth, &pageHeight);
  pageWidth -= m_configuration.PrintMargin_Left() +
    m_configuration.PrintMargin_Right();
  pageHeight -= m_configuration.PrintMargin_Top() +
    m_configuration.PrintMargin_Bot();
  m_configuration.SetCanvasSize(wxSize(pageWidth,pageHeight));
  wxLogMessage(_("printOut: Setting the page size to (%li,%li)"),
		 (long) pageWidth,
		 (long) pageHeight);
  GroupCell *group = m_pages[num - 1]->GetGroup();
  if (!group)
    return true;
  if (group->GetGroupType() == GC_TYPE_PAGEBREAK)
    group = group->GetNext();
  if (!group)
    return true;

  // Move the origin so the cell we want to print first appears on the top
  // of our page
  wxPoint deviceOrigin(
		       -m_configuration.PrintMargin_Left(),
		       -m_configuration.PrintMargin_Top() -
		       m_pages[num - 1]->GetRect(true).GetTop());
  wxLogMessage(_("Printout: Setting the device origin to %lix%li"),
	       (long) deviceOrigin.x,
	       (long) deviceOrigin.y
	       );
  dc->SetDeviceOrigin(deviceOrigin.x, deviceOrigin.y);

  // Print the page contents
  Cell *end = NULL;
  wxCoord startpoint;
  wxCoord endpoint;
  startpoint = m_pages[num - 1]->GetRect(true).GetTop();

  if (m_pages.size() > (unsigned)num) {
    endpoint = m_pages[num]->GetRect(true).GetTop() - 1;
    end = m_pages[num];
  }
  else {
    endpoint = startpoint + pageHeight;
  }
    
  dc->DestroyClippingRegion();
  wxCoord len = endpoint - startpoint;
  wxLogMessage(_("Printout: Printing the region %li-%li"),
	       (long) startpoint,
	       (long) endpoint);
  dc->SetClippingRegion(0, startpoint, pageWidth, len);

  while (group && (group->GetGroupType() != GC_TYPE_PAGEBREAK) &&
	 ((end == NULL) || (group != end->GetGroup()))) {
    group->Draw(group->GetGroup()->GetCurrentPoint(), dc, dc);
    group = group->GetNext();
  }
  return true;
}

bool Printout::OnBeginDocument(int startPage, int endPage) {
  m_configuration.SetRecalcContext(*GetDC());
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return false;
  return true;
}

void Printout::BreakPages() {
  if (m_tree == NULL)
    return;
  wxSize canvasSize = m_configuration.GetCanvasSize();

  std::vector <Cell*> breakingPoints;
  for (GroupCell &gr : OnList(m_tree.get())) {
    // Drawing a GroupCell makes it calculate the position of its output cells.
    gr.Draw(gr.GetCurrentPoint(), GetDC(), GetDC());
    // We can introduce a break after the input part of any group cell.
    if(gr.GetPrompt())
      breakingPoints.push_back(gr.GetPrompt());

    // We can introduce a break after each line of output of any
    // group cell.
    Cell *out = gr.GetOutput();
    if(out)
      {
	while(out)
	  {
	    if((out->BreakLineHere()) || (out->GetNext() == NULL))
	      breakingPoints.push_back(out);
	    out = out->GetNext();
	  }	    
      }
  }
    
  // The 1st page starts at the beginning of the document
  GroupCell *group = m_tree.get();
  m_pages.push_back(group);

  // Now see where the next pages should start
  wxCoord pageStart = 0;
  
  for (const auto &i : breakingPoints) {
    wxCoord pageStart = m_pages[m_pages.size() - 1]->GetRect(true).GetBottom(); 
    wxCoord pageHeight = i->GetRect(true).GetBottom() - pageStart;
    if(i->GetNext())
      pageHeight = i->GetNext()->GetRect(true).GetBottom() - pageStart;
    wxLogMessage(_("Printout: PageStart=%li, PageHeight=%li, canvasSize=%li"),
		 (long) pageStart,
		 (long) pageHeight,
		 (long) canvasSize.y);
    if(pageHeight > canvasSize.y)
      {
	m_pages.push_back(i);
      }
  }
}

void Printout::SetupData() {
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
  wxLogMessage(_("Printout: Print ppi: %lix%li"), (long)printPPI.x, (long)printPPI.y);

  double scaleFactor = printPPI.x / DPI_REFERENCE *
    m_configuration.PrintScale();
  wxLogMessage(_("Printout: Scalefactor: %ex%e"),
	       m_configuration.PrintScale(),
	       m_configuration.PrintScale());
  m_configuration.SetZoomFactor_temporarily(scaleFactor);
  
  // wxSize screenPPI;
  // screenPPI = m_configuration.GetDC()->GetPPI();
  // double oldZoomFactor = m_configuration.GetZoomFactor();
  // wxMessageDialog dialog(NULL,
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

void Printout::GetPageInfo(int *minPage, int *maxPage, int *fromPage,
                           int *toPage) {
  *minPage = 1;
  *maxPage = m_pages.size();
  *fromPage = 1;
  *toPage = m_pages.size();
}

void Printout::OnPreparePrinting() {
  m_configuration.SetRecalcContext(*GetDC());
  SetupData();
}

void Printout::Recalculate() {
  if (!m_tree)
    return;

  // Don't take the ppi rate from the worksheet but use a fixed one instead
  m_configuration.SetWorkSheet(NULL);
  m_configuration.SetRecalcContext(*GetDC());

  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);

  //  marginX += m_configuration.Scale_Px(m_configuration.GetBaseIndent());

  m_tree->ResetDataList();

  for (GroupCell &group : OnList(m_tree.get()))
    group.Recalculate();

  m_tree->UpdateYPositionList();
}

