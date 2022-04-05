// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
#define DPI_REFERENCE 96.0

#define wxNO_UNSAFE_WXSTRING_CONV 1
#include "Printout.h"
#include "Worksheet.h"

#include <wx/config.h>
#include <wx/busyinfo.h>
#include <wx/log.h>

#define PRINT_MARGIN_HORIZONTAL 50
#define PRINT_MARGIN_VERTICAL 50

Printout::Printout(wxString title, GroupCell *tree, double scaleFactor) :
  wxPrintout(title),
  m_configuration(GetDC(), Configuration::temporary),
  m_configPointer(&m_configuration)
{
  m_configuration.LineWidth_em(10000);
  
  m_configuration.ShowCodeCells(tree->GetConfiguration()->ShowCodeCells());
  m_configuration.ShowBrackets(tree->GetConfiguration()->PrintBrackets());
  // Don't take the ppi rate from the worksheet
  m_configuration.SetWorkSheet(NULL);

  m_configuration.ClipToDrawRegion(false);

  if(tree)
  {
    auto copy = tree->CopyList();
    m_tree = std::move(copy);
    m_tree->SetConfigurationList(&m_configPointer);

    m_scaleFactor = scaleFactor;
  }
}

Printout::~Printout()
{
  DestroyTree();
}

bool Printout::HasPage(unsigned int num)
{
  if (num > 0 && num <= m_pages.size())
    return true;
  return false;
}

bool Printout::OnPrintPage(int num)
{
  if((unsigned)num > m_pages.size())
    return false;
//  wxBusyInfo busyInfo(wxString::Format(_("Printing page %i..."),num));
  wxDC *dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();
  
  int pageWidth, pageHeight;
  int marginX, marginY;
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);
  m_configuration.SetCanvasSize({pageWidth - marginX, pageHeight - marginY});
  
  GroupCell *group = m_pages[num - 1]->GetGroup();
  if (!group)
    return true;
  if (group->GetGroupType() == GC_TYPE_PAGEBREAK)
    group = group->GetNext();
  if (!group)
    return true;

  // Print the header
  dc->SetDeviceOrigin(0,0);  
  PrintHeader(num, dc);
  
  // Print the page contents
  dc->SetDeviceOrigin(
    marginX,
    marginY + GetHeaderHeight() - m_pages[num - 1]->GetRect(true).GetTop() +
    m_configuration.Scale_Px(m_tree->GetConfiguration()->GetGroupSkip())
    );
  
  Cell *end = NULL;
  wxCoord startpoint;
  wxCoord endpoint;
  startpoint = m_pages[num - 1]->GetRect(true).GetTop();
  endpoint = startpoint + 2*pageHeight;
            
  if((m_pages.size() > (unsigned)num) && (m_pages[num]))
  {
    endpoint = m_pages[num]->GetRect(true).GetTop()-1;
    end = m_pages[num];
  }
  dc->DestroyClippingRegion();
  wxCoord len = endpoint - startpoint;
  dc->SetClippingRegion(0, startpoint, pageWidth, len);
  
  while (group &&
         (group->GetGroupType() != GC_TYPE_PAGEBREAK))
  {
    // The following line seems to mysteriously fix the "subsequent text
    // cells aren't printed" problem on linux.
    // No Idea why, though.
    dc->SetPen(wxPen(*wxBLACK, 1, wxPENSTYLE_SOLID));
    group->Draw(group->GetGroup()->GetCurrentPoint());

    if(end && (group == end->GetGroup()))
      break;

    group = group->GetNext();
  }

  wxPen pen = *(wxThePenList->FindOrCreatePen(*wxRED,
                                              10,
                                              wxPENSTYLE_SOLID)
    );
  return true;
}

bool Printout::OnBeginDocument(int startPage, int endPage)
{
  m_configuration.SetContext(*GetDC());
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return false;
  return true;
}

void Printout::BreakPages()
{
  m_configuration.SetContext(*GetDC());
  if (m_tree == NULL)
    return;

  int pageWidth, pageHeight;
  int marginX, marginY;
  int headerHeight = GetHeaderHeight();

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  wxCoord maxContentHeight = pageHeight - 2 * marginY - headerHeight;

  // The 1st page starts at the beginning of the document
  GroupCell *group = m_tree.get();
  m_pages.push_back(group);

  // Now see where the next pages should start
  for (GroupCell &group : OnList(m_tree.get()))
  {
    wxCoord pageStart = m_pages[m_pages.size()-1]->GetRect(true).GetTop();
    // Handle pagebreak cells
    if((group.GetGroupType() == GC_TYPE_PAGEBREAK) && (group.GetNext()))
    {
      m_pages.push_back(group.GetNext());
      continue;
    }

    // Add complete GroupCells as long as they fit on the page 
    if (((group.GetRect(true).GetBottom() - pageStart >
          maxContentHeight)) ||
        (&group == m_pages[m_pages.size()-1]))
    {
      if(!group.GetOutput())
      {
        if(((group.GetRect(true).GetBottom() - pageStart >
             maxContentHeight)))
          m_pages.push_back(&group);
      }
      else
      {
         // Drawing a cell assigns its output positions
        group.Recalculate();
        group.Draw(group.GetCurrentPoint());

        if((group.GetOutput()) && (group.GetOutput()->GetRect(true).GetTop() - pageStart <
                                   maxContentHeight))
        {
          wxLogMessage(wxString::Format("Page %li: Adding a partial GroupCell!",
                                        (long)m_pages
                                        .size()));
          {
            Cell *out = group.GetOutput();
            if(out->GetRect(true).GetBottom() - pageStart > maxContentHeight)
            {
              wxLogMessage(wxString::Format("Page %li: Page break after input.",
                                            (long)m_pages.size()));
              m_pages.push_back(group.GetOutput());
            }
            while(out)
            {
              pageStart = m_pages[m_pages.size()-1]->GetRect(true).GetTop();
              if(out->GetRect(true).GetBottom() - pageStart > maxContentHeight)
              {
                wxLogMessage(wxString::Format("Page %li: Page break in the output",
                                              (long)m_pages.size()));
                m_pages.push_back(out);
              }
              out = out->GetNextToDraw();
              while(out && (!out->BreakLineHere()))
                out = out->GetNextToDraw();
            }
          }
        }
        else
          m_pages.push_back(&group);
      }
    }
  }
}


void Printout::SetupData()
{
  m_configuration.SetContext(*GetDC());
  //  SetUserScale(1/DCSCALE,
  //               1/DCSCALE);
  // on MSW according to https://groups.google.com/forum/#!topic/wx-users/QF_W4g3Oe98
  // the wxFont::SetPointSize is scaled relative to the screen DPI rate in order to
  // get the right font size in pixels. Unfortunately this is true for printing, too,
  // which might employ an entirely different DPI rate.
  //
  // Also it could be shown that on a 600dpi printer the font is only half the size
  // one would get on an 300dpi printer => we need to correct the scale factor for
  // the DPI rate, too. It seems that for a 75dpi and a 300dpi printer the scaling
  // factor is 1.0.
  wxSize printPPI;
  printPPI = GetDC()->GetPPI();
  if(printPPI.x < 1)
    printPPI.x = 72;
  if(printPPI.y < 1)
    printPPI.y = 72;
  m_tree->GetConfiguration()->GetDC()->SetUserScale(1.0,1.0);
  m_configuration.SetZoomFactor_temporarily(
    printPPI.x / DPI_REFERENCE * m_tree->GetConfiguration()->PrintScale() / m_scaleFactor
    );
 
  // wxSize screenPPI;
  // screenPPI = m_tree->GetConfiguration()->GetDC()->GetPPI();
  // double oldZoomFactor = m_tree->GetConfiguration()->GetZoomFactor();
  // wxMessageDialog dialog(NULL,
  //   wxString::Format(wxT("screenPPI.x=%i,\nprintPPI.x=%i\nzoomFactor=%f\nUserScale.x=%f"),
  //     screenPPI.x, printPPI.x, oldZoomFactor, userScale_x),
  //   wxString("Printer Parameters"));
  // dialog.ShowModal();


  int pageWidth, pageHeight;
  int marginX, marginY;
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);

  m_configuration.SetClientWidth(pageWidth - 2 * marginX
    - m_configuration.Scale_Px(72) // Some additional margin to compensate for title and section indent
    - m_configuration.Scale_Px(m_configuration.GetBaseIndent()));
  m_configuration.SetClientHeight(pageHeight - 2 * marginY);

  if(m_configuration.PrintBrackets())
  {
    if(marginX < m_configuration.Scale_Px(1 + m_configuration.GetBaseIndent()))
      marginX = m_configuration.Scale_Px(1 + m_configuration.GetBaseIndent());
  }
  m_configuration.SetIndent(marginX);
  // Inform the output routines that we are printing
  m_configuration.SetPrinting(true);
  m_configuration.LineWidth_em(10000);
  Recalculate();
  BreakPages();
}

void Printout::GetPageInfo(int *minPage, int *maxPage,
                               int *fromPage, int *toPage)
{
  *minPage = 1;
  *maxPage = m_pages.size();
  *fromPage = 1;
  *toPage = m_pages.size();
}

void Printout::OnPreparePrinting()
{
  m_configuration.SetContext(*GetDC());
  SetupData();
}

void Printout::GetPageMargins(int *horizontal, int *vertical)
{
  *horizontal = (int) (m_configuration.Scale_Px(PRINT_MARGIN_HORIZONTAL));
  *vertical = (int) (m_configuration.Scale_Px(PRINT_MARGIN_VERTICAL));
}

int Printout::GetHeaderHeight()
{
  wxDC *dc = GetDC();
  int width, height;

  dc->SetFont(wxFont(m_configuration.Scale_Px(10), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &width, &height);
  return height + m_configuration.Scale_Px(12);
}

void Printout::PrintHeader(int pageNum, wxDC *dc)
{
  int page_width, page_height;
  int title_width, title_height;
  int marginX, marginY;
  int pageWidth, pageHeight;

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  dc->SetTextForeground(wxColour(wxT("grey")));
  dc->SetPen(wxPen(wxT("light grey"), m_configuration.Scale_Px(1), wxPENSTYLE_SOLID));

  dc->SetFont(wxFont(m_configuration.Scale_Px(10), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &title_width, &title_height);
  wxString page = wxString::Format(wxT("%d / %li"), pageNum, (long)m_pages.size());
  dc->GetTextExtent(page, &page_width, &page_height);

  dc->DrawText(GetTitle(), marginX, marginY);
  dc->DrawText(page, pageWidth - page_width - marginX, marginY);

  dc->DrawLine(marginX, marginY + title_height + m_configuration.Scale_Px(3),
               pageWidth - marginX, marginY + title_height + m_configuration.Scale_Px(3));

  dc->SetTextForeground(wxColour(wxT("black")));
  dc->SetPen(wxPen(wxT("black"), 1, wxPENSTYLE_SOLID));
}

void Printout::Recalculate()
{
  if(!m_tree)
    return;

  // Don't take the ppi rate from the worksheet but use a fixed one instead
  m_configuration.SetWorkSheet(NULL);
  m_configuration.SetContext(*GetDC());
  m_configuration.SetPPI(GetDC()->GetPPI());  

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);

//  marginX += m_configuration.Scale_Px(m_configuration.GetBaseIndent());

  m_tree -> ResetDataList();

  for (GroupCell &group : OnList(m_tree.get()))
    group.Recalculate();

  m_tree->UpdateYPositionList();
}

void Printout::DestroyTree()
{
  m_tree.reset();
}
