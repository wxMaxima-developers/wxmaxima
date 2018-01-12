// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file
  This file defines the class MathPrintOut

  MathPrintOut is the class that handles printing.
*/

//! Bitmaps are scaled down if the resolution of the DC is too low.
#define DCSCALE 1.0

#include "MathPrintout.h"
#include "GroupCell.h"

#include <wx/config.h>

#define PRINT_MARGIN_HORIZONTAL 5
#define PRINT_MARGIN_VERTICAL 5

MathPrintout::MathPrintout(wxString title, Configuration **configuration) : wxPrintout(title)
{
  m_configuration = configuration;
  m_oldconfig = *m_configuration;
  m_numberOfPages = 0;
  m_tree = NULL;
  m_printConfigCreated = false;
  (*m_configuration)->SetForceUpdate(true);
}

MathPrintout::~MathPrintout()
{
  DestroyTree();
  if(m_printConfigCreated)
    wxDELETE(*m_configuration);
  *m_configuration = m_oldconfig;
  MathCell::ClipToDrawRegion(true);
  (*m_configuration)->SetForceUpdate(false);
}

void MathPrintout::SetData(GroupCell *tree)
{
  m_tree = tree;
  if (m_tree != NULL)
    m_tree->BreakPage(true);
}

bool MathPrintout::HasPage(int num)
{
  if (num > 0 && num <= m_numberOfPages)
    return true;
  return false;
}

bool MathPrintout::OnPrintPage(int num)
{
  GroupCell *tmp;
  wxDC *dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();
  
  int pageWidth, pageHeight;
  int marginX, marginY;
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);
  
  // Make sure that during print nothing is outside the crop rectangle

  marginX += (*m_configuration)->Scale_Px((*m_configuration)->GetBaseIndent());

  // Go to current page
  tmp = m_pages[num - 1];

  // Print page
  if (tmp != NULL)
  {
    if (tmp->GetGroupType() == GC_TYPE_PAGEBREAK)
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    if (tmp == NULL)
      return true;

    wxPoint point;
    point.x = marginX;
    point.y = marginY + tmp->GetMaxCenter() + GetHeaderHeight();
    wxConfigBase *config = wxConfig::Get();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();

    config->Read(wxT("fontsize"), &fontsize);

    PrintHeader(num, dc);
    MathCell::ClipToDrawRegion(false);

    while (tmp != NULL && tmp->GetGroupType() != GC_TYPE_PAGEBREAK)
    {
      // The following line seems to misteriously fix the "subsequent text
      // cells aren't printed" problem on linux.
      // No Idea why, though.
      dc->SetPen(wxPen(wxT("light grey"), 1, wxPENSTYLE_SOLID));
      tmp->Draw(point, fontsize);
      if (tmp->m_next != NULL)
      {
        point.x = marginX;
        point.y += drop + tmp->m_next->GetMaxCenter();
        point.y += (*m_configuration)->Scale_Px((*m_configuration)->GetGroupSkip());
        drop = tmp->m_next->GetMaxDrop();
      }

      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
      if (tmp == NULL || tmp->BreakPageHere())
        break;
    }
    MathCell::ClipToDrawRegion(true);
    return true;
  }
  MathCell::ClipToDrawRegion(true);
  return false;
}

bool MathPrintout::OnBeginDocument(int startPage, int endPage)
{
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return false;
  return true;
}

void MathPrintout::BreakPages()
{
  if (m_tree == NULL)
    return;

  MathCell::ClipToDrawRegion(false);
  int pageWidth, pageHeight;
  int marginX, marginY;
  int headerHeight = GetHeaderHeight();

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  int currentHeight = marginY;
  int skip = (*m_configuration)->Scale_Px((*m_configuration)->GetGroupSkip());;

  GroupCell *tmp = dynamic_cast<GroupCell *>(m_tree);
  m_pages.push_back(tmp);

  m_numberOfPages = 1;
  while (tmp != NULL)
  {
    tmp->BreakPage(false);

    if (currentHeight + tmp->GetMaxHeight() + skip >= pageHeight - marginY ||
        tmp->GetGroupType() == GC_TYPE_PAGEBREAK)
    {
      if (tmp->GetGroupType() != GC_TYPE_PAGEBREAK)
        currentHeight = marginY + tmp->GetMaxHeight() + headerHeight;
      else
        currentHeight = marginY;
      tmp->BreakPage(true);
      m_pages.push_back(tmp);
      m_numberOfPages++;
    }
    else
      currentHeight += tmp->GetMaxHeight() + skip;

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  MathCell::ClipToDrawRegion(true);
}

void MathPrintout::SetupData()
{
  wxDC *dc = GetDC();  
  *m_configuration = new Configuration(*dc);
  
  m_printConfigCreated = true;
  (*m_configuration)->ShowCodeCells(m_oldconfig->ShowCodeCells());
  (*m_configuration)->ShowBrackets((*m_configuration)->PrintBrackets());
  (*m_configuration)->LineWidth_em(400);
  int pageWidth, pageHeight;
  int marginX, marginY;
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);

  dc->SetUserScale(1.0/DCSCALE,1.0/DCSCALE);
  // on MSW according to https://groups.google.com/forum/#!topic/wx-users/QF_W4g3Oe98
  // the wxFont::SetPointSize is scaled relative to the screen DPI rate in order to
  // get the right font size in pixels. Unfortunately this is true for printing, too,
  // which might employ an entirely different DPI rate.
  //
  // Also it could be shown that on a 600dpi printer the font is only half the size
  // one would get on an 300dpi printer => we need to correct the scale factor for
  // the DPI rate, too. It seems that for a 75dpi and a 300dpi printer the scaling
  // factor is 1.0.
  wxSize screenPPI;
  screenPPI = m_oldconfig->GetDC()->GetPPI();
  wxSize printPPI;
  printPPI = (*m_configuration)->GetDC()->GetPPI();

  (*m_configuration)->SetZoomFactor_temporarily(
    DCSCALE * screenPPI.x / 75.0 * 1200.0 / printPPI.x
    );
  
  wxMessageDialog dialog(NULL,
                         wxString::Format(wxT("screenPPI.x=%i,\nprintPPI.x=%i"),
                                          screenPPI.x,printPPI.x),
                         wxString("Printer Parameters"));
  dialog.ShowModal();

  (*m_configuration)->SetClientWidth(pageWidth - 2 * marginX
                               - (*m_configuration)->Scale_Px((*m_configuration)->GetBaseIndent()));
  (*m_configuration)->SetClientHeight(pageHeight - 2 * marginY);

  (*m_configuration)->SetIndent(marginX);
  // Inform the output routines that we are printing
  (*m_configuration)->SetPrinter(true);
  // Make sure that during print nothing is outside the crop rectangle
  marginX += (*m_configuration)->Scale_Px((*m_configuration)->GetBaseIndent());
  MathCell::ClipToDrawRegion(false);

  Recalculate();
  BreakPages();
}

void MathPrintout::GetPageInfo(int *minPage, int *maxPage,
                               int *fromPage, int *toPage)
{
  *minPage = 1;
  *maxPage = m_numberOfPages;
  *fromPage = 1;
  *toPage = m_numberOfPages;
}

void MathPrintout::OnPreparePrinting()
{
  SetupData();
}

void MathPrintout::GetPageMargins(int *horizontal, int *vertical)
{
  *horizontal = (int) ((*m_configuration)->Scale_Px(PRINT_MARGIN_HORIZONTAL) * 10);
  *vertical = (int) ((*m_configuration)->Scale_Px(PRINT_MARGIN_VERTICAL) * 10);
}

int MathPrintout::GetHeaderHeight()
{
  wxDC *dc = GetDC();
  int width, height;

  dc->SetFont(wxFont((*m_configuration)->Scale_Px(10), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &width, &height);
  return height + (*m_configuration)->Scale_Px(12);
}

void MathPrintout::PrintHeader(int pageNum, wxDC *dc)
{
  int page_width, page_height;
  int title_width, title_height;
  int marginX, marginY;
  int pageWidth, pageHeight;

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  dc->SetTextForeground(wxColour(wxT("grey")));
  dc->SetPen(wxPen(wxT("light grey"), 1, wxPENSTYLE_SOLID));

  dc->SetFont(wxFont((*m_configuration)->Scale_Px(10), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &title_width, &title_height);
  wxString page = wxString::Format(wxT("%d / %d"), pageNum, m_numberOfPages);
  dc->GetTextExtent(page, &page_width, &page_height);

  dc->DrawText(GetTitle(), marginX, marginY);
  dc->DrawText(page, pageWidth - page_width - marginX, marginY);

  dc->DrawLine(marginX, marginY + title_height + (*m_configuration)->Scale_Px(3),
               pageWidth - marginX, marginY + title_height + (*m_configuration)->Scale_Px(3));

  dc->SetTextForeground(wxColour(wxT("black")));
  dc->SetPen(wxPen(wxT("black"), 1, wxPENSTYLE_SOLID));
}

void MathPrintout::Recalculate()
{
  GroupCell *tmp = m_tree;

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);

  marginX += (*m_configuration)->Scale_Px((*m_configuration)->GetBaseIndent());

  while (tmp != NULL)
  {
    tmp->ResetSize();
    tmp->Recalculate();
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
}

void MathPrintout::DestroyTree()
{
  wxDELETE(m_tree);
  m_tree = NULL;
}
