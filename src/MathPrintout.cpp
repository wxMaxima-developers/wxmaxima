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
}

MathPrintout::~MathPrintout()
{
  DestroyTree();
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
  double screenScaleX, screenScaleY;
  double ppiScale = GetPPIScale();
  GroupCell *tmp;
  wxDC *dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);


  Configuration configuration(*dc);
  *m_configuration = &configuration;
  configuration.ShowCodeCells(m_oldconfig->ShowCodeCells());
  configuration.ShowBrackets(configuration.PrintBrackets());
  configuration.SetScale(ppiScale);
  configuration.LineWidth_em(400);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);
  double scale = GetPPIScale();

  configuration.SetClientWidth(pageWidth - 2 * marginX
                               - MathCell::Scale_Px((*m_configuration)->GetBaseIndent(), scale));
  configuration.SetClientHeight(pageHeight - 2 * marginY);

  configuration.SetIndent(marginX);
  // Inform the output routines that we are printing
  configuration.SetPrinter(true);
  // Make sure that during print nothing is outside the crop rectangle

  marginX += MathCell::Scale_Px((*m_configuration)->GetBaseIndent(), ppiScale);

  GetScreenScale(&screenScaleX, &screenScaleY);
  dc->SetUserScale(screenScaleX, screenScaleY);

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

    PrintHeader(num, dc, ppiScale);
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
        point.y += MathCell::Scale_Px((*m_configuration)->GetGroupSkip(), ppiScale);
        drop = tmp->m_next->GetMaxDrop();
      }

      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
      if (tmp == NULL || tmp->BreakPageHere())
        break;
    }
    MathCell::ClipToDrawRegion(true);
    *m_configuration = m_oldconfig;
    return true;
  }
  MathCell::ClipToDrawRegion(true);
  *m_configuration = m_oldconfig;
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
  double scale = GetPPIScale();

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  int currentHeight = marginY;
  int skip = MathCell::Scale_Px((*m_configuration)->GetGroupSkip(), scale);;

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
  double scale = GetPPIScale();

  *horizontal = (int) (MathCell::Scale_Px(PRINT_MARGIN_HORIZONTAL, scale) * 10);
  *vertical = (int) (MathCell::Scale_Px(PRINT_MARGIN_VERTICAL, scale) * 10);
}

int MathPrintout::GetHeaderHeight()
{
  wxDC *dc = GetDC();
  double ppiScale = GetPPIScale();
  int width, height;

  dc->SetFont(wxFont(MathCell::Scale_Px(10, ppiScale), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &width, &height);
  return height + MathCell::Scale_Px(12, ppiScale);
}

void MathPrintout::PrintHeader(int pageNum, wxDC *dc, double scale)
{
  int page_width, page_height;
  int title_width, title_height;
  int marginX, marginY;
  int pageWidth, pageHeight;

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  dc->SetTextForeground(wxColour(wxT("grey")));
  dc->SetPen(wxPen(wxT("light grey"), 1, wxPENSTYLE_SOLID));

  dc->SetFont(wxFont(MathCell::Scale_Px(10, scale), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &title_width, &title_height);
  wxString page = wxString::Format(wxT("%d / %d"), pageNum, m_numberOfPages);
  dc->GetTextExtent(page, &page_width, &page_height);

  dc->DrawText(GetTitle(), marginX, marginY);
  dc->DrawText(page, pageWidth - page_width - marginX, marginY);

  dc->DrawLine(marginX, marginY + title_height + MathCell::Scale_Px(3, scale),
               pageWidth - marginX, marginY + title_height + MathCell::Scale_Px(3, scale));

  dc->SetTextForeground(wxColour(wxT("black")));
  dc->SetPen(wxPen(wxT("black"), 1, wxPENSTYLE_SOLID));
}

void MathPrintout::Recalculate()
{
  GroupCell *tmp = m_tree;

  wxDC *dc = GetDC();
  Configuration configuration(*dc);
  *m_configuration = &configuration;
  configuration.ShowCodeCells(m_oldconfig->ShowCodeCells());
  configuration.SetScale(GetPPIScale());
  configuration.LineWidth_em(400);
  configuration.ShowBrackets(configuration.PrintBrackets());
  configuration.SetScale(GetPPIScale());
  configuration.LineWidth_em(400);

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);
  int scale = configuration.GetScale();

  configuration.SetClientWidth(pageWidth - 2 * marginX
                               - MathCell::Scale_Px((*m_configuration)->GetBaseIndent(), scale));
  configuration.SetClientHeight(pageHeight - 2 * marginY);

  configuration.SetCanvasSize(wxSize(pageWidth - marginX, pageHeight - marginY));
  marginX += MathCell::Scale_Px(configuration.GetBaseIndent(), scale);
  configuration.SetIndent(marginX);
  configuration.SetPrinter(true);
  MathCell::ClipToDrawRegion(false);

  while (tmp != NULL)
  {
    tmp->ResetSize();
    tmp->Recalculate();
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  *m_configuration = m_oldconfig;
  MathCell::ClipToDrawRegion(true);
  *m_configuration = m_oldconfig;
}

double MathPrintout::GetPPIScale()
{
  int ppiScreenX, ppiScreenY;
  int ppiPrinterX, ppiPrinterY;

  GetPPIScreen(&ppiScreenX, &ppiScreenY);
  GetPPIPrinter(&ppiPrinterX, &ppiPrinterY);

#if defined __WXMAC__
  return 0.6 * ((double) ppiPrinterY) / ((double) ppiScreenY);
#else
  return 0.6*((double)ppiPrinterY) / ((double)ppiScreenY);
#endif
}

void MathPrintout::GetScreenScale(double *scaleX, double *scaleY)
{
  int pageSizeX, pageSizeY;
  int previewSizeX, previewSizeY;
  wxDC *dc = GetDC();

  GetPageSizePixels(&pageSizeX, &pageSizeY);
  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  (*m_configuration)->SetCanvasSize(wxSize(pageSizeX - marginX, pageSizeY - marginX));
  dc->GetSize(&previewSizeX, &previewSizeY);

  *scaleX = ((double) previewSizeX) / ((double) pageSizeX);
  *scaleY = ((double) previewSizeY) / ((double) pageSizeY);
}

void MathPrintout::DestroyTree()
{
  wxDELETE(m_tree);
  m_tree = NULL;
}
