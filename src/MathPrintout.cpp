// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

MathPrintout::MathPrintout(wxString title) : wxPrintout(title)
{
  m_numberOfPages = 0;
  m_oldViewportSize = MathCell::GetCanvasSize();
  m_tree = NULL;
}

MathPrintout::~MathPrintout()
{
  DestroyTree();
  MathCell::SetCanvasSize(m_oldViewportSize);
}

void MathPrintout::SetData(GroupCell* tree)
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
  GroupCell* tmp;
  wxDC* dc = GetDC();
  dc->SetBackground(*wxWHITE_BRUSH);
  dc->Clear();
  
  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);


  Configuration configuration(*dc);
  configuration.SetScale(ppiScale);
  
  configuration.SetIndent(marginX);
  // Inform the output routines that we are printing
  configuration.SetPrinter(true);
  // Make sure that during print nothing is outside the crop rectangle

  marginX += SCALE_PX(Configuration::Get()->GetBaseIndent(), ppiScale);

  GetScreenScale(&screenScaleX, &screenScaleY);
  dc->SetUserScale(screenScaleX, screenScaleY);

  // Go to current page
  tmp = (GroupCell *)m_pages[num - 1];

  // Print page
  if (tmp != NULL)
  {
    if (tmp->GetGroupType() == GC_TYPE_PAGEBREAK)
      tmp = (GroupCell *)tmp->m_next;
    if (tmp == NULL)
      return true;

    wxPoint point;
    point.x = marginX;
    point.y = marginY + tmp->GetMaxCenter() + GetHeaderHeight();
    wxConfigBase* config = wxConfig::Get();
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
      if (tmp->m_next != NULL) {
        point.x = marginX;
        point.y += drop + tmp->m_next->GetMaxCenter();
        point.y += SCALE_PX(Configuration::Get()->GetGroupSkip(), ppiScale);
        drop = tmp->m_next->GetMaxDrop();
      }

      tmp = (GroupCell *)tmp->m_next;
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
    return ;

  int pageWidth, pageHeight;
  int marginX, marginY;
  int headerHeight = GetHeaderHeight();
  double scale = GetPPIScale();

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  int currentHeight = marginY;
  int skip = SCALE_PX(Configuration::Get()->GetGroupSkip(), scale);;

  GroupCell* tmp = (GroupCell *)m_tree;
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
      tmp->BreakPage(true);
      m_pages.push_back(tmp);
      m_numberOfPages++;
    }
    else
      currentHeight += tmp->GetMaxHeight() + skip;

    tmp = (GroupCell *)tmp->m_next;
  }
}

void MathPrintout::SetupData()
{
  wxDC *dc = GetDC();
  Configuration configuration(*dc);
  configuration.SetScale(GetPPIScale());
  Recalculate();
  BreakPages();
}

void MathPrintout::GetPageInfo(int* minPage, int* maxPage,
                               int* fromPage, int* toPage)
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

void MathPrintout::GetPageMargins(int* horizontal, int* vertical)
{
  double scale = GetPPIScale();

  *horizontal = (int)(SCALE_PX(PRINT_MARGIN_HORIZONTAL, scale) * 10);
  *vertical = (int)(SCALE_PX(PRINT_MARGIN_VERTICAL, scale) * 10);
}

int MathPrintout::GetHeaderHeight()
{
  wxDC *dc = GetDC();
  double ppiScale = GetPPIScale();
  int width, height;

  dc->SetFont(wxFont(SCALE_PX(10, ppiScale), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &width, &height);
  return height + SCALE_PX(12, ppiScale);
}

void MathPrintout::PrintHeader(int pageNum, wxDC* dc, double scale)
{
  int page_width, page_height;
  int title_width, title_height;
  int marginX, marginY;
  int pageWidth, pageHeight;

  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);

  dc->SetTextForeground(wxColour(wxT("grey")));
  dc->SetPen(wxPen(wxT("light grey"), 1, wxPENSTYLE_SOLID));

  dc->SetFont(wxFont(SCALE_PX(10, scale), wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL));
  dc->GetTextExtent(GetTitle(), &title_width, &title_height);
  wxString page = wxString::Format(wxT("%d / %d"), pageNum, m_numberOfPages);
  dc->GetTextExtent(page, &page_width, &page_height);

  dc->DrawText(GetTitle(), marginX, marginY);
  dc->DrawText(page, pageWidth - page_width - marginX, marginY);

  dc->DrawLine(marginX, marginY + title_height + SCALE_PX(3, scale),
               pageWidth - marginX, marginY + title_height + SCALE_PX(3, scale));

  dc->SetTextForeground(wxColour(wxT("black")));
  dc->SetPen(wxPen(wxT("black"), 1, wxPENSTYLE_SOLID));
}

void MathPrintout::Recalculate()
{
  GroupCell* tmp = m_tree;

  wxDC *dc = GetDC();
  Configuration configuration(*dc);
  configuration.SetScale(GetPPIScale());

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);
  int scale = configuration.GetScale();
  
  configuration.SetClientWidth(pageWidth - marginX - marginY
                               - SCALE_PX(Configuration::Get()->GetBaseIndent(), scale));
  MathCell::SetCanvasSize(wxSize(pageWidth-marginX,pageHeight-marginY));
  marginX += SCALE_PX(Configuration::Get()->GetBaseIndent(), scale);
  configuration.SetIndent(marginX);
 
  while (tmp != NULL)
  {
    tmp->ResetSize();
    tmp->Recalculate();
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
}

double MathPrintout::GetPPIScale()
{
  int ppiScreenX, ppiScreenY;
  int ppiPrinterX, ppiPrinterY;

  GetPPIScreen(&ppiScreenX, &ppiScreenY);
  GetPPIPrinter(&ppiPrinterX, &ppiPrinterY);

#if defined __WXMAC__
  return 0.6*((double)ppiPrinterY) / ((double)ppiScreenY);
#else
  return ((double)ppiPrinterY) / ((double)ppiScreenY);
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
  MathCell::SetCanvasSize(wxSize(pageSizeX-marginX,pageSizeY-marginX));
  dc->GetSize(&previewSizeX, &previewSizeY);
  
  *scaleX = ((double)previewSizeX) / ((double)pageSizeX);
  *scaleY = ((double)previewSizeY) / ((double)pageSizeY);
}

void MathPrintout::DestroyTree()
{
  if (m_tree != NULL)
  {
    DestroyTree(m_tree);
    m_tree = NULL;
  }
}

void MathPrintout::DestroyTree(GroupCell* tmp)
{
  GroupCell* tmp1;
  while (tmp != NULL)
  {
    tmp1 = tmp;
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    tmp1->Destroy();
    delete tmp1;
  }
}
