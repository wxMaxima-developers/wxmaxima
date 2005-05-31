/*
 *  Copyright (C) 2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "MathPrintout.h"

#include <wx/config.h>

#define PRINT_MARGIN_HORIZONTAL 5
#define PRINT_MARGIN_VERTICAL 5

MathPrintout::MathPrintout(wxString title) : wxPrintout(title)
{
  m_numberOfPages = 0;
  m_tree = NULL;
}

MathPrintout::~MathPrintout()
{
  DestroyTree();
}

void MathPrintout::SetData(MathCell* tree)
{
  m_tree = tree;
  if (m_tree != NULL)
    m_tree->BreakPage(true);
}

bool MathPrintout::HasPage(int num)
{
  if (num>0 && num<=m_numberOfPages)
    return true;
  return false;
}

bool MathPrintout::OnPrintPage(int num)
{
  double screenScaleX, screenScaleY;
  double ppiScale;
  MathCell* tmp = m_tree;
  wxDC* dc = GetDC();
  
  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  
  ppiScale = GetPPIScale();
  GetScreenScale(&screenScaleX, &screenScaleY);
  
  dc->SetUserScale(screenScaleX, screenScaleY);
  
  // Go to current page
  tmp = m_pages[num-1];
  
  // Print page
  if (tmp != NULL) {
    wxPoint point;
    point.x = marginX;
    point.y = marginY + tmp->GetMaxCenter() + GetHeaderHeight();
    wxConfigBase* config = wxConfig::Get();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();
    
    config->Read(wxT("fontsize"), &fontsize);
    
    PrintHeader(num, dc, ppiScale);
    CellParser parser(*dc, ppiScale);
    
    while(tmp != NULL) {
      if (!tmp->m_isBroken) {
        tmp->Draw(parser, point, fontsize, false);
        if (tmp->m_nextToDraw != NULL && !tmp->m_nextToDraw->m_isBroken &&
            tmp->m_nextToDraw->BreakLineHere()) {
          point.x = marginX;
          point.y += drop + tmp->m_nextToDraw->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += SCALE_PX(5, ppiScale);
          drop = tmp->m_nextToDraw->GetMaxDrop();
        }
        else
          point.x += (tmp->GetWidth() + SCALE_PX(2, ppiScale));
      }
      tmp = tmp->m_nextToDraw;
      if (tmp == NULL || tmp->BreakPageHere())
        break;
    }
    return true;
  }
  return false;
}

bool MathPrintout::OnBeginDocument(int startPage, int endPage)
{
  if (!wxPrintout::OnBeginDocument(startPage, endPage))
    return false;
  return true;
}

void MathPrintout::BreakLines()
{
  int pageWidth, pageHeight;
  int marginX, marginY;
  double scale = GetPPIScale();
  
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);
  
  int fullWidth = pageWidth - marginX;
  int currentWidth = marginX;
  
  MathCell* tmp = m_tree;
  
  while (tmp != NULL) {
    if (!tmp->m_isBroken) {
      tmp->BreakLine(false);
      tmp->ResetData();
      if (tmp->BreakLineHere() ||
         (currentWidth + tmp->GetWidth() >= fullWidth)) {
        currentWidth = marginX + tmp->GetWidth();
        tmp->BreakLine(true);
      }
      else
        currentWidth += (tmp->GetWidth() + SCALE_PX(2, scale));
    }
    tmp = tmp->m_nextToDraw;
  }
}

void MathPrintout::BreakPages()
{
  if (m_tree == NULL)
    return;
  
  int pageWidth, pageHeight;
  int marginX, marginY;
  int headerHeight = GetHeaderHeight();
  double scale = GetPPIScale();
  
  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);
  
  int currentHeight = marginY;
  int skip = SCALE_PX(5, scale);
  
  MathCell* tmp = m_tree;
  m_pages.push_back(tmp);

  m_numberOfPages = 1;
  while (tmp != NULL) {
    if (!tmp->m_isBroken) {
      tmp->BreakPage(false);
      if (tmp->BreakLineHere()) {
        if (currentHeight + tmp->GetMaxHeight() + skip >= pageHeight - marginY) {
          currentHeight = marginY + tmp->GetMaxHeight() + headerHeight;
          tmp->BreakPage(true);
          m_pages.push_back(tmp);
          m_numberOfPages++;
        }
        else
          currentHeight += tmp->GetMaxHeight() + skip;
        if (tmp->m_bigSkip)
          skip = SCALE_PX(5, scale);
        else
          skip = 0;
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}

void MathPrintout::SetupData()
{
  RecalculateWidths();
  BreakUpCells();
  BreakLines();
  RecalculateSize();
  BreakPages();
}

void MathPrintout::GetPageInfo(int* minPage, int* maxPage,
                               int* fromPage, int* toPage)
{
  *minPage = 1;
  *maxPage = 9999;
  *fromPage = 1;
  *toPage = 9999;
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
  
  dc->SetFont(wxFont((int)((double)10*ppiScale + 0.5),
              wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("Courier")));
  dc->GetTextExtent(wxT("wxMaxima session"), &width, &height);
  return height + SCALE_PX(12, ppiScale);
}

void MathPrintout::PrintHeader(int pageNum, wxDC* dc, double scale)
{
  int marginX, marginY;
  int pageWidth, pageHeight;
  int width, height;
  int pages_width, pages_height;
  
  GetPageMargins(&marginX, &marginY);
  GetPageSizePixels(&pageWidth, &pageHeight);
  
  double ppiScale = GetPPIScale();
  double screenScaleX, screenScaleY;
  GetScreenScale(&screenScaleX, &screenScaleY);
  scale = ppiScale*screenScaleX;
  
  dc->SetTextForeground(wxColour(wxT("grey")));
  dc->SetPen(wxPen(wxT("light grey"), 1, wxSOLID));
  
  dc->SetFont(wxFont(MAX((int)(10.0*ppiScale+0.5),1),
                     wxMODERN, wxNORMAL, wxNORMAL));
  dc->GetTextExtent(wxT("Maxima session"), &width, &height);
  wxString page = wxString::Format(wxT("%d / %d"), pageNum, m_numberOfPages);
  dc->GetTextExtent(page, &pages_width, &pages_height);
  
  dc->SetFont(wxFont(MAX((int)(10.0*scale+0.5),1),
                     wxMODERN, wxNORMAL, wxNORMAL, 0, wxT("Courier")));
  dc->DrawText(wxT("Maxima session"), marginX, marginY);
  dc->DrawText(page, pageWidth - pages_width - marginX, marginY);
  
  dc->DrawLine(marginX, marginY + height + SCALE_PX(3, scale),
               pageWidth - marginX, marginY + height + SCALE_PX(3, scale));
  
  dc->SetTextForeground(wxColour(wxT("black")));
  dc->SetPen(wxPen(wxT("black"), 1, wxSOLID));
}

void MathPrintout::RecalculateSize()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);
  MathCell* tmp = m_tree;
  double scale = GetPPIScale();
  
  wxDC *dc = GetDC();
  CellParser parser(*dc, scale);
  while (tmp != NULL) {
    tmp->RecalculateSize(parser, fontsize, false);
    tmp = tmp->m_nextToDraw;
  }
}

void MathPrintout::RecalculateWidths()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);
  MathCell* tmp = m_tree;
  double scale = GetPPIScale();
  
  wxDC *dc = GetDC();
  CellParser parser(*dc, scale);
  while (tmp != NULL) {
    tmp->RecalculateWidths(parser, fontsize, false);
    tmp = tmp->m_next;
  }
}

double MathPrintout::GetPPIScale()
{
  int ppiScreenX, ppiScreenY;
  int ppiPrinterX, ppiPrinterY;
  
  GetPPIScreen(&ppiScreenX, &ppiScreenY);
  GetPPIPrinter(&ppiPrinterX, &ppiPrinterY);
  
  return ((double)ppiPrinterX) / ((double)ppiScreenX);
}

void MathPrintout::GetScreenScale(double *scaleX, double *scaleY)
{
  int pageSizeX, pageSizeY;
  int previewSizeX, previewSizeY;
  wxDC *dc = GetDC();
  
  GetPageSizePixels(&pageSizeX, &pageSizeY);
  dc->GetSize(&previewSizeX, &previewSizeY);
  
  *scaleX = ((double)previewSizeX) / ((double)pageSizeX);
  *scaleY = ((double)previewSizeY) / ((double)pageSizeY);
}

void MathPrintout::DestroyTree()
{
  if (m_tree!=NULL) {
    DestroyTree(m_tree);
    m_tree = NULL;
  }
}

void MathPrintout::DestroyTree(MathCell* tmp)
{
  MathCell* tmp1;
  while (tmp!=NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
}

void MathPrintout::BreakUpCells()
{
  MathCell *tmp = m_tree;
  int pageWidth, pageHeight, marginX, marginY;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);
  double scale = GetPPIScale();
  
  wxDC *dc = GetDC();
  CellParser parser(*dc, scale);
  
  GetPageSizePixels(&pageWidth, &pageHeight);
  GetPageMargins(&marginX, &marginY);
  
  int fullWidth = pageWidth - marginX;
  
  while (tmp != NULL) {
    if (tmp->GetWidth() > fullWidth) {
      if (tmp->BreakUp(true))
        tmp->RecalculateWidths(parser, fontsize, false);
    }
    tmp = tmp->m_nextToDraw;
  }
}
