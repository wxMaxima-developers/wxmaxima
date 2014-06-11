///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "MathPrintout.h"
#include "GroupCell.h"

#if WXM_PRINT

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
  if (num > 0 && num <= m_numberOfPages)
    return true;
  return false;
}

bool MathPrintout::OnPrintPage(int num)
{
  double screenScaleX, screenScaleY;
  double ppiScale;
  GroupCell* tmp;
  wxDC* dc = GetDC();

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);


  ppiScale = GetPPIScale();
  GetScreenScale(&screenScaleX, &screenScaleY);

  marginX += SCALE_PX(MC_BASE_INDENT, ppiScale);

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
    CellParser parser(*dc, ppiScale);

    parser.SetIndent(marginX);

    while (tmp != NULL && tmp->GetGroupType() != GC_TYPE_PAGEBREAK)
    {
      tmp->Draw(parser, point, fontsize, false);
      if (tmp->m_next != NULL) {
        point.x = marginX;
        point.y += drop + tmp->m_next->GetMaxCenter();
        point.y += SCALE_PX(MC_GROUP_SKIP, ppiScale);
        drop = tmp->m_next->GetMaxDrop();
      }

      tmp = (GroupCell *)tmp->m_next;
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
  int skip = SCALE_PX(MC_GROUP_SKIP, scale);;

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
/*
void MathPrintout::RecalculateSize()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);
  GroupCell* tmp = m_tree;
  double scale = GetPPIScale();

  wxDC *dc = GetDC();
  CellParser parser(*dc, scale);
  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  marginX += SCALE_PX(MC_BASE_INDENT, scale);
  parser.SetIndent(marginX);

  while (tmp != NULL)
  {
    tmp->RecalculateSize(parser, fontsize, false);
    tmp = tmp->m_next;
  }
}
*/
void MathPrintout::Recalculate()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  config->Read(wxT("mathfontsize"), &mfontsize);
  GroupCell* tmp = (GroupCell *)m_tree;
  double scale = GetPPIScale();

  wxDC *dc = GetDC();
  CellParser parser(*dc, scale);

  int marginX, marginY;
  GetPageMargins(&marginX, &marginY);
  int pageWidth, pageHeight;
  GetPageSizePixels(&pageWidth, &pageHeight);

  parser.SetClientWidth(pageWidth - marginX - marginY
                        - SCALE_PX(MC_BASE_INDENT, scale));

  marginX += SCALE_PX(MC_BASE_INDENT, scale);
  parser.SetIndent(marginX);

  while (tmp != NULL)
  {
    tmp->Recalculate(parser, fontsize, mfontsize);
    tmp = (GroupCell *)tmp->m_next;
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

void MathPrintout::DestroyTree(MathCell* tmp)
{
  MathCell* tmp1;
  while (tmp != NULL)
  {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
}

#endif
