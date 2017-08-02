// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class Svgout that renders math as scalable vector graphics.
 */

#include "SVGout.h"
#include <wx/txtstrm.h> 
#include <wx/filename.h> 
#include <wx/wfstream.h>
#include "Configuration.h"
#include "GroupCell.h"
#include <wx/config.h>
#include <wx/clipbrd.h>

Svgout::Svgout(Configuration **configuration, wxString filename, int scale)
{
  m_configuration = configuration;
  m_oldconfig = *m_configuration;
  m_tree = NULL;
  m_scale = scale;
  m_svgFormat = wxDataFormat(wxT("image/svg+xml"));

  if (filename == wxEmptyString)
  {
    filename = wxFileName::CreateTempFileName(wxT("wxmaxima_"));
  }
  m_filename = filename;
}

Svgout::~Svgout()
{
  wxDELETE(m_tree);
  *m_configuration = m_oldconfig;
}

bool Svgout::SetData(MathCell *tree, long int maxSize)
{
  wxDELETE(m_tree);
  m_tree = tree;
  return Layout(maxSize);
}

bool Svgout::Layout(long int maxSize)
{
  m_dc = new wxSVGFileDC(m_filename,3200,2000,72*m_scale);

  *m_configuration = new Configuration(*m_dc);
  (*m_configuration)->ShowCodeCells(m_oldconfig->ShowCodeCells());
  (*m_configuration)->SetClientWidth(500*m_scale);
  (*m_configuration)->SetScale(m_scale);
  (*m_configuration)->SetZoomFactor(1);
  // The last time I tried it the vertical positioning of the elements
  // of a big unicode parenthesis wasn't accurate enough in svg to be
  // usable. Also the probability was high that the right font wasn't
  // available in inkscape.
//  (*m_configuration)->SetParenthesisDrawMode(Configuration::handdrawn);
  if (m_tree->GetType() != MC_TYPE_GROUP)
  {
    RecalculateWidths();
    BreakUpCells();
    BreakLines();
    RecalculateHeight();
  }
  else
  {
    GroupCell *tmp = dynamic_cast<GroupCell *>(m_tree);
    while (tmp != NULL)
    {
      tmp->Recalculate();
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
  }

  if(!m_dc->IsOk())
    return false;

  int width, height;
  GetMaxPoint(&width, &height);

  wxDELETE(m_dc);
  m_dc = new wxSVGFileDC(m_filename, width, height, 72*m_scale);
  (*m_configuration)->SetContext(*m_dc);
  
  Draw();
  wxDELETE(*m_configuration);
  wxDELETE(m_dc);
  return true;
}

double Svgout::GetRealWidth()
{
  return m_width / m_scale;
}

double Svgout::GetRealHeight()
{
  return m_height / m_scale;
}

void Svgout::RecalculateHeight()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
  MathCell *tmp = m_tree;

  while (tmp != NULL)
  {
    tmp->RecalculateHeight(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

void Svgout::RecalculateWidths()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  MathCell *tmp = m_tree;

  (*m_configuration)->SetClientWidth(500*m_scale);
  (*m_configuration)->SetClientHeight(500*m_scale);

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

void Svgout::BreakLines()
{
  int fullWidth = 500*m_scale;
  int currentWidth = 0;

  MathCell *tmp = m_tree;

  while (tmp != NULL)
  {
    if (!tmp->m_isBroken)
    {
      tmp->BreakLine(false);
      tmp->ResetData();
      if (tmp->BreakLineHere() ||
          (currentWidth + tmp->GetWidth() >= fullWidth))
      {
        currentWidth = tmp->GetWidth();
        tmp->BreakLine(true);
      }
      else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_nextToDraw;
  }
}

void Svgout::GetMaxPoint(int *width, int *height)
{
  MathCell *tmp = m_tree;
  int currentHeight = 0;
  int currentWidth = 0;
  *width = 0;
  *height = 0;
  bool bigSkip = false;
  bool firstCell = true;
  while (tmp != NULL)
  {
    if (!tmp->m_isBroken)
    {
      if (tmp->BreakLineHere() || firstCell)
      {
        firstCell = false;
        currentHeight += tmp->GetMaxHeight();
        if (bigSkip)
          currentHeight += MC_LINE_SKIP;
        *height = currentHeight;
        currentWidth = tmp->GetWidth();
        *width = MAX(currentWidth, *width);
      }
      else
      {
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
        *width = MAX(currentWidth - MC_CELL_SKIP, *width);
      }
      bigSkip = tmp->m_bigSkip;
    }
    tmp = tmp->m_nextToDraw;
  }
}

void Svgout::Draw()
{
  MathCell::ClipToDrawRegion(false);
  MathCell *tmp = m_tree;

  if (tmp != NULL)
  {
    wxPoint point;
    point.x = 0;
    point.y = tmp->GetMaxCenter();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();

    wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
    int mfontsize = fontsize;
    wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

    while (tmp != NULL)
    {
      if (!tmp->m_isBroken)
      {
        tmp->Draw(point, tmp->IsMath() ? mfontsize : fontsize);
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
        else
          point.x += (tmp->GetWidth() + MC_CELL_SKIP);
      }
      else
      {
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
      }
      tmp = tmp->m_nextToDraw;
    }
  }
  MathCell::ClipToDrawRegion(true);
}

Svgout::SVGDataObject::SVGDataObject() : wxCustomDataObject(m_svgFormat)
{
}

Svgout::SVGDataObject::SVGDataObject(wxMemoryBuffer data) : wxCustomDataObject(m_svgFormat)
{
  SetData(data.GetBufSize(), data.GetData());
}


wxDataFormat Svgout::m_svgFormat;

Svgout::SVGDataObject *Svgout::GetDataObject()
{
    wxMemoryBuffer svgContents;
    {
      char *data =(char *) malloc(8192);
      wxFileInputStream str(m_filename);
      if(str.IsOk())
        while (!str.Eof())
        {
          str.Read(data,8192);
          svgContents.AppendData(data,str.LastRead());
        }
      free(data);
    }
    wxRemoveFile(m_filename);
    m_filename = wxEmptyString;

    return new SVGDataObject(svgContents);
}

bool Svgout::ToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(GetDataObject());
    wxTheClipboard->Close();
    m_filename = wxEmptyString;
    return res;
  }
  return false;
}

void Svgout::BreakUpCells()
{
  MathCell *tmp = m_tree;
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  while (tmp != NULL)
  {
    if (tmp->GetWidth() > 500*m_scale)
    {
      if (tmp->BreakUp())
      {
        tmp->RecalculateWidths(tmp->IsMath() ? mfontsize : fontsize);
        tmp->RecalculateHeight(tmp->IsMath() ? mfontsize : fontsize);
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}
