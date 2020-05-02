// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class Svgout that renders math as scalable vector graphics.
 */

#include "SVGout.h"
#include "ErrorRedirector.h"
#include <wx/txtstrm.h> 
#include <wx/filename.h> 
#include <wx/wfstream.h>
#include <wx/stdpaths.h>
#include "Configuration.h"
#include "GroupCell.h"
#include <wx/config.h>
#include <wx/clipbrd.h>

Svgout::Svgout(Configuration **configuration, wxString filename, double scale) :
  m_CWD(wxGetCwd())
{
  m_width = m_height = -1;
  m_configuration = configuration;
  m_oldconfig = *m_configuration;
  m_tree = NULL;
  m_scale = scale;
  m_svgFormat = wxDataFormat(wxT("image/svg+xml"));

  m_filename = filename;
  if (m_filename == wxEmptyString)
    m_filename = wxFileName::CreateTempFileName(wxStandardPaths::Get().GetTempDir ()+wxT("/wxmaxima_"));
  {
    wxFileName name(m_filename);
    name.MakeAbsolute();
    m_filename = name.GetFullPath();
  }
  {
    wxString path = wxFileName(m_filename).GetPath();
    if(path.Length() > 1)
      wxSetWorkingDirectory(path);
  }
  
  m_dc = NULL;
  
  wxString m_tempFileName = wxFileName::CreateTempFileName(wxT("wxmaxima_size_"));
  m_recalculationDc = new wxSVGFileDC(m_tempFileName,700*m_scale,50000*m_scale,20*m_scale);
#if wxCHECK_VERSION(3, 1, 0)
  m_recalculationDc->SetBitmapHandler(new wxSVGBitmapEmbedHandler());
#endif
  *m_configuration = new Configuration(m_recalculationDc);
  (*m_configuration)->ShowCodeCells(m_oldconfig->ShowCodeCells());
  (*m_configuration)->SetClientWidth(700*m_scale);
  (*m_configuration)->SetZoomFactor_temporarily(1);
  // The last time I tried it the vertical positioning of the elements
  // of a big unicode parenthesis wasn't accurate enough in svg to be
  // usable. Also the probability was high that the right font wasn't
  // available in inkscape.
  (*m_configuration)->SetGrouphesisDrawMode(Configuration::handdrawn);
  (*m_configuration)->ClipToDrawRegion(false);
}

Svgout::~Svgout()
{
  wxDELETE(m_tree);
  wxDELETE(*m_configuration);
  wxDELETE(m_dc);
  wxDELETE(m_recalculationDc);
  if(wxFileExists(m_tempFileName))
  {
    // We don't want a braindead virus scanner that disallows us to delete our temp
    // files to trigger asserts.
    SuppressErrorDialogs messageBlocker;
    wxRemoveFile(m_tempFileName);
  }
  *m_configuration = m_oldconfig;
  (*m_configuration)->FontChanged(true);
  (*m_configuration)->RecalculationForce(true);
  wxSetWorkingDirectory(m_CWD);
}

wxSize Svgout::SetData(Cell *tree)
{
  wxDELETE(m_tree);
  m_tree = tree;
  if(m_tree != NULL)
  {
    m_tree = tree;
    m_tree->ResetSize();
    if(Layout())
      return wxSize(m_width / m_scale, m_height / m_scale);  
    else
      return wxSize(-1,-1);
  }
  else
    return wxSize(-1,-1);
}

bool Svgout::Layout()
{
  (*m_configuration)->SetContext(*m_recalculationDc);
  
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
      tmp = tmp->GetNext();
    }
  }

  if(!m_recalculationDc->IsOk())
  {
    return false;
  }

  GetMaxPoint(&m_width, &m_height);

  wxDELETE(m_dc);
  // Let's switch to a DC of the right size for our object.
  m_dc = new wxSVGFileDC(m_filename, m_width, m_height, 20*m_scale);
#if wxCHECK_VERSION(3, 1, 0)
  m_dc->SetBitmapHandler(new wxSVGBitmapEmbedHandler());
#endif
  (*m_configuration)->SetContext(*m_dc);
  
  Draw();
  wxDELETE(m_dc);
  m_dc = NULL;
  return true;
}

double Svgout::GetRealWidth() const
{
  return m_width / m_scale;
}

double Svgout::GetRealHeight() const
{
  return m_height / m_scale;
}

void Svgout::RecalculateHeight()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
  Cell *tmp = m_tree;

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

  Cell *tmp = m_tree;

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

  Cell *tmp = m_tree;

  while (tmp != NULL)
  {
    if (!tmp->m_isBrokenIntoLines)
    {
      tmp->SoftLineBreak(false);
      tmp->ResetData();
      if (tmp->BreakLineHere() ||
          (currentWidth + tmp->GetWidth() >= fullWidth))
      {
        currentWidth = tmp->GetWidth();
        tmp->SoftLineBreak(true);
      }
      else
        currentWidth += (tmp->GetWidth());
    }
    tmp = tmp->GetNextToDraw();
  }
}

void Svgout::GetMaxPoint(int *width, int *height)
{
  Cell *tmp = m_tree;
  int currentHeight = 0;
  int currentWidth = 0;
  *width = 0;
  *height = 0;
  bool bigSkip = false;
  bool firstCell = true;
  while (tmp != NULL)
  {
    if (!tmp->m_isBrokenIntoLines)
    {
      if (tmp->BreakLineHere() || firstCell)
      {
        firstCell = false;
        currentHeight += tmp->GetHeightList();
        if (bigSkip)
          currentHeight += MC_LINE_SKIP;
        *height = currentHeight;
        currentWidth = tmp->GetWidth();
        *width = wxMax(currentWidth, *width);
      }
      else
      {
        currentWidth += (tmp->GetWidth());
        *width = wxMax(currentWidth, *width);
      }
      bigSkip = tmp->m_bigSkip;
    }
    tmp = tmp->GetNextToDraw();
  }
}

void Svgout::Draw()
{
  Cell *tmp = m_tree;

  if (tmp != NULL)
  {
    wxPoint point;
    point.x = 0;
    point.y = tmp->GetCenterList();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();

    wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
    int mfontsize = fontsize;
    wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

    while (tmp != NULL)
    {
      if (!tmp->m_isBrokenIntoLines)
      {
        tmp->Draw(point);
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetCenterList();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
        else
          point.x += (tmp->GetWidth());
      }
      else
      {
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetCenterList();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
      }
      tmp = tmp->GetNextToDraw();
    }
  }
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
  if((m_filename != wxEmptyString) && (wxFileExists(m_filename)))
  {
    // Don't output error messages if the worst thing that can happen is that we
    // cannot clean up a temp file
    SuppressErrorDialogs messageBlocker;

    wxRemoveFile(m_filename);
  }
  m_filename = wxEmptyString;
  
  return new SVGDataObject(svgContents);
}

bool Svgout::ToClipboard()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
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
  Cell *tmp = m_tree;
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
    tmp = tmp->GetNextToDraw();
  }
}
