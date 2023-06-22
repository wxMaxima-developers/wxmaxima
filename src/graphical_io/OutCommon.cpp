// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include "OutCommon.h"
#include "ErrorRedirector.h"
#include "GroupCell.h"
#include "Worksheet.h"
#include <cmath>
#include <wx/clipbrd.h>
#include <wx/filename.h>
#include <wx/wfstream.h>

static wxString MakeTempFilename() {
  return wxFileName::CreateTempFileName(wxS("wxmaxima_size_"));
}

OutCommon::OutCommon(Configuration **configuration, const wxString &filename,
                     int fullWidth, double scale)
  : m_tempFilename(MakeTempFilename()),
    m_filename(filename.empty()
	       ? wxFileName::CreateTempFileName(wxS("wxmaxima_"))
	       : filename),
    m_configuration(configuration), m_scale(scale), m_fullWidth(fullWidth) {
  m_thisconfig.ShowCodeCells(m_oldconfig->ShowCodeCells());
  m_thisconfig.SetWorkSheet((*configuration)->GetWorkSheet());

  *m_configuration = &m_thisconfig;
  m_thisconfig.SetZoomFactor_temporarily(1);
  m_thisconfig.FontChanged();

  // The last time I tried it the vertical positioning of the elements
  // of a big unicode parenthesis wasn't accurate enough in emf to be
  // usable. Also the probability was high that the right font wasn't
  // available in inkscape.
  m_thisconfig.SetParenthesisDrawMode(Configuration::handdrawn);
  m_thisconfig.ClipToDrawRegion(false);
}

OutCommon::OutCommon(Configuration **configuration, int fullWidth, double scale)
  : OutCommon(configuration, {}, fullWidth, scale) {}

OutCommon::~OutCommon() {
  if (wxFileExists(m_tempFilename)) {
    // We don't want a braindead virus scanner that disallows us to delete our
    // temp files to trigger asserts.
    SuppressErrorDialogs messageBlocker;

    if (!wxRemoveFile(m_tempFilename))
      wxLogMessage(_("Cannot remove the file %s"), m_tempFilename.mb_str());
  }
  *m_configuration = m_oldconfig;
  (*m_configuration)->FontChanged();
}

wxSize OutCommon::GetScaledSize() const {
  int w = std::lround(m_size.x * m_scale);
  int h = std::lround(m_size.y * m_scale);
  return {w, h};
}

wxSize OutCommon::GetInvScaledSize() const {
  int w = std::lround(m_size.x / m_scale);
  int h = std::lround(m_size.y / m_scale);
  return {w, h};
}

bool OutCommon::PrepareLayout(Cell *tree) {
  wxASSERT(m_recalculationDc);
  if (!tree)
    return false;

  tree->FontsChangedList();
  tree->ResetSize();
  m_thisconfig.SetRecalcContext(*m_recalculationDc);

  if (tree->GetType() != MC_TYPE_GROUP) {
    Recalculate(tree);
    BreakUpCells(tree);
    BreakLines(tree);
    Recalculate(tree);
  } else {
    for (GroupCell &tmp : OnList(dynamic_cast<GroupCell *>(tree)))
      tmp.Recalculate();
  }

  if (!m_recalculationDc->IsOk())
    return false;

  GetMaxPoint(tree, &m_size.x, &m_size.y);
  return true;
}

void OutCommon::Recalculate(Cell *tree) const {
  auto fontsize = m_thisconfig.GetDefaultFontSize();
  auto mathFontsize = m_thisconfig.GetMathFontSize();

  for (Cell &tmp : OnList(tree))
    tmp.Recalculate(tmp.IsMath() ? mathFontsize : fontsize);
}

void OutCommon::BreakLines(Cell *tree) const {
  int currentWidth = 0;
  int fullWidth = m_fullWidth * m_scale;

  for (Cell &tmp : OnDrawList(tree)) {
    if (tmp.IsBrokenIntoLines())
      continue;

    if (tmp.SoftLineBreak(false))
      // Note: This ResetData() call was unconditional in EMFout and SVGout.
      // The condition check was only performed in BitmapOut. That was likely
      // an ommission. This note is here in case bugs were found in this area.
      tmp.ResetData();

    if (tmp.BreakLineHere() || (currentWidth + tmp.GetWidth() >= fullWidth)) {
      currentWidth = tmp.GetWidth();
      tmp.SoftLineBreak(true);
    } else
      currentWidth += (tmp.GetWidth());
  }
}

void OutCommon::GetMaxPoint(Cell *tree, int *width, int *height) const {
  int currentHeight = 0;
  int currentWidth = 0;
  *width = 0;
  *height = 0;
  bool bigSkip = false;
  bool firstCell = true;

  for (Cell &tmp : OnDrawList(tree)) {
    if (tmp.IsBrokenIntoLines())
      continue;

    if (tmp.BreakLineHere() || firstCell) {
      firstCell = false;
      currentHeight += tmp.GetHeightList();
      if (bigSkip)
        // Note: This skip was observerd in EMFout and SVGout, but not
        // BitmapOut.
        currentHeight += MC_LINE_SKIP;
      *height = currentHeight;
      currentWidth = tmp.GetWidth();
      *width = wxMax(currentWidth, *width);
    } else {
      currentWidth += (tmp.GetWidth());
      *width = wxMax(currentWidth, *width);
    }
    bigSkip = tmp.HasBigSkip();
  }
}

void OutCommon::Draw(Cell *tree) {
  wxPoint point;
  point.x = 0;
  point.y = tree->GetCenterList();
  int drop = tree->GetMaxDrop();

  for (Cell &tmp : OnDrawList(tree)) {
    Cell *const next = tmp.GetNext();
    if (!tmp.IsBrokenIntoLines()) {
      tmp.Draw(point, m_recalculationDc, m_recalculationDc);
      if (next && next->BreakLineHere()) {
        point.x = 0;
        point.y += drop + next->GetCenterList();
        if (tmp.HasBigSkip())
          // Note: This skip was observerd in EMFout and SVGout, but not
          // BitmapOut.
          point.y += MC_LINE_SKIP;
        drop = next->GetMaxDrop();
      } else
        point.x += tmp.GetWidth();
    } else {
      if (next && next->BreakLineHere()) {
        point.x = 0;
        point.y += drop + next->GetCenterList();
        if (tmp.HasBigSkip())
          // Note: This skip was observerd in EMFout and SVGout, but not
          // BitmapOut.
          point.y += MC_LINE_SKIP;
        drop = next->GetMaxDrop();
      }
    }
  }

  // Update the bitmap's size information.
  m_ppi = m_thisconfig.GetRecalcDC()->GetPPI();
  m_ppi.x *= m_scale;
  m_ppi.y *= m_scale;
}

void OutCommon::BreakUpCells(Cell *tree) {
  int fullWidth = m_fullWidth * m_scale;
  auto fontsize = m_thisconfig.GetDefaultFontSize();
  auto mathFontsize = m_thisconfig.GetMathFontSize();

  for (Cell &tmp : OnDrawList(tree)) {
    if (tmp.GetWidth() > fullWidth && tmp.BreakUp())
      tmp.Recalculate(tmp.IsMath() ? mathFontsize : fontsize);
  }
}

bool OutCommon::ToClipboard(const wxDataFormat &format) {
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    bool res = wxTheClipboard->SetData(GetDataObject(format).release());
    wxTheClipboard->Close();
    m_filename.clear();
    return res;
  }
  return false;
}

std::unique_ptr<OutCommon::DataObject>
OutCommon::GetDataObject(const wxDataFormat &format) {
  constexpr auto chunkSize = 8192;
  wxMemoryBuffer contents;
  {
    wxFileInputStream str(m_filename);
    if (str.IsOk())
      while (!str.Eof()) {
        auto *buf = contents.GetAppendBuf(chunkSize);
        str.Read(buf, chunkSize);
        contents.UngetAppendBuf(str.LastRead());
      }
  }

  if ((!m_filename.empty()) && wxFileExists(m_filename)) {
    // Don't output error messages if the worst thing that can happen is that we
    // cannot clean up a temp file
    SuppressErrorDialogs messageBlocker;
    wxRemoveFile(m_filename);
  }
  m_filename.clear();

  return std::unique_ptr<DataObject>(new DataObject(format, contents));
}

OutCommon::DataObject::DataObject(const wxDataFormat &format,
                                  const wxMemoryBuffer &data)
  : wxCustomDataObject(format), m_databuf(0)
    // We can't point m_databuf to data here, since TakeData calls Free() and will
    // ruin it!
{
  // cppcheck-suppress useInitializationList
  m_databuf = data;
}

bool OutCommon::DataObject::GetDataHere(void *buf) const {
  memcpy(buf, m_databuf.GetData(), m_databuf.GetDataLen());
  return true;
}

size_t OutCommon::DataObject::GetDataSize() const {
  return m_databuf.GetDataLen();
}

void OutCommon::DataObject::Free() { m_databuf.Clear(); }
