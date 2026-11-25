// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Defines Pdfout, which renders cells as a one-page PDF for the clipboard.
*/

#include "PDFout.h"
#include "BuildConfig.h"
#include "Configuration.h"
#include "Version.h"
#include "graphical_io/PdfExport.h"
#include "cells/Cell.h"
#include <wx/filename.h>

#ifdef WXM_USE_WXPDFDOC
#include <wx/mstream.h>
#include <wx/pdfdc.h>
#include <wx/pdfdocument.h>
#include <cmath>
#endif

namespace {
//! The width lines are broken at, in screen pixels: the same as Svgout's
constexpr int PDFOUT_LINE_WIDTH = 700;
} // namespace

Pdfout::Pdfout(const Configuration *const *configuration, std::unique_ptr<Cell> &&tree)
  : m_cmn(configuration, PDFOUT_LINE_WIDTH, 1.0), m_tree(std::move(tree)) {
#ifdef WXM_USE_WXPDFDOC
  if (!m_tree)
    return;
  // The page size is only known once the cells are laid out, which needs a DC
  // that measures text as the PDF will. So the document starts out with an
  // arbitrary page size, and the DC is one that draws into whatever page the
  // document has (wxPdfDC's "template mode", which owns no document and adds
  // no pages of its own).
  wxPdfDocument doc(wxPORTRAIT, 595, 842, wxS("pt"));
  doc.SetCreator(wxString(wxS("wxMaxima ")) + wxS(WXMAXIMA_VERSION));
  doc.SetTitle(OutCommon::AccessibleText(m_tree.get()));
  // Self-contained: the receiving program may not have the worksheet's fonts.
  doc.SetEmbedAllFonts(true);
  // Greek letters and maths symbols the fonts lack, from another font.
  doc.SetSystemFontFallback(true);
  doc.Open();
  wxPdfDC dc(&doc, 595, 842);
  dc.SetResolution(PdfExport::LayoutPpi);
  // Font sizes are meant for a 96 dpi screen, whatever screen this computer
  // has: the PDF must not depend on it.
  dc.SetFontResolution(PdfExport::ReferencePpi);

  constexpr double zoom = static_cast<double>(PdfExport::LayoutPpi) / PdfExport::ReferencePpi;
  Configuration *config = m_cmn.GetConfiguration();
  config->SetZoomFactor_temporarily(zoom);
  config->FontChanged();
  config->SetCanvasSize(wxSize(std::lround(PDFOUT_LINE_WIDTH * zoom),
                               std::lround(100000 * zoom)));
  m_cmn.SetRecalculationContext(&dc);
  if (!m_cmn.PrepareLayout(m_tree.get())) {
    config->UnsetContext();
    return;
  }

  const wxSize size = m_cmn.GetSize();
  // In points, rounded up so nothing is cut off
  m_pageSize = wxSize(std::max(1, static_cast<int>(std::ceil(size.x * 72.0 / PdfExport::LayoutPpi))),
                      std::max(1, static_cast<int>(std::ceil(size.y * 72.0 / PdfExport::LayoutPpi))));
  doc.AddPage(wxPORTRAIT, m_pageSize.x, m_pageSize.y);
  m_cmn.Draw(m_tree.get());
  config->UnsetContext();

  const wxMemoryOutputStream &out = doc.CloseAndGetBuffer();
  const wxStreamBuffer *buffer = out.GetOutputStreamBuffer();
  m_pdf.assign(static_cast<const char *>(buffer->GetBufferStart()),
               out.GetSize());
#endif
}

Pdfout::~Pdfout() {
  // OutCommon names a file to write to, which a PDF in memory doesn't need.
  if (wxFileExists(m_cmn.GetFilename()))
    wxRemoveFile(m_cmn.GetFilename());
}

bool Pdfout::Available() {
#ifdef WXM_USE_WXPDFDOC
  return true;
#else
  return false;
#endif
}

bool Pdfout::OfferedOnClipboard() {
#ifdef __WXMSW__
  return false;
#else
  return Available();
#endif
}

const wxDataFormat &Pdfout::GetDataFormat() {
  // Made on first use: a wxDataFormat registers itself with the windowing
  // system, which must be running by then.
#ifdef __WXOSX__
  static const wxDataFormat format(wxS("com.adobe.pdf"));
#else
  static const wxDataFormat format(wxS("application/pdf"));
#endif
  return format;
}
