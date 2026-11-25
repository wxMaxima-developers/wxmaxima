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
  Declares Pdfout, which renders cells as a one-page PDF for the clipboard.
*/

#ifndef PDFOUT_H
#define PDFOUT_H

#include "OutCommon.h"
#include "precomp.h"
#include <wx/dataobj.h>
#include <memory>
#include <string>

/*! Renders a part of the worksheet as a PDF whose page is exactly as big as it

  This is a clipboard format: macOS programs (Keynote, Pages, Preview, ...)
  take vector graphics from the clipboard as PDF rather than as SVG, and a
  PDF keeps the text as text in the worksheet's own fonts, which are embedded.
  Characters these fonts lack come from another installed font, as on screen.

  Unlike PdfExport there are no pages to break the cells into, no margins and
  no paper size: the page is cut to the cells. And unlike PdfExport it takes
  any list of cells, not only whole GroupCells.

  It needs wxPdfDocument (see cmake/WxmPdfDoc.cmake); without it IsOk() is
  always false.
*/
class Pdfout final
{
public:
  /*! Renders tree

    \param configuration The worksheet's configuration. The rendering uses a
                         copy of its settings, as the other clipboard
                         renderers (Svgout, BitmapOut) do.
    \param tree The cells to render. Consumed.
  */
  Pdfout(const Configuration *const *configuration, std::unique_ptr<Cell> &&tree);
  ~Pdfout();
  Pdfout(const Pdfout &) = delete;
  Pdfout &operator=(const Pdfout &) = delete;

  //! Did rendering work?
  bool IsOk() const { return !m_pdf.empty(); }
  //! The PDF file's bytes; empty if rendering failed
  const std::string &GetData() const { return m_pdf; }
  //! The page size, in points (1/72 inch)
  wxSize GetPageSize() const { return m_pageSize; }

  //! Was wxMaxima built with wxPdfDocument?
  static bool Available();
  /*! Is a PDF offered on the clipboard?

    Not on Windows: no Windows program takes a PDF from the clipboard, and
    unlike on macOS and X11 there is no name for the format all programs
    agree on.
  */
  static bool OfferedOnClipboard();
  /*! The clipboard format

    "com.adobe.pdf" (the PDF type identifier) on macOS, the MIME type
    "application/pdf" elsewhere.
  */
  static const wxDataFormat &GetDataFormat();

private:
  OutCommon m_cmn;
  std::unique_ptr<Cell> m_tree;
  std::string m_pdf;
  wxSize m_pageSize;
};

#endif // PDFOUT_H
