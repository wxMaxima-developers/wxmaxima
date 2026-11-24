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
  Declares PdfExport, which writes a worksheet to a tagged (accessible) PDF.
*/

#ifndef PDFEXPORT_H
#define PDFEXPORT_H

#include "precomp.h"
#include <wx/string.h>
#include <wx/cmndata.h>
#include <memory>
#include <vector>

class Cell;
class Configuration;
class GroupCell;

/*! Writes a worksheet to a PDF file.

  The worksheet is drawn by the cells' own Draw() methods onto a wxPdfDC, so
  the PDF looks like the printout -- but unlike a printout through a "print
  to PDF" driver:

  - the text stays text, in the worksheet's own fonts, embedded in the file;
  - characters the worksheet's fonts lack (Greek letters or maths symbols in a
    Latin-only font) are taken from another installed font that has them, as
    wxWidgets does on screen (wxPdfDocument's font fallback);
  - the PDF is tagged: a structure tree tells screen readers and text
    extraction what is a heading, a paragraph, code, a formula or a figure,
    in reading order. A formula carries Maxima's linear form of it as its
    alternate text and its MathML as an associated file, and a figure (a
    plot) the command that drew it.

  The drawing needs wxPdfDocument (thirdparty/wxpdfdoc, see
  cmake/WxmPdfDoc.cmake): without it, Available() is false and Write() fails.

  The export uses a copy of the worksheet laid out with a configuration of
  its own, the way Printout does, so the worksheet itself is left alone.
*/
class PdfExport
{
public:
  //! What goes into the PDF besides the worksheet
  struct Options
  {
    //! The document title (shown by PDF viewers instead of the file name)
    wxString title;
    //! The author
    wxString author;
    /*! The natural language of the document as a BCP 47 tag ("en", "de-DE")

      Screen readers choose their voice by it. Empty means "the language of
      wxMaxima's user interface".
    */
    wxString language;
    //! The paper size
    wxPaperSize paper = wxPAPER_A4;
    //! Portrait or landscape
    wxPrintOrientation orientation = wxPORTRAIT;
    //! Write a tagged (accessible) PDF?
    bool tagged = true;
  };

  /*! Prepares the export of a worksheet

    \param tree The first GroupCell of the worksheet. Only read: the export
                works on a copy.
    \param source The worksheet's configuration. The settings that decide what
                  is shown (code cells, the print scale and margins, the
                  styles) are taken from the user's configuration, as
                  printing does.
  */
  PdfExport(GroupCell *tree, const Configuration *source, const Options &options);
  ~PdfExport();

  /*! The resolution of the coordinates the worksheet is laid out in

    wxMaxima's geometry is integer, so this is also the precision of every
    position in the PDF: 1/720 inch, 1/10 of a point. Pdfout uses it, too.
  */
  static constexpr int LayoutPpi = 720;
  //! The resolution wxMaxima's font sizes and lengths are meant for (see Printout)
  static constexpr int ReferencePpi = 96;

  //! Writes the PDF. Returns false if the file could not be written.
  bool Write(const wxString &file);

  //! Was wxMaxima built with the PDF export?
  static bool Available();

  //! The number of pages the last Write() produced
  std::size_t GetPageCount() const { return m_pageStarts.size(); }

  /*! The paper size a name like "a4" or "letter" stands for

    \return wxPAPER_NONE for an unknown name
  */
  static wxPaperSize PaperFromName(const wxString &name);

  /*! A title for the PDF: the worksheet's first title cell

    \param fallback What to use if the worksheet has no title cell, e.g. the
                    file name
  */
  static wxString DocumentTitle(GroupCell *tree, const wxString &fallback);

  /*! The BCP 47 language tag of wxMaxima's user interface, e.g. "de-DE"

    Falls back to "en" for the C/POSIX locale.
  */
  static wxString UiLanguageTag();

private:
  class Impl;
  //! The part that needs wxPdfDocument's headers
  std::unique_ptr<Impl> m_impl;
  //! The worksheet y coordinate each page starts at
  std::vector<int> m_pageStarts;
};

#endif // PDFEXPORT_H
