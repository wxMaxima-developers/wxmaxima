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
  Writes a worksheet to a tagged PDF, see PdfExport.
*/

#include "PdfExport.h"
#include "PageBreaks.h"
#include "BuildConfig.h"
#include "Version.h"

#include <wx/uilocale.h>

#ifdef WXM_USE_WXPDFDOC

#include "Configuration.h"
#include "cells/CellList.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/ImgCellBase.h"
#include "graphical_io/OutCommon.h"
#include "worksheet/WorksheetExport.h"

#include <wx/filename.h>
#include <wx/log.h>
#include <wx/pdfdc.h>
#include <wx/pdfdocument.h>

#include <algorithm>

namespace {

//! Fold every run of whitespace to one space: a spoken label has no layout
wxString FoldWhitespace(const wxString &text) {
  wxString folded;
  folded.reserve(text.length());
  bool pendingSpace = false;
  for (const wxUniChar c : text) {
    if (wxIsspace(c))
      pendingSpace = true;
    else {
      if (pendingSpace && !folded.IsEmpty())
        folded += wxS(' ');
      pendingSpace = false;
      folded += c;
    }
  }
  return folded;
}

bool IsLabel(const Cell &cell) {
  return (cell.GetTextStyle() == TS_LABEL) || (cell.GetTextStyle() == TS_USERLABEL);
}

bool IsImage(const Cell &cell) {
  return (cell.GetType() == MC_TYPE_IMAGE) || (cell.GetType() == MC_TYPE_SLIDE);
}

//! Is this a piece of plain (non-maths) output: a message, an error, a string?
bool IsProse(const Cell &cell) {
  switch (cell.GetTextStyle()) {
  case TS_TEXT:
  case TS_STRING:
  case TS_ERROR:
  case TS_WARNING:
    return true;
  default:
    return false;
  }
}

//! The PDF structure type for a heading of a given group type
wxString HeadingTag(GroupType type) {
  switch (type) {
  case GC_TYPE_TITLE:
    return wxS("H1");
  case GC_TYPE_SECTION:
    return wxS("H2");
  case GC_TYPE_SUBSECTION:
    return wxS("H3");
  case GC_TYPE_SUBSUBSECTION:
    return wxS("H4");
  case GC_TYPE_HEADING5:
    return wxS("H5");
  default:
    // PDF (and PDF/UA) knows six levels; the title takes the first one.
    return wxS("H6");
  }
}

//! How deep a heading of a given group type sits in the outline, 0 = top
int HeadingDepth(GroupType type) {
  switch (type) {
  case GC_TYPE_TITLE:
    return 0;
  case GC_TYPE_SECTION:
    return 1;
  case GC_TYPE_SUBSECTION:
    return 2;
  case GC_TYPE_SUBSUBSECTION:
    return 3;
  case GC_TYPE_HEADING5:
    return 4;
  default:
    return 5;
  }
}
} // namespace

/*! Everything that needs wxPdfDocument

  Lays the copied worksheet out for the page width, decides where pages
  start, then draws it in one pass in reading order, so that a structure
  element (a code cell, say) that crosses a page boundary stays one element.
*/
class PdfExport::Impl
{
public:
  Impl(GroupCell *tree, const Configuration *source, const Options &options,
       std::vector<int> &pageStarts)
    : m_options(options), m_source(source), m_pageStarts(pageStarts) {
    if (tree)
      m_sourceTree = tree;
  }

  bool Write(const wxString &file);

private:
  //! Lays the worksheet out at the width of the page
  void Layout();
  //! Decides at which worksheet y coordinates pages start
  void BreakPages();
  //! Starts the next page(s) if y lies below the current one
  void EnsurePageFor(wxCoord y);
  //! Draws one cell (not the list behind it)
  void DrawCell(Cell &cell);
  //! Draws the draw list from first up to (not including) stop
  void DrawRange(Cell *first, const Cell *stop);
  //! Draws one GroupCell with its structure elements
  void DrawGroup(GroupCell &group);
  //! Draws a group's output, one structure element per chunk
  void DrawOutput(GroupCell &group, Cell *output);
  /*! Adds a bookmark (an entry of the PDF's outline) for a heading

    PDF viewers show the outline as a table of contents, and it is how a
    screen reader user finds their way through a long document.
  */
  void AddBookmark(const GroupCell &group);

  //! Opens a structure element; no-op if the document isn't tagged
  void BeginTag(const wxPdfTag &tag) {
    if (m_doc && m_doc->IsTagged())
      m_doc->BeginTag(tag);
  }
  void EndTag() {
    if (m_doc && m_doc->IsTagged())
      m_doc->EndTag();
  }

  const Options m_options;
  const Configuration *const m_source;
  GroupCell *m_sourceTree = nullptr;
  std::vector<int> &m_pageStarts;

  std::unique_ptr<wxPdfDC> m_dc;
  //! Owned by m_dc
  wxPdfDocument *m_doc = nullptr;
  std::unique_ptr<Configuration> m_configuration;
  //! Our copy of the worksheet, laid out with m_configuration
  std::unique_ptr<GroupCell> m_tree;
  //! The page currently drawn on, counting from 0
  std::size_t m_page = 0;
  /*! The heading depths (HeadingDepth()) of the bookmarks the next one may nest in

    A worksheet can skip levels (a subsection without a section above it), but
    an outline level needs a parent one level up: wxPdfDocument would link the
    bookmark to a parent that doesn't exist. So a bookmark's level is the
    number of open headings above it, not its heading depth.
  */
  std::vector<int> m_openHeadings;
  //! The margins and the size of the printable area, in device units
  wxCoord m_marginLeft = 0;
  wxCoord m_marginTop = 0;
  wxSize m_canvas;
};

void PdfExport::Impl::Layout() {
  m_configuration->SetRecalcContext(*m_dc);
  m_configuration->SetWorkSheet(nullptr);
  m_configuration->SetCanvasSize(m_canvas);
  // No cell brackets on paper, and nothing indented to make room for them.
  m_configuration->ShowBrackets(false);
  m_configuration->SetIndent(-m_configuration->GetCellBracketWidth());
  m_configuration->LineWidth_em(10000);
  m_tree->ResetSize();
  // Twice, as Printout does: the first pass can change what the second one
  // sees (the width of labels, for example).
  for (int pass = 0; pass < 2; pass++) {
    m_configuration->RecalculateForce();
    for (GroupCell &group : OnList(m_tree.get()))
      group.Recalculate();
  }
  // Positions everything: groups, their input and their output.
  m_tree->UpdateYPositionList();
}

void PdfExport::Impl::BreakPages() {
  m_pageStarts = wxm::BreakIntoPages(m_tree.get(), m_canvas.y);
}

void PdfExport::Impl::EnsurePageFor(wxCoord y) {
  while ((m_page + 1 < m_pageStarts.size()) &&
         (y >= m_pageStarts.at(m_page + 1))) {
    m_dc->EndPage();
    m_dc->StartPage();
    m_page++;
    m_dc->SetDeviceOrigin(m_marginLeft, m_marginTop - m_pageStarts.at(m_page));
  }
}

void PdfExport::Impl::DrawCell(Cell &cell) {
  EnsurePageFor(cell.GetRect().GetTop());
  if (cell.PageBreakCandidates().empty()) {
    cell.Draw(m_dc.get(), m_dc.get());
    return;
  }
  // A cell that may be spread over several pages (an oversized matrix): draw
  // the part of it each page shows, on that page. It stays one structure
  // element, as a code cell crossing a page boundary does.
  const wxCoord bottom = cell.GetRect().GetBottom();
  while (true) {
    const wxCoord pageTop = m_pageStarts.at(m_page);
    const wxCoord pageBottom = (m_page + 1 < m_pageStarts.size()) ?
      m_pageStarts.at(m_page + 1) : pageTop + m_canvas.y;
    m_configuration->SetPrintedPage(pageTop, pageBottom);
    cell.Draw(m_dc.get(), m_dc.get());
    if ((m_page + 1 >= m_pageStarts.size()) || (m_pageStarts.at(m_page + 1) > bottom))
      break;
    EnsurePageFor(m_pageStarts.at(m_page + 1));
  }
  m_configuration->ClearPrintedPage();
}

void PdfExport::Impl::DrawRange(Cell *first, const Cell *stop) {
  for (Cell &cell : OnDrawList(first)) {
    if (&cell == stop)
      break;
    DrawCell(cell);
  }
}

void PdfExport::Impl::DrawOutput(GroupCell &group, Cell *output) {
  // The same chunks the HTML export makes (see ExportOutputList()): a label
  // or an image starts a new one, and each is one structure element.
  Cell *chunkStart = output;
  while (chunkStart) {
    Cell *chunkEnd = chunkStart;
    if (!IsImage(*chunkStart) && !IsLabel(*chunkStart))
      while (chunkEnd->GetNext()) {
        const Cell *next = chunkEnd->GetNext();
        if (IsImage(*next) || IsLabel(*next))
          break;
        chunkEnd = chunkEnd->GetNext();
      }
    Cell *const stop = chunkEnd->GetNext();

    if (IsLabel(*chunkStart)) {
      // "(%o1)": read as it is.
      BeginTag(wxPdfTag(wxS("Lbl")));
      DrawRange(chunkStart, stop);
      EndTag();
    } else if (IsImage(*chunkStart)) {
      // A plot or an image. What it shows can't be put into words here, but
      // the command that made it can be.
      wxString alt;
      if (group.GetEditable() && (group.GetGroupType() == GC_TYPE_CODE))
        alt = wxString::Format(_("Graphics created by: %s"),
                               FoldWhitespace(group.GetEditable()->ToString()));
      else
        alt = FoldWhitespace(chunkStart->ToString());
      BeginTag(wxPdfTag(wxS("Figure")).SetAltText(alt));
      DrawRange(chunkStart, stop);
      EndTag();
    } else {
      bool prose = true;
      bool ascii = true;
      for (const Cell *c = chunkStart; c != stop; c = c->GetNext()) {
        prose = prose && IsProse(*c);
        ascii = ascii && (c->GetTextStyle() == TS_ASCIIMATHS);
      }
      if (prose)
        BeginTag(wxPdfTag(wxS("P")));
      else {
        auto chunk = WorksheetExport::CopySelection(chunkStart, chunkEnd);
        wxPdfTag formula(wxS("Formula"));
        formula.SetAltText(OutCommon::AccessibleText(chunk.get()));
        // Maxima's own ASCII art has no structure to put into MathML.
        if (!ascii)
          formula.SetMathML(wxS("<math xmlns=\"http://www.w3.org/1998/Math/MathML\" "
                                "display=\"block\">") +
                            chunk->ListToMathML() + wxS("</math>"));
        BeginTag(formula);
      }
      DrawRange(chunkStart, stop);
      EndTag();
    }
    chunkStart = stop;
  }
}

void PdfExport::Impl::AddBookmark(const GroupCell &group) {
  wxString text;
  // A numbered heading's number is its prompt.
  if (const Cell *prompt = group.GetPrompt())
    text = prompt->ToString() + wxS(" ");
  if (const EditorCell *input = group.GetEditable())
    text += input->ToString();
  text = FoldWhitespace(text);
  if (text.IsEmpty())
    return;

  const int depth = HeadingDepth(group.GetGroupType());
  while (!m_openHeadings.empty() && (m_openHeadings.back() >= depth))
    m_openHeadings.pop_back();
  const int level = static_cast<int>(m_openHeadings.size());
  m_openHeadings.push_back(depth);

  // The bookmark's target: the top of the heading, in points from the top of
  // its page (the document's unit is the point).
  const wxCoord pageStart = (m_page < m_pageStarts.size()) ? m_pageStarts.at(m_page) : 0;
  const wxCoord top = group.GetRect().GetTop() - pageStart + m_marginTop;
  m_doc->Bookmark(text, level, std::max(0.0, top * 72.0 / LayoutPpi));
}

void PdfExport::Impl::DrawGroup(GroupCell &group) {
  if (group.GetGroupType() == GC_TYPE_PAGEBREAK)
    return;
  EnsurePageFor(group.GetRect().GetTop());

  EditorCell *const input = group.GetEditable();
  Cell *const prompt = group.GetPrompt();
  Cell *const output = group.IsHidden() ? nullptr : group.DisplayedOutput();
  const bool showInput =
    m_configuration->ShowCodeCells() || (group.GetGroupType() != GC_TYPE_CODE);

  if (group.IsHeading()) {
    AddBookmark(group);
    BeginTag(wxPdfTag(HeadingTag(group.GetGroupType())));
    // A heading's number ("1.2") is its prompt.
    if (prompt && showInput)
      DrawCell(*prompt);
    if (input && showInput)
      DrawCell(*input);
    EndTag();
    return;
  }

  switch (group.GetGroupType()) {
  case GC_TYPE_TEXT:
    BeginTag(wxPdfTag(wxS("P")));
    if (input)
      DrawCell(*input);
    EndTag();
    if (output)
      DrawOutput(group, output);
    break;
  case GC_TYPE_IMAGE:
    BeginTag(wxPdfTag(wxS("Figure")).SetAltText(
               input ? FoldWhitespace(input->ToString()) : wxString()));
    if (output)
      DrawRange(output, nullptr);
    EndTag();
    if (input && showInput) {
      BeginTag(wxPdfTag(wxS("Caption")));
      DrawCell(*input);
      EndTag();
    }
    break;
  default:
    // A code cell: its input, then its output.
    BeginTag(wxPdfTag(wxS("Div")));
    if (showInput) {
      if (prompt) {
        BeginTag(wxPdfTag(wxS("Lbl")));
        DrawCell(*prompt);
        EndTag();
      }
      if (input) {
        BeginTag(wxPdfTag(wxS("Code")));
        DrawCell(*input);
        EndTag();
      }
    }
    if (output)
      DrawOutput(group, output);
    EndTag();
  }
}

bool PdfExport::Impl::Write(const wxString &file) {
  wxPrintData printData;
  printData.SetPaperId(m_options.paper);
  printData.SetOrientation(m_options.orientation);
  printData.SetFilename(file);

  m_dc = std::make_unique<wxPdfDC>(printData);
  m_dc->SetResolution(PdfExport::LayoutPpi);
  // Font sizes are meant for a 96 dpi screen, whatever screen this
  // computer has: the PDF must not depend on it.
  m_dc->SetFontResolution(PdfExport::ReferencePpi);
  if (!m_dc->StartDoc(m_options.title))
    return false;
  m_doc = m_dc->GetPdfDocument();
  if (!m_doc)
    return false;

  m_doc->SetTitle(m_options.title);
  m_doc->SetAuthor(m_options.author);
  m_doc->SetCreator(wxString(wxS("wxMaxima ")) + wxS(WXMAXIMA_VERSION));
  m_doc->SetLanguage(m_options.language.IsEmpty() ? PdfExport::UiLanguageTag()
                                                  : m_options.language);
  // A PDF viewer shows the title instead of the file name.
  m_doc->SetViewerPreferences(wxPDF_VIEWER_DISPLAYDOCTITLE);
  // Self-contained: the PDF looks the same on a computer that lacks the
  // worksheet's fonts. Required for accessible PDF (PDF/UA), too.
  m_doc->SetEmbedAllFonts(true);
  // Greek letters and maths symbols the worksheet's text font lacks come
  // from another installed font, as on screen.
  m_doc->SetSystemFontFallback(true);
  if (m_options.tagged)
    m_doc->SetTagged(true);

  // The layout is made with a configuration of our own, which reads the
  // user's settings, as Printout's does.
  m_configuration = std::make_unique<Configuration>(m_dc.get(), Configuration::temporary);
  Printing printing(m_configuration.get());
  m_configuration->ClipToDrawRegion(false);
  if (m_source)
    m_configuration->ShowCodeCells(m_source->ShowCodeCells());
  const double printScale = m_source ? m_source->PrintScale() : 1.0;
  m_configuration->SetZoomFactor_temporarily(
    static_cast<double>(PdfExport::LayoutPpi) / PdfExport::ReferencePpi * printScale);

  // The print margins are set in the configuration dialog as plain numbers;
  // here they are millimetres.
  const auto mm = [](double millimetres) {
    return static_cast<wxCoord>(millimetres / 25.4 * PdfExport::LayoutPpi);
  };
  const double marginLeft = m_source ? m_source->PrintMargin_Left() : 10;
  const double marginRight = m_source ? m_source->PrintMargin_Right() : 10;
  const double marginTop = m_source ? m_source->PrintMargin_Top() : 10;
  const double marginBot = m_source ? m_source->PrintMargin_Bot() : 10;
  m_marginLeft = mm(marginLeft);
  m_marginTop = mm(marginTop);
  wxCoord pageWidth = 0, pageHeight = 0;
  m_dc->GetSize(&pageWidth, &pageHeight);
  m_canvas = wxSize(pageWidth - mm(marginLeft) - mm(marginRight),
                    pageHeight - mm(marginTop) - mm(marginBot));

  if (m_sourceTree) {
    m_tree = m_sourceTree->CopyList();
    m_tree->SetConfigurationList(m_configuration.get());
    Layout();
    BreakPages();
  }

  m_page = 0;
  m_openHeadings.clear();
  m_dc->StartPage();
  m_dc->SetDeviceOrigin(m_marginLeft,
                        m_marginTop - (m_pageStarts.empty() ? 0 : m_pageStarts.front()));
  // wxPdfDocument puts everything into a Document element itself.
  if (m_tree)
    for (GroupCell &group : OnList(m_tree.get()))
      DrawGroup(group);
  if (m_pageStarts.empty())
    m_pageStarts.push_back(0);
  m_dc->EndPage();
  // Writes the file.
  m_dc->EndDoc();
  m_doc = nullptr;
  m_tree.reset();
  return wxFileName::FileExists(file);
}

PdfExport::PdfExport(GroupCell *tree, const Configuration *source, const Options &options)
  : m_impl(std::make_unique<Impl>(tree, source, options, m_pageStarts)) {}

PdfExport::~PdfExport() = default;

bool PdfExport::Write(const wxString &file) {
  if (wxFileName::FileExists(file) && !wxRemoveFile(file))
    return false;
  return m_impl->Write(file);
}

bool PdfExport::Available() { return true; }

#else // WXM_USE_WXPDFDOC

class PdfExport::Impl {};

PdfExport::PdfExport(GroupCell *, const Configuration *, const Options &)
{}
PdfExport::~PdfExport() = default;
bool PdfExport::Write(const wxString &) { return false; }
bool PdfExport::Available() { return false; }

#endif // WXM_USE_WXPDFDOC

wxString PdfExport::DocumentTitle(GroupCell *tree, const wxString &fallback) {
#ifdef WXM_USE_WXPDFDOC
  for (const GroupCell &group : OnList(tree))
    if ((group.GetGroupType() == GC_TYPE_TITLE) && group.GetEditable()) {
      wxString title = FoldWhitespace(group.GetEditable()->ToString());
      if (!title.IsEmpty())
        return title;
    }
#else
  wxUnusedVar(tree);
#endif
  return fallback;
}

wxPaperSize PdfExport::PaperFromName(const wxString &name) {
  const wxString paper = name.Lower();
  if (paper == wxS("a3"))
    return wxPAPER_A3;
  if (paper == wxS("a4"))
    return wxPAPER_A4;
  if (paper == wxS("a5"))
    return wxPAPER_A5;
  if (paper == wxS("letter"))
    return wxPAPER_LETTER;
  if (paper == wxS("legal"))
    return wxPAPER_LEGAL;
  return wxPAPER_NONE;
}

wxString PdfExport::UiLanguageTag() {
  // "de_DE.UTF-8" -> "de-DE"
  wxString name = wxUILocale::GetCurrent().GetName();
  name = name.BeforeFirst(wxS('.')).BeforeFirst(wxS('@'));
  name.Replace(wxS("_"), wxS("-"));
  if (name.IsEmpty() || (name == wxS("C")) || (name == wxS("POSIX")))
    return wxS("en");
  return name;
}
