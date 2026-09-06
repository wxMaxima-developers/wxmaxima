// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include <utility>
#include <algorithm>
#include <memory>
#include <cstdlib>
#include <vector>
#include "WXMformat.h"
#include "cells/CellList.h"
#include "cells/ImgCell.h"
#include <wx/debug.h>
#include <wx/textbuf.h>
#include <wx/tokenzr.h>

namespace Format {

  const wxString WXMFirstLine =
    wxS("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/");

  struct WXMHeader //-V730
  {
    WXMHeaderId id;
    wxString start = {};
    wxString end = {};
  };

  static const WXMHeader WXMHeaders[] = {
    {WXM_INPUT, wxS("/* [wxMaxima: input   start ] */"),
        wxS("/* [wxMaxima: input   end   ] */")},
    {WXM_TITLE, wxS("/* [wxMaxima: title   start ]"),
     wxS("   [wxMaxima: title   end   ] */")},
    {WXM_SECTION, wxS("/* [wxMaxima: section start ]"),
     wxS("   [wxMaxima: section end   ] */")},
    {WXM_SUBSECTION, wxS("/* [wxMaxima: subsect start ]"),
     wxS("   [wxMaxima: subsect end   ] */")},
    {WXM_SUBSUBSECTION, wxS("/* [wxMaxima: subsubsect start ]"),
     wxS("   [wxMaxima: subsubsect end   ] */")},
    {WXM_HEADING5, wxS("/* [wxMaxima: heading5 start ]"),
     wxS("   [wxMaxima: heading5 end   ] */")},
    {WXM_HEADING6, wxS("/* [wxMaxima: heading6 start ]"),
     wxS("   [wxMaxima: heading6 end   ] */")},
    {WXM_COMMENT, wxS("/* [wxMaxima: comment start ]"),
     wxS("   [wxMaxima: comment end   ] */")},
    {WXM_CAPTION, wxS("/* [wxMaxima: caption start ]"),
     wxS("   [wxMaxima: caption end   ] */")},
    {WXM_PAGEBREAK, wxS("/* [wxMaxima: page break    ] */")},
    {WXM_IMAGE, wxS("/* [wxMaxima: image   start ]"),
     wxS("   [wxMaxima: image   end   ] */")},
    {WXM_ANSWER, wxS("/* [wxMaxima: answer  start ] */"),
     wxS("/* [wxMaxima: answer  end   ] */")},
    {WXM_QUESTION, wxS("/* [wxMaxima: question  start ] */"),
     wxS("/* [wxMaxima: question  end   ] */")},
    {WXM_FOLD, wxS("/* [wxMaxima: fold    start ] */"),
     wxS("/* [wxMaxima: fold    end   ] */")},
    {WXM_FOLD_END, wxS("/* [wxMaxima: fold    end   ] */")},
    {WXM_HIDE, wxS("/* [wxMaxima: hide output   ] */")},
    {WXM_AUTOANSWER, wxS("/* [wxMaxima: autoanswer    ] */")},
    {WXM_HIDDEN_INPUT, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: input   start ] */"),
        wxS("/* [wxMaxima: input   end   ] */")},
    {WXM_HIDDEN_TITLE, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: title   start ]"),
     wxS("   [wxMaxima: title   end   ] */")},
    {WXM_HIDDEN_SECTION, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: section start ]"),
     wxS("   [wxMaxima: section end   ] */")},
    {WXM_HIDDEN_SUBSECTION, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: subsect start ]"),
     wxS("   [wxMaxima: subsect end   ] */")},
    {WXM_HIDDEN_SUBSUBSECTION, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: subsubsect start ]"),
     wxS("   [wxMaxima: subsubsect end   ] */")},
    {WXM_HIDDEN_HEADING5, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: heading5 start ]"),
     wxS("   [wxMaxima: heading5 end   ] */")},
    {WXM_HIDDEN_HEADING6, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: heading6 start ]"),
     wxS("   [wxMaxima: heading6 end   ] */")},
    {WXM_HIDDEN_COMMENT, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: comment start ]"),
     wxS("   [wxMaxima: comment end   ] */")},
    {WXM_HIDDEN_CAPTION, wxS("/* [wxMaxima: hide output   ] *//* [wxMaxima: caption start ]"),
     wxS("   [wxMaxima: caption end   ] */")},
  };

  class WXMHeaderCollection {
  public:
    static constexpr std::size_t size = sizeof(WXMHeaders) / sizeof(WXMHeaders[0]);
    WXMHeaderCollection() {
      bool check = std::is_sorted(
                                  std::begin(WXMHeaders), std::end(WXMHeaders),
                                  [](const WXMHeader &l, const WXMHeader &r) { return l.id < r.id; });
      wxASSERT(check);
    }
    static const wxString &GetStart(WXMHeaderId index) {
      if((index >= 0 && std::size_t(index) < size))
        return WXMHeaders[index].start;
      else
        return m_emptyString;
    }
    static const wxString &GetEnd(WXMHeaderId index) {
      if((index >= 0 && std::size_t(index) < size))
        return WXMHeaders[index].end;
      else
        return m_emptyString;
    }
    static const wxString &GetStart(GroupType type) {
      return GetStart(WXMHeaderId(type));
    }
    static const wxString &GetEnd(GroupType type) {
      return GetEnd(WXMHeaderId(type));
    }
    static WXMHeaderId LookupStart(const wxString &start) {
      for (auto &c : WXMHeaders)
        // cppcheck-suppress useStlAlgorithm
        if (c.start == start)
          return c.id;
      return WXM_INVALID;
    }
    static bool IsEndMarker(const wxString &line) {
      for (auto &c : WXMHeaders)
        if (!c.end.empty() && c.end == line)
          return true;
      return false;
    }
    static bool IsAnyMarker(const wxString &line) {
      for (auto &c : WXMHeaders)
        if (c.start == line || (!c.end.empty() && c.end == line))
          return true;
      return false;
    }
  };

  static WXMHeaderCollection Headers;

  static wxString EscapeWXMContent(const wxString &content) {
    wxStringTokenizer tokens(content, wxS('\n'), wxTOKEN_RET_EMPTY_ALL);
    wxString result;
    while (tokens.HasMoreTokens()) {
      wxString line = tokens.GetNextToken();
      if (Headers.IsAnyMarker(line))
        result << wxS(" ") << line;
      else
        result << line;
      if (tokens.HasMoreTokens())
        result << wxS('\n');
    }
    return result;
  }

  // GH #1907: unlike an input/code cell -- whose start and end markers are
  // each a single, already-closed "/* ... */" comment on their own line, so
  // the code between them sits outside any comment at all -- a
  // title/section/subsection/heading/comment/caption cell's start marker
  // ("/* [wxMaxima: title   start ]", no closing "*/") opens a C-style
  // comment that stays open across that cell's *entire* content, only
  // closing at the end marker's own trailing "*/". A literal "*/" inside
  // such a cell's own text therefore closes that comment early: everything
  // from there up to whatever "*/" a plain `load()`/`batch()` scan happens
  // to find next is read as live, executable Maxima input instead of inert
  // prose -- e.g. a title cell containing "abc */ x:2$ /* def " silently
  // runs "x:2$" when the file is loaded. This escapes every '/' that sits
  // immediately next to a '*' (i.e. would form a literal "/*" or "*/") with
  // the HTML numeric entity "&#47;", leaving the adjacent '*' and every
  // other '/' (e.g. an ordinary "1/2") untouched; UnescapeWXMSlashes()
  // below is its exact inverse. Escaping '&' first keeps the transform
  // unambiguous even if the original content already contains a literal
  // '&' -- without that, content already containing the literal text
  // "&#47;" would be corrupted on the way back. This can't fix a .wxm file
  // already on disk from before this existed, and can't be applied to code
  // cells at all (they must stay byte-identical so a plain Maxima can still
  // batch() them with zero wxMaxima-specific decoding) -- see the callers.
  static wxString EscapeWXMSlashes(const wxString &content) {
    wxString ampEscaped = content;
    ampEscaped.Replace(wxS("&"), wxS("&amp;"));

    wxString result;
    result.reserve(ampEscaped.Length());
    for (size_t i = 0; i < ampEscaped.Length(); i++) {
      wxUniChar c = ampEscaped[i];
      if (c == wxS('/') &&
          ((i > 0 && ampEscaped[i - 1] == wxS('*')) ||
           (i + 1 < ampEscaped.Length() && ampEscaped[i + 1] == wxS('*'))))
        result << wxS("&#47;");
      else
        result << c;
    }
    return result;
  }

  //! The exact inverse of EscapeWXMSlashes() -- see its comment.
  static wxString UnescapeWXMSlashes(const wxString &content) {
    wxString result = content;
    result.Replace(wxS("&#47;"), wxS("/"));
    result.Replace(wxS("&amp;"), wxS("&"));
    return result;
  }

  static wxString EscapeWXMTextContent(const wxString &content) {
    wxString result = EscapeWXMContent(EscapeWXMSlashes(content));
    return result;
  }

  wxString TreeToWXM(GroupCell *cell, bool wxm) {
    wxString retval;
    if (cell->IsHidden())
      retval += Headers.GetStart(WXM_HIDE) + '\n';

    auto const groupType = cell->GetGroupType();
    switch (groupType) {
    case GC_TYPE_CODE:
      if (wxm)
        retval << Headers.GetStart(groupType) << '\n'
               << EscapeWXMContent(cell->GetEditable()->ToString(true)) << '\n'
               << Headers.GetEnd(groupType) << '\n';
      else {
        retval << cell->GetEditable()->ToString(true) << '\n';
      }

      // Export the list of known answers
      if (wxm) {
        for (auto const &[question, answer] : cell->m_knownAnswers)
          retval << Headers.GetStart(WXM_QUESTION) << '\n'
                 << EscapeWXMTextContent(question) << '\n'
                 << Headers.GetEnd(WXM_QUESTION) << '\n'
                 << Headers.GetStart(WXM_ANSWER) << '\n'
                 << EscapeWXMTextContent(answer) << '\n'
                 << Headers.GetEnd(WXM_ANSWER) << '\n';
        if (cell->AutoAnswer())
          // The trailing newline matters here too (see the image case below):
          // the clipboard copy concatenates cells' WXM without a separator, so
          // without it the next cell fuses onto the auto-answer marker and is
          // lost on paste.
          retval << Headers.GetStart(WXM_AUTOANSWER) << '\n';
      }
      break;
    case GC_TYPE_TEXT:
      if (wxm)
        retval << Headers.GetStart(groupType) << '\n'
               << EscapeWXMTextContent(cell->GetEditable()->ToString(true)) << '\n'
               << Headers.GetEnd(groupType) << '\n';
      else {
        // Same open-comment injection risk as the wxm branch above (GH
        // #1907) -- this .mac export has no wxMaxima-specific unescaping on
        // read (.mac is a foreign/interop format, read as plain Maxima
        // comments), so a round trip through this exact file will show the
        // escaped entities literally rather than "/"; that's a purely
        // cosmetic cost, worth paying to keep the exported .mac inert.
        retval << wxS("/* ") << EscapeWXMSlashes(cell->GetEditable()->ToString(true)) << wxS(" */\n");
      }
      break;
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
    case GC_TYPE_SUBSUBSECTION:
    case GC_TYPE_HEADING5:
    case GC_TYPE_HEADING6:
    case GC_TYPE_TITLE:
      retval << Headers.GetStart(groupType) << '\n'
             << (wxm ? EscapeWXMTextContent(cell->GetEditable()->ToString(true))
                     : cell->GetEditable()->ToString(true)) << '\n'
             << Headers.GetEnd(groupType) << '\n';
      break;
    case GC_TYPE_IMAGE:
      retval << Headers.GetStart(groupType) << '\n'
             << (wxm ? EscapeWXMTextContent(cell->GetEditable()->ToString(true))
                     : cell->GetEditable()->ToString(true)) << '\n'
             << Headers.GetEnd(groupType) << '\n';
      if (cell->GetLabel() && cell->GetLabel()->GetType() == MC_TYPE_IMAGE) {
        const ImgCell *image = dynamic_cast<ImgCell *>(cell->GetLabel());
        retval << Headers.GetStart(WXM_IMAGE) << '\n'
               << image->GetExtension() << '\n'
               << wxBase64Encode(image->GetCompressedImage()) << '\n'
               // The trailing newline matters: every other cell type ends its
               // WXM with one, and the clipboard copy path (Worksheet::CopyCells)
               // concatenates cells' WXM without any separator. Without it the
               // next cell's start marker fuses onto the "end image" line, so on
               // paste getLinesUntil() never finds the end marker and swallows
               // every following cell into this image.
               << Headers.GetEnd(WXM_IMAGE) << '\n';
      }
      break;
    case GC_TYPE_PAGEBREAK:
      // Trailing newline as for the other cell types: the clipboard copy joins
      // cells' WXM without a separator, so a following cell would otherwise fuse
      // onto the page-break marker and be lost on paste.
      retval << Headers.GetStart(WXM_PAGEBREAK) << '\n';
      break;
    default:
      break;
    }

    // Export eventual hidden trees.
    GroupCell *tree = cell->GetHiddenTree();
    if (tree) {
      if (wxm)
        retval += Headers.GetStart(WXM_FOLD) + '\n';
      for (auto &tmp : OnList(tree))
        retval += TreeToWXM(&tmp, wxm);
      if (wxm)
        retval += Headers.GetEnd(WXM_FOLD) + '\n';
    }

    return retval;
  }

  std::unique_ptr<GroupCell> TreeFromWXM(const std::vector<wxString> &wxmLines,
                                         Configuration *config, int depth) {
    // Each nested WXM_FOLD recurses into TreeFromWXM; bound that so a crafted
    // .wxm (or pasted text) with pathologically deep fold nesting can't overflow
    // the stack. Mirrors MathParser's XML nesting cap; real documents nest folds
    // only a handful of levels, so a deep-enough fold is simply dropped.
    if (depth > 250)
      return {};

    auto wxmLine = wxmLines.begin();
    auto const end = wxmLines.end();

    //! Consumes and concatenates lines until a closing tag is reached,
    //! consumes the tag and returns the line.
    const auto getLinesUntil = [&wxmLine, end](const wxString &tag) -> wxString {
      wxString line;
      bool first = true;
      while (wxmLine != end) {
        wxString thisLn = *wxmLine++;
        if (thisLn.StartsWith(wxS(" ")) && Headers.IsAnyMarker(thisLn.Mid(1)))
          thisLn = thisLn.Mid(1);
        if (thisLn == tag)
          break;
        if (!first)
          line << '\n';
        line << thisLn;
        first = false;
      }
      return line;
    };

    bool hide = false;
    //! Hides the cell if a hide flag was set
    const auto hideCell = [&hide](GroupCell *cell) {
      if (hide && cell) {
        cell->Hide(true);
        hide = false;
      }
    };

    // Show a busy cursor while we read
    wxBusyCursor crs;
    CellListBuilder<GroupCell> tree;
    wxString question;

    while (wxmLine != end) {
      GroupCell *const last = tree.GetTail();
      std::unique_ptr<GroupCell> cell;
      wxString thisLine = *wxmLine++;
      if (thisLine.StartsWith(wxS(" ")) && Headers.IsAnyMarker(thisLine.Mid(1)))
        thisLine = thisLine.Mid(1);
      WXMHeaderId headerId = Headers.LookupStart(thisLine);
      wxString line;

      switch (headerId) {
        // Read hide tag
      case WXM_HIDE:
        hide = true;
        break;

        // Read title, section, subsection, subsubsection, heading5, heading6,
        // comment -- these need UnescapeWXMSlashes() to reverse
        // EscapeWXMTextContent()'s write-side escaping (GH #1907).
      case WXM_TITLE:
      case WXM_SECTION:
      case WXM_SUBSECTION:
      case WXM_SUBSUBSECTION:
      case WXM_HEADING5:
      case WXM_HEADING6:
      case WXM_COMMENT:
        line = UnescapeWXMSlashes(getLinesUntil(Headers.GetEnd(headerId)));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId), line);
        hideCell(cell.get());
        break;

        // Read input -- must stay byte-identical to what was written (no
        // unescaping): a code cell's start/end markers are each a complete,
        // already-closed comment on their own line, so its content was
        // never inside an open comment and was never escaped on write
        // either (see EscapeWXMTextContent()'s callers) -- a plain Maxima
        // must be able to load() this file with zero wxMaxima-specific
        // decoding.
      case WXM_INPUT:
        line = getLinesUntil(Headers.GetEnd(headerId));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId), line);
        hideCell(cell.get());
        break;
        // Same UnescapeWXMSlashes()/byte-identical split as above, for the
        // hidden variants.
      case WXM_HIDDEN_TITLE:
      case WXM_HIDDEN_SECTION:
      case WXM_HIDDEN_SUBSECTION:
      case WXM_HIDDEN_SUBSUBSECTION:
      case WXM_HIDDEN_HEADING5:
      case WXM_HIDDEN_HEADING6:
      case WXM_HIDDEN_COMMENT:
      case WXM_HIDDEN_CAPTION:
        hide = true;
        line = UnescapeWXMSlashes(getLinesUntil(Headers.GetEnd(headerId)));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId - 128), line);
        hideCell(cell.get());
        break;
      case WXM_HIDDEN_INPUT:
        hide = true;
        line = getLinesUntil(Headers.GetEnd(headerId));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId - 128), line);
        hideCell(cell.get());
        break;

        // Read an image caption
      case WXM_CAPTION:
        line = UnescapeWXMSlashes(getLinesUntil(Headers.GetEnd(headerId)));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId));
        cell->GetEditable()->SetValue(line);
        hideCell(cell.get());
        break;

        // Read an image bitmap
      case WXM_IMAGE:
        if (wxmLine != end) { // Read the image type
          wxString const imgtype = *wxmLine++;
          auto ln = getLinesUntil(Headers.GetEnd(headerId));
          if (last && last->GetGroupType() == GC_TYPE_IMAGE)
            last->SetOutput(std::make_unique<ImgCell>(
                                                      last, config, wxBase64Decode(ln), imgtype));
        }
        break;

        // Read an answer. WXM_ANSWER's own start/end markers are both
        // self-closed single-line comments (like WXM_INPUT's), so this
        // content was never at injection risk -- unescaped anyway, purely
        // for symmetry with EscapeWXMTextContent() writing it that way.
      case WXM_ANSWER:
        line = UnescapeWXMSlashes(getLinesUntil(Headers.GetEnd(headerId)));
        if (last && !question.empty()) {
          last->SetAnswer(question, line);
          question.Clear();
        }
        break;

        // Read a question -- see the WXM_ANSWER comment just above.
      case WXM_QUESTION:
        line = UnescapeWXMSlashes(getLinesUntil(Headers.GetEnd(headerId)));
        question = line;
        break;

        // Read autoanswer tag
      case WXM_AUTOANSWER:
        if (last)
          last->SetAutoAnswer(true);
        break;

        // Read a page break tag
      case WXM_PAGEBREAK:
        cell = std::make_unique<GroupCell>(config, GC_TYPE_PAGEBREAK);
        break;

        // Read a folded tree and build it
      case WXM_FOLD: {
        std::vector<wxString> hiddenTree;
        auto const &endHeader = Headers.GetEnd(headerId);
        while (wxmLine != end && *wxmLine != endHeader)
          hiddenTree.push_back(*wxmLine++);

        // A fold marker with no preceding cell to attach the hidden tree to
        // (e.g. a .wxm/pasted text that opens with the fold marker) leaves
        // "last" null; every other case here already guards it, so guard this
        // one too instead of dereferencing null.
        if (last)
          last->HideTree(TreeFromWXM(hiddenTree, config, depth + 1));
      } break;

      case WXM_INVALID:
        if (last && last->GetEditable()) {
          if (thisLine.Trim().IsEmpty())
            break;
          if (thisLine.StartsWith(wxS("/* Old versions of Maxima abort on loading files that end in a comment.")) ||
              thisLine.StartsWith(wxS("\"Created with wxMaxima ")) ||
              thisLine == WXMFirstLine ||
              thisLine.StartsWith(wxS("/* [ Created with wxMaxima version ")))
            break;

          wxString content = last->GetEditable()->GetValue();
          wxString unescaped = thisLine;
          if (!content.empty())
            content << '\n';
          content << unescaped;
          last->GetEditable()->SetValue(content);
        }
        break;
      case WXM_FOLD_END:
      case WXM_MAX:
        {}
      }

      if (cell)
        tree.Append(std::move(cell));
    }
  /* The warning from gcc is correct. But an old MacOs compiler errors out
     on correct code, here. */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-move"
#endif
    return std::move(tree);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
  }

  std::unique_ptr<GroupCell> ParseWXMFile(wxTextBuffer &text,
                                          Configuration *config) {
    std::vector<wxString> wxmLines;
    for (auto line = text.GetFirstLine();; line = text.GetNextLine()) {
      wxmLines.push_back(line);
      if (text.Eof())
        break;
    }

    return Format::TreeFromWXM(wxmLines, config);
  }

  std::unique_ptr<GroupCell> ParseMACContents(const wxString &macContents,
                                              Configuration *config) {
    wxString wxmLines;
    CellListBuilder<GroupCell> tree;
    auto const end = macContents.end();

    struct State {
      wxChar lastChar;
      wxString::const_iterator ch;
    };
    auto const readUntil = [end](wxString &line, State s, wxChar until) {
      while (s.ch != end) {
        wxChar c = *s.ch++;
        line += s.lastChar = c;
        if (c == until)
          break;
      }
      return s;
    };

    wxString line;
    for (State s{' ', macContents.begin()}; s.ch != macContents.end();) {
      wxChar c = *s.ch;
      // Handle comments
      if (s.lastChar == '/' && c == '*') {
        // Does the current line contain nothing but a comment?
        bool isCommentLine = false;
        wxString trimmed = line;
        trimmed.Trim(false);
        if (trimmed == wxS('/')) {
          isCommentLine = true;
          line = trimmed;
        }

        // Skip to the end of the comment
        while (s.ch != macContents.end()) {
          wxChar ch = *s.ch++;
          bool finished = (s.lastChar == wxS('*') && ch == wxS('/'));
          line += s.lastChar = ch;
          if (finished)
            break;
        }

        if (isCommentLine) {
          line.Trim(true);
          line.Trim(false);

          // Is this a comment from wxMaxima?
          if (line.StartsWith(wxS("/* [wxMaxima: "))) {
            // Add the rest of this comment block to the "line". Stop at EOF too:
            // readUntil() returns unchanged once the iterator reaches the end, so
            // a wxMaxima comment block with no matching " end   ] */" marker (a
            // truncated/corrupt .mac) would otherwise spin here forever.
            while (s.ch != end &&
                   !line.EndsWith(" end   ] */") &&
                   !line.EndsWith(" end   ] */\n")) {
              s = readUntil(line, s, '\n');
            }

            // If the last block was a caption block we need to read in the image
            // the caption was for, as well.
            if (line.StartsWith(Headers.GetStart(WXM_CAPTION))) {
              if (s.ch != macContents.end())
                line += s.lastChar = *s.ch++;

              s = readUntil(line, s, '\n');

              while (s.ch != end &&
                     !line.EndsWith(" end   ] */") &&
                     !line.EndsWith(" end   ] */\n")) {
                s = readUntil(line, s, '\n');
              }
            }

            // Add this array of lines to the block of wxm code we will interpret.
            wxmLines += line;
          } else {
            if (!wxmLines.IsEmpty()) {
              // Convert the comment block to an array of lines
              wxStringTokenizer tokenizer(wxmLines, "\n");
              std::vector<wxString> commentLines;
              while (tokenizer.HasMoreTokens())
                commentLines.push_back(tokenizer.GetNextToken());

              // Interpret the comment block
              if (!tree.Append(TreeFromWXM(commentLines, config)))
                tree.Append(
                            std::make_unique<GroupCell>(config, GC_TYPE_TEXT, wxmLines));
              wxmLines.Clear();
            }
            if ((line.EndsWith(" */")) || (line.EndsWith("\n*/")))
              line.Truncate(line.length() - 3);
            else
              line.Truncate(line.length() - 2);

            if ((line.StartsWith("/* ")) || (line.StartsWith("/*\n")))
              line.erase(0, 3);
            else
              line.erase(0, 2);

            tree.Append(std::make_unique<GroupCell>(config, GC_TYPE_TEXT, line));
          }
          line.clear();
        }
      }
      // Handle strings
      else if (c == '\"') {
        // Skip to the end of the string
        s = readUntil(line, s, '"');
      }
      // Handle escaped chars
      else if (c == '\\') {
        line += s.lastChar = c;
        ++s.ch;
      }
      // Handle all other chars
      else {
        line += c;

        // A line ending followed by a new line means: We want to insert a new
        // code cell.
        if ((s.lastChar == wxS('$') || s.lastChar == wxS(';')) &&
            (c == wxS('\n'))) {
          line.Trim(true);
          line.Trim(false);
          tree.Append(std::make_unique<GroupCell>(config, GC_TYPE_CODE, line));
          line.clear();
        }
        s.lastChar = c;
        ++s.ch;
      }
    }
    if (!wxmLines.IsEmpty()) {
      // Convert the comment block to an array of lines
      wxStringTokenizer tokenizer(wxmLines, "\n");
      std::vector<wxString> commentLines;
      while (tokenizer.HasMoreTokens())
        commentLines.push_back(tokenizer.GetNextToken());

      // Interpret the comment block
      if (!tree.Append(TreeFromWXM(commentLines, config)))
        tree.Append(std::make_unique<GroupCell>(config, GC_TYPE_TEXT, wxmLines));
      wxmLines.Clear();
    }

    line.Trim(true);
    line.Trim(false);
    if (!line.empty())
      tree.Append(std::make_unique<GroupCell>(config, GC_TYPE_CODE, line));

  /* The warning from gcc is correct. But an old MacOs compiler errors out
     on correct code, here. */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-move"
#endif
    return std::move(tree);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
  }

  std::unique_ptr<GroupCell> ParseMACFile(wxTextBuffer &text, bool xMaximaFile,
                                          Configuration *config) {
    bool input = true;
    wxString macContents;

    for (auto line = text.GetFirstLine();; line = text.GetNextLine()) {
      if (xMaximaFile) {
        // Detect output cells.
        if (line.StartsWith(wxS("(%o")))
          input = false;

        if (line.StartsWith(wxS("(%i"))) {
          int end = line.Find(wxS(")"));
          if (end > 0) {
            line = line.Right(line.Length() - end - 2);
            input = true;
          }
        }
      }

      if (input)
        macContents << line << wxS('\n');

      if (text.Eof())
        break;
    }

    return Format::ParseMACContents(macContents, config);
  }

} // namespace Format
const wxString Format::m_emptyString;
