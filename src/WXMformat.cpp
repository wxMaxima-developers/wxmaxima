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
  };

  static WXMHeaderCollection Headers;

  wxString TreeToWXM(GroupCell *cell, bool wxm) {
    wxString retval;
    bool trailingNewline = true;
    if (cell->IsHidden())
      retval += Headers.GetStart(WXM_HIDE) + '\n';

    auto const groupType = cell->GetGroupType();
    switch (groupType) {
    case GC_TYPE_CODE:
      if (wxm)
        retval << Headers.GetStart(groupType) << '\n'
               << cell->GetEditable()->ToString(true) << '\n'
               << Headers.GetEnd(groupType) << '\n';
      else {
        retval << cell->GetEditable()->ToString(true) << '\n';
        trailingNewline = false;
      }

      // Export the list of known answers
      if (wxm) {
        for (auto it = cell->m_knownAnswers.begin();
             it != cell->m_knownAnswers.end(); ++it)
          retval << Headers.GetStart(WXM_QUESTION) << '\n'
                 << it->first << '\n'
                 << Headers.GetEnd(WXM_QUESTION) << '\n'
                 << Headers.GetStart(WXM_ANSWER) << '\n'
                 << it->second << '\n'
                 << Headers.GetEnd(WXM_ANSWER) << '\n';
        if (cell->AutoAnswer())
          retval << Headers.GetStart(WXM_AUTOANSWER);
      }
      break;
    case GC_TYPE_TEXT:
      if (wxm)
        retval << Headers.GetStart(groupType) << '\n'
               << cell->GetEditable()->ToString(true) << '\n'
               << Headers.GetEnd(groupType) << '\n';
      else {
        retval << wxS("/* ") << cell->GetEditable()->ToString(true) << wxS(" */\n");
        trailingNewline = false;
      }
      break;
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
    case GC_TYPE_SUBSUBSECTION:
    case GC_TYPE_HEADING5:
    case GC_TYPE_HEADING6:
    case GC_TYPE_TITLE:
      retval << Headers.GetStart(groupType) << '\n'
             << cell->GetEditable()->ToString(true) << '\n'
             << Headers.GetEnd(groupType) << '\n';
      break;
    case GC_TYPE_IMAGE:
      retval << Headers.GetStart(groupType) << '\n'
             << cell->GetEditable()->ToString(true) << '\n'
             << Headers.GetEnd(groupType) << '\n';
      if (cell->GetLabel() && cell->GetLabel()->GetType() == MC_TYPE_IMAGE) {
        const ImgCell *image = dynamic_cast<ImgCell *>(cell->GetLabel());
        retval << Headers.GetStart(WXM_IMAGE) << '\n'
               << image->GetExtension() << '\n'
               << wxBase64Encode(image->GetCompressedImage()) << '\n'
               << Headers.GetEnd(WXM_IMAGE);
      }
      break;
    case GC_TYPE_PAGEBREAK:
      retval << Headers.GetStart(WXM_PAGEBREAK);
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
    if (trailingNewline)
      retval += '\n';

    return retval;
  }

  std::unique_ptr<GroupCell> TreeFromWXM(const std::vector<wxString> &wxmLines,
                                         Configuration *config) {
    auto wxmLine = wxmLines.begin();
    auto const end = wxmLines.end();

    //! Consumes and concatenates lines until a closing tag is reached,
    //! consumes the tag and returns the line.
    const auto getLinesUntil = [&wxmLine, end](const wxString &tag) -> wxString {
      wxString line;
      while (wxmLine != end) {
        auto const thisLn = wxmLine++;
        if (*thisLn == tag)
          break;
        if (!line.empty())
          line << '\n';
        line << *thisLn;
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
      WXMHeaderId headerId = Headers.LookupStart(*wxmLine++);
      wxString line;

      switch (headerId) {
        // Read hide tag
      case WXM_HIDE:
        hide = true;
        break;

        // Read title, section, subsection, subsubsection, heading5, heading6,
        //      comment, input
      case WXM_TITLE:
      case WXM_SECTION:
      case WXM_SUBSECTION:
      case WXM_SUBSUBSECTION:
      case WXM_HEADING5:
      case WXM_HEADING6:
      case WXM_COMMENT:
      case WXM_INPUT:
        line = getLinesUntil(Headers.GetEnd(headerId));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId), line);
        hideCell(cell.get());
        break;
      case WXM_HIDDEN_TITLE:
      case WXM_HIDDEN_SECTION:
      case WXM_HIDDEN_SUBSECTION:
      case WXM_HIDDEN_SUBSUBSECTION:
      case WXM_HIDDEN_HEADING5:
      case WXM_HIDDEN_HEADING6:
      case WXM_HIDDEN_COMMENT:
      case WXM_HIDDEN_INPUT:
      case WXM_HIDDEN_CAPTION:
        hide = true;
        line = getLinesUntil(Headers.GetEnd(headerId));
        cell = std::make_unique<GroupCell>(config, GroupType(headerId - 128), line);
        hideCell(cell.get());
        break;

        // Read an image caption
      case WXM_CAPTION:
        line = getLinesUntil(Headers.GetEnd(headerId));
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

        // Read an answer
      case WXM_ANSWER:
        line = getLinesUntil(Headers.GetEnd(headerId));
        if (last && !question.empty())
          last->SetAnswer(question, line);
        break;

        // Read a question
      case WXM_QUESTION:
        line = getLinesUntil(Headers.GetEnd(headerId));
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

        last->HideTree(TreeFromWXM(hiddenTree, config));
      } break;

      case WXM_INVALID:
      case WXM_FOLD_END:
      case WXM_MAX:
        {}
      }

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
    wxString wxmBlock;
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
            // Add the rest of this comment block to the "line".
            while (!line.EndsWith(" end   ] */") &&
                   !line.EndsWith(" end   ] */\n")) {
              s = readUntil(line, s, '\n');
            }

            // If the last block was a caption block we need to read in the image
            // the caption was for, as well.
            if (line.StartsWith(Headers.GetStart(WXM_CAPTION))) {
              if (s.ch != macContents.end())
                line += s.lastChar = *s.ch++;

              s = readUntil(line, s, '\n');

              while (!line.EndsWith(" end   ] */") &&
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
