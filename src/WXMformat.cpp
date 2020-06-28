// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#include "WXMformat.h"
#include "ImgCell.h"
#include <wx/debug.h>
#include <wx/textbuf.h>
#include <wx/tokenzr.h>
#include <algorithm>
#include <cstdlib>
#include <vector>

namespace Format
{

const wxString WXMFirstLine = wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/");

struct WXMHeader
{
  WXMHeaderId id;
  wxString start = {};
  wxString end = {};
};

static const WXMHeader WXMHeaders[] = {
  {WXM_INPUT, wxT("/* [wxMaxima: input   start ] */"),
   wxT("/* [wxMaxima: input   end   ] */")},
  {WXM_TITLE, wxT("/* [wxMaxima: title   start ]"),
   wxT("   [wxMaxima: title   end   ] */")},
  {WXM_SECTION, wxT("/* [wxMaxima: section start ]"),
   wxT("   [wxMaxima: section end   ] */")},
  {WXM_SUBSECTION, wxT("/* [wxMaxima: subsect start ]"),
   wxT("   [wxMaxima: subsect end   ] */")},
  {WXM_SUBSUBSECTION, wxT("/* [wxMaxima: subsubsect start ]"),
   wxT("   [wxMaxima: subsubsect end   ] */")},
  {WXM_HEADING5, wxT("/* [wxMaxima: heading5 start ]"),
   wxT("   [wxMaxima: heading5 end   ] */")},
  {WXM_HEADING6, wxT("/* [wxMaxima: heading6 start ]"),
   wxT("   [wxMaxima: heading6 end   ] */")},
  {WXM_COMMENT,  wxT("/* [wxMaxima: comment start ]"),
   wxT("   [wxMaxima: comment end   ] */")},
  {WXM_CAPTION, wxT("/* [wxMaxima: caption start ]"),
   wxT("   [wxMaxima: caption end   ] */")},
  {WXM_PAGEBREAK, wxT("/* [wxMaxima: page break    ] */")},
  {WXM_IMAGE, wxT("/* [wxMaxima: image   start ]"),
   wxT("   [wxMaxima: image   end   ] */")},
  {WXM_ANSWER, wxT("/* [wxMaxima: answer  start ] */"),
   wxT("/* [wxMaxima: answer  end   ] */")},
  {WXM_QUESTION, wxT("/* [wxMaxima: question  start ] */"),
   wxT("/* [wxMaxima: question  end   ] */")},
  {WXM_FOLD, wxT("/* [wxMaxima: fold    start ] */"),
   wxT("/* [wxMaxima: fold    end   ] */")},
  {WXM_FOLD_END, wxT("/* [wxMaxima: fold    end   ] */")},
  {WXM_HIDE, wxT("/* [wxMaxima: hide output   ] */")},
  {WXM_AUTOANSWER, wxT("/* [wxMaxima: autoanswer    ] */")},
  };

class WXMHeaderCollection
{
public:
  static constexpr size_t size = sizeof(WXMHeaders) / sizeof(WXMHeaders[0]);
  WXMHeaderCollection()
  {
    bool check = std::is_sorted(
      std::begin(WXMHeaders), std::end(WXMHeaders),
      [](const WXMHeader &l, const WXMHeader &r){ return l.id < r.id; });
    if (!check) abort(); // An assertion is not enough - this is a programming bug
  }
  static const wxString &GetStart(WXMHeaderId index)
  {
    wxASSERT(index >= 0 && size_t(index) < size);
    return WXMHeaders[index].start;
  }
  static const wxString &GetEnd(WXMHeaderId index)
  {
    wxASSERT(index >= 0 && size_t(index) < size);
    return WXMHeaders[index].end;
  }
  static const wxString &GetStart(GroupType type) { return GetStart(WXMHeaderId(type)); }
  static const wxString &GetEnd(GroupType type) { return GetEnd(WXMHeaderId(type)); }
  static WXMHeaderId LookupStart(const wxString &start)
  {
    for (auto &c : WXMHeaders)
      // cppcheck-suppress useStlAlgorithm
      if (c.start == start) return c.id;
    return WXM_INVALID;
  }
};

static WXMHeaderCollection Headers;

static wxString &TreeToWXM(wxString &retval, GroupCell *cell, bool wxm)
{
  bool trailingNewline = true;
  if (cell->IsHidden())
    retval += Headers.GetStart(WXM_HIDE);

  auto const groupType = cell->GetGroupType();
  switch (groupType)
  {
  case GC_TYPE_CODE:
    if (wxm)
      retval << Headers.GetStart(groupType) << '\n'
             << cell->GetEditable()->ToString() << '\n'
             << Headers.GetEnd(groupType) << '\n';
    else
    {
      retval << cell->GetEditable()->ToString() << '\n';
      trailingNewline = false;
    }

    // Export the list of known answers
    if (wxm)
    {
      for (auto it = cell->m_knownAnswers.begin(); it != cell->m_knownAnswers.end(); ++ it)
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
             << cell->GetEditable()->ToString() << '\n'
             << Headers.GetEnd(groupType) << '\n';
    else
    {
      retval << wxT("/* ") << cell->GetEditable()->ToString() << wxT(" */\n");
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
           << cell->GetEditable()->ToString() << '\n'
           << Headers.GetEnd(groupType) << '\n';
    break;
  case GC_TYPE_IMAGE:
    retval << Headers.GetStart(groupType) << '\n'
           << cell->GetEditable()->ToString() << '\n'
           << Headers.GetEnd(groupType) << '\n';
    if (cell->GetLabel() && cell->GetLabel()->GetType() == MC_TYPE_IMAGE)
    {
      ImgCell *image = dynamic_cast<ImgCell *>(cell->GetLabel());
      retval << Headers.GetStart(WXM_IMAGE) << '\n'
             << image->GetExtension() << '\n'
             << wxBase64Encode(image->GetCompressedImage()) << '\n'
             << Headers.GetEnd(WXM_IMAGE);
    }
    break;
  case GC_TYPE_PAGEBREAK:
    retval << Headers.GetStart(WXM_PAGEBREAK);
    break;
  }

  // Export eventual hidden trees.
  GroupCell *tmp = cell->GetHiddenTree();
  if (tmp)
  {
    if (wxm)
      retval << Headers.GetStart(WXM_FOLD) << '\n';
    while (tmp)
    {
      retval << TreeToWXM(retval, tmp, wxm);
      tmp = tmp->GetNext();
    }
    if (wxm)
      retval << Headers.GetEnd(WXM_FOLD) << '\n';
  }
  if (trailingNewline)
    retval << '\n';

  return retval;
}

wxString TreeToWXM(GroupCell *cell, bool wxm)
{
  wxString retval;
  TreeToWXM(retval, cell, wxm);
  return retval;
}

GroupCell *TreeFromWXM(const wxArrayString &wxmLines, Configuration **config)
{
  auto wxmLine = wxmLines.begin();
  auto const end = wxmLines.end();

  //! Consumes and concatenates lines until a closing tag is reached,
  //! consumes the tag and returns the line.
  const auto getLinesUntil = [&wxmLine, end](const wxChar *tag) -> wxString
  {
    wxString line;
    while (wxmLine != end)
    {
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
  const auto hideCell = [&hide](GroupCell *cell)
  {
    if (hide && cell)
    {
      cell->Hide(true);
      hide = false;
    }
  };

  // Show a busy cursor while we read
  wxBusyCursor crs;
  GroupCell *tree = {};
  GroupCell *last = {};
  wxString question;

  while (wxmLine != end)
  {
    GroupCell *cell = {};
    WXMHeaderId headerId = Headers.LookupStart(*wxmLine ++);
    wxString line;

    switch (headerId)
    {
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
      cell = new GroupCell(config, GroupType(headerId), line);
      hideCell(cell);
      break;

      // Read an image caption
    case WXM_CAPTION:
      line = getLinesUntil(Headers.GetEnd(headerId));
      cell = new GroupCell(config, GroupType(headerId));
      cell->GetEditable()->SetValue(line);
      hideCell(cell);
      break;

      // Read an image bitmap
    case WXM_IMAGE:
      if (wxmLine != end)
      { // Read the image type
        wxString const imgtype = *wxmLine ++;
        auto ln = getLinesUntil(Headers.GetEnd(headerId));
        if (last && last->GetGroupType() == GC_TYPE_IMAGE)
        last->SetOutput(
            new ImgCell(NULL, config, wxBase64Decode(ln), imgtype));
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
        last->AutoAnswer(true);
      break;

      // Read a page break tag
    case WXM_PAGEBREAK:
      cell = new GroupCell(config, GC_TYPE_PAGEBREAK);
      break;

      // Read a folded tree and build it
    case WXM_FOLD:
    {
      wxArrayString hiddenTree;
      auto const &endHeader = Headers.GetEnd(headerId);
      while (wxmLine != end && *wxmLine != endHeader)
        hiddenTree.Add(*wxmLine ++);

      last->HideTree(TreeFromWXM(hiddenTree, config));
    }
    break;

    case WXM_INVALID:
    case WXM_FOLD_END:
    case WXM_MAX:
      ;
    }

    if (!cell)
      continue;

    // We have created a cell in this pass
    if (!tree)
      tree = last = cell;
    else
    {
      last->m_next = cell;
      last->SetNextToDraw(cell);
      last->m_next->m_previous = last;

      last = last->GetNext();
    }
  }
  return tree;
}

GroupCell *ParseWXMFile(wxTextBuffer &text, Configuration **config)
{
  wxArrayString wxmLines;
  for (auto line = text.GetFirstLine(); ; line = text.GetNextLine())
  {
    wxmLines.Add(line);
    if (text.Eof())
      break;
  }

  GroupCell *tree = Format::TreeFromWXM(wxmLines, config);
  return tree;
}

GroupCell *ParseMACContents(const wxString &macContents, Configuration **config)
{
  GroupCell *tree = {}, *last = {};
  auto const appendCell = [&last, &tree](GroupCell *cell)
  {
    if (last)
      last->AppendCell(cell);
    else
      tree = cell;

    last = cell;
  };

  auto const end = macContents.end();

  struct State { wxChar lastChar; wxString::const_iterator ch; };
  auto const readUntil = [end](wxString &line, State s, wxChar until)
  {
    while (s.ch != end)
    {
      wxChar c = *s.ch++;
      line += s.lastChar = c;
      if (c == until)
        break;
    }
    return s;
  };

  wxString line;
  for (State s{' ', macContents.begin()}; s.ch != macContents.end(); )
  {
    wxChar c = *s.ch;
    // Handle comments
    if (s.lastChar == '/' && c == '*')
    {
      // Does the current line contain nothing but a comment?
      bool isCommentLine = false;
      wxString trimmed = line;
      trimmed.Trim(false);
      if (trimmed == wxT('/'))
      {
        isCommentLine = true;
        line = trimmed;
      }

      // Skip to the end of the comment
      while (s.ch != macContents.end())
      {
        wxChar ch = *s.ch++;
        bool finished = (s.lastChar == wxT('*') && ch == wxT('/'));
        line += s.lastChar = ch;
        if (finished) break;
      }

      if (isCommentLine)
      {
        line.Trim(true);
        line.Trim(false);

        // Is this a comment from wxMaxima?
        if (line.StartsWith(wxT("/* [wxMaxima: ")))
        {
          // Add the rest of this comment block to the "line".
          while(
            !line.EndsWith(" end   ] */") &&
            !line.EndsWith(" end   ] */\n")
            )
          {
            s = readUntil(line, s, '\n');
          }

          // If the last block was a caption block we need to read in the image
          // the caption was for, as well.
          if (line.StartsWith(Headers.GetStart(WXM_CAPTION)))
          {
            if (s.ch != macContents.end())
              line += s.lastChar = *s.ch++;

            s = readUntil(line, s, '\n');

            while(
              !line.EndsWith(" end   ] */") &&
              !line.EndsWith(" end   ] */\n")
              )
            {
              s = readUntil(line, s, '\n');
            }
          }

          //  Convert the comment block to an array of lines
          wxStringTokenizer tokenizer(line, "\n");
          wxArrayString commentLines;
          while (tokenizer.HasMoreTokens())
            commentLines.Add(tokenizer.GetNextToken());

          // Interpret this array of lines as wxm code.
          GroupCell *cell;
          appendCell((cell = TreeFromWXM(commentLines, config)));
        }
        else
        {
          if ((line.EndsWith(" */")) || (line.EndsWith("\n*/")))
            line.Truncate(line.length() - 3);
          else
            line.Truncate(line.length() - 2);

          if ((line.StartsWith("/* ")) || (line.StartsWith("/*\n")))
            line.erase(0, 3);
          else
            line.erase(0, 2);

          GroupCell *cell;
          appendCell((cell = new GroupCell(config, GC_TYPE_TEXT, line)));
        }
        line.clear();
      }
    }
    // Handle strings
    else if (c == '\"')
    {
      // Skip to the end of the string
      s = readUntil(line, s, '"');
    }
    // Handle escaped chars
    else if (c == '\\')
    {
      line += s.lastChar = c;
      ++s.ch;
    }
    // Handle all other chars
    else
    {
      line += c;

      // A line ending followed by a new line means: We want to insert a new code cell.
      if ((s.lastChar == wxT('$') || s.lastChar == wxT(';')) && (c == wxT('\n')))
      {
        line.Trim(true);
        line.Trim(false);
        GroupCell *cell;
        appendCell((cell = new GroupCell(config, GC_TYPE_CODE, line)));
        line.clear();
      }
      s.lastChar = c;
      ++s.ch;
    }
  }

  line.Trim(true);
  line.Trim(false);
  if (!line.empty())
  {
    GroupCell *cell;
    appendCell((cell = new GroupCell(config, GC_TYPE_CODE, line)));
  }

  return tree;
}

GroupCell *ParseMACFile(wxTextBuffer &text, bool xMaximaFile, Configuration **config)
{
  bool input = true;
  wxString macContents;

  for (auto line = text.GetFirstLine(); ; line = text.GetNextLine())
  {
    if (xMaximaFile)
    {
      // Detect output cells.
      if (line.StartsWith(wxT("(%o")))
        input = false;

      if (line.StartsWith(wxT("(%i")))
      {
        int end = line.Find(wxT(")"));
        if (end > 0)
        {
          line = line.Right(line.Length() - end - 2);
          input = true;
        }
      }
    }

    if (input)
      macContents << line << wxT('\n');

    if (text.Eof())
      break;
  }

  GroupCell *tree = Format::ParseMACContents(macContents, config);
  return tree;
}

} // namespace Format
