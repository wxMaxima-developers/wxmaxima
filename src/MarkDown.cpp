// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015-2018  Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file contains the code for MarkDownParser.

  MarkDownParser is the class that handles the markdown syntax
*/

#include "MarkDown.h"
#include "StringUtils.h"
#include <wx/tokenzr.h>
#include <wx/regex.h>
#include <algorithm>

MarkDownParser::~MarkDownParser()
{
}

MarkDownParser::MarkDownParser(Configuration *cfg, const ElementPack &elts) :
    m_configuration(cfg), m_e(elts)
{
}

struct MarkDownParser::ElementPack
{
  wxString quoteBegin;        //!< The marker that says we want to start quote
  wxString quoteEnd;          //!< The marker that says we want to end quote
  wxString quoteChar;         //!< The marker for a quote
  wxString itemizeBegin;      //!< The marker for the begin of an item list
  wxString itemizeEnd;        //!< The marker for the end of an item list
  wxString itemizeItem;       //!< The marker for the begin of an item
  wxString itemizeEndItem;    //!< The marker for the end of an item
  wxString newLine;           //!< The marker for the beginning of a new line
};

wxString MarkDownParser::MarkDown(wxString str)
{
  static wxString bulletSpace{wxT("* ")};
  static wxString quoteSpace = m_e.quoteChar + wxT(' ');

  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  DoReplacementsOn(str);

  // The result of this action
  wxString result;

  // The list of indentation levels for bullet lists we found
  // so far
  struct Indent
  {
    ssize_t level;
    char type;
    Indent (ssize_t level, wxChar type) : level(level), type(type) {}
  };
  std::vector<Indent> indentations;

  wxString::const_iterator const strEnd = str.end();
  wxString::const_iterator nextLine = str.begin();

  // Now process the input string line-by-line.
  while (nextLine != strEnd)
  {
    // Find the ending of the current line
    wxString::const_iterator line = nextLine;
    wxString::const_iterator lineEnd = std::find(line, strEnd, wxT('\n'));
    nextLine = (lineEnd == strEnd) ? lineEnd : std::next(lineEnd);
    auto const lineStart = line;

    // Skip opening whitespace
    line = AdvanceTrim(line, lineEnd);
    ssize_t const level = std::distance(lineStart, line);

    // Skip the line if it had nothing but whitespace
    if (line == lineEnd)
      continue;

    // Let's see if the line is the start of a bullet list item?
    if ((indentations.empty() || indentations.back().type == '*')
        && AdvanceOverOne(line, lineEnd, bulletSpace))
    {
      // The line is the start of a bullet list item; we skipped the start marker now.

      // Skip leading and trailing whitespace
      line = AdvanceTrim(line, RetractTrim(line, lineEnd));

      // Let's see if this is the first item in the list
      // or if we switched to a higher indentation level
      if (indentations.empty() || level > indentations.back().level)
      {
        // This is the first item on this level => Start the itemization.
        result += m_e.itemizeBegin;
        indentations.emplace_back(level, '*');
      }
      // Are we at the same level?
      else if (level == indentations.back().level)
      {
        // End the previous item before we start a new one on the same level.
        result += m_e.itemizeEndItem;
      }
      // Or did we switch to a lower indentation level?
      else if (level < indentations.back().level)
      {
        while (!indentations.empty() && (level < indentations.back().level))
        {
          result += m_e.itemizeEndItem;
          result += m_e.itemizeEnd;
          indentations.pop_back();
        }
        result += m_e.itemizeEndItem;
      }

      // Add a new item marker.
      result += m_e.itemizeItem;

      // Add the item itself
      // Discard whitespace at the end of the item
      result.append(line, RetractTrim(line, lineEnd)) += wxT(' ');
    }
    else if (AdvanceOverOne(line, lineEnd, quoteSpace))
    {
      // We are part of a quotation; we skipped the quote start marker.

      // Skip leading and trailing whitespace
      line = AdvanceTrim(line, RetractTrim(line, lineEnd));

      // Let's see if this is the first item in the list,
      // or if we are on a new indentation level?
      if (indentations.empty() || level > indentations.back().level)
      {
        // This is the first item => Start the itemization.
        result += m_e.quoteBegin;
        indentations.emplace_back(level, '>');
      }
      else
      {
        // We are inside a bullet list

        // End lists if we are at an old indentation level.
        // cppcheck-suppress knownConditionTrueFalse
        while (!indentations.empty() && (level < indentations.back().level))
       {
          if (indentations.back().type == '*')
          {
            result += m_e.itemizeEndItem;
            result += m_e.itemizeEnd;
          }
          else
            result += m_e.quoteEnd;
          indentations.pop_back();
        }
      }
      result.append(line, RetractTrim(line, lineEnd)) += wxT(' ');
    }
    else
    {
      // Ordinary text.
      //
      // If we are at a old indentation level we need to end some lists
      // and add a new item if we still are inside a list.
      if (!indentations.empty())
      {
        // Add the text to the output.
        if((result != wxEmptyString) &&
           (!result.EndsWith(m_e.itemizeEndItem)) &&
           (!result.EndsWith(m_e.itemizeEnd)) &&
           (!result.EndsWith(m_e.quoteEnd))
          )
          result += m_e.newLine;
        if (indentations.back().level > level)
        {
          while ((!indentations.empty()) &&
                 (indentations.back().level > level))
          {
            if (indentations.back().type == '*')
            {
              result += m_e.itemizeEndItem;
              result += m_e.itemizeEnd;
            }
            else
              result += m_e.quoteEnd;

            indentations.pop_back();
          }
        }
      }
      result.append(line, RetractTrim(line, lineEnd));
    }
  }

  // Close all item lists
  while (!indentations.empty())
  {
    if (indentations.back().type == wxT('*'))
    {
      result += m_e.itemizeEndItem;
      result += m_e.itemizeEnd;
    }
    else
      result += m_e.quoteEnd;
    indentations.pop_back();
  }
  return result;
}

struct TeXElementPack : public MarkDownParser::ElementPack
{
  TeXElementPack()
  {
    quoteBegin = wxT("\\begin{quote}\n");
    quoteEnd = wxT("\\end{quote}\n");
    quoteChar = wxT("\\ensuremath{>}");
    itemizeBegin = wxT("\\begin{itemize}\n");
    itemizeEnd = wxT("\\end{itemize}\n");
    itemizeItem = wxT("\\item ");
    itemizeEndItem = wxString{};
    newLine = wxT("\n\n");
  }
};
static const TeXElementPack texElementPack;

MarkDownTeX::MarkDownTeX(Configuration *configuration) :
    MarkDownParser(configuration, texElementPack)
{
}

struct HTMLElementPack : public MarkDownParser::ElementPack
{
  HTMLElementPack()
  {
    quoteBegin =  wxT("<blockquote>\n");
    quoteEnd =  wxT("</blockquote>\n");
    quoteChar =  wxT("&gt;");
    itemizeBegin =  wxT("<ul>\n");
    itemizeEnd =  wxT("</ul>\n");
    itemizeItem =  wxT("<li>");
    itemizeEndItem =  wxT("</li>\n");
    newLine =  wxT("<br/>");
  }
};
static const HTMLElementPack htmlElementPack;


MarkDownHTML::MarkDownHTML(Configuration *configuration) :
    MarkDownParser(configuration, htmlElementPack)
{
}

struct Replacer
{
  wxRegEx regex;
  wxString replacement;
  Replacer(const wxChar *regex, const wxChar *replacement) :
      replacement(replacement)
  {
    bool result = this->regex.Compile(regex);
    wxASSERT_MSG(result, "A Markdown parser regex has failed to compile.");
  }
};

void MarkDownTeX::DoReplacementsOn(wxString &str)
{
  static const Replacer replacers[] = {
    {(wxT("#")), (wxT("\\\\#"))},
    {wxT("\\\\verb\\|<\\|=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longleftrightarrow}")},
    {wxT("=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longrightarrow}")},
    {wxT("\\\\verb\\|<\\|-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longleftrightarrow}")},
    {wxT("-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longrightarrow}")},
    {wxT("\\\\verb\\|<\\|-"), wxT("\\\\ensuremath{\\\\longleftarrow}")},
    {wxT("\\\\verb\\|<\\|="), wxT("\\\\ensuremath{\\\\leq}")},
    {wxT("\\\\verb\\|>\\|="), wxT("\\\\ensuremath{\\\\geq}")},
    {wxT("\\+/-"), wxT("\\\\ensuremath{\\\\pm}")},
    {wxT("\\\\verb\\|>\\|\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\gg}")},
    {wxT("\\\\verb\\|<\\|\\\\verb\\|<\\|"), wxT("\\\\ensuremath{\\\\ll}")},
  };
  for (auto &replacer : replacers)
    replacer.regex.Replace(&str, replacer.replacement);
}

void MarkDownHTML::DoReplacementsOn(wxString &str)
{
  static const Replacer replacers[] = {
    {wxT("\\&lt);=\\&gt;"), wxT("\u21d4")},
    {wxT("=\\&gt);"), wxT("\u21d2")},
    {wxT("&lt);-\\&gt;"), wxT("\u2194")},
    {wxT("-\\&gt);"), wxT("\u2192")},
    {wxT("\\&lt);-"), wxT("\u2190")},
    {wxT("\\&lt);="), wxT("\u2264")},
    {wxT("\\&gt);="), wxT("\u2265")},
    {wxT("\\+/-"), wxT("\u00B1")},
    {wxT("\u00A0"), wxT("\u00A0")},
  };
  for (auto &replacer : replacers)
    replacer.regex.Replace(&str, replacer.replacement);
}
