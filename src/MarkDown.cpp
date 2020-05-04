// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015-2018  Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C) 2020  Kuba Ober <kuba@bertec.com>
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

// Indentation level type
enum class LVL {
  NONE,
  BULLET,
  QUOTE,
  TEXT
};

class MarkDownParser::IndentManager
{
  struct Level
  {
    ssize_t level = -1;
    LVL type = LVL::NONE;
    Level() = default;
    Level(ssize_t level, LVL type) : level(level), type(type) {}
  };
  std::vector<Level> m_levels{Level{}};
  bool m_lastOpWasDown = false;
  const MarkDownParser::ElementPack &m_e;
  wxString &m_result;

public:
  IndentManager(const MarkDownParser::ElementPack &ep, wxString &result) :
      m_e(ep), m_result(result) {}
  ~IndentManager() { CloseAll(); }
  LVL LastType() const { return m_levels.back().type; }
  void CloseAll() { SetLevel(-1, LVL::NONE); }
  void SetLevel(ssize_t level, LVL type)
  {
    wxASSERT(level >= 0 || type == LVL::NONE);
    while (level < m_levels.back().level)
    {
      // Close out levels above current one
      if (CloseLevel(type))
        m_levels.pop_back();
      m_lastOpWasDown = true;
    }
    auto &last = m_levels.back();
    if (level > last.level || type != last.type)
    {
      if (type == LVL::BULLET)
        m_result << m_e.itemizeBegin << m_e.itemizeItem;
      else if (type == LVL::QUOTE && last.type != LVL::QUOTE)
        m_result << m_e.quoteBegin;
      m_levels.emplace_back(level, type);
    }
    else
    {
      wxASSERT(level == last.level);
      if (type == LVL::BULLET)
        m_result << m_e.itemizeEndItem << m_e.itemizeItem;
      else if (type == LVL::TEXT && !m_lastOpWasDown)
        m_result << m_e.newLine;
    }
    m_lastOpWasDown = false;
  }
  bool CloseLevel(LVL forType)
  {
    auto &last = m_levels.back();
    if (last.type == LVL::BULLET)
      m_result << m_e.itemizeEndItem << m_e.itemizeEnd;
    else if (last.type == LVL::QUOTE && forType != LVL::QUOTE)
      m_result << m_e.quoteEnd;
    else
      return false;
    return true;
  }
  void OpenLevel(LVL type)
  {
    auto &last = m_levels.back();
    if (type == LVL::BULLET)
      m_result << m_e.itemizeBegin << m_e.itemizeItem;
    else if (type == LVL::QUOTE && last.type != LVL::QUOTE)
      m_result << m_e.quoteBegin;
  }
};

wxString MarkDownParser::MarkDown(wxString str)
{
  static wxString bulletSpace{wxT("* ")};
  static wxString quoteSpace = m_e.quoteChar + wxT(' ');

  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  DoReplacementsOn(str);

  // MarkDown parsed into the desired format
  wxString result;

  MarkDownParser::IndentManager indents(m_e, result);

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

    // Skip opening whitespace, skip the line if if it has only whitespace
    line = AdvanceTrim(line, lineEnd);
    if (line == lineEnd)
      continue;

    // Compute the level of the line's indentation
    ssize_t const level = std::distance(lineStart, line);

    if (AdvanceOverOne(line, lineEnd, bulletSpace))
      indents.SetLevel(level, LVL::BULLET);
    else if (AdvanceOverOne(line, lineEnd, quoteSpace))
      indents.SetLevel(level, LVL::QUOTE);
    else
      indents.SetLevel(level, LVL::TEXT);

    // Skip leading and trailing whitespace
    line = AdvanceTrim(line, RetractTrim(line, lineEnd));
    result.append(line, RetractTrim(line, lineEnd));
    if (indents.LastType() != LVL::TEXT)
      result += wxT(' ');
  }

  return result;
}

struct MarkDownTeX::ElementPack : public MarkDownParser::ElementPack
{
  ElementPack()
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
const MarkDownTeX::ElementPack MarkDownTeX::elementPack;

MarkDownTeX::MarkDownTeX(Configuration *configuration) :
    MarkDownParser(configuration, elementPack)
{
}

struct MarkDownHTML::ElementPack : public MarkDownParser::ElementPack
{
  ElementPack()
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
const MarkDownHTML::ElementPack MarkDownHTML::elementPack;


MarkDownHTML::MarkDownHTML(Configuration *configuration) :
    MarkDownParser(configuration, elementPack)
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
