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
#include <wx/tokenzr.h>
#include <wx/regex.h>

MarkDownParser::~MarkDownParser()
{
}

MarkDownParser::MarkDownParser(Configuration *cfg) :
    m_configuration(cfg)
{
}

wxString MarkDownParser::MarkDown(wxString str)
{
  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  DoReplacementsOn(str);

  // The result of this action
  wxString result;

  // The list of indentation levels for bullet lists we found
  // so far
  std::vector<size_t> indentationLevels;
  std::vector<wxChar> indentationTypes;

  // Now process the input string line-by-line.
  wxStringTokenizer lines(str, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    wxString quotingStart;

    wxString st = line;
    st.Trim(false);
    size_t index = line.Length() - st.Length();

    // Determine the amount of indentation and the contents of the rest
    // of the line.

    // Trailing whitespace doesn't help much.
    line = line.Trim();

    // Does the line contain anything other than spaces?
    if (st != wxEmptyString)
    {
      // The line contains actual text..

      // Let's see if the line is the start of a bullet list item
      if ((st.StartsWith("* ")) &&
          ((indentationTypes.empty())||(indentationTypes.back() == wxT('*'))))
      {

        // Remove the bullet list start marker from our string.
        st.Remove(0, 2);
        st.Trim(false);

        // Let's see if this is the first item in the list
        if (indentationLevels.empty())
        {
          // This is the first item => Start the itemization.
          result += itemizeBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('*'));
        }
        else
        {
          // End the previous item before we start a new one on the same level.
          if (index == indentationLevels.back())
            result += itemizeEndItem();
        }

        // Did we switch to a higher indentation level?
        if (index > indentationLevels.back())
        {
          // A higher identation level => add the itemization-start-command.
          result += itemizeBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('*'));
        }

        // Did we switch to a lower indentation level?
        if (index < indentationLevels.back())
        {
          while (!indentationLevels.empty() && (index < indentationLevels.back()))
          {
            result += itemizeEndItem();
            result += itemizeEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }
          result += itemizeEndItem();
        }

        // Add a new item marker.
        result += itemizeItem();

        // Add the item itself
        while (!st.IsEmpty() && st.Last() == ' ')
          st.Truncate(st.Length() - 1); // We can't use Trim, since it removes newlines too!
        if(st.EndsWith(newLine()))
          st.Truncate(st.Length() - newLine().Length());
        st.Trim();
        result += st += wxT(" ");
      }
      else if (st.StartsWith(quoteChar() + wxT(' ')))
      {
        // We are part of a quotation.
        //
        // Remove the bullet list start marker from our string.
        st.Remove(0, quoteChar().Length() + 1);
        st.Trim(false);

        // Let's see if this is the first item in the list
        if (indentationLevels.empty())
        {
          // This is the first item => Start the itemization.
          result += quoteBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('>'));
        }
        else
        {
          // We are inside a bullet list.

          // Are we on a new indentation level?
          if (indentationLevels.back() < index)
          {
            // A new identation level => add the itemization-start-command.
            result += quoteBegin();
            indentationLevels.push_back(index);
            indentationTypes.push_back(wxT('>'));
          }

          // End lists if we are at a old indentation level.
          // cppcheck-suppress knownConditionTrueFalse
          while (!indentationLevels.empty() && (indentationLevels.back() > index))
          {
            if (indentationTypes.back() == wxT('*'))
            {
              result += itemizeEndItem();
              result += itemizeEnd();
            }
            else
              result += quoteEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }
        }
        result += st += wxT(" ");
      }
      else
      {
        // Ordinary text.
        //
        // If we are at a old indentation level we need to end some lists
        // and add a new item if we still are inside a list.
        if (!indentationLevels.empty())
        {
          // Add the text to the output.
          if((result != wxEmptyString) &&
             (!result.EndsWith(itemizeEndItem())) &&
             (!result.EndsWith(itemizeEnd())) &&
             (!result.EndsWith(quoteEnd()))
            )
            result += newLine();
          if (indentationLevels.back() > index)
          {
            while ((!indentationLevels.empty()) &&
                   (indentationLevels.back() > index))
            {
              if (indentationTypes.back() == wxT('*'))
              {
                result += itemizeEndItem();
                result += itemizeEnd();
              }
              else
                result += quoteEnd();

              indentationLevels.pop_back();
              indentationTypes.pop_back();
            }
          }
        }
        line.Remove(0, index);
        result += line;
      }
    }
  }

  // Close all item lists
  while (!indentationLevels.empty())
  {
    if (indentationTypes.back() == wxT('*'))
    {
      result += itemizeEndItem();
      result += itemizeEnd();
    }
    else
      result += quoteEnd();
    indentationLevels.pop_back();
    indentationTypes.pop_back();
  }
  return result;
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
