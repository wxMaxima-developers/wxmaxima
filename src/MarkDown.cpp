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

MarkDownParser::MarkDownParser(Configuration *cfg)
{
  m_configuration = cfg;
}

wxString MarkDownParser::MarkDown(wxString str)
{
  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  for (auto const &replacer : regexReplaceList)
    replacer.DoReplace(str);

  // The result of this action
  wxString result;

  // The list of indentation levels for bullet lists we found
  // so far
  std::list<size_t> indentationLevels;
  std::list<wxChar> indentationTypes;

  // Now process the input string line-by-line.
  wxStringTokenizer lines(str, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    wxString quotingStart;
    wxString lineTrimmed = line;
    lineTrimmed.Trim(false);

    wxString st = line;
    st = st.Trim(false);
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
        st = st.Right(st.Length() - 2);
        st = st.Trim(false);

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
        st.Trim();
        if(st.EndsWith(NewLine()))
          st = st.Left(st.Length() - NewLine().Length());
        result += st += wxT(" ");
      }
      else if (st.StartsWith(quoteChar() + wxT(" ")))
      {
        // We are part of a quotation.
        //
        // Remove the bullet list start marker from our string.
        st = st.Right(st.Length() - quoteChar().Length() - 1);
        st = st.Trim(false);

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
            result += NewLine();
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
        line = line.Right(line.Length() - index);
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

MarkDownTeX::MarkDownTeX(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.emplace_back(
    wxT("#"), wxT("\\\\#"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|<\\|=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longleftrightarrow}"));
  regexReplaceList.emplace_back(
    wxT("=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longrightarrow}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|<\\|-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longleftrightarrow}"));
  regexReplaceList.emplace_back(
    wxT("-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longrightarrow}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|<\\|-"), wxT("\\\\ensuremath{\\\\longleftarrow}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|<\\|="), wxT("\\\\ensuremath{\\\\leq}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|>\\|="), wxT("\\\\ensuremath{\\\\geq}"));
  regexReplaceList.emplace_back(
    wxT("\\+/-"), wxT("\\\\ensuremath{\\\\pm}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|>\\|\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\gg}"));
  regexReplaceList.emplace_back(
    wxT("\\\\verb\\|<\\|\\\\verb\\|<\\|"), wxT("\\\\ensuremath{\\\\ll}"));
}

MarkDownHTML::MarkDownHTML(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.emplace_back(
    wxT("\\&lt);=\\&gt;"), wxT("\u21d4"));
  regexReplaceList.emplace_back(
    wxT("=\\&gt);"), wxT("\u21d2"));
  regexReplaceList.emplace_back(
    wxT("&lt);-\\&gt;"), wxT("\u2194"));
  regexReplaceList.emplace_back(
    wxT("-\\&gt);"), wxT("\u2192"));
  regexReplaceList.emplace_back(
    wxT("\\&lt);-"), wxT("\u2190"));
  regexReplaceList.emplace_back(
    wxT("\\&lt);="), wxT("\u2264"));
  regexReplaceList.emplace_back(
    wxT("\\&gt);="), wxT("\u2265"));
  regexReplaceList.emplace_back(
    wxT("\\+/-"), wxT("\u00B1"));
  regexReplaceList.emplace_back(
    wxT("\u00A0"), wxT("\u00A0"));
}
