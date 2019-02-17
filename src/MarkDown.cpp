﻿// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  This file contains the code for MarkDownParser.

  MarkDownParser is the class that handles the markdown syntax
*/

#include "MarkDown.h"

MarkDownParser::~MarkDownParser()
{
  while (!regexReplaceList.empty())
  {
    wxDELETE(regexReplaceList.front());
    regexReplaceList.pop_front();
  }
}

MarkDownParser::MarkDownParser(Configuration *cfg)
{
  m_configuration = cfg;
}

wxString MarkDownParser::MarkDown(wxString str)
{
  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  for (replaceList::iterator it = regexReplaceList.begin();
       it != regexReplaceList.end();
       ++it)
    (*it)->DoReplace(&str);

  // The result of this action
  wxString result = wxEmptyString;

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

    wxString str = line;
    str = str.Trim(false);
    size_t index = line.Length() - str.Length();

    // Determine the amount of indentation and the contents of the rest
    // of the line.

    // Trailing whitespace doesn't help much.
    line = line.Trim();

    // Does the line contain anything other than spaces?
    if (str != wxEmptyString)
    {
      // The line contains actual text..

      // If the line begins with a star followed by a space it is part
      // of a bullet list
      if (str.Left(2) == wxT("* "))
      {
        // We are part of a bullet list.

        // Remove the bullet list start marker from our string.
        str = str.Right(str.Length() - 2);
        str = str.Trim(false);

        // Let's see if this is the first item in the list
        if (indentationLevels.empty())
        {
          // This is the first item => Start the itemization.
          result += itemizeBegin() + itemizeItem();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('*'));
        }
        else
        {
          // We are inside a bullet list.

          // Are we on a new indentation level?
          if (indentationLevels.back() < index)
          {
            // A new identation level => add the itemization-start-command.
            result += itemizeEndItem() + itemizeBegin();
            indentationLevels.push_back(index);
            indentationTypes.push_back(wxT('*'));
          }

          // End lists if we are at a old indentation level.
          while (!indentationLevels.empty() && (indentationLevels.back() > index))
          {
            if (indentationTypes.back() == wxT('*'))
              result += itemizeEnd();
            else
              result += quoteEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }

          // Add a new item marker.
          result += itemizeEndItem() + itemizeItem();
        }
        result += str += wxT(" ");
      }
      else if (str.StartsWith(quoteChar() + wxT(" ")))
      {
        // We are part of a quotation.
        //
        // Remove the bullet list start marker from our string.
        str = str.Right(str.Length() - quoteChar().Length() - 1);
        str = str.Trim(false);

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
          while (!indentationLevels.empty() && (indentationLevels.back() > index))
          {
            if (indentationTypes.back() == wxT('*'))
              result += itemizeEnd();
            else
              result += quoteEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }
        }
        result += str += wxT(" ");
      }
      else
      {
        // Ordinary text.
        //
        // If we are at a old indentation level we need to end some lists
        // and add a new item if we still are inside a list.
        if (!indentationLevels.empty())
        {
          if (indentationLevels.back() > index)
          {
            if (!m_configuration->GetAutoWrap())
              result += itemizeEndItem();
            while ((!indentationLevels.empty()) &&
                   (indentationLevels.back() > index))
            {
              if (indentationTypes.back() == wxT('*'))
                result += itemizeEnd();
              else
                result += quoteEnd();
              indentationLevels.pop_back();
              indentationTypes.pop_back();
            }
            if (!indentationLevels.empty()) result += itemizeItem();
          }
          line = line.Right(line.Length() - index);
        }

        // Add the text to the output.        
//        if (!m_configuration->GetAutoWrap())
//          result += line + "\n";
//        else
          result += line + NewLine();

      }
    }
    else
    {
      if (lines.HasMoreTokens()) result += NewLine();
    }
  }

  // Close all item lists
  while (!indentationLevels.empty())
  {
    if (indentationTypes.back() == wxT('*'))
      result += itemizeEnd();
    else
      result += quoteEnd();
    indentationLevels.pop_back();
    indentationTypes.pop_back();
  }
  return result;
}

MarkDownTeX::MarkDownTeX(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.push_back(
          new RegexReplacer(wxT("#"), wxT("\\\\#")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|<\\|=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longleftrightarrow}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longrightarrow}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|<\\|-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longleftrightarrow}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longrightarrow}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|<\\|-"), wxT("\\\\ensuremath{\\\\longleftarrow}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|<\\|="), wxT("\\\\ensuremath{\\\\leq}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|>\\|="), wxT("\\\\ensuremath{\\\\geq}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\+/-"), wxT("\\\\ensuremath{\\\\pm}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|>\\|\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\gg}")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\\\verb\\|<\\|\\\\verb\\|<\\|"), wxT("\\\\ensuremath{\\\\ll}")));
  regexReplaceList.push_back(
          new RegexReplacer(L"\xDCB6", wxT("~")));
}

MarkDownHTML::MarkDownHTML(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\&lt;=\\&gt;"), wxT("\\&hArr;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("=\\&gt;"), wxT("\\&rArr;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("&lt;-\\&gt;"), wxT("\\&harr;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("-\\&gt;"), wxT("\\&rarr;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\&lt;-"), wxT("\\&larr;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\&lt;="), wxT("\\&le;")));
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\&gt;="), wxT("\\&ge;")));;
  regexReplaceList.push_back(
          new RegexReplacer(wxT("\\+/-"), wxT("\\&plusmn;")));;
  regexReplaceList.push_back(
          new RegexReplacer(L"\xDCB6", wxT("&nbsp;")));
}
