// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015-2017  Gunter Königsmann <wxMaxima@physikbuch.de>
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

    // We will add our own newline characters when needed.
    line.Replace(NewLine(), wxT(" "));

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
            if (NewLineBreaksLine() && !m_configuration->GetAutoWrap())

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
        if (!m_configuration->GetAutoWrap())
          result += line + "\n";
        else
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
#if wxUSE_UNICODE
  /*
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B1",wxT("\\\\ensuremath{\\\\alpha}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x00B2",wxT("\\\\ensuremath{^2}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x00B3",wxT("\\\\ensuremath{^3}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x221A",wxT("\\\\ensuremath{\\\\sqrt{}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2148",wxT("\\\\ensuremath{\\\\mathbbm{i}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2147",wxT("\\\\ensuremath{\\\\mathbbm{e}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x210f",wxT("\\\\ensuremath{\\\\hbar}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2208",wxT("\\\\ensuremath{\\\\in}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x21D2",wxT("\\\\ensuremath{\\\\Longrightarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x221e",wxT("\\\\ensuremath{\\\\infty}")));
  // TODO: Are there LaTeX symbols for <ESC>TB<ESC> and <ESC>tb<ESC>?
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22C0",wxT("\\\\ensuremath{\\\\wedge}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22C1",wxT("\\\\ensuremath{\\\\vee}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22bb",wxT("\\\\ensuremath{\\\\oplus}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22BC",wxT("\\\\ensuremath{\\\\overline{\\\\wedge}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22BB",wxT("\\\\ensuremath{\\\\overline{\\\\vee}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x00AC",wxT("\\\\ensuremath{\\\\setminus}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22C3",wxT("\\\\ensuremath{\\\\cup}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x22C2",wxT("\\\\ensuremath{\\\\cap}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2286",wxT("\\\\ensuremath{\\\\subseteq}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2282",wxT("\\\\ensuremath{\\\\subset}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2288",wxT("\\\\ensuremath{\\\\not\\\\subseteq}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0127",wxT("\\\\ensuremath{\\\\hbar}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0126",wxT("\\\\ensuremath{\\\\Hbar}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2205",wxT("\\\\ensuremath{\\\\emptyset}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x00BD",wxT("\\\\ensuremath{\\\\frac{1}{2}}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B2",wxT("\\\\ensuremath{\\\\beta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B3",wxT("\\\\ensuremath{\\\\gamma}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B4",wxT("\\\\ensuremath{\\\\delta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B5",wxT("\\\\ensuremath{\\\\epsilon}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B6",wxT("\\\\ensuremath{\\\\zeta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B7",wxT("\\\\ensuremath{\\\\eta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B8",wxT("\\\\ensuremath{\\\\theta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03B9",wxT("\\\\ensuremath{\\\\iota}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BA",wxT("\\\\ensuremath{\\\\kappa}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BB",wxT("\\\\ensuremath{\\\\lambda}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BC",wxT("\\\\ensuremath{\\\\mu}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BD",wxT("\\\\ensuremath{\\\\nu}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BE",wxT("\\\\ensuremath{\\\\xi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03BF",wxT("\\\\ensuremath{\\\\omicron}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C0",wxT("\\\\ensuremath{\\\\pi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C1",wxT("\\\\ensuremath{\\\\rho}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C3",wxT("\\\\ensuremath{\\\\sigma}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C4",wxT("\\\\ensuremath{\\\\tau}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C5",wxT("\\\\ensuremath{\\\\upsilon}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C6",wxT("\\\\ensuremath{\\\\phi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C7",wxT("\\\\ensuremath{\\\\chi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C8",wxT("\\\\ensuremath{\\\\psi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03C9",wxT("\\\\ensuremath{\\\\omega}")));

    regexReplaceList.push_back(
    new RegexReplacer(L"\x0391",wxT("\\\\ensuremath{\\\\Alpha}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0392",wxT("\\\\ensuremath{\\\\Beta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0393",wxT("\\\\ensuremath{\\\\Gamma}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0394",wxT("\\\\ensuremath{\\\\Delta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0395",wxT("\\\\ensuremath{\\\\Epsilon}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0396",wxT("\\\\ensuremath{\\\\Zeta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0397",wxT("\\\\ensuremath{\\\\Eta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0398",wxT("\\\\ensuremath{\\\\Theta}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x0399",wxT("\\\\ensuremath{\\\\Iota}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039A",wxT("\\\\ensuremath{\\\\Kappa}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039B",wxT("\\\\ensuremath{\\\\Lambda}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039C",wxT("\\\\ensuremath{\\\\Mu}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039D",wxT("\\\\ensuremath{\\\\Nu}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039E",wxT("\\\\ensuremath{\\\\Xi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x039F",wxT("\\\\ensuremath{\\\\Omicron}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A0",wxT("\\\\ensuremath{\\\\Pi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A1",wxT("\\\\ensuremath{\\\\Rho}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A3",wxT("\\\\ensuremath{\\\\Sigma}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A4",wxT("\\\\ensuremath{\\\\Tau}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A5",wxT("\\\\ensuremath{\\\\Upsilon}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A6",wxT("\\\\ensuremath{\\\\Phi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A7",wxT("\\\\ensuremath{\\\\Chi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A8",wxT("\\\\ensuremath{\\\\Psi}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x03A9",wxT("\\\\ensuremath{\\\\Omega}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2202",wxT("\\\\ensuremath{\\\\partial}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x222b",wxT("\\\\ensuremath{\\\\int}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2245",wxT("\\\\ensuremath{\\\\approx}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x221d",wxT("\\\\ensuremath{\\\\propto}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x2260",wxT("\\\\ensuremath{\\\\neq}")));
  regexReplaceList.push_back(
    new RegexReplacer(L"\x220e",wxT("\\\\ensuremath{\\\\blacksquare}")));
  regexReplaceList.push_back(
  new RegexReplacer(L"\x2263",wxT("\\\\ensuremath{\\\\equiv}")));*/
//  regexReplaceList.push_back(
//    new RegexReplacer(wxT("~"),wxT("\\\\ensuremath{\\\\sim }")));
  regexReplaceList.push_back(
          new RegexReplacer(L"\xDCB6", wxT("~")));
#endif
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
