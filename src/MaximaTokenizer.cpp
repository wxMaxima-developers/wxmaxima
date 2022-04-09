// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ExptCell

  ExptCell is the Cell type that represents exponents.
 */

#include "MaximaTokenizer.h"
#include <wx/wx.h>
#include <wx/string.h>
#include <vector>


MaximaTokenizer::MaximaTokenizer(wxString commands, Configuration *configuration):
  m_configuration(configuration)
{
  if(m_hardcodedFunctions.empty())
  {
    m_hardcodedFunctions["for"] = 1;
    m_hardcodedFunctions["in"] = 1;
    m_hardcodedFunctions["then"] = 1;
    m_hardcodedFunctions["while"] = 1;
    m_hardcodedFunctions["do"] = 1;
    m_hardcodedFunctions["thru"] = 1;
    m_hardcodedFunctions["next"] = 1;
    m_hardcodedFunctions["step"] = 1;
    m_hardcodedFunctions["unless"] = 1;
    m_hardcodedFunctions["from"] = 1;
    m_hardcodedFunctions["if"] = 1;
    m_hardcodedFunctions["else"] = 1;
    m_hardcodedFunctions["elseif"] = 1;
    m_hardcodedFunctions["and"] = 1;
    m_hardcodedFunctions["or"] = 1;
    m_hardcodedFunctions["not"] = 1;
    m_hardcodedFunctions["true"] = 1;
    m_hardcodedFunctions["false"] = 1;
  }
  
  // ----------------------------------------------------------------
  // --------------------- Step one:                -----------------
  // --------------------- Break a line into tokens -----------------
  // ----------------------------------------------------------------
  wxString::const_iterator it = commands.begin();
      
  if(configuration->InLispMode())
  {
    wxString token;
    while(
      (it < commands.end()) &&
      ((!token.EndsWith("(to-maxima)"))) &&
      ((!token.EndsWith(wxString("(to")+wxT("\u2212")+"maxima)"))))
    {
      token +=*it;
      ++it;
    }
    token.Trim(true);
    if(!token.IsEmpty())
      m_tokens.emplace_back(token, TS_CODE_LISP);
  }
  while (it < commands.end())
  {
    // Determine the current char and the one that will follow it
    wxChar Ch = *it;
    wxString::const_iterator it2(it);
    if(it2 < commands.end())
      ++it2;
    wxChar nextChar;

    if(it2 < commands.end())
      nextChar = *it2;
    else
      nextChar = wxT(' ');

    // Handle newline characters (hard+soft line break)
    if (m_linebreaks.Contains(Ch))
    {
      m_tokens.emplace_back(wxChar(Ch));
      ++it;
      continue;
    }
    // Check for comments
    if ((Ch == '/') && ((nextChar == wxT('*')) || (nextChar == wxT('\u00B7'))))
    {
      wxString token;
      // Add the comment start
      token+=*it;++it;
      token+=*it;++it;

      int commentDepth = 0;
      while (it < commands.end())
      {
        // Handle escaped chars
        if(*it == '\\')
        {
          token += *it;
          ++it;
          if(it < commands.end())
          {
            token += *it;
            ++it;
          }
          continue;
        }
        
        wxString::const_iterator it3(it);
        if(it3 < commands.end())
          ++it3;
        wxChar nextCh = ' ';
        if(it3 < commands.end())
          nextCh = *it3;

        // handle comment begins within comments.
        if((*it == '/') && ((nextCh == '*') || (nextCh == wxT('\u00B7'))))
        {
          commentDepth++;
          token += *it;
          ++it;
          if(it < commands.end())
          {
            token += *it;
            ++it;
          }
          continue;
        }
        // handle comment endings
        if(((*it == '*') || (*it == wxT('\u00B7'))) && (nextCh == '/'))
        {
          commentDepth--;
          token += *it;
          ++it;
          if(it < commands.end())
          {
            token += *it;
            ++it;
          }
          if(commentDepth < 0)
            break;
          continue;
        }
        if(it < commands.end())
        {
          token += *it;
          ++it;
        }
      }
      m_tokens.emplace_back(token, TS_CODE_COMMENT);
      continue;
    }
    // Handle operators and :lisp commands
    if (Operators().Find(Ch) != wxNOT_FOUND)
    {
      if(Ch == ':')
      {
        wxString breakCommand;
        wxString::const_iterator it3(it);
        int len = 14;
        while((len>0) && (it3 < commands.end()))
        {
          len--;
          breakCommand += wxString(*it3);
          ++it3;
        }
        if(
          breakCommand.StartsWith(":lisp ") ||
          breakCommand.StartsWith(":lisp-quiet ") ||
          breakCommand.StartsWith(":lisp\t") ||
          breakCommand.StartsWith(":lisp-quiet\t"))
        {
          wxString token;
          while((it < commands.end()) && (*it != '\n'))
          {
            token += wxString(*it);
            ++it;
          }
          m_tokens.emplace_back(token, TS_CODE_LISP);
        }
          else
          {
            m_tokens.emplace_back(wxString(Ch), TS_CODE_OPERATOR);
            ++it;
          }
      }
      else
      {
        wxString token = wxString(Ch);
        if (configuration->GetChangeAsterisk())
        {
          token.Replace(wxT("*"), wxT("\u00B7"));
          token.Replace(wxT("-"), wxT("\u2212"));
        }
        
        m_tokens.emplace_back(token, TS_CODE_OPERATOR);
        ++it;
      }
      continue;
    }
    // Handle strings
    if (Ch == wxT('\"'))
    {
      wxString token;
      // Add the opening quote
      token = Ch;
      ++it;

      // Add the string contents
      while (it < commands.end())
      {
        Ch = *it;
        token += Ch;
        ++it;
        if(Ch == wxT('\\'))
        {
          if(it < commands.end())
          {
            token += *it;
            ++it;
          }
        }
        else if(Ch == wxT('\"'))
          break;
      }
      m_tokens.emplace_back(token, TS_CODE_STRING);
      continue;
    }
    // Handle number-like symbols
    if(UnicodeNumbers().Find(Ch) != wxNOT_FOUND)
    {
       wxString token = Ch;
       ++it;
       m_tokens.emplace_back(token, TS_CODE_NUMBER);
       continue;
    } 
    // Handle numbers. Numbers begin with a digit, but can continue with letters and can
    // contain a + or - that follows an e, f, g, h or l.
    if (IsNum(Ch))
    {
      wxString token;
      wxChar lastChar = *it;
      while ((it < commands.end()) &&
             (
               (IsNum(*it) ||
                ((*it >= 'a') && (*it <= 'z')) ||
                ((*it >= 'A') && (*it <= 'Z'))
                 )
               || (
                 (
                   (lastChar == 'e') || (lastChar == 'E') ||
                   (lastChar == 'f') || (lastChar == 'F') ||
                   (lastChar == 'g') || (lastChar == 'G') ||
                   (lastChar == 'h') || (lastChar == 'H') ||
                   (lastChar == 'l') || (lastChar == 'L')
                   ) && (
                     (m_plusSigns.Contains(*it)) ||
                     (m_minusSigns.Contains(*it))
                     )
                 )))
      {
        wxChar ch = *it;
        for (wxString::const_iterator it3 = m_plusSigns.begin(); it3 != m_plusSigns.end(); ++it3)
          if(ch == *it3)
            ch = '+';
        for (wxString::const_iterator it3 = m_minusSigns.begin(); it3 != m_minusSigns.end(); ++it3)
          if(ch == *it3)
            ch = '-';
        token += ch;
        lastChar = *it;
        ++it;
      }
      
      m_tokens.emplace_back(token, TS_CODE_NUMBER);
      continue;
    }
    if (m_plusSigns.Contains(Ch))
    {
      wxString token = "+";
      m_tokens.emplace_back(token);
      continue;
    }
    if (m_minusSigns.Contains(Ch))
    {
      wxString token = "-";
      m_tokens.emplace_back(token);
      continue;
    }
    // Merge consecutive spaces into one single token
    if (IsSpace(Ch))
    {
      wxString token;
      while ((it < commands.end()) && IsSpace(Ch))
      {
	if(Ch == '\t')
          token += "\t";
        else
          token += " ";
        if (++it < commands.end())
          Ch = *it;
      }
      m_tokens.emplace_back(token);
      continue;
    }
    // Handle keywords
    if (IsAlpha(Ch) || (Ch == '\\') || (Ch == '?'))
    {
      wxString token;
      if(Ch == '?')
      {
        token += Ch;
        ++it;
        Ch = *it;
      }

      while ((it < commands.end()) && (IsAlphaNum(*it) || (*it == '\\')))
      {
        Ch = *it;
        token += Ch;
        if (Ch == wxT('\\'))
        {
          ++it;
          if (it < commands.end())
          {
            Ch = *it;
            if (Ch != wxT('\n'))
              token += Ch;
            else
            {
              m_tokens.emplace_back(token);
              token = wxEmptyString;

              break;
            }
          }
        }
        if(it < commands.end())
          ++it;
      }
      if(token == ("to_lisp"))
      {
        while((it < commands.end()) && ((!token.EndsWith("(to-maxima)"))) && ((!token.EndsWith(wxString("(to")+wxT("\u2212")+"maxima)"))))
        {
          token += wxString(*it);
          ++it;
        }
        m_tokens.emplace_back(token, TS_CODE_LISP);
      }
      else
      {
        if(m_hardcodedFunctions.find(token) != m_hardcodedFunctions.end())
          m_tokens.emplace_back(token, TS_CODE_FUNCTION);
        else if(m_configuration->m_maximaOperators.find(token) != m_configuration->m_maximaOperators.end())
          m_tokens.emplace_back(token, TS_CODE_OPERATOR);
        else
        {
          // Let's look what the next char looks like
          wxString::const_iterator it3(it);
          while ((it3 < commands.end()) &&
                 ((*it3 == ' ') || (*it3 == '\t') || (*it3 == '\n') || (*it3 == '\r')))
            ++it3;
          if(it3 >= commands.end())
            m_tokens.emplace_back(token, TS_CODE_VARIABLE);
          else
          {
            if(*it3 == '(')
              m_tokens.emplace_back(token, TS_CODE_FUNCTION);
            else
              m_tokens.emplace_back(token, TS_CODE_VARIABLE);
          }
        }
      }
      continue;
    }   
    if((Ch == '$') || (Ch == ';'))
    {
      m_tokens.emplace_back(wxString(Ch), TS_CODE_ENDOFLINE);
      ++it;
      continue;
    }

    {
      // Everything that hasn't been handled until now.
      m_tokens.emplace_back(wxString(Ch));
      ++it;
      continue;
    }
  }
}

MaximaTokenizer::MaximaTokenizer(wxString commands,
                                 Configuration *configuration,
                                 const TokenList &initialTokens)
  : MaximaTokenizer(commands, configuration)
{
  // cppcheck-suppress useInitializationList
  m_tokens = initialTokens;
}

bool MaximaTokenizer::IsAlpha(wxChar ch)
{
  if (wxIsalpha(ch))
    return true;

  if(m_not_alphas.Find(ch) != wxNOT_FOUND)
    return false;
  
  if(IsSpace(ch))
    return false;

  // If it cannot be converted to asciiand we didn't detect it as a char we know how to deal with
  // it (in maxima's view) is an ordinary letter.
  if(ch > 127)
    return true;
  
  return (m_additional_alphas.Find(ch) != wxNOT_FOUND);
}

bool MaximaTokenizer::IsSpace(wxChar ch)
{
  return m_spaces.Find(ch) != wxNOT_FOUND;
}

bool MaximaTokenizer::IsNum(wxChar ch)
{
  return ch >= '0' && ch <= '9';
}

bool MaximaTokenizer::IsAlphaNum(wxChar ch)
{
  return IsAlpha(ch) || IsNum(ch);
}

const wxString MaximaTokenizer::m_additional_alphas = wxT("\\_%µ");
const wxString MaximaTokenizer::m_not_alphas = wxT("\u00B7\u2212\u2260\u2264\u2265\u2265\u2212\u00B2\u00B3\u00BD\u221E\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u2264\u2265\u2212")
  wxT("\uFE62")
  wxT("\uFF0B")
  wxT("\uFB29")
  wxT("\u2795")
  wxT("\u2064")
  wxT("\u2796")
  wxT("\uFE63")
  wxT("\uFF0D");
const wxString MaximaTokenizer::m_spaces = wxT(" ")
  wxT("\u00A0") // A non-breakable space
  wxT("\xDCB6") // A non-breakable space (alternate version)
  wxT("\u1680") // Ogham space mark
  wxT("\u2000") // en quad
  wxT("\u2001") // em quad
  wxT("\u2002") // en space
  wxT("\u2003") // em space
  wxT("\u2004") // 1/3 em space
  wxT("\u2005") // 1/4 em space
  wxT("\u2006") // 1/6 em space
  wxT("\u2007") // figure space
  wxT("\u2008") // punctuation space
  wxT("\t")
  wxT("\r"); // A soft linebreak

const wxString MaximaTokenizer::m_linebreaks =
  wxT("\n")
  wxT("\u2028")
  wxT("\u2029");

const wxString MaximaTokenizer::m_plusSigns =
  "+"
  wxT("\uFE62")
  wxT("\uFF0B")
  wxT("\uFB29")
  wxT("\u2795")
  wxT("\u2064");

const wxString MaximaTokenizer::m_minusSigns =
  "-"
  wxT("\u2796")
  wxT("\uFE63")
  wxT("\uFF0D");

const wxString MaximaTokenizer::m_unicodeNumbers =
  wxT("\u00BD\u00B2\u00B3\u221E");

const wxString MaximaTokenizer::m_operators =
  wxT("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}");

MaximaTokenizer::StringHash MaximaTokenizer::m_hardcodedFunctions;
