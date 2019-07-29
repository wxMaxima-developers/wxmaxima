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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class ExptCell

  ExptCell is the Cell type that represents exponents.
 */

#include "MaximaTokenizer.h"
#include <wx/wx.h>
#include <wx/string.h>

MaximaTokenizer::MaximaTokenizer(wxString commands)
{
  // ----------------------------------------------------------------
  // --------------------- Step one:                -----------------
  // --------------------- Break a line into tokens -----------------
  // ----------------------------------------------------------------
  wxString::const_iterator it = commands.begin();

  TextStyle style = TS_DEFAULT;
  
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
    if ((Ch == wxT('\n')) || (Ch == wxT('\r')))
    {
      m_tokens.push_back(new Token(wxChar(Ch)));
      ++it;
    }
    // Handle operators and :lisp commands
    else if (Operators().Find(Ch) != wxNOT_FOUND)
    {
      if(Ch == ':')
      {
        wxString breakCommand;
        wxString::const_iterator it2(it);
        int len = 14;
        while((len>0) && (it2 < commands.end()))
        {
          len--;
          breakCommand += wxString(*it2);
          it2++;
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
          m_tokens.push_back(new Token(token, TS_CODE_LISP));
        }
          else
          {
            m_tokens.push_back(new Token(wxString(Ch), TS_CODE_OPERATOR));
            ++it;
          }
      }
      else
      {
        m_tokens.push_back(new Token(wxString(Ch), TS_CODE_OPERATOR));
        ++it;
      }
    }
    // Check for comments
    else if ((Ch == '/') && ((nextChar == wxT('*')) || (nextChar == wxT('\xB7'))))
    {
      wxString token;
      // Add the comment start
      token+=*it;++it;
      token+=*it;++it;

      int commentDepth = 0;
      wxChar lastCh = ' ';
      while (it < commands.end())
      {

        // Handle escaped chars
        if(*it == '\\')
        {
          token += *it;
          it++;
          if(it < commands.end())
          {
            token += *it;
            it++;
          }
        }

        // handle comment begins within comments.
        if((lastCh == '/') && (*it == '*'))
          commentDepth++;
        // handle comment endings
        if((lastCh == '*') && (*it == '/'))
        {
          commentDepth--;
          if(commentDepth < 1)
          {
            token += *it;
            break;
          }
        }
        if(it < commands.end())
        {
          token += *it;
          it++;
        }
      }      
      m_tokens.push_back(new Token(token, TS_CODE_COMMENT));
    }
    // Handle strings
    else if (Ch == wxT('\"'))
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
      m_tokens.push_back(new Token(token, TS_CODE_STRING));
      token = wxEmptyString;
    }
    // Handle numbers. Numbers begin with a digit, but can continue with letters and can
    // contain a + or - that follows an e, f, g, h or l.
    else if (IsNum(Ch))
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
                     (*it == '+') ||
                     (*it == '-') ||
                     (*it == wxT('\x2212'))
                     )
                 )))
      {
        token += wxString(*it);
        lastChar = *it;
        ++it;
      }
      
      m_tokens.push_back(new Token(token, TS_CODE_NUMBER));
      token = wxEmptyString;
    }
    // Merge consecutive spaces into one single token
    else if ((Ch == wxT(' ')) || (Ch == wxT('\t')))
    {
      wxString token;
      while ((it < commands.end()) &&
             ((Ch == wxT(' ') || (Ch == wxT('\t')))
               ))
      {
        token += wxString(Ch);
        if (++it < commands.end()) {
          Ch = *it;
        }
      }

      m_tokens.push_back(new Token(token));
      token = wxEmptyString;
    }
    // Handle keywords
    else if (IsAlpha(Ch) || (Ch == '\\') || (Ch == '?'))
    {
      wxString token;
      if(Ch == '?')
      {
        token += Ch;
        it++;
        Ch = *it;
      }

      while ((it < commands.end()) && (IsAlphaNum(Ch = *it)))
      {
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
              m_tokens.push_back(new Token(token));
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
        ++it;
        while((it < commands.end()) && (!token.EndsWith("(to-maxima")))
        {
          token += wxString(*it);
          ++it;
        }
        m_tokens.push_back(new Token(token, TS_CODE_LISP));
      }
      else
      {
        if (token == wxT("for") ||
            token == wxT("in") ||
            token == wxT("then") ||
            token == wxT("while") ||
            token == wxT("do") ||
            token == wxT("thru") ||
            token == wxT("next") ||
            token == wxT("step") ||
            token == wxT("unless") ||
            token == wxT("from") ||
            token == wxT("if") ||
            token == wxT("else") ||
            token == wxT("elif") ||
            token == wxT("and") ||
            token == wxT("or") ||
            token == wxT("not") ||
            token == wxT("not") ||
            token == wxT("true") ||
            token == wxT("false"))
          m_tokens.push_back(new Token(token, TS_CODE_FUNCTION));
        else
        {
          // Let's look what the next char looks like
          wxString::const_iterator it2(it);
          while ((it2 < commands.end()) &&
                 ((*it2 == ' ') || (*it2 == '\t') || (*it2 == '\n') || (*it2 == '\r')))
            ++it2;
          if(it2 >= commands.end())
            m_tokens.push_back(new Token(token, TS_CODE_VARIABLE));
          else
          {
            if(*it2 == '(')
              m_tokens.push_back(new Token(token, TS_CODE_FUNCTION));
            else
              m_tokens.push_back(new Token(token, TS_CODE_VARIABLE));
          }
        }
      }
      token = wxEmptyString;
    }   
    else if((Ch == '$') || (Ch == ';'))
    {
      m_tokens.push_back(new Token(wxString(Ch), TS_CODE_ENDOFLINE));
      ++it;
    }
    else
    {
      m_tokens.push_back(new Token(wxString(Ch)));
      ++it;
    }
  }
}

bool MaximaTokenizer::IsAlpha(wxChar ch)
{
  static const wxString additional_alphas = wxT("\\_%");

  if (wxIsalpha(ch))
    return true;

  return (additional_alphas.Find(ch) != wxNOT_FOUND);
}

bool MaximaTokenizer::IsNum(wxChar ch)
{
  return ch >= '0' && ch <= '9';
}

bool MaximaTokenizer::IsAlphaNum(wxChar ch)
{
  return IsAlpha(ch) || IsNum(ch);
}
