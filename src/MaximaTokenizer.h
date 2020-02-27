// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef MAXIMATOKENIZER_H
#define MAXIMATOKENIZER_H

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/arrstr.h>
#include "TextStyle.h"
#include "Configuration.h"
#include <list>
#include <memory>

/*!\file

  This file declares the class Maximatokenizer.

  Maximatokenizer breaks down maxima input to individual commands.
 */

/*! Maximatokenizer breaks down maxima input to individual commands.
 */

class MaximaTokenizer
{
public:
  MaximaTokenizer(wxString commands, Configuration *configuration);

  class Token
  {
  public:
    Token(){m_style = TS_DEFAULT;}
    explicit Token(wxString text) : m_text(text){m_style = TS_DEFAULT;}
    Token(wxString text, TextStyle style) :
      m_text(text),
      m_style(style)
      {}
    Token& operator=(const Token& t){m_text = t.m_text;m_style = t.m_style; return *this;}
    Token(const Token &token){*this = token;}
    TextStyle GetStyle() const {return m_style;}
    wxString GetText() const {return m_text;}
    operator wxString() const {return GetText();}
  private:
    wxString m_text;
    TextStyle m_style;
  };
  typedef std::list<std::shared_ptr<Token>> TokenList;
  static bool IsAlpha(wxChar ch);
  static bool IsNum(wxChar ch);
  static bool IsAlphaNum(wxChar ch);
  static bool IsSpace(wxChar ch);
  static const wxString UnicodeNumbers()
    {
      return wxString(
        wxT("\u00BD\u00B2\u00B3\u221E")
        );
    }
  static const wxString Operators(){return wxString(
      wxT("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}"
        )
      );}

  TokenList GetTokens(){return m_tokens;}

  
protected:
  //! The tokens the string is divided into
  TokenList m_tokens;
  //! ASCII symbols that wxIsalnum() doesn't see as chars, but maxima does.
  static const wxString m_additional_alphas;
  //! Unicode Operators and other special non-ascii characters
  static const wxString m_not_alphas;
  //! Space characters
  static const wxString m_spaces;
  //! Plus sign
  static const wxString m_plusSigns;
  //! Minus sign
  static const wxString m_minusSigns;
  //! Linebreak characters
  static const wxString m_linebreaks;
};

#endif // MAXIMATOKENIZER_H
