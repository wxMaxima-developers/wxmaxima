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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef MAXIMATOKENIZER_H
#define MAXIMATOKENIZER_H

#include <utility>
#include <vector>
#include <memory>
#include <wx/wx.h>
#include <wx/string.h>
#include <wx/arrstr.h>
#include "precomp.h"
#include "cells/TextStyle.h"
#include "Configuration.h"
#include <unordered_map>

/*!\file

  This file declares the class Maximatokenizer.

  Maximatokenizer breaks down maxima input to individual commands.
*/

/*! Maximatokenizer breaks down maxima input to individual commands.

  Effectively this is a maxima parser minus the fact that maxima is
  extensible and we cannot know in what directions the user or a
  loaded package has extended it.
*/
class MaximaTokenizer
{
public:
  /*! The constructor

    \param commands The maxima commands to tokenize
    \param configuration A pointer to the configuration object
  */
  MaximaTokenizer(const wxString &commands, const Configuration * const configuration);

  //! A maxima code snippet from this tokenizer
  class Token
  {
  public:
    Token() = default;
    explicit Token(wxString&& text) : m_text(std::move(text)) {}
    explicit Token(const wxString &text) : m_text(text) {}
    Token(wxString&& text, TextStyle style) : m_text(std::move(text)), m_style(style) {}
    Token(const wxString& text, TextStyle style) : m_text(text), m_style(style) {}
    Token& operator=(Token&& t) noexcept { m_text = std::move(t.m_text); m_style = t.m_style; return *this; }
    Token& operator=(const Token& t) { m_text = t.m_text; m_style = t.m_style; return *this; }
    Token(Token&& token) noexcept { *this = std::move(token); }
    Token(const Token &token) { *this = token ;}
    TextStyle GetTextStyle() const { return m_style; }
    const wxString &GetText() const { return m_text; }
    operator const wxString &() const { return m_text; }
  private:
    wxString m_text;
    TextStyle m_style = TS_CODE_DEFAULT;
  };
  static bool IsAlpha(wxUniChar ch);
  static bool IsNum(wxUniChar ch);
  static bool IsAlphaNum(wxUniChar ch);
  static bool IsSpace(wxUniChar ch);
  static const wxString UnicodeNumbers() {
    return wxS("\u00BD\u00B2\u00B3\u221E"); // VULGAR FRACTION ONE HALF, SUPERSCRIPT TWO, SUPERSCRIPT THREE, INFINITY
  }
  static const wxString Operators() {
    return wxS("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b\u2264\u2265\u2211"
      "\u2260+-*/^:=#'!()[]{}");
  }

  using TokenList = std::vector<Token>;
  TokenList PopTokens() && { return std::move(m_tokens); }

  //! A constructor that adds additional words to the token list
  MaximaTokenizer(const wxString &commands, const Configuration * const configuration,
                  const TokenList &initialTokens);

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

  const Configuration * const m_configuration = NULL;

  typedef std::unordered_map <wxString, int, wxStringHash> StringHash;
  /*! Names of functions that don't require parenthesis

    The maxima parser automatically parses everything that is followed by
    an opening parenthesis as a function. But a few things like "then"
    are very similar to functions except that they don't require an
    argument. These fake functions are kept in this hash.
  */
  static StringHash m_hardcodedFunctions;

};

#endif // MAXIMATOKENIZER_H
