// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "WizardHelpKeywords.h"
#include <wx/wxcrt.h>
#include <algorithm>

namespace {
bool IsWordStart(wxUniChar ch) {
  return wxIsalpha(ch) || (ch == '_') || (ch == '%');
}
bool IsWordChar(wxUniChar ch) {
  return wxIsalnum(ch) || (ch == '_') || (ch == '%');
}
void AddOnce(std::vector<wxString> &keywords, const wxString &word) {
  if (std::find(keywords.begin(), keywords.end(), word) == keywords.end())
    keywords.push_back(word);
}
} // namespace

std::vector<wxString> wxm::WizardHelpKeywords(const wxString &commandRule) {
  std::vector<wxString> keywords;

  // Drop the placeholders first: their digits would otherwise end up as
  // parts of words ("a#1#" -> "a1"), and their "#" as Maxima's "#" operator.
  wxString rule = commandRule;
  for (int i = 1; i <= 10; i++)
    rule.Replace(wxString::Format(wxS("#%i#"), i), wxS(" "));

  // A word is what Maxima would read as an identifier: it may contain
  // digits, it just can't start with one ("ode2", "ic1", but not "2").
  for (auto it = rule.begin(); it != rule.end();) {
    if (!IsWordStart(*it)) {
      ++it;
      continue;
    }
    wxString word;
    while ((it != rule.end()) && IsWordChar(*it))
      word += *it++;
    AddOnce(keywords, word);
  }

  // The terminator of the last statement is part of every command.
  wxString body = std::move(rule);
  body.Trim();
  if (body.EndsWith(wxS(";")) || body.EndsWith(wxS("$")))
    body.RemoveLast();

  // "*", "^" and "[" are in nearly every formula, so they are offered only
  // for a command that is nothing but operators, as the matrix wizards'
  // "#1#*#2#" and "#1#[#2#]" are.
  const bool onlyOperators = keywords.empty();

  for (const auto &op : {wxS("''"), wxS("'"), wxS("!"), wxS("#"), wxS(";"),
                         wxS("$"), wxS("\\"), wxS("^^")})
    if (body.Contains(op))
      AddOnce(keywords, op);

  // "." is the matrix product unless it is the decimal point of a number.
  for (size_t i = 0; i < body.Length(); i++) {
    if (body[i] != '.')
      continue;
    const bool digitBefore = (i > 0) && wxIsdigit(body[i - 1]);
    const bool digitAfter = (i + 1 < body.Length()) && wxIsdigit(body[i + 1]);
    if (!digitBefore && !digitAfter) {
      AddOnce(keywords, wxS("."));
      break;
    }
  }

  if (onlyOperators) {
    if (body.Contains(wxS("*")))
      AddOnce(keywords, wxS("*"));
    // A subscript, as in "#1#[#2#]", the row of a matrix.
    if (body.Contains(wxS("[")))
      AddOnce(keywords, wxS("["));
    // A "^" that is half of a "^^" (the matrix power) doesn't count.
    wxString withoutMatrixPower = body;
    withoutMatrixPower.Replace(wxS("^^"), wxS(" "));
    if (withoutMatrixPower.Contains(wxS("^")))
      AddOnce(keywords, wxS("^"));
  }
  return keywords;
}
