// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@bertec.com>
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

#include "../../src/StringUtils.cpp"
#include <cstdio>


void check(bool cond, const char *errMsg = nullptr)
{
  if (cond) return;
  if (errMsg)
    fprintf(stderr, "Test failed: %s\n", errMsg);
  fflush(stderr);
  abort();
}

void test_AppendLeft()
{
  wxString out = wxT("ABC");
  AppendLeft(out, wxT("DEF"), 0);
  check(out == wxT("ABC"));
  AppendLeft(out, wxT("DEF"), 1);
  check(out == wxT("ABCD"));
  AppendLeft(out, wxT("EFG"), 2);
  check(out == wxT("ABCDEF"));
  AppendLeft(out, wxT("GHI"), 3);
  check(out == wxT("ABCDEFGHI"));
  AppendLeft(out, wxT("JKL"), 10);
  check(out == wxT("ABCDEFGHIJKL"));
  out = wxT("XYZ");
  AppendLeft(out, wxT("ghi"), -3);
  check(out == wxT("XYZ"));
  AppendLeft(out, wxT("ghi"), -2);
  check(out == wxT("XYZg"));
  AppendLeft(out, wxT("hij"), -1);
  check(out == wxT("XYZghi"));
  AppendLeft(out, wxT("jkl"), -10);
  check(out == wxT("XYZghi"));
}

void test_AppendRight()
{
  wxString out = wxT("ABC");
  AppendRight(out, wxT("FED"), 0);
  check(out == wxT("ABC"));
  AppendRight(out, wxT("FED"), 1);
  check(out == wxT("ABCD"));
  AppendRight(out, wxT("GEF"), 2);
  check(out == wxT("ABCDEF"));
  AppendRight(out, wxT("GHI"), 3);
  check(out == wxT("ABCDEFGHI"));
  AppendRight(out, wxT("JKL"), 10);
  check(out == wxT("ABCDEFGHIJKL"));
  out = wxT("XYZ");
  AppendRight(out, wxT("ghi"), -3);
  check(out == wxT("XYZ"));
  AppendRight(out, wxT("ihg"), -2);
  check(out == wxT("XYZg"));
  AppendRight(out, wxT("jhi"), -1);
  check(out == wxT("XYZghi"));
  AppendRight(out, wxT("jkl"), -10);
  check(out == wxT("XYZghi"));
}

void test_Shorten()
{
  wxString out = wxT("abcdef");
  Shorten(out, 0);
  check(out == wxT("abcdef"));
  Shorten(out, 1);
  check(out == wxT("abcde"));
  Shorten(out, 1);
  check(out == wxT("abcd"));
  Shorten(out, 3);
  check(out == wxT("a"));
  Shorten(out, 1);
  check(out == wxT(""));
  out = wxT("abcdef");
  Shorten(out, 6);
  check(out == wxT(""));
  out = wxT("abcdef");
  Shorten(out, 20);
  check(out == wxT(""));
}

void test_AdvanceIf()
{
  wxString str = wxT("abcdef");
  auto out = AdvanceIf(str.begin(), str.end(), [](auto){ return false; });
  check(out == str.begin());
  out = AdvanceIf(str.begin(), str.end(), [](auto){ return true; });
  check(out == str.end());
  out = AdvanceIf(str.begin(), str.end(), [](auto ch){ return ch == 'a' || ch == 'b'; });
  check(out == str.begin() + 2);
}

void test_RetractIf()
{
  wxString str = wxT("ABCDEF");
  auto out = RetractIf(str.begin(), str.end(), [](auto){ return false; });
  check(out == str.end());
  out = RetractIf(str.begin(), str.end(), [](auto){ return true; });
  check(out == str.begin());
  out = RetractIf(str.begin(), str.end(), [](auto ch){ return ch == 'E' || ch == 'F'; });
  check(out == str.end() - 2);
}

void test_AdvanceTrim()
{
  wxString str;
  auto out = AdvanceTrim(str.begin(), str.end());
  check(out == str.begin());
  str = wxT("ABCDEF");
  out = AdvanceTrim(str.begin(), str.end());
  check(out == str.begin());
  str = wxT(" ABC ");
  out = AdvanceTrim(str.begin(), str.end());
  check(out == str.begin() + 1);
  str = wxT(" \f\n\r\t\v");
  out = AdvanceTrim(str.begin(), str.end());
  check(out == str.end());
  str = wxT(" \fab\nc\rd\te\v");
  out = AdvanceTrim(str.begin(), str.end());
  check(out == str.begin() + 2);
}

void test_RetractTrim()
{
  wxString str;
  auto out = RetractTrim(str.begin(), str.end());
  check(out == str.end());
  str = wxT("ABCDEF");
  out = RetractTrim(str.begin(), str.end());
  check(out == str.end());
  str = wxT(" ABC ");
  out = RetractTrim(str.begin(), str.end());
  check(out == str.end() - 1);
  str = wxT(" \f\n\r\t\v");
  out = RetractTrim(str.begin(), str.end());
  check(out == str.begin());
  str = wxT(" \fab\nc\rd\t\v");
  out = RetractTrim(str.begin(), str.end());
  check(out == str.end() - 2);
}

void test_AdvanceOverOne()
{
  wxString str;
  auto it = str.cbegin();
  auto rc = AdvanceOverOne(it, str.end(), wxT(""));
  check(it == str.begin() && rc);
  rc = AdvanceOverOne(it, str.end(), wxT("foo"));
  check(it == str.begin() && !rc);
  str = "abc";
  it = str.cend();
  rc = AdvanceOverOne(it, str.end(), wxT(""));
  check(it == str.end() && rc);
  it = str.cbegin();
  rc = AdvanceOverOne(it, str.end(), wxT(""));
  check(it == str.begin() && rc);
  rc = AdvanceOverOne(it, str.end(), wxT("foo"));
  check(it == str.begin() && !rc);
  rc = AdvanceOverOne(it, str.end(), wxT("f"));
  check(it == str.begin() && !rc);

  it = str.cbegin();
  rc = AdvanceOverOne(it, str.end(), wxT("abc"));
  check(it == str.end() && rc);
  it = str.cbegin();
  rc = AdvanceOverOne(it, str.end(), wxT("a"));
  check(it == str.begin()+1 && rc);
  rc = AdvanceOverOne(it, str.end(), wxT("bc"));
  check(it == str.end() && rc);
}

void test_RetractOverOne()
{
  wxString str;
  auto it = str.cend();
  auto rc = RetractOverOne(str.cbegin(), it, wxT(""));
  check(it == str.end() && rc);
  rc = RetractOverOne(str.cbegin(), it, wxT("foo"));
  check(it == str.end() && !rc);
  str = "abc";
  it = str.cbegin();
  rc = RetractOverOne(str.cbegin(), it, wxT(""));
  check(it == str.begin() && rc);
  it = str.cend();
  rc = RetractOverOne(str.cbegin(), it, wxT(""));
  check(it == str.end() && rc);
  rc = RetractOverOne(str.cbegin(), it, wxT("foo"));
  check(it == str.end() && !rc);
  rc = RetractOverOne(str.cbegin(), it, wxT("f"));
  check(it == str.end() && !rc);

  it = str.cend();
  rc = RetractOverOne(str.cbegin(), it, wxT("abc"));
  check(it == str.begin() && rc);
  it = str.cend();
  rc = RetractOverOne(str.cbegin(), it, wxT("c"));
  check(it == str.end()-1 && rc);
  rc = RetractOverOne(str.cbegin(), it, wxT("ab"));
  check(it == str.begin() && rc);
}

void test_RetractOver()
{
  wxString str;
  auto it = str.cend();
  auto rc = RetractOver(str.cbegin(), it, wxT('\0'));
  check(it == str.end() && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('f'));
  check(it == str.end() && !rc);

  str = "abc";
  it = str.cbegin();
  rc = RetractOver(str.cbegin(), it, wxT('\0'));
  check(it == str.begin() && !rc);
  it = str.cend();
  rc = RetractOver(str.cbegin(), it, wxT('\0'));
  check(it == str.end() && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('a'));
  check(it == str.end() && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('f'));
  check(it == str.end() && !rc);

  it = str.cend();
  rc = RetractOver(str.cbegin(), it, wxT('c'));
  check(it == str.end()-1 && rc);
  rc = RetractOver(str.cbegin(), it, wxT('c'));
  check(it == str.end()-1 && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('b'));
  check(it == str.end()-2 && rc);
  rc = RetractOver(str.cbegin(), it, wxT('a'));
  check(it == str.begin() && rc);
  rc = RetractOver(str.cbegin(), it, wxT('a'));
  check(it == str.begin() && !rc);

  str = "deeeff";
  it = str.cend();
  rc = RetractOver(str.cbegin(), it, wxT('d'));
  check(it == str.end() && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('f'));
  check(it == str.end()-2 && rc);
  rc = RetractOver(str.cbegin(), it, wxT('f'));
  check(it == str.end()-2 && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('e'));
  check(it == str.end()-5 && rc);
  rc = RetractOver(str.cbegin(), it, wxT('e'));
  check(it == str.end()-5 && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('\0'));
  check(it == str.end()-5 && !rc);
  rc = RetractOver(str.cbegin(), it, wxT('d'));
  check(it == str.begin() && rc);
  rc = RetractOver(str.cbegin(), it, wxT('d'));
  check(it == str.begin() && !rc);
}

void test_StartsWith()
{
  wxString str = wxT("abcdef");
  bool rc = StartsWith(str.begin(), str.end(), wxT(""));
  check(rc);
  rc = StartsWith(str, wxT(""));
  check(rc);
  rc = StartsWith(str.begin(), str.begin(), wxT(""));
  check(rc);
  rc = StartsWith(str.begin(), str.end(), wxT(""));
  check(rc);
  rc = StartsWith(str.begin(), str.end(), wxT("abcdefg"));
  check(!rc);
  rc = StartsWith(str, wxT("abcdefg"));
  check(!rc);
  rc = StartsWith(str.begin(), str.end(), wxT("abcdefgh"));
  check(!rc);
  rc = StartsWith(str, wxT("abcdefgh"));
  check(!rc);
  rc = StartsWith(str.begin(), str.end(), wxT("abcdef"));
  check(rc);
  rc = StartsWith(str, wxT("abcdef"));
  check(rc);
  rc = StartsWith(str.begin(), str.end(), wxT("abc"));
  check(rc);
  rc = StartsWith(str, wxT("abc"));
  check(rc);
  rc = StartsWith(str.begin(), str.end(), wxT("a"));
  check(rc);
  rc = StartsWith(str, wxT("a"));
  check(rc);
}

void test_EndsWith()
{
  wxString str = wxT("abcdef");
  bool rc = EndsWith(str.begin(), str.end(), wxT(""));
  check(rc);
  rc = EndsWith(str, wxT(""));
  check(rc);
  rc = EndsWith(str.begin(), str.begin(), wxT(""));
  check(rc);
  rc = EndsWith(str.begin(), str.end(), wxT(""));
  check(rc);
  rc = EndsWith(str.begin(), str.end(), wxT("zabcdef"));
  check(!rc);
  rc = EndsWith(str, wxT("zabcdef"));
  check(!rc);
  rc = EndsWith(str.begin(), str.end(), wxT("yzabcdef"));
  check(!rc);
  rc = EndsWith(str, wxT("yzabcdef"));
  check(!rc);
  rc = EndsWith(str.begin(), str.end(), wxT("abcdef"));
  check(rc);
  rc = EndsWith(str, wxT("abcdef"));
  check(rc);
  rc = EndsWith(str.begin(), str.end(), wxT("def"));
  check(rc);
  rc = EndsWith(str, wxT("def"));
  check(rc);
  rc = EndsWith(str.begin(), str.end(), wxT("f"));
  check(rc);
  rc = EndsWith(str, wxT("f"));
  check(rc);
}

void test_IsOneOf()
{
  bool out = IsOneOf('a');
  check(!out);
  out = IsOneOf('a', 'z');
  check(!out);
  out = IsOneOf('a', 'z', 'u');
  check(!out);
  out = IsOneOf('a', 'z', 'f', 'a');
  check(out);
  out = IsOneOf('a', 'z', 'a', 'f');
  check(out);
  out = IsOneOf('a', 'a', 'z', 'f');
  check(out);
}

template <typename String>
void test_literal_operators_impl()
{
  String out = String(wxT("ab"));
  out << wxT("");
  check(out == wxT("ab"));
  out << liT("");
  check(out == wxT("ab"));
  out << wxT("cdef");
  check(out == wxT("abcdef"));
  out << wxT("ghi");
  check(out == wxT("abcdefghi"));

  out = String(wxT("ab")) += wxT("");
  check(out == wxT("ab"));
  out += wxT("");
  check(out == wxT("ab"));
  out += wxT("cdef");
  check(out == wxT("abcdef"));
  out += wxT("ghi");
  check(out == wxT("abcdefghi"));

  out = String(wxT("ab")) + wxT("");
  check(out == wxT("ab"));
  out = out + wxT("");
  check(out == wxT("ab"));
  out = out + wxT("cdef");
  check(out == wxT("abcdef"));
}

void test_literal_operators()
{
  test_literal_operators_impl<wxString>();
  test_literal_operators_impl<std::basic_string<wxStringCharType>>();
}

template <typename String>
void test_Empty_impl()
{
  String out;
  out << Empty();
  check(out.empty());
  out = wxT("abcdef");
  check(out == wxT("abcdef"));
  out << Empty();
  check(out.empty());
}

void test_Empty()
{
  test_Empty_impl<wxString>();
  test_Empty_impl<std::basic_string<wxStringCharType>>();
}

template <typename String>
void test_Reserve_impl()
{
  String out = wxT("abcdef");
  auto ref = out.capacity();
  check(out.capacity() < (2 * ref));
  out.reserve(2 * ref);
  check(out.capacity() >= (2 * ref));

  out = String() << Reserve(2 * ref);
  check(out.capacity() >= (2 * ref));
}

void test_Reserve()
{
  test_Reserve_impl<wxString>();
  test_Reserve_impl<std::basic_string<wxStringCharType>>();
}

int compare(const wxString &str, StringView view)
{
  return str.compare(0, str.size(), view.data(), view.length());
}

void test_ViewRange()
{
  wxString str = wxT("abcdef");
  auto out = ViewRange(str, 0, 0);
  check(compare(wxT("a"), out) == 0);
  out = ViewRange(str, 5, 5);
  check(compare(wxT("f"), out) == 0);
  out = ViewRange(str, 1, 4);
  check(compare(wxT("bcde"), out) == 0);
  out = ViewRange(str, 0, 5);
  check(compare(wxT("abcdef"), out) == 0);
}

void test_ViewLeft()
{
  wxString str = wxT("abcdef");
  auto out = ViewLeft(str, 0);
  check(compare(wxT(""), out) == 0);
  out = ViewLeft(str, 1);
  check(compare(wxT("a"), out) == 0);
  out = ViewLeft(str, 2);
  check(compare(wxT("ab"), out) == 0);
  out = ViewLeft(str, 5);
  check(compare(wxT("abcde"), out) == 0);
  out = ViewLeft(str, 6);
  check(compare(wxT("abcdef"), out) == 0);
}

void test_ViewRight()
{
  wxString str = wxT("abcdef");
  auto out = ViewRight(str, 0);
  check(compare(wxT(""), out) == 0);
  out = ViewRight(str, 1);
  check(compare(wxT("f"), out) == 0);
  out = ViewRight(str, 2);
  check(compare(wxT("ef"), out) == 0);
  out = ViewRight(str, 5);
  check(compare(wxT("bcdef"), out) == 0);
  out = ViewRight(str, 6);
  check(compare(wxT("abcdef"), out) == 0);
}

int main()
{
  test_AppendLeft();
  test_AppendRight();
  test_Shorten();
  test_AdvanceIf();
  test_RetractIf();
  test_AdvanceTrim();
  test_RetractTrim();
  test_AdvanceOverOne();
  test_RetractOverOne();
  test_RetractOver();
  test_StartsWith();
  test_EndsWith();
  test_IsOneOf();
  test_literal_operators();
  test_Empty();
  test_Reserve();
  test_ViewRange();
  test_ViewLeft();
  test_ViewRight();
}
