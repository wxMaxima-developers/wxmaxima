// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C)      2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class RegexSearch

  RegexSearch is the find/replace dialog
*/

#include "RegexSearch.h"
#include <iostream>
#include <wx/log.h>

RegexSearch::RegexSearch(wxString regex)
    : wxRegEx(regex) {}

RegexSearch::~RegexSearch()
{
}

RegexSearch::Match RegexSearch::FindNext(wxString string, size_t start)
{
    wxLogNull suppress;
    Match retval;
    if(start >= string.Length())
        return retval;
    wxString src = string.Right(string.Length() - start);
    if(!Matches(src))
        return retval;
    size_t matchstart;
    size_t length;
    GetMatch(&matchstart, &length, 0);
    retval.SetStart(start + matchstart);
    retval.SetLength (length);
    return retval;
}

RegexSearch::Match RegexSearch::Replace(wxString *string, size_t start, wxString replacement)
{
    wxLogNull suppress;
    Match retval;
    if(start >= string->Length())
        return retval;
    wxString src = string->Right(string->Length() - start);
    if(!Matches(src))
        return retval;
    size_t matchstart;
    size_t length;
    GetMatch(&matchstart, &length, 0);
    wxRegEx::Replace(&src, replacement, 1);
    if(matchstart != 0)
        return retval;
    *string = string->Left(start) + src;
    retval.SetStart(start + matchstart);
    retval.SetLength (length);
    return retval;
}

RegexSearch::Match RegexSearch::Replace_Reverse(wxString *string, size_t start,
                                                wxString replacement)
{
    return Replace(string, start, replacement);
}

RegexSearch::Match RegexSearch::FindNext_Reverse(wxString string, size_t start)
{
    wxLogNull suppress;
    Match retval;
    size_t matchstart;
    size_t length;
    wxString src = string.Left(start);
    long offset = 0;
    // We want to find the last match before start.
    while(Matches(src))
    {
        GetMatch(&matchstart, &length, 0);
        retval.SetStart(offset + matchstart);
        retval.SetLength(length);
        src = src.Right(src.Length() - matchstart - length);
        offset += matchstart + length;
    }
    return retval;
}
