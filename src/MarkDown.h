// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef MARKDOWN_H
#define MARKDOWN_H

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/regex.h>
#include <list>
#include <memory>
#include "Configuration.h"

/*! A generic markdown Parser.

 */

class MarkDownParser
{
protected:
  Configuration *m_configuration;

  //! A pair of a regExp and a string that has to replace the matches.
  class RegexReplacer : public wxRegEx
  {
  public:
    RegexReplacer(wxString from, const wxString &to) :
      wxRegEx(from),
      replaceBy(to)
    {}

    void DoReplace(wxString &line) const
    { Replace(&line, replaceBy); }

  private:
    wxString replaceBy; //!< The thing we replace it with
  };

  typedef std::list<RegexReplacer> replaceList;
  replaceList regexReplaceList;
public:
  explicit MarkDownParser(Configuration *cfg);

  wxString MarkDown(wxString str);

  //! A list of things we want to replace.
  const replaceList &RegexReplaceList() const
    { return regexReplaceList; }

private:
  virtual wxString itemizeBegin()=0;      //!< The marker for the begin of an item list
  virtual wxString itemizeEnd()=0;        //!< The marker for the end of an item list
  virtual wxString quoteChar()=0;         //!< The marker for a quote
  virtual wxString quoteBegin()=0;        //!< The marker that says we want to start quote
  virtual wxString quoteEnd()=0;        //!< The marker that says we want to end quote
  virtual wxString itemizeItem()=0;       //!< The marker for the begin of an item
  virtual wxString itemizeEndItem()=0;    //!< The marker for the end of an item
  virtual wxString NewLine()=0;           //!< The marker for the beginning of a new line
};

//! A markdown parser for TeX
class MarkDownTeX : public MarkDownParser
{
public:
  explicit MarkDownTeX(Configuration *cfg);

private:
  virtual wxString quoteBegin() override
  { return wxT("\\begin{quote}\n"); }

  virtual wxString quoteEnd() override
  { return wxT("\\end{quote}\n"); }

  virtual wxString quoteChar() override
  { return wxT("\\ensuremath{>}"); }

  virtual wxString itemizeBegin() override
  { return wxT("\\begin{itemize}\n"); }

  virtual wxString itemizeEnd() override
  { return wxT("\\end{itemize}\n"); }

  virtual wxString itemizeItem() override
  { return wxT("\\item "); }

  virtual wxString itemizeEndItem() override
  { return wxEmptyString; }

  virtual wxString NewLine() override
  { return wxT("\n\n"); }

};

//! A markdown parser for HTML
class MarkDownHTML : public MarkDownParser
{
public:
  explicit MarkDownHTML(Configuration *cfg);

private:
  virtual wxString quoteChar() override
  { return wxT("&gt;"); }

  virtual wxString quoteBegin() override
  { return wxT("<blockquote>\n"); }

  virtual wxString quoteEnd() override
  { return wxT("</blockquote>\n"); }

  virtual wxString itemizeBegin() override
  { return wxT("<ul>\n"); }

  virtual wxString itemizeEnd() override
  { return wxT("</ul>\n"); }

  virtual wxString itemizeItem() override
  { return wxT("<li>"); }

  virtual wxString itemizeEndItem() override
  { return wxT("</li>\n"); }

  virtual wxString NewLine() override
  { return wxT("<br/>"); }
};

#endif // MARKDOWN_H
