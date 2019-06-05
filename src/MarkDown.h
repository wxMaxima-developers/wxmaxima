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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
    RegexReplacer(wxString From, wxString To) : wxRegEx(From)
    {
      replaceBy = To;
    }

    void DoReplace(wxString *line)
    {
      Replace(line, replaceBy);
    }

  private:
    wxString replaceBy; //!< The thing we replace it with
  };

  typedef std::list<RegexReplacer *> replaceList;
  replaceList regexReplaceList;
public:
  MarkDownParser(Configuration *cfg);

  virtual ~MarkDownParser();

  wxString MarkDown(wxString str);

  //! A list of things we want to replace.
  std::list<RegexReplacer *> RegexReplaceList()
  { return regexReplaceList; }

private:
  virtual wxString itemizeBegin()=0;      //!< The marker for the begin of an item list
  virtual wxString itemizeEnd()=0;        //!< The marker for the end of an item list
  virtual wxString quoteChar()=0;         //!< The marker for an quote
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
  MarkDownTeX(Configuration *cfg);

private:
  virtual wxString quoteBegin()
  { return wxT("\\begin{quote}\n"); }

  virtual wxString quoteEnd()
  { return wxT("\\end{quote}\n"); }

  virtual wxString quoteChar()
  { return wxT("\\ensuremath{>}"); }

  virtual wxString itemizeBegin()
  { return wxT("\\begin{itemize}\n"); }

  virtual wxString itemizeEnd()
  { return wxT("\\end{itemize}\n"); }

  virtual wxString itemizeItem()
  { return wxT("\\item "); }

  virtual wxString itemizeEndItem()
  { return wxEmptyString; }

  virtual wxString NewLine()
  { return wxT("\n\n"); }

};

//! A markdown parser for HTML
class MarkDownHTML : public MarkDownParser
{
public:
  MarkDownHTML(Configuration *cfg);

private:
  virtual wxString quoteChar()
  { return wxT("&gt;"); }

  virtual wxString quoteBegin()
  { return wxT("<blockquote>\n"); }

  virtual wxString quoteEnd()
  { return wxT("</blockquote>\n"); }

  virtual wxString itemizeBegin()
  { return wxT("<ul>\n"); }

  virtual wxString itemizeEnd()
  { return wxT("</ul>\n"); }

  virtual wxString itemizeItem()
  { return wxT("<li>"); }

  virtual wxString itemizeEndItem()
  { return wxT("</li>\n"); }

  virtual wxString NewLine()
  { return wxT("<br>"); }
};

#endif // MARKDOWN_H
