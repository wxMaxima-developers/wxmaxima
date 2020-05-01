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

/*! \file
  This file contains the code for MarkDownParser.

  MarkDownParser is the class that handles the markdown syntax
*/

#ifndef MARKDOWN_H
#define MARKDOWN_H

#include <wx/string.h>
#include "Configuration.h"

/*! A generic markdown Parser.

 */
class MarkDownParser
{
protected:
  Configuration *m_configuration;
  virtual void DoReplacementsOn(wxString &str) = 0;

public:
  explicit MarkDownParser(Configuration *cfg);
  virtual ~MarkDownParser();

  wxString MarkDown(wxString str);

private:
  virtual const wxString &itemizeBegin()=0;      //!< The marker for the begin of an item list
  virtual const wxString &itemizeEnd()=0;        //!< The marker for the end of an item list
  virtual const wxString &quoteChar()=0;         //!< The marker for a quote
  virtual const wxString &quoteBegin()=0;        //!< The marker that says we want to start quote
  virtual const wxString &quoteEnd()=0;          //!< The marker that says we want to end quote
  virtual const wxString &itemizeItem()=0;       //!< The marker for the begin of an item
  virtual const wxString &itemizeEndItem()=0;    //!< The marker for the end of an item
  virtual const wxString &newLine()=0;           //!< The marker for the beginning of a new line
};

//! A markdown parser for TeX
class MarkDownTeX : public MarkDownParser
{
public:
  explicit MarkDownTeX(Configuration *cfg) : MarkDownParser(cfg) {}
  void DoReplacementsOn(wxString &str) override;

private:
  const wxString &quoteBegin() override
  { static const wxString s = wxT("\\begin{quote}\n"); return s; }

  const wxString &quoteEnd() override
  { static const wxString s = wxT("\\end{quote}\n"); return s; }

  const wxString &quoteChar() override
  { static const wxString s = wxT("\\ensuremath{>}"); return s; }

  const wxString &itemizeBegin() override
  { static const wxString s = wxT("\\begin{itemize}\n"); return s; }

  const wxString &itemizeEnd() override
  { static const wxString s = wxT("\\end{itemize}\n"); return s; }

  const wxString &itemizeItem() override
  { static const wxString s = wxT("\\item "); return s; }

  const wxString &itemizeEndItem() override
  { static const wxString s; return s; }

  const wxString &newLine() override
  { static const wxString s = wxT("\n\n"); return s; }

};

//! A markdown parser for HTML
class MarkDownHTML : public MarkDownParser
{
public:
  explicit MarkDownHTML(Configuration *cfg) : MarkDownParser(cfg) {}
  void DoReplacementsOn(wxString &str) override;

private:
  const wxString &quoteChar() override
  { static const wxString s =  wxT("&gt;"); return s; }

  const wxString &quoteBegin() override
  { static const wxString s =  wxT("<blockquote>\n"); return s; }

  const wxString &quoteEnd() override
  { static const wxString s =  wxT("</blockquote>\n"); return s; }

  const wxString &itemizeBegin() override
  { static const wxString s =  wxT("<ul>\n"); return s; }

  const wxString &itemizeEnd() override
  { static const wxString s =  wxT("</ul>\n"); return s; }

  const wxString &itemizeItem() override
  { static const wxString s =  wxT("<li>"); return s; }

  const wxString &itemizeEndItem() override
  { static const wxString s =  wxT("</li>\n"); return s; }

  const wxString &newLine() override
  { static const wxString s =  wxT("<br/>"); return s; }
};

#endif // MARKDOWN_H
