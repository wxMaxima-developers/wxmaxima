//
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef MARKDOWN_H
#define MARKDOWN_H

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <list>

/*! A generic markdown Parser.

 */

class MarkDownParser
{
public:
  MarkDownParser(){}
  wxString MarkDown(wxString str);

  virtual wxString itemizeBegin()=0;  //!< The marker for the begin of an item list
  virtual wxString itemizeEnd()=0;    //!< The marker for the end of an item list
  virtual wxString itemizeItem()=0;   //!< The marker for the begin of an item
  virtual wxString itemizeEndItem()=0;//!< The marker for the end of an item
  virtual wxString NewLine()=0;       //!< The marker for the beginning of a new line
};

//! A markdown parser for TeX
class MarkDownTeX: public MarkDownParser
{
public:
  MarkDownTeX() : MarkDownParser() {}
  virtual wxString itemizeBegin(){return wxT("\\begin{itemize}\n");}
  virtual wxString itemizeEnd(){return wxT("\\end{itemize}\n");}
  virtual wxString itemizeItem(){return wxT("\\item");}
  virtual wxString itemizeEndItem(){return wxEmptyString;}
  virtual wxString NewLine(){return wxT("\n");}
};

//! A markdown parser for HTML
class MarkDownHTML: public MarkDownParser
{
public:
  MarkDownHTML() : MarkDownParser() {}
  virtual wxString itemizeBegin(){return wxT("<UL>");}
  virtual wxString itemizeEnd(){return wxT("</UL>");}
  virtual wxString itemizeItem(){return wxT("<LI>");}
  virtual wxString itemizeEndItem(){return wxT("</LI>");}
  virtual wxString NewLine(){return wxT("<BR>");}
};

#endif // MARKDOWN_H
