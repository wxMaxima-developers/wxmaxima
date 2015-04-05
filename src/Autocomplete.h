// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef AUTOCOMPLETE_H
#define AUTOCOMPLETE_H

#include <wx/wx.h>
#include <wx/arrstr.h>
#include <wx/regex.h>

class AutoComplete
{
public:
  //! All types of things we can autocomplete
  enum autoCompletionType
  {
    command, //! Command names. \attention Must be the first entry in this enum
    tmplte,  //! Function templates
    unit     //! Unit names. \attention Must be the last entry in this enum 
  };
    
  AutoComplete();
  bool LoadSymbols(wxString file);
  void AddSymbol(wxString fun, autoCompletionType type=command);
  wxArrayString CompleteSymbol(wxString partial, autoCompletionType type=command);
  wxString FixTemplate(wxString templ);
private:
  wxArrayString m_wordList[3];
  wxRegEx m_args;
};

#endif // AUTOCOMPLETE_H
