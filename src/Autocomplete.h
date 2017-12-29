// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015 Gunter Königsmann     <wxMaxima@physikbuch.de>
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

/*! \file
  This file declares the class AutoComplete.

  AutoComplete creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#ifndef AUTOCOMPLETE_H
#define AUTOCOMPLETE_H

#include <wx/wx.h>
#include <wx/dir.h>
#include <wx/arrstr.h>
#include <wx/regex.h>
#include <wx/filename.h>
#include "Dirstructure.h"

class AutoComplete
{
  WX_DECLARE_STRING_HASH_MAP(int, WorksheetWords);

public:
  //! All types of things we can autocomplete
  enum autoCompletionType
  {
    command, //! Command names. \attention Must be the first entry in this enum
    tmplte,  //! Function templates
    loadfile,//! loadable files
    demofile,//! loadable files
    unit    //! Unit names. \attention Must be the last entry in this enum
  };

  AutoComplete();

  bool LoadSymbols(wxString file);

  void AddSymbol(wxString fun, autoCompletionType type = command);

  //! Add words to the list of words that appear in the workSheet's code cells
  void AddWorksheetWords(wxArrayString wordlist);

  //! Clear the list of words that appear in the workSheet's code cells
  void ClearWorksheetWords();

  wxArrayString CompleteSymbol(wxString partial, autoCompletionType type = command);

  wxString FixTemplate(wxString templ);

private:

  class GetMacFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    GetMacFiles_includingSubdirs(wxArrayString& files) : m_files(files) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename)
      {
        if(
          (filename.EndsWith(".mac"))||
          (filename.EndsWith(".lisp"))||
          (filename.EndsWith(".wxm"))
          )
        {
          wxFileName newItemName(filename);
          wxString newItem = "\"" + newItemName.GetName() + "\"";
          if(m_files.Index(newItem) == wxNOT_FOUND)
            m_files.Add(newItem);
        }
        return wxDIR_CONTINUE;
      }
    virtual wxDirTraverseResult OnDir(const wxString& WXUNUSED(dirname))
      {
        return wxDIR_CONTINUE;
      }
    wxArrayString& GetResult(){return m_files;}
  private:
    wxArrayString& m_files;
  };
  
  class GetMacFiles : public GetMacFiles_includingSubdirs
  {
  public:
    GetMacFiles(wxArrayString& files) : GetMacFiles_includingSubdirs(files){ }
    virtual wxDirTraverseResult OnDir(const wxString& WXUNUSED(dirname))
      {
        return wxDIR_IGNORE;
      }
  };
  
  class GetDemoFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    GetDemoFiles_includingSubdirs(wxArrayString& files) : m_files(files) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename)
      {
        if(filename.EndsWith(".dem"))
        {
          wxFileName newItemName(filename);
          wxString newItem = "\"" + newItemName.GetName() + "\"";
          if(m_files.Index(newItem) == wxNOT_FOUND)
            m_files.Add(newItem);
        }
        return wxDIR_CONTINUE;
      }
    virtual wxDirTraverseResult OnDir(const wxString& WXUNUSED(dirname))
      {
        return wxDIR_CONTINUE;
      }
    wxArrayString& GetResult(){return m_files;}
  private:
    wxArrayString& m_files;
  };
  
  class GetDemoFiles : public GetDemoFiles_includingSubdirs
  {
  public:
    GetDemoFiles(wxArrayString& files) : GetDemoFiles_includingSubdirs(files){ }
    virtual wxDirTraverseResult OnDir(const wxString& WXUNUSED(dirname))
      {
        return wxDIR_IGNORE;
      }
  };

  wxArrayString m_wordList[5];
  wxRegEx m_args;
  WorksheetWords m_worksheetWords;
};

#endif // AUTOCOMPLETE_H
