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
//  SPDX-License-Identifier: GPL-2.0+

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
#include "Configuration.h"

/* The autocompletion logic

   The wordlists for autocompletion for keywords come from several sources:

     - wxMaxima::ReadLoadSymbols receive the contents of maxima's variables
       "values" and "functions" after a package is loaded.
     - all words that appear in the worksheet
     - and a list of maxima's builtin commands.
 */
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
    generalfile,//! general files
    esccommand, //! Esc commmands describing symbols
    unit    //! Unit names. \attention Must be the last entry in this enum
  };

  AutoComplete(Configuration *configuration);

  Configuration *m_configuration;
  
  bool LoadSymbols();

  //! Manually add a autocompletable symbol to our symbols lists
  void AddSymbol(wxString fun, autoCompletionType type = command);
  //! Interprets the XML autocompletable symbol list maxima can send us
  void AddSymbols(wxString xml);

  //! Replace the list of files in the directory the worksheet file is in to the demo files list
  void UpdateDemoFiles(wxString partial, wxString maximaDir);
  //! Replace the list of files in the directory the worksheet file is in to the load files list
  void UpdateLoadFiles(wxString partial, wxString maximaDir);
  //! Assemble a list of files
  void UpdateGeneralFiles(wxString partial, wxString maximaDir);
  
  //! Add words to the list of words that appear in the workSheet's code cells
  void AddWorksheetWords(wxArrayString wordlist);

  //! Clear the list of words that appear in the workSheet's code cells
  void ClearWorksheetWords();
  //! Clear the list of files load() can be applied on
  void ClearLoadfileList(){m_wordList[loadfile] = m_builtInLoadFiles;}
  //! Clear the list of files demo() can be applied on
  void ClearDemofileList(){m_wordList[demofile] = m_builtInDemoFiles;}
  
  //! Returns a list of possible autocompletions for the string "partial"
  wxArrayString CompleteSymbol(wxString partial, autoCompletionType type = command);
  wxString FixTemplate(wxString templ);

private:

  wxArrayString m_builtInLoadFiles;
  wxArrayString m_builtInDemoFiles;

  class GetGeneralFiles : public wxDirTraverser
  {
  public:
    GetGeneralFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files) { m_prefix = prefix; }
    virtual wxDirTraverseResult OnFile(const wxString& filename)
      {
        wxFileName newItemName(filename);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_CONTINUE;
      }
    virtual wxDirTraverseResult OnDir(const wxString& dirname)
      {
        wxFileName newItemName(dirname);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "/\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_IGNORE;
      }
    wxArrayString& GetResult(){return m_files;}
  protected: 
    wxArrayString& m_files;
    wxString m_prefix;
  };

  class GetMacFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    GetMacFiles_includingSubdirs(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files) { m_prefix = prefix; }
    virtual wxDirTraverseResult OnFile(const wxString& filename)
      {
        if(
          (filename.EndsWith(".mac"))||
          (filename.EndsWith(".lisp"))||
          (filename.EndsWith(".wxm"))
          )
        {
          wxFileName newItemName(filename);
          wxString newItem = "\"" + m_prefix + newItemName.GetName() + "\"";
          newItem.Replace(wxFileName::GetPathSeparator(),"/");
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
  protected: 
    wxArrayString& m_files;
    wxString m_prefix;
  };
  
  class GetMacFiles : public GetMacFiles_includingSubdirs
  {
  public:
    GetMacFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      GetMacFiles_includingSubdirs(files, prefix){ }
    virtual wxDirTraverseResult OnDir(const wxString& dirname)
      {
        wxFileName newItemName(dirname);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "/\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_IGNORE;
      }
  };
  
  class GetDemoFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    GetDemoFiles_includingSubdirs(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files) { m_prefix = prefix; }
    virtual wxDirTraverseResult OnFile(const wxString& filename)
      {
        if(filename.EndsWith(".dem"))
        {
          wxFileName newItemName(filename);
          wxString newItem = "\"" + m_prefix + newItemName.GetName() + "\"";
          newItem.Replace(wxFileName::GetPathSeparator(),"/");
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
  protected: 
    wxArrayString& m_files;
    wxString m_prefix;
  };
  
  class GetDemoFiles : public GetDemoFiles_includingSubdirs
  {
  public:
    GetDemoFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      GetDemoFiles_includingSubdirs(files, prefix){ }
    virtual wxDirTraverseResult OnDir(const wxString& dirname)
      {
        wxFileName newItemName(dirname);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "/\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_IGNORE;
      }
  };

  wxArrayString m_wordList[7];
  wxRegEx m_args;
  WorksheetWords m_worksheetWords;
};

#endif // AUTOCOMPLETE_H
