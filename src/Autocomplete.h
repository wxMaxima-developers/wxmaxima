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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares the class AutoComplete.

  AutoComplete creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#ifndef AUTOCOMPLETE_H
#define AUTOCOMPLETE_H

#include "precomp.h"
#include <thread>
#include <memory>
#include <wx/wx.h>
#include <wx/dir.h>
#include <wx/arrstr.h>
#include <wx/regex.h>
#include <wx/filename.h>
#include <vector>
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
  using WordList = std::vector<wxString>;

  //! All types of things we can autocomplete
  enum autoCompletionType
  {
    command, //! Command names. \attention Must be the first entry in this enum
    tmplte,  //! Function templates
    loadfile,//! loadable files
    demofile,//! loadable files
    generalfile,//! general files
    esccommand, //! Esc commands describing symbols
    unit    //! Unit names. \attention Must be the last entry in this enum
  };
  explicit AutoComplete(Configuration *configuration);

  //! The destructor of AutoComplete
  ~AutoComplete();
  
  //! Load all autocomplete symbols wxMaxima knows about by itself
  void LoadSymbols();

  /*! Makes wxMaxima know all its builtin symbols.

    This function might cause a compiler warning because it is 
    suspiciously long. 
    For the same reason it has been split into a separate file.
  */
  bool LoadBuiltinSymbols();

  //! Manually add an autocompletable symbol to our symbols lists
  void AddSymbol(wxString fun, autoCompletionType type = command);
  //! Interprets the XML autocompletable symbol list maxima can send us
  void AddSymbols(wxString xml);
  //! The real work of AddSymbols is made here and in the background
  void AddSymbols_Backgroundtask(wxString xml);

  //! Replace the list of files in the directory the worksheet file is in to the demo files list
  void UpdateDemoFiles(wxString partial, wxString maximaDir);
  //! Replace the list of files in the directory the worksheet file is in to the load files list
  void UpdateLoadFiles(wxString partial, wxString maximaDir);
  //! Assemble a list of files
  void UpdateGeneralFiles(wxString partial, wxString maximaDir);
  
  //! Add words to the list of words that appear in the workSheet's code cells
  void AddWorksheetWords(const WordList &words);
  void AddWorksheetWords(WordList::const_iterator begin, WordList::const_iterator end);

  //! Clear the list of words that appear in the workSheet's code cells
  void ClearWorksheetWords();
  //! Clear the list of files demo() can be applied on
  void ClearDemofileList();
  
  //! Returns a list of possible autocompletions for the string "partial"
  wxArrayString CompleteSymbol(wxString partial, autoCompletionType type = command);
  //! Basically runs a regex over templates
  static wxString FixTemplate(wxString templ);

private:
  //! An AddSymbol that doesn't wait for background tasks to finish
  void AddSymbol_nowait(wxString fun, autoCompletionType type = command);
  //! The configuration storage
  Configuration *m_configuration;
  //! Loads the list of loadable files and can be run in a background task
  void LoadableFiles_BackgroundTask();
  //! Prepares the list of built-in symbols and can be run in a background task
  void BuiltinSymbols_BackgroundTask();

  //! Replace the list of files in the directory the worksheet file is in to the load files list
  void UpdateLoadFiles_BackgroundTask(wxString partial, wxString maximaDir);
  //! The list of loadable files maxima provides
  wxArrayString m_builtInLoadFiles;
  //! The list of demo files maxima provides
  wxArrayString m_builtInDemoFiles;

  //! Scans the maxima directory for a list of loadable files
  class GetGeneralFiles : public wxDirTraverser
  {
  public:
    explicit GetGeneralFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files), m_prefix(prefix) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename) override
      {
        wxFileName newItemName(filename);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_CONTINUE;
      }
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override
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

  //! Recursively scans the maxima directory for a list of .mac files
  class GetMacFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    explicit GetMacFiles_includingSubdirs(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files), m_prefix(prefix)  { }
    virtual wxDirTraverseResult OnFile(const wxString& filename) override
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
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override
      {
        if((dirname.EndsWith(".git")) ||
           (dirname.EndsWith("/share/share")) ||
           (dirname.EndsWith("/src/src")) ||
           (dirname.EndsWith("/doc/doc")) ||
           (dirname.EndsWith("/interfaces/interfaces"))
          )
          return wxDIR_STOP;
        else
          return wxDIR_CONTINUE;
      }
    wxArrayString& GetResult(){return m_files;}
  protected: 
    wxArrayString& m_files;
    wxString m_prefix;
  };
  
  //! Scans the user directory for a list of .mac files
  class GetMacFiles : public GetMacFiles_includingSubdirs
  {
  public:
    explicit GetMacFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      GetMacFiles_includingSubdirs(files, prefix){ }
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override
      {
        wxFileName newItemName(dirname);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "/\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_IGNORE;
      }
  };
  
  //! Scans a directory for a list of demo files
  class GetDemoFiles_includingSubdirs : public wxDirTraverser
  {
  public:
    explicit GetDemoFiles_includingSubdirs(wxArrayString& files, wxString prefix = wxEmptyString) :
      m_files(files), m_prefix(prefix) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename) override
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
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override
      {
        if((dirname.EndsWith(".git")) ||
           (dirname.EndsWith("/share/share")) ||
           (dirname.EndsWith("/src/src")) ||
           (dirname.EndsWith("/doc/doc")) ||
           (dirname.EndsWith("/interfaces/interfaces"))
          )
          return wxDIR_STOP;
        else
          return wxDIR_CONTINUE;
      }
    wxArrayString& GetResult(){return m_files;}
  protected: 
    wxArrayString& m_files;
    wxString m_prefix;
  };
  
  //! Scans the maxima directory for a list of demo files
  class GetDemoFiles : public GetDemoFiles_includingSubdirs
  {
  public:
    explicit GetDemoFiles(wxArrayString& files, wxString prefix = wxEmptyString) :
      GetDemoFiles_includingSubdirs(files, prefix){ }
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override
      {
        wxFileName newItemName(dirname);
        wxString newItem = "\"" + m_prefix + newItemName.GetFullName() + "/\"";
        newItem.Replace(wxFileName::GetPathSeparator(),"/");
        if(m_files.Index(newItem) == wxNOT_FOUND)
          m_files.Add(newItem);
        return wxDIR_IGNORE;
      }
  };

  void WaitForBackgroundThread_Symbols();
  void WaitForBackgroundThread_Files();
  void WaitForBackgroundThreads();
  
  //! The lists of autocompletible symbols for the classes defined in autoCompletionType
  wxArrayString m_wordList[7];
  static wxRegEx m_args;
  WorksheetWords m_worksheetWords;
  std::unique_ptr<std::thread> m_addSymbols_backgroundThread;
  std::unique_ptr<std::thread> m_addFiles_backgroundThread;
};

#endif // AUTOCOMPLETE_H
