// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2019 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
  This file defines the class AutoComplete.

  AutoComplete creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are
  defined.
*/

#include "Autocomplete.h"
#include "Dirstructure.h"
#include "ErrorRedirector.h"
#include <wx/filename.h>
#include <wx/sstream.h>
#include <wx/textfile.h>
#include <wx/txtstrm.h>
#include <wx/utils.h>
#include <wx/wfstream.h>
#include <wx/xml/xml.h>
#include <algorithm>

AutoComplete::AutoComplete(Configuration *configuration) : wxEvtHandler() {
  std::vector<wxString> emptyList;
  for(unsigned i = 0; i < autoCompletionType::numberOfTypes; i++)
    m_wordList.push_back(emptyList);
  m_configuration = configuration;
}

void AutoComplete::ClearWorksheetWords() {
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  m_worksheetWords.clear();
}

std::vector<wxString> AutoComplete::GetDemoFilesList() {
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  return m_builtInDemoFiles;
}

std::vector<wxString> AutoComplete::GetSymbolList()
{
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  return m_wordList.at(command);
}


bool AutoComplete::HasDemofile(wxString commandname)
{
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  for (auto &file : m_wordList.at(demofile))
    {
      if(file == "\"" + commandname + "\"")
        return true;
    }
  return false;
}


void AutoComplete::ClearDemofileList() {
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  m_wordList.at(demofile) = m_builtInDemoFiles;
}

void AutoComplete::AddSymbols(wxString xml) {
  if(m_addSymbols_backgroundThread.joinable())
    {
      wxLogMessage(_("Waiting for m_addSymbols_backgroundThread to finish"));
      m_addSymbols_backgroundThread.join();
    }
  wxLogMessage(_("Scheduling a background task that compiles a new list "
                 "of autocompletable maxima commands."));

  wxString sharedir = m_configuration->MaximaShareDir();
  sharedir.Replace("\n", "");
  sharedir.Replace("\r", "");

  m_addSymbols_backgroundThread = std::thread(&AutoComplete::AddSymbols_Backgroundtask,
                                              this, xml);
}

void AutoComplete::AddSymbols_Backgroundtask(wxString xml) {
  wxXmlDocument xmldoc;
  wxStringInputStream xmlStream(xml);
  xmldoc.Load(xmlStream, wxS("UTF-8"));
  wxXmlNode *node = xmldoc.GetRoot();
  if (node != NULL) {
    wxXmlNode *children = node->GetChildren();
    while (children != NULL) {
      if (children->GetType() == wxXML_ELEMENT_NODE) {
        if (children->GetName() == wxS("function")) {
          wxXmlNode *val = children->GetChildren();
          if (val) {
            wxString name = val->GetContent();
            AddSymbol(name, command);
          }
        }

        if (children->GetName() == wxS("template")) {
          wxXmlNode *val = children->GetChildren();
          if (val) {
            wxString name = val->GetContent();
            AddSymbol(name, tmplte);
          }
        }

        if (children->GetName() == wxS("unit")) {
          wxXmlNode *val = children->GetChildren();
          if (val) {
            wxString name = val->GetContent();
            AddSymbol(name, unit);
          }
        }

        if (children->GetName() == wxS("value")) {
          wxXmlNode *val = children->GetChildren();
          if (val) {
            wxString name = val->GetContent();
            AddSymbol(name, command);
          }
        }
      }
      children = children->GetNext();
    }
  }
}

void AutoComplete::AddWorksheetWords(WordList::const_iterator const begin,
                                     WordList::const_iterator const end) {
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  for (auto word = begin; word != end; std::advance(word, 1))
    m_worksheetWords[*word] = 1;
}

void AutoComplete::AddWorksheetWords(const WordList &words) {
  AddWorksheetWords(words.begin(), words.end());
}

AutoComplete::~AutoComplete() {
   if (m_addSymbols_backgroundThread.joinable())
     m_addSymbols_backgroundThread.join();
   if(m_addFiles_backgroundThread.joinable())
     m_addFiles_backgroundThread.join();
}

void AutoComplete::LoadSymbols() {
  wxString sharedir = m_configuration->MaximaShareDir();
  sharedir.Replace("\n", "");
  sharedir.Replace("\r", "");
  if(m_addFiles_backgroundThread.joinable())
    {
      wxLogMessage(_("Waiting for m_addFiles_backgroundThread to finish"));
      m_addFiles_backgroundThread.join();
    }
  if(m_addSymbols_backgroundThread.joinable())
    {
      wxLogMessage(_("Waiting for m_addSymbols_backgroundThread to finish"));
      m_addSymbols_backgroundThread.join();
    }
  m_addSymbols_backgroundThread = std::thread(&AutoComplete::BuiltinSymbols_BackgroundTask, this);
  m_addFiles_backgroundThread = std::thread(&AutoComplete::LoadableFiles_BackgroundTask, this, sharedir);
}

void AutoComplete::BuiltinSymbols_BackgroundTask() {
  {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    for(auto &wordlist:m_wordList)
    wordlist.clear();
  }
  LoadBuiltinSymbols();

  for (auto it = Configuration::EscCodesBegin();
       it != Configuration::EscCodesEnd(); ++it)
    {
      const std::lock_guard<std::mutex> lock(m_keywordsLock);
      m_wordList.at(esccommand).push_back(it->first);
    }

  for(auto &wordlist:m_wordList)
    {
      const std::lock_guard<std::mutex> lock(m_keywordsLock);
      std::sort(wordlist.begin(), wordlist.end());
    }

  wxString line;

  /// Load private symbol list (do something different on Windows).
  wxString privateList;
  privateList = Dirstructure::Get()->UserAutocompleteFile();
  wxLogMessage(_("Trying to load a list of autocompletable symbols from file %s"),
               privateList.utf8_str());
  if (wxFileExists(privateList)) {
    wxTextFile priv(privateList);

    priv.Open();

    wxRegEx function("^[fF][uU][nN][cC][tT][iI][oO][nN] *: *");
    wxRegEx option("^[oO][pP][tT][iI][oO][nN] *: *");
    wxRegEx templte("^[tT][eE][mM][pP][lL][aA][tT][eE] *: *");
    wxRegEx unt("^[uU][nN][iI][tT] *: *");
    for (line = priv.GetFirstLine(); !priv.Eof(); line = priv.GetNextLine()) {
      line.Trim(true);
      line.Trim(false);
      if (!line.StartsWith("#")) {
        if (function.Replace(&line, ""))
          {
            const std::lock_guard<std::mutex> lock(m_keywordsLock);
            m_wordList.at(command).push_back(line);
          }
        else if (option.Replace(&line, ""))
          {
            const std::lock_guard<std::mutex> lock(m_keywordsLock);
            m_wordList.at(command).push_back(line);
          }
        else if (templte.Replace(&line, ""))
          {
            const std::lock_guard<std::mutex> lock(m_keywordsLock);
            m_wordList.at(tmplte).push_back(FixTemplate(line));
          }
        else if (unt.Replace(&line, ""))
          {
            const std::lock_guard<std::mutex> lock(m_keywordsLock);
            m_wordList.at(unit).push_back(line);
          }
        else
          wxLogMessage(_("%s: Can't interpret line: %s"),
                       privateList.mb_str(),
                       line.mb_str());
      }
    }
    priv.Close();
  } else {
    SuppressErrorDialogs logNull;
    wxFileOutputStream output(privateList);
    if (output.IsOk()) {
      wxTextOutputStream text(output);
      text << "# This file allows users to add their own symbols\n";
      text << "# to wxMaxima's autocompletion feature.\n";
      text << "# If a useful built-in symbol of Maxima is lacking\n";
      text << "# in wxMaxima's autocompletion please inform the wxMaxima\n";
      text << "# maintainers about this!\n";
      text << "# \n";
      text << "# The format of the entries in this file is:\n";
      text << "# FUNCTION: myfunction\n";
      text << "# OPTION: myvariable\n";
      text << "# UNIT: myunit\n";
      text << "# Template: mycommand(<expr>, <x>)";
      text.Flush();
    }
  }
}

void AutoComplete::LoadableFiles_BackgroundTask(wxString sharedir) {
  // Error dialogues need to be created by the foreground thread.
  SuppressErrorDialogs suppressor;

  // Prepare a list of all built-in loadable files of maxima.
  {
    GetMacFiles_includingSubdirs maximaLispIterator(m_builtInLoadFiles, &m_keywordsLock);
    if (sharedir.IsEmpty())
      wxLogMessage(_("Seems like the package with the maxima share files isn't "
                     "installed."));
    else {
      wxFileName shareDir(sharedir + "/");
      shareDir.MakeAbsolute();
      wxLogMessage(_("Autocompletion: Scanning %s recursively for loadable lisp files."),
                   shareDir.GetFullPath().utf8_str());
      wxDir maximadir(shareDir.GetFullPath());
      if (maximadir.IsOpened())
        maximadir.Traverse(maximaLispIterator); // todo
    }
    GetMacFiles userLispIterator(m_builtInLoadFiles, &m_keywordsLock);
    wxFileName userDir(Dirstructure::Get()->UserConfDir() + "/");
    userDir.MakeAbsolute();
    wxDir maximauserfilesdir(userDir.GetFullPath());
    wxLogMessage(_("Autocompletion: Scanning %s for loadable lisp files."),
                 userDir.GetFullPath().utf8_str());
    if (maximauserfilesdir.IsOpened())
      maximauserfilesdir.Traverse(userLispIterator);
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    long num = m_builtInLoadFiles.size();
    wxLogMessage(_("Found %li loadable files."), num);
  }

  // Prepare a list of all built-in demos of maxima.
  {
    wxFileName shareDir(sharedir + "/");
    shareDir.MakeAbsolute();
    wxFileName demoDir(shareDir.GetFullPath() + "/");
    demoDir.MakeAbsolute();
    demoDir.RemoveLastDir();
    GetDemoFiles_includingSubdirs maximaLispIterator(m_builtInDemoFiles, &m_keywordsLock);
    wxLogMessage(_("Autocompletion: Scanning %s for loadable demo files."),
                 demoDir.GetFullPath().utf8_str());

    wxDir maximadir(demoDir.GetFullPath());
    if (maximadir.IsOpened())
      maximadir.Traverse(maximaLispIterator);
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    long num = m_builtInDemoFiles.size();
    wxLogMessage(_("Found %li demo files."), num);
  }
  const std::lock_guard<std::mutex> lock(m_keywordsLock);
  std::sort(m_builtInLoadFiles.begin(), m_builtInLoadFiles.end());
  std::sort(m_builtInDemoFiles.begin(), m_builtInDemoFiles.end());
  m_wordList.at(demofile) = m_builtInDemoFiles;
  m_wordList.at(loadfile) = m_builtInLoadFiles;
  // Inform the main thread that there are new demo files
  wxCommandEvent *event = new wxCommandEvent(NEW_DEMO_FILES_EVENT);
  QueueEvent(event);
}

void AutoComplete::UpdateDemoFiles(wxString partial, wxString maximaDir) {
  // Remove the opening quote from the partial.
  if (partial.at(0) == wxS('\"'))
    partial = partial.Right(partial.Length() - 1);

  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxS('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxS("/");

  // Determine if we need to add the path to maxima's current dir to the path in
  // partial
  if (!wxFileName(partial).IsAbsolute()) {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }

  // Determine the name of the directory
  if ((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";

  // Remove all files from the maxima directory from the demo file list
  ClearDemofileList();

  // Add all files from the maxima directory to the demo file list
  if (partial != wxS("//")) {
    GetDemoFiles userLispIterator(m_wordList.at(demofile), &m_keywordsLock, prefix);
    wxDir demofilesdir(partial);
    if (demofilesdir.IsOpened())
      demofilesdir.Traverse(userLispIterator);
  }
}

void AutoComplete::UpdateGeneralFiles(wxString partial, wxString maximaDir) {
  // Remove the opening quote from the partial.
  if (partial.at(0) == wxS('\"'))
    partial = partial.Right(partial.Length() - 1);

  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxS('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxS("/");

  // Determine if we need to add the path to maxima's current dir to the path in
  // partial
  if (!wxFileName(partial).IsAbsolute()) {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }

  // Determine the name of the directory
  if ((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";

  // Add all files from the maxima directory to the demo file list
  if (partial != wxS("//")) {
    GetGeneralFiles fileIterator(m_wordList[generalfile], &m_keywordsLock, prefix);
    wxDir generalfilesdir(partial);
    if (generalfilesdir.IsOpened())
      generalfilesdir.Traverse(fileIterator);
  }
}

void AutoComplete::UpdateLoadFiles(wxString partial, wxString maximaDir) {
  wxLogMessage(_("Scheduling a background task that scans for autocompletable "
                 "file names."));
  // Remove the opening quote from the partial.
  if (partial.at(0) == wxS('\"'))
    partial = partial.Right(partial.Length() - 1);

  partial.Replace(wxFileName::GetPathSeparator(), "/");
  int pos;
  if ((pos = partial.Find(wxS('/'), true)) == wxNOT_FOUND)
    partial = wxEmptyString;
  else
    partial = partial.Left(pos);
  wxString prefix = partial + wxS("/");

  // Determine if we need to add the path to maxima's current dir to the path in
  // partial
  if (!wxFileName(partial).IsAbsolute()) {
    partial = maximaDir + wxFileName::GetPathSeparator() + partial;
    partial.Replace(wxFileName::GetPathSeparator(), "/");
  }

  // Determine the name of the directory
  if ((partial != wxEmptyString) && wxDirExists(partial))
    partial += "/";

  // Remove all files from the maxima directory from the load file list
  {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    m_wordList[loadfile] = m_builtInLoadFiles;
  }

  // Add all files from the maxima directory to the load file list
  if (partial != wxS("//")) {
    GetMacFiles userLispIterator(m_wordList.at(loadfile), &m_keywordsLock, prefix);
    wxDir loadfilesdir(partial);
    if (loadfilesdir.IsOpened())
      loadfilesdir.Traverse(userLispIterator);
  }
}

/// Returns a string array with functions which start with partial.
std::vector<wxString> AutoComplete::CompleteSymbol(wxString partial,
                                                   autoCompletionType type) {
  std::vector<wxString> completions;
  std::vector<wxString> perfectCompletions;

  if (((type == AutoComplete::demofile) || (type == AutoComplete::loadfile)) &&
      (partial.EndsWith("\"")))
    partial = partial.Left(partial.Length() - 1);

  wxASSERT_MSG((type >= command) && (type <= unit),
               _("Bug: Autocompletion requested for unknown type of item."));

  if ((type != tmplte) && (type >=0 ) && (type < numberOfTypes )) {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    for (const auto &i : m_wordList.at(type)) {
      if (i.StartsWith(partial) &&
          (std::find(completions.begin(), completions.end(), i) == completions.end()))
        completions.push_back(i);
    }
  } else if (type == tmplte) {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    for (const auto &i: m_wordList.at(type)) {
      if (i.StartsWith(partial)) {
        if (std::find(completions.begin(), completions.end(), i) == completions.end())
          completions.push_back(i);
        if (i.SubString(0, static_cast<std::size_t>(i.Find(wxS("("))) - 1) == partial &&
            (std::find(perfectCompletions.begin(), perfectCompletions.end(), i) ==
             perfectCompletions.end()))
          perfectCompletions.push_back(i);
      }
    }
  }

  // Add a list of words that were defined on the work sheet but that aren't
  // defined as maxima commands or functions.
  if (type == command) {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    for (const auto &it : m_worksheetWords) {
      if (it.first.StartsWith(partial)) {
        if (std::find(completions.begin(), completions.end(), it.first) == completions.end()) {
          completions.push_back(it.first);
        }
      }
    }
  }

  std::sort(completions.begin(), completions.end());
  if (perfectCompletions.size() > 0)
    return perfectCompletions;
  return completions;
}

void AutoComplete::AddSymbol(wxString fun, autoCompletionType type) {
  /// Check for function of template
  if (fun.StartsWith(wxS("FUNCTION: "))) {
    fun = fun.Mid(10);
    type = command;
  } else if (fun.StartsWith(wxS("TEMPLATE: "))) {
    fun = fun.Mid(10);
    type = tmplte;
  } else if (fun.StartsWith(wxS("UNIT: "))) {
    fun = fun.Mid(6);
    type = unit;
  }
  auto spacepos = fun.Find(" ");
  if(spacepos != wxNOT_FOUND)
    fun = fun.Left(spacepos);
  /// Add symbols
  {
    const std::lock_guard<std::mutex> lock(m_keywordsLock);
    if ((type != tmplte) &&
        (std::find(m_wordList.at(type).begin(), m_wordList.at(type).end(), fun) ==
         m_wordList.at(type).end()))
      m_wordList.at(type).push_back(fun);
  }
  /// Add templates - for given function and given argument count we
  /// only add one template. We count the arguments by counting '<'
  if (type == tmplte) {
    fun = FixTemplate(fun);
    auto openpos = fun.Find(wxS("("));
    if(openpos < 0)
      wxLogMessage(_("Cannot interpret template %s"), fun.mb_str());
    else
      {
        wxString funName = fun.SubString(0, openpos);
        auto count = fun.Freq('<');
        std::size_t i = 0;
        {
          const std::lock_guard<std::mutex> lock(m_keywordsLock);
          for (const auto &o: m_wordList.at(type)) {
            if (o.StartsWith(funName) && (o.Freq('<') == count))
              break;
            i++;
          }
          if (i == m_wordList.at(type).size())
            {
              m_wordList.at(type).push_back(fun);
            }
        }
      }
  }
}

wxString AutoComplete::FixTemplate(wxString templ) {
  templ.Replace(wxS(" "), wxEmptyString);
  templ.Replace(wxS(",..."), wxEmptyString);

  /// This will change optional arguments
  m_args.ReplaceAll(&templ, wxS("<[\\1]>"));

  return templ;
}

wxRegEx AutoComplete::m_args("[<\\([^>]*\\)>]");
wxDEFINE_EVENT(NEW_DEMO_FILES_EVENT, wxCommandEvent);
