// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2017 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
  This file defines the class AutoComplete.

  AutoComplete creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#include "Autocomplete.h"
#include "Dirstructure.h"

#include <wx/textfile.h>

AutoComplete::AutoComplete()
{
  wxASSERT(m_args.Compile(wxT("[[]<([^>]*)>[]]")));
}

void AutoComplete::ClearWorksheetWords()
{
  m_worksheetWords.clear();
}

void AutoComplete::AddWorksheetWords(wxArrayString wordlist)
{
  wxArrayString::iterator it;
  for (it = wordlist.begin(); it != wordlist.end(); ++it)
  {
    if (m_worksheetWords[*it] != 1)
      m_worksheetWords[*it] = 1;
  }
}

bool AutoComplete::LoadSymbols(wxString file)
{
  if (!wxFileExists(file))
    return false;

  for (int i = command; i <= unit; i++)
  {
    if (m_wordList[i].GetCount() != 0)
      m_wordList[i].Clear();
  }

  wxString line;
  wxString rest, function;
  wxTextFile index(file);

  index.Open();

  for (line = index.GetFirstLine(); !index.Eof(); line = index.GetNextLine())
  {
    if (line.StartsWith(wxT("FUNCTION: ")) ||
        line.StartsWith(wxT("OPTION  : ")))
      m_wordList[command].Add(line.Mid(10));
    else if (line.StartsWith(wxT("TEMPLATE: ")))
      m_wordList[tmplte].Add(FixTemplate(line.Mid(10)));
    else if (line.StartsWith(wxT("UNIT: ")))
      m_wordList[unit].Add(FixTemplate(line.Mid(6)));
  }

  index.Close();

  /// Add wxMaxima functions
  m_wordList[command].Add(wxT("wxanimate_framerate"));
  m_wordList[command].Add(wxT("wxanimate_autoplay"));
  m_wordList[command].Add(wxT("wxplot_pngcairo"));
  m_wordList[command].Add(wxT("set_display"));
  m_wordList[command].Add(wxT("wxplot2d"));
  m_wordList[tmplte].Add(wxT("wxplot2d(<expr>,<x_range>)"));
  m_wordList[command].Add(wxT("wxplot3d"));
  m_wordList[tmplte].Add(wxT("wxplot3d(<expr>,<x_range>,<y_range>)"));
  m_wordList[command].Add(wxT("wximplicit_plot"));
  m_wordList[command].Add(wxT("wxcontour_plot"));
  m_wordList[command].Add(wxT("wxanimate"));
  m_wordList[command].Add(wxT("wxanimate_draw"));
  m_wordList[command].Add(wxT("wxanimate_draw3d"));
  m_wordList[command].Add(wxT("with_slider"));
  m_wordList[tmplte].Add(wxT("with_slider(<a_var>,<a_list>,<expr>,<x_range>)"));
  m_wordList[command].Add(wxT("with_slider_draw"));
  m_wordList[command].Add(wxT("with_slider_draw2d"));
  m_wordList[command].Add(wxT("with_slider_draw3d"));
  m_wordList[command].Add(wxT("wxdraw"));
  m_wordList[command].Add(wxT("wxdraw2d"));
  m_wordList[command].Add(wxT("wxdraw3d"));
  m_wordList[command].Add(wxT("wxfilename"));
  m_wordList[command].Add(wxT("wxhistogram"));
  m_wordList[command].Add(wxT("wxscatterplot"));
  m_wordList[command].Add(wxT("wxbarsplot"));
  m_wordList[command].Add(wxT("wxpiechart"));
  m_wordList[command].Add(wxT("wxboxplot"));
  m_wordList[command].Add(wxT("wxplot_size"));
  m_wordList[command].Add(wxT("wxdraw_list"));
  m_wordList[command].Add(wxT("wxbuild_info"));
  m_wordList[command].Add(wxT("wxbug_report"));
  m_wordList[command].Add(wxT("show_image"));
  m_wordList[tmplte].Add(wxT("show_image(<imagename>)"));
  m_wordList[command].Add(wxT("table_form"));
  m_wordList[tmplte].Add(wxT("table_form(<data>)"));
  m_wordList[tmplte].Add(wxT("table_form(<data>,<[options]>)"));
  m_wordList[command].Add(wxT("wxsubscripts"));
  m_wordList[command].Add(wxT("wxdeclare_subscripted"));
  m_wordList[tmplte].Add(wxT("wxdeclare_subscripted(<name>,<[false]>)"));
  m_wordList[command].Add(wxT("wxanimate_from_imgfiles"));
  m_wordList[tmplte].Add(wxT("wxanimate_from_imgfiles(<filename>,<[filename,...]>)"));
  m_wordList[command].Add(wxT("wxstatusbar"));
  m_wordList[tmplte].Add(wxT("wxstatusbar(<string>)"));

  /// Load private symbol list (do something different on Windows).
  wxString privateList;
  Dirstructure dirstruct;

  privateList = dirstruct.UserAutocompleteFile();

  if (wxFileExists(privateList))
  {
    wxTextFile priv(privateList);

    priv.Open();

    for (line = priv.GetFirstLine(); !priv.Eof(); line = priv.GetNextLine())
    {
      if (line.StartsWith(wxT("FUNCTION: ")) ||
          line.StartsWith(wxT("OPTION  : ")))
        m_wordList[command].Add(line.Mid(10));
      else if (line.StartsWith(wxT("TEMPLATE: ")))
        m_wordList[tmplte].Add(FixTemplate(line.Mid(10)));
      else if (line.StartsWith(wxT("UNIT: ")))
        m_wordList[unit].Add(FixTemplate(line.Mid(6)));
    }

    priv.Close();
  }
  
  // Prepare a list of all built-in loadable files of maxima.
  {
    GetMacFiles_includingSubdirs maximaLispIterator (m_wordList[loadfile]);
    wxDir maximadir(dirstruct.MaximaLispLocation()+ "/share/");
    if(maximadir.IsOpened())
      maximadir.Traverse(maximaLispIterator);
    GetMacFiles userLispIterator (m_wordList[loadfile]);
    wxDir maximauserdir(dirstruct.UserConfDir());
    if(maximauserdir.IsOpened())
      maximauserdir.Traverse(userLispIterator);
  }
  

  // Prepare a list of all built-in demos of maxima.
  {
    GetDemoFiles_includingSubdirs maximaLispIterator (m_wordList[demofile]);
    wxDir maximadir(dirstruct.MaximaLispLocation());
    if(maximadir.IsOpened())
      maximadir.Traverse(maximaLispIterator);
    GetDemoFiles userLispIterator (m_wordList[demofile]);
    wxDir maximauserdir(dirstruct.UserConfDir());
    if(maximauserdir.IsOpened())
      maximauserdir.Traverse(userLispIterator);
  }
  

  m_wordList[command].Sort();
  m_wordList[tmplte].Sort();
  m_wordList[unit].Sort();
  m_wordList[loadfile].Sort();
  m_wordList[demofile].Sort();

  return false;
}

/// Returns a string array with functions which start with partial.
wxArrayString AutoComplete::CompleteSymbol(wxString partial, autoCompletionType type)
{
  wxArrayString completions;
  wxArrayString perfectCompletions;

  wxASSERT_MSG((type >= command) && (type <= unit), _("Bug: Autocompletion requested for unknown type of item."));

  if (type != tmplte)
  {
    for (size_t i = 0; i < m_wordList[type].GetCount(); i++)
    {
      if (m_wordList[type][i].StartsWith(partial) &&
          completions.Index(m_wordList[type][i]) == wxNOT_FOUND)
        completions.Add(m_wordList[type][i]);
    }
  }
  else
  {
    for (size_t i = 0; i < m_wordList[type].GetCount(); i++)
    {
      wxString templ = m_wordList[type][i];
      if (templ.StartsWith(partial))
      {
        if (completions.Index(templ) == wxNOT_FOUND)
          completions.Add(templ);
        if (templ.SubString(0, templ.Find(wxT("(")) - 1) == partial &&
            perfectCompletions.Index(templ) == wxNOT_FOUND)
          perfectCompletions.Add(templ);
      }
    }
  }

  // Add a list of words that were definied on the work sheet but that aren't
  // defined as maxima commands or functions.
  if (type == command)
  {
    WorksheetWords::iterator it;
    for (it = m_worksheetWords.begin(); it != m_worksheetWords.end(); ++it)
    {
      if (it->first.StartsWith(partial))
      {
        if (completions.Index(it->first) == wxNOT_FOUND)
        {
          completions.Add(it->first);
        }
      }
    }
  }

  completions.Sort();

  if (perfectCompletions.Count() > 0)
    return perfectCompletions;
  return completions;
}

void AutoComplete::AddSymbol(wxString fun, autoCompletionType type)
{
  /// Check for function of template
  if (fun.StartsWith(wxT("FUNCTION: ")))
  {
    fun = fun.Mid(10);
    type = command;
  }
  else if (fun.StartsWith(wxT("TEMPLATE: ")))
  {
    fun = fun.Mid(10);
    type = tmplte;
  }
  else if (fun.StartsWith(wxT("UNIT: ")))
  {
    fun = fun.Mid(6);
    type = unit;
  }

  /// Add symbols
  if ((type != tmplte) && m_wordList[type].Index(fun, true, true) == wxNOT_FOUND)
    m_wordList[type].Add(fun);

  /// Add templates - for given function and given argument count we
  /// only add one template. We count the arguments by counting '<'
  if (type == tmplte)
  {
    fun = FixTemplate(fun);
    wxString funName = fun.SubString(0, fun.Find(wxT("(")));
    long count = fun.Freq('<');
    size_t i = 0;
    for (i = 0; i < m_wordList[type].GetCount(); i++)
    {
      wxString t = m_wordList[type][i];
      if (t.StartsWith(funName) && (t.Freq('<') == count))
        break;
    }
    if (i == m_wordList[type].GetCount())
      m_wordList[type].Add(fun);
  }
}

wxString AutoComplete::FixTemplate(wxString templ)
{
  templ.Replace(wxT(" "), wxEmptyString);
  templ.Replace(wxT(",..."), wxEmptyString);

  /// This will change optional arguments
  m_args.ReplaceAll(&templ, wxT("<[\\1]>"));

  return templ;
}
