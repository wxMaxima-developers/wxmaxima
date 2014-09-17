///
///  Copyright (C) 2009-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "Autocomplete.h"

#include <wx/textfile.h>

AutoComplete::AutoComplete()
{
  m_args.Compile(wxT("[[]<([^>]*)>[]]"));
}

bool AutoComplete::LoadSymbols(wxString file)
{
  if (!wxFileExists(file))
    return false;

  if (m_symbolList.GetCount() > 0)
    m_symbolList.Empty();
  if (m_templateList.GetCount() > 0)
    m_templateList.Empty();

  wxString line;
  wxString rest, function;
  wxTextFile index(file);

  index.Open();

  for(line = index.GetFirstLine(); !index.Eof(); line = index.GetNextLine())
  {
    if (line.StartsWith(wxT("FUNCTION: ")) ||
        line.StartsWith(wxT("OPTION  : ")))
      m_symbolList.Add(line.Mid(10));
    else if (line.StartsWith(wxT("TEMPLATE: ")))
      m_templateList.Add(FixTemplate(line.Mid(10)));
  }

  index.Close();

  /// Add wxMaxima functions
  m_symbolList.Add(wxT("set_display"));
  m_symbolList.Add(wxT("wxplot2d"));
  m_templateList.Add(wxT("wxplot2d(<expr>,<x_range>)"));
  m_symbolList.Add(wxT("wxplot3d"));
  m_templateList.Add(wxT("wxplot3d(<expr>,<x_range>,<y_range>)"));
  m_symbolList.Add(wxT("wximplicit_plot"));
  m_symbolList.Add(wxT("wxcontour_plot"));
  m_symbolList.Add(wxT("wxanimate"));
  m_symbolList.Add(wxT("wxanimate_draw"));
  m_symbolList.Add(wxT("wxanimate_draw3d"));
  m_symbolList.Add(wxT("with_slider"));
  m_templateList.Add(wxT("with_slider(<a_var>,<a_list>,<expr>,<x_range>)"));
  m_symbolList.Add(wxT("with_slider_draw"));
  m_symbolList.Add(wxT("with_slider_draw3d"));
  m_symbolList.Add(wxT("wxdraw"));
  m_symbolList.Add(wxT("wxdraw2d"));
  m_symbolList.Add(wxT("wxdraw3d"));
  m_symbolList.Add(wxT("wxhistogram"));
  m_symbolList.Add(wxT("wxscatterplot"));
  m_symbolList.Add(wxT("wxbarsplot"));
  m_symbolList.Add(wxT("wxpiechart"));
  m_symbolList.Add(wxT("wxboxplot"));
  m_symbolList.Add(wxT("wxplot_size"));
  m_symbolList.Add(wxT("wxdraw_list"));
  m_symbolList.Add(wxT("table_form"));
  m_symbolList.Add(wxT("wxbuild_info"));
  m_templateList.Add(wxT("table_form(<data>)"));
  m_templateList.Add(wxT("table_form(<data>,<[options]>)"));

  /// Load private symbol list (do something different on Windows).
  wxString privateList;
#if defined __WXMSW__
  privateList = wxGetHomeDir() + wxT("\\wxmax.ac");
#else
  privateList = wxGetHomeDir() + wxT("/.wxmaxima.ac");
#endif

  if (wxFileExists(privateList))
  {
    wxTextFile priv(privateList);

    priv.Open();

    for(line = priv.GetFirstLine(); !priv.Eof(); line = priv.GetNextLine())
    {
      if (line.StartsWith(wxT("FUNCTION: ")) ||
          line.StartsWith(wxT("OPTION  : ")))
        m_symbolList.Add(line.Mid(10));
      else if (line.StartsWith(wxT("TEMPLATE: ")))
        m_templateList.Add(FixTemplate(line.Mid(10)));
      else
        m_symbolList.Add(line);
    }

    priv.Close();
  }

  m_symbolList.Sort();

  return false;
}

/// Returns a string array with functions which start with partial.
wxArrayString AutoComplete::CompleteSymbol(wxString partial, bool templates)
{
  wxArrayString completions;
  wxArrayString perfectCompletions;

  if (!templates) {
    for (int i=0; i<m_symbolList.GetCount(); i++)
    {
      if (m_symbolList[i].StartsWith(partial) &&
          completions.Index(m_symbolList[i]) == wxNOT_FOUND)
        completions.Add(m_symbolList[i]);
    }
  }

  else {
    for (int i=0; i<m_templateList.GetCount(); i++)
    {
      wxString templ = m_templateList[i];
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

  if (perfectCompletions.Count() > 0)
    return perfectCompletions;
  return completions;
}

void AutoComplete::AddSymbol(wxString fun, bool templ)
{
  /// Check for function of template
  if (fun.StartsWith(wxT("FUNCTION: ")))
  {
    fun = fun.Mid(10);
    templ = false;
  }
  else if (fun.StartsWith(wxT("TEMPLATE: ")))
  {
    fun = fun.Mid(10);
    templ = true;
  }

  /// Add symbols
  if (!templ && m_symbolList.Index(fun, true, true) == wxNOT_FOUND)
    m_symbolList.Add(fun);

  /// Add templates - for given function and given argument count we
  /// only add one template. We count the arguments by counting '<'
  if (templ)
  {
    fun = FixTemplate(fun);
    wxString funName = fun.SubString(0, fun.Find(wxT("(")));
    int count = fun.Freq('<'), i=0;
    for (i=0; i<m_templateList.GetCount(); i++)
    {
      wxString t = m_templateList[i];
      if (t.StartsWith(funName) && t.Freq('<') == count)
        break;
    }
    if (i == m_templateList.GetCount())
      m_templateList.Add(fun);
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
