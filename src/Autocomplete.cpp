///
///  Copyright (C) 2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

/// This functions loads functions/variables from maxima documentation.
/// It reads the file index.hhk and looks for lines like
/// |   <param name="Name" value="function"></object>|
bool AutoComplete::LoadSymbols(wxString file)
{
  if (!wxFileExists(file))
    return false;

  if (m_symbolList.GetCount() > 0)
    m_symbolList.Empty();

  wxString line;
  wxString rest, function;
  wxTextFile index(file);

  index.Open();

  for(line = index.GetFirstLine(); !index.Eof(); line = index.GetNextLine())
  {
    if (!line.StartsWith(wxT("   <param name=\"Name\" value=\""), &rest))
      continue;

    if (!rest.EndsWith(wxT("\"></object>"), &function))
      continue;

    m_symbolList.Add(function);
  }

  /// Add wxMaxima functions
  m_symbolList.Add(wxT("set_display"));
  m_symbolList.Add(wxT("wxplot2d"));
  m_symbolList.Add(wxT("wxplot3d"));
  m_symbolList.Add(wxT("wximplicit_plot"));
  m_symbolList.Add(wxT("wxcontour_plot"));
  m_symbolList.Add(wxT("wxanimate"));
  m_symbolList.Add(wxT("wxanimate_draw"));
  m_symbolList.Add(wxT("wxanimate_draw2d"));
  m_symbolList.Add(wxT("wxanimate_draw3d"));
  m_symbolList.Add(wxT("with_slider"));
  m_symbolList.Add(wxT("with_slider_draw"));
  m_symbolList.Add(wxT("with_slider_draw3d"));
  m_symbolList.Add(wxT("wxdraw"));
  m_symbolList.Add(wxT("wxdraw2d"));
  m_symbolList.Add(wxT("wxdraw3d"));
  m_symbolList.Add(wxT("wxhistogram"));
  m_symbolList.Add(wxT("wxhistogram"));
  m_symbolList.Add(wxT("wxscatterplot"));
  m_symbolList.Add(wxT("wxbarsplot"));
  m_symbolList.Add(wxT("wxpiechart"));
  m_symbolList.Add(wxT("wxboxplot"));

  m_symbolList.Sort();

  index.Close();

  return false;
}

/// Returns a string array with functions which start with partial.
wxArrayString AutoComplete::CompleteSymbol(wxString partial)
{
  wxArrayString completions;

  for (int i=0; i<m_symbolList.GetCount(); i++)
  {
    if (m_symbolList[i].StartsWith(partial))
      completions.Add(m_symbolList[i]);
  }

  return completions;
}

void AutoComplete::AddSymbol(wxString fun)
{
  if (m_symbolList.Index(fun, true, true) == wxNOT_FOUND)
    m_symbolList.Add(fun);
}
