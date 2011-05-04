///
///  Copyright (C) 2006-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

//
// This class exists because the wxFileTipProvider does not translate tips.
// This is a bug in wxWidgets. This class can be removed after this bug is fixed
// and is common in wxWidgets libraries distributed all over.
//

#include "MyTipProvider.h"

MyTipProvider::MyTipProvider(const wxString& filename, int n)
    :  wxTipProvider(n), m_current(n), m_file(filename)
{
  m_file.Open();
}

MyTipProvider::~MyTipProvider()
{
  m_file.Close();
}

wxString MyTipProvider::GetTip()
{
  int count = m_file.GetLineCount();
  if (!count)
    return _("Tips not available, sorry!");

  wxString tip;

  for (int i = 0; i < count; i++)
  {
    if (m_current >= count)
      m_current = 0;

    tip = m_file.GetLine(m_current++);

    if (tip.Trim() != wxEmptyString)
      break;
  }

  if (tip.StartsWith(wxT("_(\"" ), &tip))
  {
    tip = tip.BeforeLast(wxT('\"'));
    tip.Replace(wxT("\\\""), wxT("\""));
    tip = wxGetTranslation(tip);
  }

  return tip;
}
