/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */


#include <wx/utils.h>

#include "wxMaxima.h"
#include "DragNDrop.h"
#include "Gen1Wiz.h"

#if wxUSE_DRAG_AND_DROP

bool FileDrop::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& files)
{
  for (unsigned int i=0; i<files.GetCount(); i++) {
    wxmax->Raise();
    wxString file = files.Item(i);
#if defined (__WXMSW__)
    file.Replace(wxT("\\"), wxT("/"));
#endif
    if (type == DND_LOAD) {
      wxString choices[] = {wxT("load"), wxT("batch"),
                            wxT("demo"), wxT("loadfile"), wxT("custom")};
      wxString choice = wxGetSingleChoice(_T("Select function for loading ") +
                                          wxFileNameFromPath(files.Item(i)),
                                          _T("Function"), 5, choices, wxmax);
      if (choice.Length()==0)
        return false;
      if (choice == wxT("custom")) {
        choice = GetTextFromUser(_T("Enter function for loading ") +
                                 wxFileNameFromPath(files.Item(i)),
                                 _T("Function"), wxT("load"), wxmax);
        if (choice.Length()==0)
          return false;
      }
      if (choice.Find(wxT("%file%"))>-1) {
        choice.Replace(wxT("%file%"), wxT("\"") + file + wxT("\""));
        wxmax->SendMaxima(choice + wxT("$"));
      }
      else
        wxmax->SendMaxima(choice + wxT("(\"") + file + wxT("\")$"));
    }
    else
      input->WriteText(files.Item(i));
  }
  return true;
}

#endif
