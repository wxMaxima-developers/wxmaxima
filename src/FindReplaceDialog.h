// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class FindReplaceDialog

  This dialog represents the "find" and "replace" dialogue.
 */

#ifndef FINDREPLACEDIALOG_H
#define FINDREPLACEDIALOG_H

#include <wx/dialog.h>
#include <wx/event.h>
#include "FindReplacePane.h"

/*! The find+replace dialog
 */
class FindReplaceDialog: public wxDialog
{
public:
  FindReplaceDialog(wxWindow *parent, wxFindReplaceData *data, const wxString &title, int style=0);
  wxFindReplaceData *GetData(){return m_contents->GetData();}

protected:
  void OnActivate(wxActivateEvent& event);
  void OnKeyDown(wxKeyEvent& event);
  void OnClose(wxCloseEvent& event);
  FindReplacePane *m_contents;
  DECLARE_EVENT_TABLE()

  private:
  bool m_active;
  
};
  
#endif // FINDREPLACEDIALOG_H
