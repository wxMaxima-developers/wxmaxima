// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C)      2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

  FindReplaceDialog is the find/replace dialog
 */


#include "FindReplaceDialog.h"
#include "EditorCell.h"

FindReplaceDialog::FindReplaceDialog(wxWindow *parent, wxFindReplaceData *data, const wxString &title, int style):
  wxFindReplaceDialog(parent,data,title,style)
{
  m_active = true;
}

void FindReplaceDialog::OnFocus(wxFocusEvent& event)
{
  SetTransparent(255);
  m_active = true;
}

void FindReplaceDialog::OnFocusLoss(wxFocusEvent& event)
{
  SetTransparent(64);
  m_active = false;
}


BEGIN_EVENT_TABLE(FindReplaceDialog, wxFindReplaceDialog)
 EVT_SET_FOCUS(FindReplaceDialog::OnFocus)
 EVT_KILL_FOCUS(FindReplaceDialog::OnFocusLoss)

END_EVENT_TABLE()
