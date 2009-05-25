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

#include "History.h"

#include <wx/sizer.h>

History::History(wxWindow* parent, int id) : wxPanel(parent, id)
{
  m_history = new wxListBox(this, history_ctrl_id);
  wxFlexGridSizer * box = new wxFlexGridSizer(1);
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);

  box->Add(m_history, 0, wxEXPAND | wxALL, 0);
  SetSizer(box);
  box->Fit(this);
  box->SetSizeHints(this);
}

History::~History()
{
  //TODO: Load/save history?
}

void History::AddToHistory(wxString cmd)
{
  commands.Insert(cmd, 0);
  m_history->Set(commands);
}
