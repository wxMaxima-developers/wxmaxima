///
///  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
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

#include "Inspector.h"

Inspector::Inspector(wxWindow* parent, int id) : wxPanel(parent, id)
{
  // splitter window
  //m_splitter = new wxSplitterWindow(this, -1);
  //m_splitter->SetSashGravity(0.5);

  //listbox
  m_listbox = new wxListBox(this, inspector_listbox_id,
      wxDefaultPosition, wxDefaultSize,
      0, NULL, wxLB_SINGLE | wxLB_NEEDED_SB);

  //mathctrl
  m_minimathctrl = new MiniMathCtrl(this, -1, wxDefaultPosition, wxDefaultSize);

  // LAYOUT
  // left side
  wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *vboxleft = new wxBoxSizer(wxVERTICAL);

  vboxleft->Add(
      new wxStaticText(this, -1, wxT("Variables:")),
      0, wxTOP | wxLEFT, 5);

  vboxleft->Add(m_listbox, 1, wxEXPAND | wxALL, 5);

  hbox->Add(vboxleft, 2, wxEXPAND, 0); // left side
  hbox->Add(m_minimathctrl, 3, wxEXPAND | wxALL, 0); // right side

  //SetAutoLayout(true);
  SetSizer(hbox);
  hbox->Fit(this);
  hbox->SetSizeHints(this);
//  Layout();

  // DEBUG
  wxArrayString test;
  test.Add(wxT("Test variable1"));
  test.Add(wxT("Test variable2"));
  test.Add(wxT("Test variable3"));
  m_listbox->Set(test);
  /*
  wxFlexGridSizer * box = new wxFlexGridSizer(1);
  box->AddGrowableCol(0);
  box->AddGrowableRow(0);

  box->Add(m_history, 0, wxEXPAND | wxALL, 0);
  box->Add(m_regex, 0, wxEXPAND | wxALL, 1);

  SetSizer(box);
  box->Fit(this);
  box->SetSizeHints(this);
  */
}

Inspector::~Inspector()
{
}
