// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//            (C) 2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "Wireshark.h"

#include <wx/sizer.h>
#include <wx/regex.h>

Wireshark::Wireshark(wxWindow* parent, int id) : wxTextCtrl(parent,id,wxEmptyString,wxDefaultPosition,wxDefaultSize,wxTE_READONLY | wxTE_RICH | wxHSCROLL | wxTE_MULTILINE)
{
}

Wireshark::~Wireshark()
{
}

void Wireshark::Add(wxString text)
{
  text.Replace(wxT(">"),wxT(">\n"));
  AppendText(text);
}
