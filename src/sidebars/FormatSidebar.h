// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  This file declares the class StatSidebar, that provides some statistics buttons.
*/
#ifndef FORMATSIDEBAR_H
#define FORMATSIDEBAR_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/button.h>
#include "EventIDs.h"
#include <wx/sizer.h>
#include <wx/panel.h>

class FormatSidebar : public wxScrolled<wxPanel>
{
public:
  FormatSidebar(wxWindow *parent, int ID = wxID_ANY);
};

#endif // FORMATSIDEBAR_H
