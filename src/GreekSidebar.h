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
  This file declares the class wxMaximaFrame

  wxMaximaFrame draws everything which can be seen
  surrounding the worksheet.
*/
#ifndef GREEKSIDEBAR_H_
#define GREEKSIDEBAR_H_

#include "precomp.h"
#include <array>
#include <wx/wx.h>
#include "EventIDs.h"
#include <wx/sizer.h>
#include <wx/panel.h>
#include <CharButton.h>
#include "ButtonWrapSizer.h"
#include <list>

class GreekSidebar : public wxScrolled<wxPanel>
{
 public:
  GreekSidebar(wxWindow *parent, Configuration *configuration, wxWindow *worksheet,
               int ID = wxID_ANY);
 protected:
  void UpdateSymbols();
  void OnMouseRightDown(wxMouseEvent &event);
  void OnMenu(wxCommandEvent &event);
  void OnSize(wxSizeEvent &event);
 private:
  Configuration *m_configuration;
  wxSizer *m_lowercaseSizer;
  wxSizer *m_uppercaseSizer;
  wxWindow *m_worksheet;
};


#endif // GREEKSIDEBAR_H_
