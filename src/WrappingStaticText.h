// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file declares all the wizards the draw sidepane needs.
 */

#ifndef WRAPPINGSTATICTEXT_H
#define WRAPPINGSTATICTEXT_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/stattext.h>

/*! A wxStaticText variant that auto-wraps, if necessary

  The idea to this class was taken from https://forums.wxwidgets.org/viewtopic.php?t=27850
 */
class WrappingStaticText : public wxPanel
{
public:
  WrappingStaticText(wxWindow* parent, int id, wxString text);
protected:
  void OnSize(wxSizeEvent &event);
private:
  wxString m_text;
  wxStaticText *m_textCtrl;
};

#endif // WRAPPINGSTATICTEXT_H
