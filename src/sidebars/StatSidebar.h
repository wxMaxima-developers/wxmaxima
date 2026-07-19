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
#ifndef STATSIDEBAR_H
#define STATSIDEBAR_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/button.h>
#include "EventIDs.h"
#include <wx/sizer.h>
#include <wx/panel.h>

class Buttonwrapsizer;

class StatSidebar : public wxScrolled<wxPanel>
{
public:
  explicit StatSidebar(wxWindow *parent, int ID = wxID_ANY);

  /*! Grow the virtual height to the wrapped button rows

    Public so tests can drive it directly: under a headless wxGTK without an
    event loop SetSize() does not deliver wxEVT_SIZE synchronously.
  */
  void UpdateVirtualSize();

protected:
  //! Calls UpdateVirtualSize() whenever the sidebar's size changes
  void OnSize(wxSizeEvent &event);

private:
  //! The wrapping sizer holding the Mean/Median/Variance/Deviation buttons
  Buttonwrapsizer *m_wrapSizer = {};
  //! The last virtual size we set, to only re-set it on real changes
  wxSize m_lastVirtualSize;
};

#endif // STATSIDEBAR_H
