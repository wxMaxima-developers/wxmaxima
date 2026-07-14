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
#ifndef SYMBOLSSIDEBAR_H
#define SYMBOLSSIDEBAR_H

#include "precomp.h"
#include <array>
#include <wx/wx.h>
#include "EventIDs.h"
#include <wx/sizer.h>
#include <wx/panel.h>
#include "CharButton.h"
#include "ButtonWrapSizer.h"
#include <list>


//cppcheck-suppress ODR
class SymbolsSidebar : public wxScrolled<wxPanel>
{
public:
  SymbolsSidebar(wxWindow *parent, Configuration *configuration, wxWindow *worksheet, int ID = wxID_ANY);
  //! Update the "user symbols" portion of the symbols pane.
  void UpdateUserSymbols();
  /*! Add the symbols to the "user symbols" portion of the symbols pane.

    If these symbols already are populated UpdateUserSymbols() is the right place to go. 
  */
  void AddUserSymbols();
  /*! Re-wrap the symbol buttons and grow the virtual height to fit the wrapped
    rows. Called on resize; public so a test can drive it without an event loop. */
  void UpdateVirtualSize();
protected:
  void OnMouseRightDown(wxMouseEvent &event);
  void OnMenu(wxCommandEvent &event);
  void OnSize(wxSizeEvent &event);
private:
  //! A panel that shows all user-defined symbols on the symbols pane.
  wxPanel *m_userSymbols;
  //! A button per user defined symbol
  std::list<wxWindow *> m_userSymbolButtons;
  Buttonwrapsizer *m_userSymbolsSizer;
  //! The panel holding the built-in symbol buttons.
  wxPanel *m_builtInSymbols;
  Buttonwrapsizer *m_builtInSymbolsSizer;
  Configuration *m_configuration;
  wxWindow *m_worksheet;
  //! The user symbols that are currently displayed
  wxString m_userSymbols_Last;
  //! The virtual size UpdateVirtualSize() last applied, so it can skip
  //! re-layouting when nothing changed (see the comment in its body).
  wxSize m_lastVirtualSize = wxDefaultSize;
};

#endif // SYMBOLSSIDEBAR_H
