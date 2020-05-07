// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef CHARBUTTON_H
#define CHARBUTTON_H

/*! \file

  This file contains the definition of the class Charbutton that allows to 
  select arbitrary unicode symbols.
 */
#include <wx/panel.h>

/*! This class generates a pane containing the last commands that were issued.

 */
class CharButton : public wxPanel
{
public:
  //! A definition of this button, used to construct it.
  struct Definition
  {
    wchar_t symbol = {};                 ///< The unicode symbol
    const wxString description = empty;  ///< The help text for the symbol
    bool matchesMaximaCommand = false;   ///< Whether this symbol is automatically translated into a maxima command/operator
    static const wxString empty;
  };

  /*! A flat, compact button for the greek and the symbols pane

    \param parent The parent panel/window
    \param def The definition of the button
  */
  CharButton(wxPanel *parent, wxWindow *worksheet, const Definition &def);
protected:
  void ForwardToParent(wxMouseEvent &event);
  void CharButtonPressed(wxMouseEvent &event);
  wchar_t m_char;
private:
  wxWindow *m_worksheet;
};

#endif // CHARBUTTON_H
