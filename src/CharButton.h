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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file

  This file contains the definition of the class Charbutton that allows to 
  select arbitrary unicode symbols.
 */
#include <UnicodeSidebar.h>
#include <wx/grid.h>

#ifndef CHARBUTTON_H
#define CHARBUTTON_H

/*! This class generates a pane containing the last commands that were issued.

 */
class CharButton : public wxPanel
{
public:
  /*! A flat, compact button for the greek and the symbols pane
    
    \param parent The parent panel/window
    \param ch The unicode symbol
    \param description The help text for the symbol
    \param matchesMaximaCommand true means that this symbol is automatically
    translated into a maxima command/operator
    
  */
  CharButton(wxPanel *parent, wxWindow *worksheet, wxChar ch, wxString description = wxEmptyString, bool matchesMaximaCommand = false);
protected:
  void ForwardToParent(wxMouseEvent &event);
  void CharButtonPressed(wxMouseEvent &event);
  wxChar m_char;
private:
  wxWindow *m_worksheet;
};

#endif // CHARBUTTON_H
