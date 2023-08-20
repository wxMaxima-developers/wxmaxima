// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class FindReplaceDialog

  This dialog represents the "find" and "replace" dialogue.
*/

#ifndef FINDREPLACEDIALOG_H
#define FINDREPLACEDIALOG_H

#include "precomp.h"
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/radiobut.h>
#include "FindReplacePane.h"

/*! The find+replace dialog
 */
class FindReplaceDialog : public wxDialog
{
public:
  FindReplaceDialog(wxWindow *parent, FindReplacePane::FindReplaceData *data,
                    const wxString &title,
                    FindReplaceDialog **m_pointerToDialogue = NULL,
                    int style = wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);

  ~FindReplaceDialog();
  //! Returns the standard wxFindReplaceData data structure
  wxFindReplaceData *GetData()
    { return m_contents->GetData(); }

  bool GetRegexSearch() const {return m_contents->GetRegexSearch();}

  //! Set the string we are currently searching for
  void SetFindString(wxString strng)
    { m_contents->SetFindString(strng); }

protected:
  //! Is called if this element looses or gets the focus
  void OnActivate(wxActivateEvent &WXUNUSED(event));

  //! We catch a few hot keys here as we don't provide a menu that could declare them
  void OnKeyDown(wxKeyEvent &WXUNUSED(event));

  /*! The contents of the dialog.

    The contents is split into a separate panel so we can easily make it dockable
    once dockable dialogues aren't this ugly any more.
  */
  FindReplacePane *m_contents;

private:
  FindReplaceDialog **m_pointerToDialogue;
  //! true means: The next Activation event is generated during construction
  bool m_activateDuringConstruction;
  /*! Allows to remember how wide the window was the last time it was used.

    I don't think it makes sense to keep this between sessions.
  */
  static wxSize m_windowSize;
  /*! Allows to remember how wide the window was the last time it was used.

    We don't keep this value between sessions because the user might change
    the screen (and therefore the screen resolution) between sessions. And
    putting a window off screen (where it is hard to grab and to move it)
    is possible at least on MSW.
  */

  static wxPoint m_windowPos;
};

#endif // FINDREPLACEDIALOG_H
