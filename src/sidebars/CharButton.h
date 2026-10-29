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
#include "precomp.h"
#include "StringUtils.h"
#include "Configuration.h"
#include <wx/panel.h>
#include <wx/stattext.h>

/*! This class generates a pane containing the last commands that were issued.

 */
class CharButton : public wxPanel
{
public:
  //! A definition of this button, used to construct it.
  struct Definition
  {
    /*! The symbol this button enters.

      Usually one character, but can be a letter with combining accents
      (t̄ is t + U+0304), see wxm::SplitIntoSymbols().
    */
    wxString symbol;
    const wxString description = wxm::emptyString;  ///< The help text for the symbol
    bool matchesMaximaCommand = false;              ///< Whether this symbol is automatically translated into a maxima command/operator
  };

  /*! A flat, compact button for the greek and the symbols pane

    \param parent The parent panel/window
    \param worksheet The worksheet this button sends char events to
    \param def The definition of the button
  */
  CharButton(wxWindow *parent, wxWindow *worksheet, Configuration *config,
             const Definition &def,
             bool forceShow = false);
  wxWindow *GetTextObject() const { return m_buttonText; }
  //! The symbol this button enters
  const wxString &GetSymbol() const { return m_symbol; }
  /*! The button a mouse event in a sidebar happened on, or nullptr.

    The event may come from the button or from the text inside it.
   */
  static CharButton *FromEventObject(wxObject *object);
  /*! Asks for the keyboard shortcut that types \p symbol and stores it.

    Returns whether the shortcut changed.
  */
  static bool EditShortcut(wxWindow *parent, Configuration *configuration,
                           const wxString &symbol);
  /*! Adds the "Keyboard shortcut for..." entry to a sidebar's context menu

    if the right click that opens it was on a button. Returns the symbol of
    that button, which the sidebar keeps until the entry is chosen, or an
    empty string.
  */
  static wxString AppendShortcutMenuItem(wxMenu *menu, const wxMouseEvent &event);
  /*! Shows the symbol's keyboard shortcut, if it has one, in the tooltip.

    Also runs whenever the mouse enters the button, so the tooltip is never
    behind a shortcut changed elsewhere.
  */
  void UpdateToolTip();
  //! Emit this button's symbol to the worksheet -- the button's action, shared
  //! by the mouse handler and the accessibility "Press" default action.
  void ActivateButton();
#if wxUSE_ACCESSIBILITY
  //! The screen-reader label for this button: its description, or, if it has
  //! none, the symbol glyph itself.
  wxString GetAccessibleLabel() const {
    return m_description.IsEmpty() ? m_symbol : m_description;
  }
#endif
  /*! Sets the min size the button needs to show its symbol.

    Called from the constructor, so the size is known before the button ever
    receives a size event, and again on every size event, in case the font or
    the scale factor has changed since.
  */
  void UpdateMinSize();
protected:
  wxString m_symbol;
  Configuration *m_configuration;
  void CharButtonPressed(wxMouseEvent &event);
  void OnSize(wxSizeEvent &event);
  void OnIdleEvent(wxIdleEvent &event);
  void MouseOverPanel(wxMouseEvent &event);
  void MouseOverText(wxMouseEvent &event);
  void MouseLeftPanel(wxMouseEvent &event);
  void MouseLeftText(wxMouseEvent &event);
protected:
  void MouseOverTextIs(bool mouseOver = true);
  void MouseOverPanelIs(bool mouseOver = true);
  wxString m_description;
  //! The tooltip without the keyboard shortcut
  wxString m_toolTipBase;
  //! The tooltip as last set
  wxString m_toolTip;

private:
  bool m_mouseOverPanel = false;
  bool m_mouseOverText = false;
  bool m_backgroundColorChangeNeeded = false;
  wxStaticText *m_buttonText;
  wxWindow *m_worksheet;
};

#endif // CHARBUTTON_H
