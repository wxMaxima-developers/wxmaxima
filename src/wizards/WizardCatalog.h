// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  The standard wizards of wxMaxima's menus, described as data.

  Most menu entries open the generic wizard (GenWizPanel): a few labelled
  fields and a command rule like "taylor(#1#,#2#,#3#,#4#);", in which #n# is
  replaced by the n-th field. Each of them used to need its own branch in a
  menu handler that passed up to 33 positional arguments, plus a Bind() in
  wxMaxima.cpp -- and a forgotten Bind() left a menu entry that silently did
  nothing ("Dito, and evaluate the result" in the Calculus menu did, until
  this catalog bound it).

  A wizard of that kind is now one WizardSpec in WizardCatalog(), and
  wxMaxima binds every ID the catalog names to one handler. Wizards that need
  more than text fields (checkboxes, choices, grids, file pickers) still have
  classes of their own.

  This file needs no GUI, so test_WizardCatalog can check every entry.
*/

#ifndef WIZARDCATALOG_H
#define WIZARDCATALOG_H

#include <wx/string.h>
#include <wx/defs.h>
#include <optional>
#include <vector>

namespace wxm {

  //! One field of a standard wizard
  struct WizardField {
    //! The text in front of the field
    wxString label = {};
    //! What the field contains when the wizard opens
    wxString defaultValue = {};
    //! The field's tooltip; may be empty
    wxString tooltip = {};
    /*! Whether the field starts with the expression the wizard is applied to

      That is the selection, or the current input or %, as
      wxMaxima::GetDefaultEntry() finds it -- known only once the wizard is
      opened, so the catalog can only say that it goes here.
    */
    bool takesSelection = false;
  };

  //! A field that starts with the given default value
  inline WizardField Field(const wxString &label, const wxString &defaultValue,
                           const wxString &tooltip = {}) {
    return {label, defaultValue, tooltip, false};
  }

  //! A field that starts with the expression the wizard is applied to
  inline WizardField SelectionField(const wxString &label,
                                    const wxString &tooltip = {}) {
    return {label, wxString(), tooltip, true};
  }

  /*! Everything the generic wizard needs to know about one menu command

    Every member has a default initializer: the catalog leaves out the ones
    a wizard doesn't need (designated initializers), and GCC's
    -Wmissing-field-initializers only stays quiet about an omitted member
    that has one.
  */
  struct WizardSpec {
    //! The menu entries and sidebar buttons that open this wizard
    std::vector<wxWindowID> ids = {};
    //! The wizard's caption
    wxString title = {};
    //! The text of the wizard's "Description" tab; may be empty
    wxString description = {};
    //! The tooltip of the "Description" tab; may be empty
    wxString descriptionTooltip = {};
    //! The Maxima command, with #n# standing for the n-th field
    wxString rule = {};
    //! The fields, at most GenWizPanel's 10
    std::vector<WizardField> fields = {};
  };

  //! What a wizard's command depends on besides its fields
  struct WizardContext {
    //! Whether Maxima has loaded the "fourie" package already
    bool fourierLoaded = false;
    /*! What the titles of the numerical integration wizards start with

      The integral sign, or "integrate" if the GUI font cannot draw it.
    */
    wxString integralSign = wxS("\u222B");
  };

  /*! All standard wizards

    Built anew on every call, rather than once at static initialization, so
    that _() translates into the language wxMaxima runs in. The strings are
    the same msgids the menu handlers used before, so moving a wizard here
    leaves its translations intact.
  */
  std::vector<WizardSpec> WizardCatalog(const WizardContext &context = {});

  //! The standard wizard \p id opens, if there is one
  std::optional<WizardSpec> FindWizard(wxWindowID id,
                                       const WizardContext &context = {});

  /*! \p spec's fields with \p selection filled into the ones that take it

    GenWizPanel gets only plain default values.
  */
  std::vector<WizardField> FieldsWithSelection(const WizardSpec &spec,
                                               const wxString &selection);
}

#endif // WIZARDCATALOG_H
