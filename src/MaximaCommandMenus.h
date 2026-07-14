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
  The menu/toolbar command handlers wxMaxima binds to the "insert a Maxima
  command" menu and button events.

  These handlers translate a menu/button click into a Maxima command (often via
  a wizard dialog) and hand it to wxMaxima::MenuCommand(). They used to be
  members of the wxMaxima god class; they are being peeled off into this class,
  one menu at a time, to shrink wxMaxima.cpp. Each handler still drives wxMaxima
  (MenuCommand, the wizards, the worksheet) through the m_wxMaxima reference, so
  MaximaCommandMenus is a friend of wxMaxima; the handlers are bound directly to
  the MaximaCommandMenus instance the wxMaxima frame owns.
*/

#ifndef MAXIMACOMMANDMENUS_H
#define MAXIMACOMMANDMENUS_H

#include <wx/event.h>

class wxMaxima;

/*! The menu-command handlers extracted from the wxMaxima god class.

  Owned by value by the wxMaxima frame, which binds the relevant menu/button
  events straight to these handlers. Holds a reference back to that frame for the
  services the handlers need (MenuCommand, the command wizards, the worksheet).
*/
class MaximaCommandMenus
{
public:
  explicit MaximaCommandMenus(wxMaxima &wxm) : m_wxMaxima(wxm) {}

  //! Handles the "Plot" menu and its toolbar buttons (2D/3D plot wizards,
  //! plot-format wizard, animation autoplay/framerate).
  void PlotMenu(wxCommandEvent &event);

  //! Handles the "Calculus" menu and its toolbar buttons (integrate/diff/limit/
  //! sum/series wizards and the change-of-variable/Taylor/Laplace/... commands).
  void CalculusMenu(wxCommandEvent &event);

  //! Handles the "Numeric" menu (float/bfloat/engineering-format toggles and the
  //! numeric-integration / to-poly / continued-fraction command wizards).
  void NumericalMenu(wxCommandEvent &event);

  //! Handles the "Simplify" menu and its toolbar buttons (ratsimp/factor/expand/
  //! trig/rectform/... and the substitute/nouns/... command wizards).
  void SimplifyMenu(wxCommandEvent &event);

  //! Handles the "Equations" menu and its toolbar buttons (solve/solve-numerically,
  //! the algebraic/linear system wizards, ODEs, roots and the eliminate/... wizards).
  void EquationsMenu(wxCommandEvent &event);

  //! Handles the "Algebra"/matrix menu and its toolbar buttons (enter/generate
  //! matrix wizards, .csv import/export, and the matrix-operation command wizards).
  void MatrixMenu(wxCommandEvent &event);

  //! Handles the "List" menu (build/sort/map/… lists, .csv list import/export and
  //! the actual-values-storage wizard).
  void ListMenu(wxCommandEvent &event);

  //! Handles the "Variable/Function properties" menu (declare/remove properties,
  //! assume/forget, ... - almost entirely direct MenuCommand entries).
  void PropertiesMenu(wxCommandEvent &event);

  //! Handles the "Statistics" menu (mean/median/.../test wizards and the
  //! read-/write-matrix-from-.csv wizard).
  void StatsMenu(wxCommandEvent &event);

  //! Handles the "Draw" menu (2D/3D scene wizards plus the explicit/implicit/
  //! parametric/points/contour/... draw-parameter wizards).
  void DrawMenu(wxCommandEvent &event);

  //! Handles the "Help" menu (help browser/sidebar toggles, About/License/
  //! ChangeLog dialogs, tip-of-the-day, update check, apropos/example wizards).
  void HelpMenu(wxCommandEvent &event);

  //! Handles the "File" menu (open/save/save-as, export, load/batch package,
  //! compare-files diff frame, jump-to-UUID, animation start/stop).
  void FileMenu(wxCommandEvent &event);

  //! Handles the "Cell"/insert menu (cell insertion and type conversion,
  //! insert image, fold/unfold, auto-answer toggles, add-watch).
  void OnInsertMenu(wxCommandEvent &event);

  //! Handles the "Edit" menu (copy/cut/paste/undo/redo, the copy-as-image/
  //! MathML/RTF variants, zoom, display-digits/label toggles, preferences,
  //! find/replace, history navigation and the gnuplot pop-out).
  void EditMenu(wxCommandEvent &event);

  //! Handles the worksheet/table-of-contents right-click context menu (fold/
  //! unfold, image max-size/resolution/reload, section evaluation, copy-as,
  //! and the solve/plot/integrate/... quick wizards).
  void PopupMenu(wxCommandEvent &event);

  //! Handles the "Maxima" menu (restart/kill toggles, the debug/display/time
  //! options, the evaluate-all/-till-here actions and the large family of
  //! programming-, string-processing- and OS-command wizards).
  void MaximaMenu(wxCommandEvent &event);

  //! Handles a click on one of the dynamically-built demo-file menu items.
  void OnDemoFileMenu(wxCommandEvent &ev);

  //! Activates the selected input cell's editor for editing (popup "edit").
  void EditInputMenu(wxCommandEvent &event);

  //! Handles the "Print" menu item (prints the worksheet).
  void PrintMenu(wxCommandEvent &event);

private:
  //! The wxMaxima frame whose services (MenuCommand, wizards, worksheet) the
  //! handlers drive. Not owned; the frame owns this object.
  wxMaxima &m_wxMaxima;
};

#endif // MAXIMACOMMANDMENUS_H
