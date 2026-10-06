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
  The sections the main toolbar is made of, and the order they are shown in.

  The toolbar is built from a list of sections -- groups of buttons that
  belong together, like "Open and Save" or the evaluation buttons -- in an
  order the user can change in the configuration dialogue. Every section can
  also be hidden.

  This file is pure bookkeeping, without any GUI and without the config file,
  so that it can be tested directly: what ToolBar::AddTools() does with it
  lives in ToolBar.cpp, and what the user sees of it in ConfigDialogue.cpp.
*/

#ifndef WXMAXIMA_TOOLBARSECTIONS_H
#define WXMAXIMA_TOOLBARSECTIONS_H

#include <vector>
#include <wx/string.h>

namespace ToolBarSections {

/*! A section of the main toolbar

  The numerical values are never stored anywhere (the config file holds
  Key()s instead), so new sections can be inserted at any position.
*/
enum class Section {
  New,           //!< The "New document" button
  OpenSave,      //!< The "Open" and "Save" buttons
  Print,         //!< The "Print" button
  UndoRedo,      //!< The "Undo" and "Redo" buttons
  Options,       //!< The button that opens the configuration dialogue
  CopyPaste,     //!< "Cut", "Copy" and "Paste"
  SelectAll,     //!< The "Select all" button
  Search,        //!< The "Find and replace" button
  MaximaControl, //!< "Restart Maxima", "Interrupt" and "Follow"
  Evaluate,      //!< The four evaluation buttons
  HideCode,      //!< The "Hide code" button
  CellStyle,     //!< The drop-down list that sets the type of the current cell
  TextFormat,    //!< Bold, italic, underline and strikethrough for text cells
  Animation,     //!< The animation start/stop button and the animation slider
  FlexibleSpace, //!< Empty space that pushes everything after it to the right
  Help           //!< The "Help" button
};

//! All sections, in the order a new installation shows them in
const std::vector<Section> &DefaultOrder();

//! The name a section is stored under in the config file. Never translated.
wxString Key(Section section);

//! The config key that says if a section is shown
wxString VisibilityConfigKey(Section section);

//! Is this section shown if the user never said otherwise?
bool ShownByDefault(Section section);

/*! Turns a stored order into a complete one

  Unknown names (from a newer wxMaxima, or a typo in a hand-edited config
  file) and duplicates are dropped. Sections the stored order doesn't
  mention (because it was written by a wxMaxima that didn't know them yet)
  are inserted after the section they follow in DefaultOrder(), so a new
  section shows up next to its neighbours instead of at the very end.
  The result therefore always contains every section exactly once.

  \param stored A comma-separated list of Key()s; empty means "the default".
*/
std::vector<Section> ParseOrder(const wxString &stored);

//! The comma-separated list of Key()s ParseOrder() reads back
wxString OrderToString(const std::vector<Section> &order);

/*! Does the toolbar need a separator between these two neighbouring sections?

  Sections that belong together (New, Open/Save; Cut/Copy/Paste and Select
  all; the cell type and the text formatting and animation controls) aren't
  separated, which is how the toolbar always looked. Nothing is separated
  from the flexible space, which already separates.
*/
bool SeparatorBetween(Section left, Section right);

} // namespace ToolBarSections

#endif // WXMAXIMA_TOOLBARSECTIONS_H
