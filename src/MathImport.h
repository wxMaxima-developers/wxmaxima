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
  Turns maths written in LaTeX or in MathML into a Maxima command
  (GH #932, GH #760).

  Neither LaTeX nor presentation MathML says what a formula *means*, only how
  it looks: a fraction bar, a raised exponent, a d in front of an x. So the
  conversion happens in two stages, and both input languages share the
  second one:

   1. A front end reads the input into a small **layout tree** (MathNode):
      rows of identifiers, numbers and operators, plus fractions, roots,
      sub-/superscripts, fences and tables. LaTeX and presentation MathML
      describe exactly these things, so both map onto it almost one to one.
   2. The **interpretation** (in MathImport.cpp) reads a meaning into that
      layout, which is where all the guessing lives: implicit
      multiplication, "\sin x" taking an argument without parentheses,
      d/dx being a derivative, the "dx" ending an integral, e^x being %e^x,
      and so on. It then prints the result with the fewest parentheses
      Maxima needs.

  The guesses are conservative: anything that has no unambiguous reading
  (a plus-minus sign, an accent, text) fails with a message saying what it
  was, instead of producing a command that silently means something else.
  The result is shown to the user in a code cell before anything is
  evaluated anyway.

  MathML that carries an `<annotation encoding="application/x-maxima">`
  (wxMaxima's own "Copy as MathML" adds one) needs no guessing at all:
  that annotation is the command, and is used as it is.

  Everything here is GUI-free and needs no Maxima, so it is unit-tested
  directly (test_MathImport).
*/

#ifndef MATHIMPORT_H
#define MATHIMPORT_H

#include <wx/string.h>

namespace wxm {

//! The outcome of converting a formula to a Maxima command
struct MathImportResult {
  //! The Maxima command, without a terminating ";". Empty on failure.
  wxString maxima;
  //! Why the conversion failed, readable by a user. Empty on success.
  wxString error;
  bool Ok() const { return error.IsEmpty(); }
};

//! Converts LaTeX maths, with or without $...$, \(...\) or \[...\] around it
MathImportResult LaTeXToMaxima(const wxString &latex);

//! Converts a MathML <math> element (presentation markup, or an annotation)
MathImportResult MathMLToMaxima(const wxString &mathml);

//! True if the text is a MathML document rather than anything else
bool LooksLikeMathML(const wxString &text);

//! Converts either: MathML if LooksLikeMathML(), LaTeX otherwise
MathImportResult MathToMaxima(const wxString &text);

} // namespace wxm

#endif // MATHIMPORT_H
