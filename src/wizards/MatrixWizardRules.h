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
  The command rules of the Matrix menu's wizards (GH #647).

  Kept apart from the menu handlers so that a test can check which field ends
  up where: "Generate Matrix from Expression" used to ask for a width and a
  height, which users read the wrong way round, and its fields now say "Rows"
  and "Columns" -- in the order "Enter Matrix" asks for them.
*/

#ifndef MATRIXWIZARDRULES_H
#define MATRIXWIZARDRULES_H

#include <wx/string.h>

namespace wxm {
  /*! The rule of the "Generate Matrix from Expression" wizard

    The fields are, in this order: #1# the expression each element is
    computed from, #2# the row index variable, #3# the column index variable,
    #4# the number of rows, #5# the number of columns. A matrix is a list of
    rows, so the outer makelist() runs over the rows.
  */
  inline wxString GenMatrixFromExpressionRule() {
    return wxS("apply('matrix,makelist(makelist(#1#,#3#,1,#5#),#2#,1,#4#));");
  }

  /*! The rule of a wizard that applies a one-argument function to a matrix

    The "Classic matrix operations" (invert(), determinant(), ...) used to
    insert their command with the selection, or %, as the argument, with no
    way to name the matrix before the command was sent. Their wizard's only
    field, #1#, is that matrix.
  */
  inline wxString MatrixFunctionRule(const wxString &maximaFunction) {
    return maximaFunction + wxS("(#1#);");
  }
}

#endif // MATRIXWIZARDRULES_H
