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

#ifndef MATRIXSCROLLHOST_H
#define MATRIXSCROLLHOST_H

#include <wx/defs.h>

class MatrCell;

/*! Whatever can give a matrix real scrollbars

  A matrix too large for the window can be shown in a viewport with native
  scrollbars (Configuration::OversizedMatrices::scroll). Cells are model
  objects, though: the same MatrCell is laid out for the screen, for
  printing and for every graphical export, and only one of those has a
  window a scrollbar could live in. So the cell never creates a widget
  itself. It only knows its viewport, its scroll offset and where its
  scrollbars belong, and tells the host each time it is drawn; the host
  -- the Worksheet's MatrixScrollbars -- owns the actual wxScrollBars.

  The host is reached through Configuration::GetMatrixScrollHost(), which
  only the worksheet's own configuration ever has set. A configuration for
  printing or exporting doesn't, and a matrix laid out under one elides its
  middle instead, since paper and image files can't scroll.
*/
class MatrixScrollHost
{
public:
  virtual ~MatrixScrollHost() = default;

  //! How thick a native scrollbar is, in worksheet pixels
  virtual wxCoord ScrollbarThickness() const = 0;

  /*! A matrix that needs scrollbars has just been drawn

    Called from MatrCell::Draw(), i.e. from inside the paint handler, so the
    host must not create, move or show windows here -- only note what is
    needed, and act once the paint is over.
  */
  virtual void MatrixDrawn(MatrCell *matrix) = 0;
};

#endif // MATRIXSCROLLHOST_H
