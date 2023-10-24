// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef SVGOUT_H
#define SVGOUT_H

#include "OutCommon.h"
#include "precomp.h"
#include <wx/dcsvg.h>
#include <memory>

/* Renders portions of the work sheet (including 2D maths) as svg.

   This is used for exporting HTML with embedded maths as a scalable vector
   graphics and for them on the clipboard
*/
class Svgout final
{
public:
    /*! The constructor.
     */
    explicit Svgout(Configuration **configuration, const wxString &filename = {}, double scale = 1.0);
    explicit Svgout(Configuration **configuration, std::unique_ptr<Cell> &&tree,
                    const wxString &filename = {}, double scale = 1.0);
    ~Svgout();

    /*! Renders tree as svg

      \param tree The list of cells that is to be rendered
      \return true, if the svgout could be created.
    */
    wxSize Render(std::unique_ptr<Cell> &&tree);

    wxSize GetSize() const { return m_size; }
    bool IsOk() const { return m_isOk; }

    //! Copies the svg representation of the list of cells that was passed to SetData()
    bool ToClipboard();

    //! Returns the svg representation in a format that can be placed on the clipBoard.
    std::unique_ptr<wxCustomDataObject> GetDataObject();

private:
    std::unique_ptr<Cell> m_tree;
    OutCommon m_cmn;
    wxSVGFileDC m_recalculationDc;
    wxSize m_size = wxDefaultSize;
    bool m_isOk = false;

    /*! The current working directory we were in when we started creating a svg file

      wxWidgets tends to place bitmaps it links to svg files in its current working
      directory, not in the dir of the .svg file so we temporarily switch the working
      directory.
    */
    wxString m_CWD;

    bool Layout();
};

#endif // SVGOUT_H
