// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef IMGCELLBASE_H
#define IMGCELLBASE_H

#include <memory>
#include "Cell.h"
#include <wx/image.h>
#include "Image.h"

#include <wx/filesys.h>
#include <wx/fs_arc.h>

/* ! The base class of ImgCell and Animation

   Images and animation share most of the functionality with each other => we
   can generate a class ImgCellBase that allows us to create pointers that both
   can point to an ImgCell and an Animation and that can use the methods common
   to both
*/
class ImgCellBase : public Cell
{
public:
    ImgCellBase(GroupCell *group, Configuration *config);

    virtual std::unique_ptr<Cell> Copy(GroupCell *group) const override = 0;
    virtual const CellTypeInfo &GetInfo() override = 0;
    virtual ~ImgCellBase() override;

    //! This class can be derived from wxAccessible which has no copy constructor
    ImgCellBase &operator=(const ImgCellBase&) = delete;

    //! The name of the file with gnuplot commands that created this file
    virtual wxString GnuplotSource() const override = 0;

    //! Set the image's resolution
    virtual void SetPPI(int ppi) = 0;
    virtual int GetPPI() const = 0;
    virtual size_t GetOriginalWidth() const = 0;
    virtual size_t GetOriginalHeight() const = 0;

    //! Can this image be exported in SVG format?
    virtual bool CanExportSVG() const = 0;

    friend class AnimationCell;

    /*! Writes the image to a file

      The image file that is written is either a bit-per-bit copy of the original
      file loaded into the ImgCellybase - or in the case that there is no original file
      a losslessly compressed png version of the bitmap.

      See also GetExtension().
    */
    virtual wxSize ToImageFile(wxString filename)  = 0;

    /*! Removes the cached scaled image from memory

      The scaled version of the image will be recreated automatically once it is
      needed.
    */
    virtual void ClearCache() override = 0;

    virtual const wxString &GetToolTip(wxPoint point) const override = 0;

    //! Copies the cell to the system's clipboard
    virtual bool CopyToClipboard() const override = 0;

    //! Returns the file name extension that matches the image type
    virtual wxString GetExtension() const = 0;

    virtual wxCoord GetMaxWidth() const = 0;
    virtual wxCoord GetHeightList() const = 0;
    virtual void SetMaxWidth(wxCoord width) = 0;
    virtual void SetMaxHeight(wxCoord height) = 0;

    virtual void Recalculate(AFontSize fontsize) override = 0;

    virtual void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override = 0;

    virtual wxString ToMatlab() const override = 0;
    virtual wxString ToRTF() const override = 0;
    virtual wxString ToString() const override = 0;
    virtual wxString ToTeX() const override = 0;
    virtual wxString ToXML() const override = 0;

    virtual bool CanPopOut() const override = 0;
};

#endif // IMGCELLBASE_H
