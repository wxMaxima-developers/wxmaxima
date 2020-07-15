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

#ifndef BITMAPOUT_H
#define BITMAPOUT_H

#include "OutCommon.h"

/*! Renders portions of the work sheet (including 2D maths) as bitmap.

   This is used for exporting HTML with embedded maths as bitmap
   and for putting bitmaps for the clipboard
 */
class BitmapOut final
{
public:
  /*! The constructor.

    \param scale By which factor the resolution should be increased in respect
           to the default 755 DPI?
    \param configuration A pointer to the pointer to this worksheet's configuration
           storage
  */
  explicit BitmapOut(Configuration **configuration, int scale = 1);
  ~BitmapOut();

  /*! Renders tree as bitmap
    
    \param tree The list of cells that is to be rendered
    \param maxSize maxSize tells the maximum size [in square pixels] that will be rendered. 
           -1 means: No limit.

    \return true, if the bitmap could be created.
   */
  bool SetData(std::unique_ptr<Cell> &&tree, long int maxSize = -1);

  /*! Exports this bitmap to a file

    \return The size of the bitmap in millimeters. Sizes <0 indicate that the export has failed.
   */
  wxSize ToFile(wxString file);

  //! Returns the bitmap representation of the list of cells that was passed to SetData()
  wxBitmap GetBitmap() const { return m_bmp; }

  //! Copies the bitmap representation of the list of cells that was passed to SetData()
  bool ToClipboard();

private:
  std::unique_ptr<Cell> m_tree;
  OutCommon m_cmn;
  wxBitmap m_bmp;
  wxMemoryDC m_dc;

  bool Layout(long int maxSize = -1);
  void Draw();
};

#endif // BITMAPOUT_H
