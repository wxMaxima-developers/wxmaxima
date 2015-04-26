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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

#ifndef BITMAP_H
#define BITMAP_H

#include "MathCell.h"

class Bitmap
{
public:
  Bitmap(int scale=1);
  ~Bitmap();
  void SetData(MathCell* tree);
  /*! Exports this bitmap to a file

    \return The size of the bitmap in millimeters. Sizes <0 indicate that the export has failed.
   */
  wxSize ToFile(wxString file);
  bool ToClipboard();
protected:
  void DestroyTree();
  void RecalculateWidths();
  void BreakLines();
  void RecalculateSize();
  void GetMaxPoint(int* width, int* height);
  void BreakUpCells();
  void Layout();
  void Draw();
  MathCell *m_tree;
  double GetRealHeight();
  double GetRealWidth();

private:
  //! How many times the natural resolution do we want this bitmap to be?
  int m_scale;
  wxBitmap m_bmp;
  //! The width of the current bitmap;
  long m_width;
  //! The height of the current bitmap;
  long m_height;
  //! The resolution of the bitmap.
  wxSize m_ppi;
  
};

#endif // BITMAP_H
