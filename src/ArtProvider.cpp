// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2023      Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*\file

  Generates all the bitmaps we need.
*/

#include "ArtProvider.h"
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/artprov.h>
#include "SvgBitmap.h"
#include "nanosvg_private.h"
#include "nanosvgrast_private.h"
#include "art/menu/Text-questionmark.h"
#include "art/menu/cell-divide.h"
#include "art/menu/watchlist.h"
#include "art/menu/cell-merge.h"

wxBitmap ArtProvider::GetImage(wxWindow *win, const wxString &name, int width,
                               unsigned const char *data, std::size_t dataLen) {
  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_TOOLBAR,
                                          wxSize(width * 4, width * 4));
  wxImage img;

  if (bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }
  if (img.IsOk())
    {
      img.Rescale(width, width, wxIMAGE_QUALITY_BICUBIC);
      bmp = wxBitmap(img, wxBITMAP_SCREEN_DEPTH);
    }
  if(!bmp.IsOk())
    bmp = SvgBitmap(win, data, dataLen, width, width);


  if(!bmp.IsOk())
    bmp = wxBitmap(wxSize(width, width), wxBITMAP_SCREEN_DEPTH);
  return bmp;
}

wxBitmap ArtProvider::GetQuestionmarkBitmap(wxWindow *win, wxSize siz)
{
  return GetImage(win, wxS("dialog-question"), siz.x,
                  TEXT_QUESTIONMARK_SVG, TEXT_QUESTIONMARK_SVG_SIZE);
}

#if wxCHECK_VERSION(3, 2, 0)
wxBitmapBundle ArtProvider::m_questionmarkBundle =
  wxBitmapBundle::FromSVG(reinterpret_cast<char *>(TEXT_QUESTIONMARK_SVG), wxSize(16, 16));
wxBitmapBundle ArtProvider::m_dividecellBundle =
  wxBitmapBundle::FromSVG(reinterpret_cast<char *>(CELL_DIVIDE_SVG), wxSize(16, 16));
wxBitmapBundle ArtProvider::m_addToWatchlistBundle =
  wxBitmapBundle::FromSVG(reinterpret_cast<char *>(WATCHLIST_SVG), wxSize(16, 16));
wxBitmapBundle ArtProvider::m_cellMergeBundle =
  wxBitmapBundle::FromSVG(reinterpret_cast<char *>(CELL_MERGE_SVG), wxSize(16, 16));
#endif

