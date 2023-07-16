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
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"

wxBitmap ArtProvider::GetImage(wxWindow *win, wxString name, int width,
			       unsigned const char *data, size_t dataLen) {  
  wxBitmap bmp = wxArtProvider::GetBitmap(name, wxART_TOOLBAR,
                                          wxSize(width * 4, width * 4));
  wxImage img;
  
  if (bmp.IsOk()) {
    img = bmp.ConvertToImage();
  }
  if (img.IsOk())
    {
      img.Rescale(width, width, wxIMAGE_QUALITY_BICUBIC);
#if defined __WXOSX__
      int scaleFactor = win->GetContentScaleFactor();
      if(scaleFactor < 1)
	scaleFactor = 1;
      if(scaleFactor > 16)
	scaleFactor = 16;
      
      bmp = wxBitmap(img, wxBITMAP_SCREEN_DEPTH, scaleFactor);
#else
      bmp = wxBitmap(img, wxBITMAP_SCREEN_DEPTH);
#endif
    }
  if(!bmp.IsOk())
    bmp = SvgBitmap(win, data, dataLen, width, width);

#if defined __WXOSX__
  int scaleFactor = win->GetContentScaleFactor();
  if(scaleFactor < 1)
	scaleFactor = 1;
      if(scaleFactor > 16)
	scaleFactor = 16;
#endif

  if(!bmp.IsOk())
#if defined __WXOSX__
      
    bmp = wxBitmap(wxSize(width, width), wxBITMAP_SCREEN_DEPTH, scaleFactor);
#else
    bmp = wxBitmap(wxSize(width, width), wxBITMAP_SCREEN_DEPTH);
#endif
  return bmp;
}
