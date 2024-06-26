// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class ToolBar that represents wxMaxima's main tool bar.
*/

#include <wx/bitmap.h>
#include <wx/window.h>
#if wxCHECK_VERSION(3, 2, 0)
#include <wx/bmpbndl.h>
#endif

#ifndef _ARTPROVIDER_H
#define _ARTPROVIDER_H

class ArtProvider
{
public:
  ArtProvider(){}
  static wxBitmap GetImage(wxWindow *win, const wxString &name, int width,
                           unsigned const char *data,
                           std::size_t dataLen);
  static wxBitmap GetQuestionmarkBitmap(wxWindow *win, wxSize siz);
#if wxCHECK_VERSION(3, 2, 0)
  static wxBitmapBundle GetQuestionmarkBundle(){return m_questionmarkBundle;}
  static wxBitmapBundle GetDivideCellBundle(){return m_dividecellBundle;}
  static wxBitmapBundle GetAddToWatchlistBundle(){return m_addToWatchlistBundle;}
  static wxBitmapBundle GetCellMergeBundle(){return m_cellMergeBundle;}
private:
  static wxBitmapBundle m_questionmarkBundle;
  static wxBitmapBundle m_dividecellBundle;
  static wxBitmapBundle m_addToWatchlistBundle;
  static wxBitmapBundle m_cellMergeBundle;
#endif
};

#endif
