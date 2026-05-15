// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2026 Wolfgang Dautermann
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
  This file defines the class wxMaximaArtprovider - our own wxArtProvider with additional images.
*/

// only use that for recent wxWidgets versions
#include <wx/version.h>
#if wxCHECK_VERSION(3, 2, 0)

#include <wx/bmpbndl.h>


#include <wx/mstream.h>
#include <wx/zstream.h>
#include <wx/sstream.h>

#include "wxMaximaArtProvider.h"


#include "art/media-playback-start.h"
#include "art/media-playback-reverse.h"

#include "art/toolbar/arrow-up-square.h"
#include "art/toolbar/dialog-information.h"
#include "art/toolbar/eye-slash.h"
#include "art/toolbar/go-bottom.h"
#include "art/toolbar/go-jump.h"
#include "art/toolbar/go-last.h"
#include "art/toolbar/go-next.h"
#include "art/toolbar/gtk-preferences.h"
#include "art/toolbar/gtk-select-all.h"
#include "art/toolbar/gtk-stop.h"
#include "art/toolbar/input.h"
#include "art/toolbar/media-playback-stop.h"
#include "art/toolbar/software-update-urgent.h"
#include "art/toolbar/text.h"
#include "art/toolbar/view-refresh1.h"

#include "art/config/document-export.h"
#include "art/config/edit-copy_backup.h"
#include "art/config/edit-copy-confdialogue.h"
#include "art/config/editing.h"
#include "art/config/maxima.h"
#include "art/config/options.h"
#include "art/config/styles.h"
#include "art/config/view-refresh.h"

// Used to gunzip the (gzip compressed) SVG in Memory
wxString wxMaximaArtProvider::gunzip(unsigned char * svg_gz, size_t svg_gz_size)
{
    wxMemoryInputStream memIn(svg_gz, svg_gz_size);
    wxZlibInputStream gzipInput(memIn, wxZLIB_GZIP);
    // Output to a string and return the result as wxString
    wxStringOutputStream stringOut;
    stringOut.Write(gzipInput);
    return stringOut.GetString();
}

wxBitmapBundle wxMaximaArtProvider::CreateBitmapBundle(const wxArtID& id,
                                    const wxArtClient& client,
                                    const wxSize& size = wxDefaultSize)
{
  wxSize artsize = size;
  if (artsize == wxDefaultSize) artsize = GetSizeHint(client);

  if (id == wxmaximaART_MEDIA_PLAYBACK_START) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(MEDIA_PLAYBACK_START_SVG_GZ, MEDIA_PLAYBACK_START_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_MEDIA_PLAYBACK_REVERSE) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(MEDIA_PLAYBACK_REVERSE_SVG_GZ, MEDIA_PLAYBACK_REVERSE_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_ARROW_UP_SQUARE) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(ARROW_UP_SQUARE_SVG_GZ, ARROW_UP_SQUARE_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_DIALOG_INFORMATION) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(DIALOG_INFORMATION_SVG_GZ, DIALOG_INFORMATION_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_EYE_SLASH) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(EYE_SLASH_SVG_GZ, EYE_SLASH_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GO_BOTTOM) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GO_BOTTOM_SVG_GZ, GO_BOTTOM_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GO_JUMP) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GO_JUMP_SVG_GZ, GO_JUMP_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GO_LAST) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GO_LAST_SVG_GZ, GO_LAST_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GO_NEXT) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GO_NEXT_SVG_GZ, GO_NEXT_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GTK_PREFERENCES) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GTK_PREFERENCES_SVG_GZ, GTK_PREFERENCES_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GTK_SELECT_ALL) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GTK_SELECT_ALL_SVG_GZ, GTK_SELECT_ALL_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_GTK_STOP) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(GTK_STOP_SVG_GZ, GTK_STOP_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_INPUT) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(INPUT_SVG_GZ, INPUT_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_MEDIA_PLAYBACK_STOP) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(MEDIA_PLAYBACK_STOP_SVG_GZ, MEDIA_PLAYBACK_STOP_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_SOFTWARE_UPDATE_URGENT) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(SOFTWARE_UPDATE_URGENT_SVG_GZ, SOFTWARE_UPDATE_URGENT_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_TEXT) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(TEXT_SVG_GZ, TEXT_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_VIEW_REFRESH1) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(VIEW_REFRESH1_SVG_GZ, VIEW_REFRESH1_SVG_GZ_SIZE).mb_str(), artsize);

  } else if (id == wxmaximaART_CONFIG_DOCUMENT_EXPORT) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(DOCUMENT_EXPORT_SVG_GZ, DOCUMENT_EXPORT_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_EDIT_COPY_BACKUP) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(EDIT_COPY_BACKUP_SVG_GZ, EDIT_COPY_BACKUP_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_EDIT_COPY_CONFDIALOGUE) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(EDIT_COPY_CONFDIALOGUE_SVG_GZ, EDIT_COPY_CONFDIALOGUE_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_EDITING) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(EDITING_SVG_GZ, EDITING_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_MAXIMA) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(MAXIMA_SVG_GZ, MAXIMA_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_OPTIONS) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(OPTIONS_SVG_GZ, OPTIONS_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_STYLES) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(STYLES_SVG_GZ, STYLES_SVG_GZ_SIZE).mb_str(), artsize);
  } else if (id == wxmaximaART_CONFIG_VIEW_REFRESH) {
    return wxBitmapBundle::FromSVG(wxMaximaArtProvider::gunzip(VIEW_REFRESH_SVG_GZ, VIEW_REFRESH_SVG_GZ_SIZE).mb_str(), artsize);
  } else {
    return wxNullBitmap;
  }
}
#endif
