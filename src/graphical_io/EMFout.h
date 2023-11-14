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

#ifndef EMFOUT_H
#define EMFOUT_H

#include "precomp.h"
#include "OutCommon.h"

#if wxUSE_ENH_METAFILE
#include <wx/msw/enhmeta.h>
#include <memory>

//! Renders portions of the work sheet (including 2D maths) as extended Metafile.
class Emfout final
{
public:
  explicit Emfout(Configuration **configuration, const wxString &filename = {});
  explicit Emfout(Configuration **configuration, std::unique_ptr<Cell> &&tree, const wxString &filename = {});
  ~Emfout();

  /*! Renders tree as emf

    \param tree The list of cells that is to be rendered
    \return true, if the emfout could be created.
  */
  wxSize Render(std::unique_ptr<Cell> &&tree);

  wxSize GetSize() const { return m_size; }
  bool IsOk() const { return m_isOk; }

  //! Copies the emf representation of the list of cells that was passed to SetData()
  bool ToClipboard();

  //! Returns the emf representation in a format that can be placed on the clipBoard.
  std::unique_ptr<wxEnhMetaFileDataObject> GetDataObject() const;

private:
  std::unique_ptr<Cell> m_tree;
  OutCommon m_cmn;
  //! The draw context we draw to during recalculation.
  wxEnhMetaFileDC m_recalculationDc;
  //! The most recently rendered metafile - used to paste to clipboard.
  std::unique_ptr<wxEnhMetaFile> m_metaFile;
  wxSize m_size = wxDefaultSize;
  bool m_isOk = false;

  bool Layout();
};

#endif // wxUSE_ENH_METAFILE
#endif // EMFOUT_H
