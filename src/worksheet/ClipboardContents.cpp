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

/*! \file
  Clipboard formats that are only rendered once a program asks for them (GH #2030).
*/

#include "ClipboardContents.h"
#include "cells/Cell.h"
#include "cells/GroupCell.h"
#include <cstring>

ClipboardSnapshot::ClipboardSnapshot(Configuration *configuration,
                                     const Cell *cells)
  : m_configuration(configuration),
    m_group(std::make_unique<GroupCell>(configuration, GC_TYPE_CODE)),
    m_cells(Cell::CopyList(m_group.get(), cells)) {}

ClipboardSnapshot::~ClipboardSnapshot() = default;

std::unique_ptr<Cell> ClipboardSnapshot::CopyCells() const {
  return Cell::CopyList(m_group.get(), m_cells.get());
}

LazyDataObject::LazyDataObject(
  const wxDataFormat &format,
  std::shared_ptr<const LazyValue<std::string>> data)
  : wxDataObjectSimple(format), m_data(std::move(data)) {}

size_t LazyDataObject::GetDataSize() const { return m_data->Get().size(); }

bool LazyDataObject::GetDataHere(void *buf) const {
  const std::string &data = m_data->Get();
  if (data.empty())
    return false;
  std::memcpy(buf, data.data(), data.size());
  return true;
}

bool LazyDataObject::SetData(size_t, const void *) { return false; }

LazyBitmapDataObject::LazyBitmapDataObject(
  std::shared_ptr<const LazyValue<wxBitmap>> bitmap)
  : m_bitmap(std::move(bitmap)) {}

bool LazyBitmapDataObject::Render() const {
  if (!m_rendered) {
    m_rendered = true;
    const wxBitmap &bmp = m_bitmap->Get();
    // Only hand over a valid bitmap: the ports' SetBitmap() convert it to
    // their clipboard representation (PNG, DIB, ...) right away.
    if (bmp.IsOk())
      // Never actually a const object: data objects are created with new
      // and owned by the clipboard. Only the interface that asks for the
      // data is const.
      const_cast<LazyBitmapDataObject *>(this)->SetBitmap(bmp);
  }
  return wxBitmapDataObject::GetBitmap().IsOk();
}

wxBitmap LazyBitmapDataObject::GetBitmap() const {
  Render();
  return wxBitmapDataObject::GetBitmap();
}

size_t LazyBitmapDataObject::GetDataSize() const {
  return Render() ? wxBitmapDataObject::GetDataSize() : 0;
}

bool LazyBitmapDataObject::GetDataHere(void *buf) const {
  return Render() && wxBitmapDataObject::GetDataHere(buf);
}

// The per-format overloads are what wxDataObjectComposite calls. They are
// routed through wxDataObjectSimple, which forwards them to the overloads
// above, rather than through wxBitmapDataObject, which not every port
// declares them in.
size_t LazyBitmapDataObject::GetDataSize(const wxDataFormat &format) const {
  return Render() ? wxDataObjectSimple::GetDataSize(format) : 0;
}

bool LazyBitmapDataObject::GetDataHere(const wxDataFormat &format,
                                       void *buf) const {
  return Render() && wxDataObjectSimple::GetDataHere(format, buf);
}

#if wxUSE_ENH_METAFILE
LazyEnhMetaFileDataObject::LazyEnhMetaFileDataObject(
  std::shared_ptr<const LazyValue<wxEnhMetaFile>> metafile)
  : m_metafile(std::move(metafile)) {}

bool LazyEnhMetaFileDataObject::Render() const {
  if (!m_rendered) {
    m_rendered = true;
    const wxEnhMetaFile &metafile = m_metafile->Get();
    if (metafile.IsOk())
      // See LazyBitmapDataObject::Render() on why the const_cast is safe.
      const_cast<LazyEnhMetaFileDataObject *>(this)->SetMetafile(metafile);
  }
  return wxEnhMetaFileDataObject::GetMetafile().IsOk();
}

wxEnhMetaFile LazyEnhMetaFileDataObject::GetMetafile() const {
  Render();
  return wxEnhMetaFileDataObject::GetMetafile();
}

size_t LazyEnhMetaFileDataObject::GetDataSize(const wxDataFormat &format) const {
  return Render() ? wxEnhMetaFileDataObject::GetDataSize(format) : 0;
}

bool LazyEnhMetaFileDataObject::GetDataHere(const wxDataFormat &format,
                                            void *buf) const {
  return Render() && wxEnhMetaFileDataObject::GetDataHere(format, buf);
}
#endif
