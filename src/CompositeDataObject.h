// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@bertec.com>
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

#ifndef COMPOSITEDATAOBJECT_H
#define COMPOSITEDATAOBJECT_H

#include <wx/clipbrd.h>
#include <memory>
#include <vector>

/*!
 * \file The declaration of the CompositeDataObject class - a superset
 * of the wxDataObjectComposite functionality.
 */

//! A composite data object like wxDataObjectComposite, but accepts also
//! non-simple data objects. Only the Get direction is supported.
class CompositeDataObject final : public wxDataObject
{
public:
  CompositeDataObject();
  ~CompositeDataObject() override;

  void Add(wxDataObject *object, bool preferred = false);
  wxDataObject *GetObject(const wxDataFormat& format,
                                wxDataObjectBase::Direction dir = Get) const;
  wxDataFormat GetPreferredFormat(Direction dir=Get) const override;
  void SetPreferredFormat(const wxDataFormat &format);

  size_t GetFormatCount(Direction dir=Get) const override;
  void GetAllFormats(wxDataFormat *formats, Direction dir=Get) const override;
  size_t GetDataSize(const wxDataFormat &format) const override;
  bool GetDataHere(const wxDataFormat &format, void *buf) const override;

#ifdef __WXMSW__
  const void* GetSizeFromBuffer(const void* buffer, size_t* size,
                                const wxDataFormat& format) override;
  void* SetSizeInBuffer(void* buffer, size_t size, const wxDataFormat& format) override;
  size_t GetBufferOffset(const wxDataFormat& format) override;
#endif

private:
  struct Entry
  {
    wxDataFormat format;
    std::shared_ptr<wxDataObject> object;
    Entry(const wxDataFormat &format, std::shared_ptr<wxDataObject> object) :
        format(format), object(object) {}
  };
  std::vector<Entry> m_entries;
  wxDataFormat m_preferredFormat;
};

#endif // COMPOSITEDATAOBJECT_H
