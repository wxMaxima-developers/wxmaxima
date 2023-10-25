// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include "CompositeDataObject.h"

CompositeDataObject::CompositeDataObject() {}

CompositeDataObject::~CompositeDataObject() {}

void CompositeDataObject::Add(wxDataObject *object, bool preferred) {
    if (!object)
        return;

    // Check if the object already exists
    for (auto &entry : m_entries)
        if (entry.object.get() == object)
            return;

    std::shared_ptr<wxDataObject> objPtr{object};

    std::vector<wxDataFormat> addedFormats(object->GetFormatCount());
    object->GetAllFormats(addedFormats.data());

    if (preferred && !addedFormats.empty())
        SetPreferredFormat(addedFormats.front());

    // Check if any prior entries have some of the added formats, and replace
    // their objects if necessary.
    for (auto &priorEntry : m_entries)
        for (auto addedFormat = addedFormats.begin();
             addedFormat != addedFormats.end();) {
            if (priorEntry.format == *addedFormat) {
                priorEntry.format = *addedFormat;
                priorEntry.object = objPtr;
                addedFormat = addedFormats.erase(addedFormat);
                continue;
            }
            ++addedFormat;
        }

    // Add all remaining formats
    for (auto &addedFormat : addedFormats)
        // cppcheck-suppress useStlAlgorithm
        m_entries.emplace_back(addedFormat, objPtr);
}

wxDataObject *
CompositeDataObject::GetObject(const wxDataFormat &format,
                               wxDataObjectBase::Direction dir) const {
    if (!(dir & wxDataObject::Get))
        return 0;

    for (auto &entry : m_entries)
        // cppcheck-suppress useStlAlgorithm
        if (entry.format == format)
            return entry.object.get();

    return {};
}

wxDataFormat CompositeDataObject::GetPreferredFormat(Direction dir) const {
    return (dir & wxDataObject::Get) ? m_preferredFormat : wxDataFormat();
}

void CompositeDataObject::SetPreferredFormat(const wxDataFormat &format) {
    m_preferredFormat = format;
}

std::size_t CompositeDataObject::GetFormatCount(Direction dir) const {
    return (dir & wxDataObject::Get) ? m_entries.size() : 0;
}

void CompositeDataObject::GetAllFormats(wxDataFormat *formats,
                                        Direction dir) const {
    if (!(dir & wxDataObject::Get))
        return;

    for (auto const &entry : m_entries)
        *formats++ = entry.format;
}

std::size_t CompositeDataObject::GetDataSize(const wxDataFormat &format) const {
    for (auto &entry : m_entries)
        // cppcheck-suppress useStlAlgorithm
        if (entry.format == format)
            return entry.object->GetDataSize(format);

    return 0;
}

bool CompositeDataObject::GetDataHere(const wxDataFormat &format,
                                      void *buf) const {
    for (auto &entry : m_entries)
        // cppcheck-suppress useStlAlgorithm
        if (entry.format == format)
            return entry.object->GetDataHere(format, buf);

    return false;
}

#ifdef __WXMSW__

const void *CompositeDataObject::GetSizeFromBuffer(const void *buffer,
                                                   std::size_t *size,
                                                   const wxDataFormat &format) {
    if (!size)
        return {};

    auto *object = GetObject(format);
    if (!object) {
        *size = 0;
        return {};
    }

    return object->GetSizeFromBuffer(buffer, size, format);
}

void *CompositeDataObject::SetSizeInBuffer(void *buffer, std::size_t size,
                                           const wxDataFormat &format) {
    auto *object = GetObject(format);
    return object ? object->SetSizeInBuffer(buffer, size, format) : nullptr;
}

std::size_t CompositeDataObject::GetBufferOffset(const wxDataFormat &format) {
    auto *object = GetObject(format);
    return object ? object->GetBufferOffset(format) : 0;
}

#endif // __WXMSW__
