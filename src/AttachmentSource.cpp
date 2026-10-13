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
#include "AttachmentSource.h"
#include <wx/wfstream.h>
#include <wx/zipstrm.h>

AttachmentSource AttachmentSource::FromWxmx(const wxString &wxmxFile) {
  AttachmentSource source;
  source.m_wxmxFile = wxmxFile;
  return source;
}

AttachmentSource AttachmentSource::FromMemory(Files files) {
  AttachmentSource source;
  source.m_files = std::make_shared<const Files>(std::move(files));
  return source;
}

wxMemoryBuffer AttachmentSource::Read(const wxString &name) const {
  wxMemoryBuffer result;
  if (m_files) {
    auto file = m_files->find(name);
    if (file != m_files->end())
      result.AppendData(file->second.data(), file->second.size());
    return result;
  }
  if (m_wxmxFile.IsEmpty())
    return result;

  wxFileInputStream wxmx(m_wxmxFile);
  if (!wxmx.IsOk())
    return result;
  wxZipInputStream zip(wxmx, wxConvLocal);
  // GetNextEntry() hands the caller ownership of each entry
  std::unique_ptr<wxZipEntry> entry;
  while (!zip.Eof()) {
    entry.reset(zip.GetNextEntry());
    if (!entry)
      return result;
    if (entry->GetName() == name)
      break;
  }
  if (!entry || (entry->GetName() != name))
    return result;
  // Read straight into the result's own (heap) storage: a chunk buffer big
  // enough to read efficiently is too big for the stack, which matters on
  // the background threads that load images.
  constexpr size_t chunkSize = 65536;
  while (zip.IsOk() && !zip.Eof()) {
    zip.Read(result.GetAppendBuf(chunkSize), chunkSize);
    result.UngetAppendBuf(zip.LastRead());
    if (zip.LastRead() == 0)
      break;
  }
  return result;
}
