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
  Where the files a worksheet's XML refers to (images, gnuplot sources and
  data) can be read from.
*/

#ifndef ATTACHMENTSOURCE_H
#define ATTACHMENTSOURCE_H

#include <map>
#include <memory>
#include <string>
#include <wx/buffer.h>
#include <wx/string.h>

/*! The files a worksheet's XML refers to by name.

  The XML of a cell doesn't contain its images: an \c \<img\> only names a
  file, which a .wxmx keeps next to content.xml in the same zip archive. The
  XML wxMaxima puts on the clipboard (GH #2474) has no zip archive around
  it, so it carries the same files base64-encoded in an \c \<attachments\>
  element instead (see Format::CellsToXMLDocument()). This class hides
  which of the two the files come from from everything that reads them.

  An AttachmentSource is cheap to copy, and copies may be read from any
  thread: images and gnuplot sources are read by background tasks, which
  keep their own copy alive for as long as they need it.
*/
class AttachmentSource
{
public:
  //! The files, by name
  using Files = std::map<wxString, std::string>;

  //! No files at all: names refer to the file system.
  AttachmentSource() = default;

  //! The files stored in a .wxmx (that is: zip) archive
  static AttachmentSource FromWxmx(const wxString &wxmxFile);
  //! Files held in memory
  static AttachmentSource FromMemory(Files files);

  //! True if names refer to the file system instead of to attachments
  bool IsEmpty() const { return m_wxmxFile.IsEmpty() && !m_files; }

  /*! The contents of the file with this name

    \return the file's contents, or an empty buffer if there is no such file.
    The buffer is a fresh one, owned by the caller alone.
  */
  wxMemoryBuffer Read(const wxString &name) const;

private:
  //! The .wxmx archive the files are read from, if they come from one
  wxString m_wxmxFile;
  //! The files, if they are held in memory. Never changed once created.
  std::shared_ptr<const Files> m_files;
};

#endif // ATTACHMENTSOURCE_H
