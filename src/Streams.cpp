// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include "Streams.h"
#include <wx/file.h>
#include <wx/filesys.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>

stx::optional<wxMemoryBuffer> CompressData(const wxMemoryBuffer &input)
{
  wxMemoryBuffer retbuf;
  MemBufOutputStream ostream(retbuf);
  return CompressDataInto(input, ostream) ? stx::make_optional(retbuf) : stx::nullopt;
}

stx::optional<wxMemoryBuffer> DecompressData(const wxMemoryBuffer &input)
{
  wxMemoryBuffer retbuf;
  MemBufOutputStream ostream(retbuf);
  return DecompressDataInto(input, ostream) ? stx::make_optional(retbuf) : stx::nullopt;
}

bool CompressDataInto(const wxMemoryBuffer &input, wxOutputStream &ostream)
{
  if (ostream.IsOk())
  {
    auto zlib_flags = wxZlibOutputStream::CanHandleGZip() ? wxZLIB_GZIP : wxZLIB_ZLIB;
    MemBufInputStream mistream(input);
    wxZlibOutputStream zostream(ostream, wxZ_BEST_COMPRESSION, zlib_flags);
    zostream.Write(mistream);
    if (zostream.IsOk() && mistream.Eof())
      return true;
  }
  return false;
}

bool DecompressDataInto(const wxMemoryBuffer &input, wxOutputStream &ostream)
{
  if (ostream.IsOk())
  {
    MemBufInputStream mistream(input);
    wxZlibInputStream zistream(mistream);
    ostream.Write(zistream);
    if (ostream.IsOk() && zistream.Eof())
      return true;
  }
  return false;
}

stx::optional<wxMemoryBuffer> ReadAll(const wxString &fileName, wxFileSystem *filesystem)
{
#ifdef HAVE_OPENMP_TASKS
#pragma omp critical (OpenFSFile)
#endif
  std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(fileName));
  if (fsfile)
  { // open successful
    wxMemoryBuffer obuf;
    wxInputStream *istream = fsfile->GetStream();
    MemBufOutputStream mostream(obuf);
    istream->Read(mostream);
    if (istream->Eof() && mostream.IsOk())
      return obuf;
  }
  // Destroying fsfile is important: Otherwise, opening .wxmx files containing
  // hundreds of images might lead to a "too many open files" error.
  return {};
}

stx::optional<wxMemoryBuffer> ReadAll(wxFile &file)
{
  if (file.IsOpened())
  {
    wxFileInputStream istream(file);
    if (istream.IsOk())
    {
      wxMemoryBuffer obuf;
      MemBufOutputStream mostream(obuf);
      istream.Read(mostream);
      if (istream.Eof() && mostream.IsOk())
        return obuf;
    }
  }
  return {};
}
