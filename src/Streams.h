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

/*! \file
 *
 * This file contains additional streams-related utilities.
 */

#ifndef WXMAXIMA_STREAMS_H
#define WXMAXIMA_STREAMS_H

#include "stx/optional.hpp"
#include <wx/buffer.h>
#include <wx/stream.h>
#include <wx/txtstrm.h>
#include <cstring>

//! An output stream that writes into a provided memory buffer.
class MemBufOutputStream final : public wxOutputStream
{
public:
  MemBufOutputStream(wxMemoryBuffer &buffer) : m_o_buf(buffer) {}
  wxFileOffset GetLength() const override { return m_o_buf.GetDataLen(); }

private:
  wxMemoryBuffer &m_o_buf;

  size_t OnSysWrite(const void *buffer, size_t nbytes) override
  {
    m_o_buf.AppendData(buffer, nbytes);
    return nbytes;
  }
  wxDECLARE_NO_COPY_CLASS(MemBufOutputStream);
};

//! An input stream that reads from a provided memory buffer.
class MemBufInputStream final : public wxInputStream
{
public:
  MemBufInputStream(const wxMemoryBuffer &buffer) : m_i_buf(buffer) {}
  wxFileOffset GetLength() const override { return m_i_buf.GetDataLen(); }
  size_t size() const { return m_i_buf.GetDataLen(); }

private:
  size_t m_pos = 0;
  const wxMemoryBuffer &m_i_buf;
  size_t OnSysRead(void *buffer, size_t size) override
  {
    if (!buffer)
      return (m_lastcount = 0);

    wxASSERT(m_pos <= this->size());
    ssize_t sizeDelta = (m_pos + size) - GetLength();
    m_lastcount = sizeDelta <= 0 ? size : size + sizeDelta;
    if (m_lastcount)
      memcpy(buffer, reinterpret_cast<const char*>(m_i_buf.GetData()) + m_pos, m_lastcount);
    m_pos += m_lastcount;

    wxASSERT(m_pos <= this->size());
    if (m_pos == this->size())
      m_lasterror = wxSTREAM_EOF;
    return m_lastcount;
  }
  wxDECLARE_NO_COPY_CLASS(MemBufInputStream);
};

//! Returns compressed data from a memory buffer, or no value on failure.
stx::optional<wxMemoryBuffer> CompressData(const wxMemoryBuffer &input);

//! Returns decompressed data from a memory buffer, or no value on failure.
stx::optional<wxMemoryBuffer> DecompressData(const wxMemoryBuffer &input);

//! Compresses data from a memory buffer into a stream. Returns true if successful.
bool CompressDataInto(const wxMemoryBuffer &input, wxOutputStream &ostream);

//! Decompresses data from a memory buffer into a stream. Returns true if successful.
bool DecompressDataInto(const wxMemoryBuffer &input, wxOutputStream &ostream);

class wxFileSystem;
//! Read the contents of a wxFSFile into a memory buffer.
stx::optional<wxMemoryBuffer> ReadAll(const wxString &fileName, wxFileSystem *filesystem);

class wxFSFile;
//! Read the contents of a wxFSFile into a memory buffer.
stx::optional<wxMemoryBuffer> ReadAll(wxFSFile *file);

class wxFile;
//! Read the contents of a wxFile into a memory buffer.
stx::optional<wxMemoryBuffer> ReadAll(wxFile &file);

/*! Reads the input text stream line-by-line until EOF, and performs a filter action on
 * each line.
 *
 * \returns true when the input stream has reached its end, and the output stream
 * is still Ok.
 */
template <typename Filter> inline
  bool CopyAndFilterText(wxTextInputStream &tistream, wxTextOutputStream &tostream,
                    Filter &&filter)
{
  wxString line;
  auto &istream = tistream.GetInputStream();
  auto &ostream = tostream.GetOutputStream();
  while (istream.IsOk() && ostream.IsOk() && !istream.Eof())
  {
    line = tistream.ReadLine();
    filter(line);
    tostream << line << '\n';
  }
  tostream.Flush();
  return istream.Eof() && ostream.IsOk();
}

/*! Reads the input stream line-by-line until EOF, and performs a filter action on each
 * line.
 *
 * \returns true when the input stream has reached its end, and the output stream is
 * still OK.
 */
template <typename Filter, typename ...Args> inline
  bool CopyAndFilterText(wxInputStream &istream, wxOutputStream &ostream,
                    Filter &&filter, Args &&...args)
{
  wxTextInputStream tistream(istream, std::forward<Args>(args)...);
  wxTextOutputStream tostream(ostream);
  return CopyAndFilterText(tistream, tostream, std::forward<Filter>(filter));
}

#endif // WXMAXIMA_STREAMS_H
