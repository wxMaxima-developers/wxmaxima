// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Everyone is permitted to copy and distribute verbatim copies
//  of this licence document, but changing it is not allowed.
//
//                       WXWINDOWS LIBRARY LICENCE
//     TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public Licence as published by
//  the Free Software Foundation; either version 2 of the Licence, or (at your
//  option) any later version.
//
//  This library is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//  Licence for more details.
//
//  You should have received a copy of the GNU Library General Public Licence
//  along with this software, usually in a file named COPYING.LIB.  If not,
//  write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
//  Floor, Boston, MA 02110-1301 USA.
//
//  EXCEPTION NOTICE
//
//  1. As a special exception, the copyright holders of this library give
//  permission for additional uses of the text contained in this release of the
//  library as licenced under the wxWindows Library Licence, applying either
//  version 3.1 of the Licence, or (at your option) any later version of the
//  Licence as published by the copyright holders of version 3.1 of the Licence
//  document.
//
//  2. The exception is that you may use, copy, link, modify and distribute
//  under your own terms, binary object code versions of works based on the
//  Library.
//
//  3. If you copy code from files distributed under the terms of the GNU
//  General Public Licence or the GNU Library General Public Licence into a
//  copy of this library, as this licence permits, the exception does not apply
//  to the code that you add in this way.  To avoid misleading anyone as to the
//  status of such modified files, you must delete this exception notice from
//  such code and/or adjust the licensing conditions notice accordingly.
//
//  4. If you write modifications of your own for this library, it is your
//  choice whether to permit this exception to apply to your modifications.  If
//  you do not wish that, you must delete the exception notice from such code
//  and/or adjust the licensing conditions notice accordingly.
//
//  SPDX-License-Identifier: wxWindows

#include "StreamUtils.h"
#include <wx/stream.h>
#include <algorithm>
#include <cstring>

UTF8Decoder::UTF8Decoder()
#if !(defined(__WINDOWS__) && wxUSE_UNICODE)
    // This works on newer Windows 10, but fails on older Windows,
    // thus we fall back to the deprecated utf8 codec.
    : m_locale("en_US.UTF8"),
      m_codec(std::use_facet<std::remove_reference<decltype(m_codec)>::type>(m_locale))
#endif
{
}

UTF8Decoder::DecodeResult UTF8Decoder::Decode(UTF8Decoder::State &state,
                                              wxInputStream &in, size_t maxRead,
                                              size_t maxWrite)
{
  return state.Decode(m_codec, in, maxRead, maxWrite);
}

UTF8Decoder::DecodeResult UTF8Decoder::State::Decode(const Codec &codec,
                                                     wxInputStream &in,
                                                     size_t maxRead,
                                                     size_t maxWrite)
{
  if (!maxRead || !maxWrite)
    return {};

  // Append input data to the buffer
  if (m_inBuf.size() < maxRead)
    m_inBuf.resize(maxRead);

  if (maxRead > m_inBufCount)
  {
    in.Read(m_inBuf.data() + m_inBufCount, maxRead - m_inBufCount);
    m_inBufCount += in.LastRead();
  }

  // Prepare the output buffer
  if (m_outBuf.size() < maxWrite)
    m_outBuf.resize(maxWrite);

  // Decode
  auto const *inPtr = m_inBuf.data();
  auto *outPtr = m_outBuf.data();

  auto const dr = codec.in(m_codecState, inPtr, inPtr + m_inBufCount, inPtr,
                           outPtr, outPtr + m_outBuf.size(), outPtr);

  // Fallback for noconv
  if (dr == std::codecvt_base::noconv)
  {
    wxASSERT(inPtr == m_inBuf.data() && outPtr == m_outBuf.data());
    auto toCopy = std::min(m_inBufCount, m_outBuf.size());
    std::copy(inPtr, inPtr+toCopy, outPtr);
    inPtr += toCopy;
    outPtr += toCopy;
  }
  else if (dr == std::codecvt_base::error)
  {
    m_hadError = true;
  }

  auto const outBufCount = outPtr - m_outBuf.data();

  // Shove leftover input data to the beginning of the buffer
  auto const inBufPos = inPtr - m_inBuf.data();
  auto const inLeftCount = m_inBufCount - inBufPos;
  memmove(m_inBuf.data(), inPtr, inLeftCount);
  m_inBufCount = inLeftCount;

  DecodeResult result;
  result.bytesRead = in.LastRead();
  result.outputSize = outBufCount;
  result.output = m_outBuf.data();
  result.outputEnd = m_outBuf.data() + outBufCount;
  result.ok = (dr != std::codecvt_base::error);
  return result;
}
