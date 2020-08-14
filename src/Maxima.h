// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef WXMAXIMA_MAXIMA_H
#define WXMAXIMA_MAXIMA_H

/*! \file
 *
 * Declares the interface to the Maxima process.
 */

#include "StreamUtils.h"
#include <wx/socket.h>
#include <wx/sckstrm.h>
#include <memory>

/*! Interface to the Maxima process
 *
 * Eventually this class will be the entire stand-alone Maxima
 * interface, factored out from wxMaxima. For now, it only provides
 * some trivial network-reception-related functionality.
 */
class Maxima
{
public:
  explicit Maxima(wxSocketBase *socket);
  ~Maxima();

  wxSocketBase *Socket() const { return m_socket.get(); }

  bool Eof() const { return m_socketInput.Eof(); }
  bool Error() const { return m_socket->Error(); }
  bool IsData() const { return m_socket->IsData(); }
  bool IsConnected() const { return m_socket->IsConnected(); }
  wxSocketBase &Write(const void *buffer, wxUint32 nbytes) { return m_socket->Write(buffer, nbytes); }

  UTF8Decoder::DecodeResult DecodeFromSocket(size_t maxRead);

private:
  std::unique_ptr<wxSocketBase> m_socket;
  wxSocketInputStream m_socketInput{*m_socket};
  UTF8Decoder m_decoder;
  UTF8Decoder::State m_clientDecodeState;
};

#endif
