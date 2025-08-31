// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  A mutex that locks if we spawn more threads than we have processors
*/

#include "ThreadNumberLimiter.h"
#include <thread>
#include <wx/wx.h>
#include <wx/log.h>
#include <wx/string.h>

std::mutex ThreadNumberLimiter::m_mutex;
std::condition_variable ThreadNumberLimiter::m_block;
std::atomic<int> ThreadNumberLimiter::m_numberOfBackgroundThreads;

ThreadNumberLimiter::ThreadNumberLimiter()
{
  // Prevent new threads from being created until we have determined if we
  // have CPUs available for them.
  std::unique_lock<std::mutex> lock(m_mutex);

  // Determine how many CPUs we can run threads on
  int maxThreads =  std::thread::hardware_concurrency();
  if(maxThreads < 2)
    maxThreads = 8;
  if(maxThreads < 4)
    maxThreads = 4;

  if(m_numberOfBackgroundThreads++ > maxThreads)
    m_block.wait(lock);
}

ThreadNumberLimiter::~ThreadNumberLimiter()
{
  // Prevent new threads from being created until we have determined if we
  // have CPUs available for them.
  std::unique_lock<std::mutex> lock(m_mutex);
  m_numberOfBackgroundThreads--;
  m_block.notify_one();
}
