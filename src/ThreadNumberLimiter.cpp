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
#include <wx/log.h>

std::mutex ThreadNumberLimiter::m_counterMutex;
std::mutex ThreadNumberLimiter::m_mutex;
int ThreadNumberLimiter::m_numberOfBackgroundThreads;

ThreadNumberLimiter::ThreadNumberLimiter()
{
  m_mutex.lock();
  int maxThreads =  std::thread::hardware_concurrency();
  if(maxThreads < 2)
    maxThreads = 2;
  bool unlock;
  {
    std::lock_guard<std::mutex> lock_guard(m_counterMutex);
    unlock = m_numberOfBackgroundThreads++ < maxThreads;
  }
  if(unlock)
    m_mutex.unlock();
}

ThreadNumberLimiter::~ThreadNumberLimiter()
{
  std::lock_guard<std::mutex> lock_guard(m_counterMutex);
  m_numberOfBackgroundThreads--;
  m_mutex.unlock();  
}
