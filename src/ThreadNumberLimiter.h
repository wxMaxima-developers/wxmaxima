// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  A mutex that locks if we have too many background threads

  Creates a mutex that locks as long as we have more background threads than processors
*/

#ifndef THREADNUMBERLIMITER_H
#define THREADNUMBERLIMITER_H

#include <mutex>


//! A dialog that shows the program's license.
class ThreadNumberLimiter
{
public:
    explicit ThreadNumberLimiter(bool *running = NULL);
    ~ThreadNumberLimiter();
private:
    static int m_numberOfBackgroundThreads;
    static std::mutex m_mutex;
    static std::mutex m_counterMutex;
    bool *m_runningIndicator;
};

#endif // THREADNUMBERLIMITER_H
