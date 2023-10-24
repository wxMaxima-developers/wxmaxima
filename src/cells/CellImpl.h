// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include <memory>

/*! \file
 * Macros and types used only by cell implementations, not by cell users.
 */

#ifndef WXMAXIMA_CELLIMPL_H
#define WXMAXIMA_CELLIMPL_H

#define DEFINE_CELL_TYPEINFO(type)                                      \
    const CellTypeInfo &type::GetInfo()                                 \
    {                                                                   \
        class type##TypeInfo final : public CellTypeInfo {              \
        public:                                                         \
        /* cppcheck-suppress returnTempReference */                     \
        const wxString &GetName() const override { return S_(#type); }  \
        };                                                              \
        const type##TypeInfo static info;                               \
        return info;                                                    \
    }                                                                   \

#define DEFINE_CELL_COPY(type)                                  \
    std::unique_ptr<Cell> type::Copy(GroupCell *group) const    \
    {                                                           \
        return std::make_unique<type>(group, *this);            \
    }                                                           \

#define DEFINE_CELL(type)                       \
    DEFINE_CELL_COPY(type)                      \
    DEFINE_CELL_TYPEINFO(type)                  \

#endif
