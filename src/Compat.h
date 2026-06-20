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
  Small C++ standard-library compatibility shims.

  wxMaxima targets C++20 but aims to keep building on toolchains a few years
  behind. This header provides minimal fall-backs for std features that may be
  missing there: std::jthread / std::stop_token / std::stop_source, and
  std::ranges::contains. These were historically carried in Version.h; they live
  here now because they have nothing to do with the version and are pure,
  CMake-independent C++ (so this header is a plain committed file, not generated).
*/

#ifndef WXM_COMPAT_H
#define WXM_COMPAT_H

#include <thread>
#include <version>
#ifdef __cpp_lib_jthread
    using jthread = std::jthread;
    using stop_token = std::stop_token;
    using stop_source = std::stop_source;
#else
    struct stop_token {
        bool stop_requested() const { return false; }
    };
    // A minimal stop_source shim for compilers without std::jthread: on those
    // compilers cancellation is a no-op (as it always was here).
    struct stop_source {
        stop_token get_token() const { return {}; }
        bool request_stop() noexcept { return false; }
        bool stop_requested() const noexcept { return false; }
    };
    // A minimal jthread shim for compilers without std::jthread
    class jthread : public std::thread {
    public:
        jthread() noexcept = default;
        template<typename F, typename... Args>
        explicit jthread(F&& f, Args&&... args)
            : std::thread([](std::decay_t<F> f, std::decay_t<Args>... args) {
                if constexpr (std::is_invocable_v<std::decay_t<F>, stop_token, std::decay_t<Args>...>) {
                    f(stop_token{}, std::move(args)...);
                } else {
                    f(std::move(args)...);
                }
            }, std::forward<F>(f), std::forward<Args>(args)...) {}

        jthread& operator=(jthread&& other) noexcept {
            if (joinable()) join();
            std::thread::operator=(std::move(other));
            return *this;
        }
        ~jthread() {
            if (joinable()) join();
        }
        void request_stop() noexcept {}
    };
#endif

#include <algorithm>
#include <ranges>
namespace ranges {
#if defined(__cpp_lib_ranges_contains) && __cpp_lib_ranges_contains >= 202207L
    using std::ranges::contains;
#else
    template<typename R, typename T>
    constexpr bool contains(R&& r, const T& value) {
        return std::ranges::find(r, value) != std::ranges::end(r);
    }
#endif
}
#endif // WXM_COMPAT_H
