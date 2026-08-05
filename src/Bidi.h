// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Claude
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

#ifndef BIDI_H
#define BIDI_H

#include <cstdint>
#include <vector>
#include <wx/string.h>

/*! \file
  Reorders a line of text according to the Unicode Bidirectional Algorithm
  (UAX #9), for lines that mix left-to-right and right-to-left scripts.

  No wxWidgets backend exposes this itself: Pango/CoreText/DirectWrite (or
  Uniscribe) all compute it internally in order to shape and place the glyphs
  they draw, but none of them hand the logical-to-visual reordering back to
  the application, on any platform. EditorCell needs it anyway, to place the
  caret, the selection highlight and the click-to-position mapping correctly
  on a line that isn't wholly one direction - so this class recomputes it,
  using libfribidi where that is available at build time (see USE_FRIBIDI in
  BuildConfig.h). Where it isn't, GetRuns() returns the line as a single run
  in the given paragraph direction: the same single-direction approximation
  EditorCell used before this class existed, and still exactly what a build
  without libfribidi gets today.
*/

//! One maximal run of same-direction text within a display line, in visual
//! (left-to-right on screen) order.
struct BidiRun {
  size_t logicalStart; //!< index of the run's first character, in logical (storage) order
  size_t logicalEnd;   //!< one past the index of the run's last character (logicalStart < logicalEnd)
  bool rightToLeft;    //!< whether this run is drawn right-to-left
};

class Bidi {
public:
  //! True if libfribidi is compiled in, so GetRuns() reorders mixed-direction lines correctly.
  static bool IsAvailable();

  /*! The runs that make up line, in visual (left-to-right on screen) order.

    \param line The text of a single display line (no '\n' or soft breaks in it).
    \param paragraphIsRightToLeft The direction to resolve the line's overall
           direction to if it contains no strongly-directional character at
           all - normally Configuration::RightToLeftDocument().

    Never returns an empty vector for a non-empty line: on any error (or when
    libfribidi isn't compiled in) the whole line comes back as one run in
    paragraphIsRightToLeft's direction.
  */
  static std::vector<BidiRun> GetRuns(const wxString &line, bool paragraphIsRightToLeft);

  //! The same computation as GetRuns(), but on raw Unicode code points - the
  //! part that doesn't need wxWidgets, kept separate so it can be tested on
  //! its own.
  static std::vector<BidiRun> GetRuns(const std::vector<uint32_t> &codepoints,
                                      bool paragraphIsRightToLeft);
};

#endif // BIDI_H
