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

#include "Bidi.h"
#include "BuildConfig.h"
#include <algorithm>

#ifdef USE_FRIBIDI
#include <fribidi/fribidi.h>
#endif

bool Bidi::IsAvailable() {
#ifdef USE_FRIBIDI
  return true;
#else
  return false;
#endif
}

std::vector<BidiRun> Bidi::GetRuns(const wxString &line, bool paragraphIsRightToLeft) {
  std::vector<uint32_t> codepoints;
  codepoints.reserve(line.Length());
  for (auto it = line.begin(); it != line.end(); ++it)
    codepoints.push_back(static_cast<uint32_t>(static_cast<wxUint32>(*it)));
  return GetRuns(codepoints, paragraphIsRightToLeft);
}

std::vector<BidiRun> Bidi::GetRuns(const std::vector<uint32_t> &codepoints,
                                   bool paragraphIsRightToLeft) {
  std::vector<BidiRun> runs;
  const size_t len = codepoints.size();
  if (len == 0)
    return runs;

#ifdef USE_FRIBIDI
  std::vector<FriBidiChar> input(len);
  for (size_t i = 0; i < len; i++)
    input[i] = static_cast<FriBidiChar>(codepoints[i]);

  // visual_str and positions_V_to_L aren't needed here (runs are derived from
  // embedding_levels and positions_L_to_V below), but a real buffer is passed
  // for all of them regardless of the "NULL is ignored" contract the header
  // documents, since it's cheap for a single editor line and removes any
  // doubt about it.
  std::vector<FriBidiChar> visual(len);
  std::vector<FriBidiStrIndex> logicalToVisual(len);
  std::vector<FriBidiStrIndex> visualToLogical(len);
  std::vector<FriBidiLevel> levels(len);
  FriBidiParType baseDir = paragraphIsRightToLeft ? FRIBIDI_PAR_RTL : FRIBIDI_PAR_LTR;

  const FriBidiLevel maxLevel = fribidi_log2vis(
      input.data(), static_cast<FriBidiStrIndex>(len), &baseDir, visual.data(),
      logicalToVisual.data(), visualToLogical.data(), levels.data());

  if (maxLevel > 0) {
    // Split into maximal same-level runs. These are contiguous in logical
    // order for any text that doesn't use explicit embedding/override
    // control characters (U+202A..U+202E, U+2066..U+2069) - which wxMaxima
    // never inserts and Maxima's reader has no reason to accept either, so
    // in practice this always holds for what ends up here.
    size_t runStart = 0;
    while (runStart < len) {
      size_t runEnd = runStart + 1;
      while (runEnd < len && levels[runEnd] == levels[runStart])
        runEnd++;
      runs.push_back({runStart, runEnd, (levels[runStart] % 2) != 0});
      runStart = runEnd;
    }
    // Order the runs the way they appear on screen, left to right: a run's
    // own characters are already contiguous on screen (odd levels just draw
    // that stretch back to front), so the run's leftmost visual position -
    // the lower of its first and last character's visual index - orders the
    // runs against each other correctly.
    std::sort(runs.begin(), runs.end(), [&](const BidiRun &a, const BidiRun &b) {
      auto visualStart = [&](const BidiRun &r) {
        return std::min(logicalToVisual[r.logicalStart], logicalToVisual[r.logicalEnd - 1]);
      };
      return visualStart(a) < visualStart(b);
    });
    return runs;
  }
  // maxLevel == 0 means fribidi_log2vis failed (allocation failure, most
  // likely) - fall through to the single-run approximation below rather than
  // return nothing.
#endif

  runs.push_back({0, len, paragraphIsRightToLeft});
  return runs;
}
