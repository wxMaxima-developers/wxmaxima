// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Gemini CLI
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

#ifndef DIFFALGORITHM_H
#define DIFFALGORITHM_H

#include <wx/string.h>
#include <vector>
#include "WXMformat.h"

namespace Diff {

/**
 * @brief Represents matching metadata for a cell.
 */
struct CellMatchData {
  wxString uuid;
  wxString content;
  GroupType type;
};

/**
 * @brief Analyzes the distribution of Levenshtein distances to find an optimal match threshold.
 */
int FindOptimalThreshold(const std::vector<CellMatchData>& s1, 
                         const std::vector<CellMatchData>& s2);

/**
 * @brief Computes the optimal alignment between two sequences of cells.
 *
 * This uses a Dynamic Programming approach for the Longest Common
 * Subsequence (LCS) problem, extended with fuzzy matching and UUID priority.
 *
 * @param s1 Metadata for cells in the first file.
 * @param s2 Metadata for cells in the second file.
 * @param threshold Levenshtein distance threshold (as percentage of length).
 * @return A vector of pairs where each pair (i, j) represents matched indices.
 *         -1 indicates a gap (no match).
 */
std::vector<std::pair<int, int>> Align2(const std::vector<CellMatchData>& s1,
                                        const std::vector<CellMatchData>& s2,
                                        int threshold = 20);

//! A half-open [start, end) character range within a cell's text.
using CharRange = std::pair<std::size_t, std::size_t>;

/**
 * @brief Sorts the given ranges and merges the ones that overlap or touch.
 *
 * Empty ranges are dropped. Used to union the highlight ranges a cell
 * accumulates against multiple counterparts in a 3-way diff.
 */
std::vector<CharRange> MergeRanges(std::vector<CharRange> ranges);

/**
 * @brief Computes the intra-cell difference between two text contents.
 *
 * Returns, for each side, the character ranges whose text the other side
 * does not contain — the ranges the diff viewer paints with the
 * diff-highlight background. The diff runs in two stages: a line-level LCS
 * first, then a token-level (word/whitespace-run/single-character) LCS
 * within each changed block, so highlights snap to word boundaries.
 * Degenerate inputs (blocks too large for the quadratic DP) fall back to
 * highlighting the whole block.
 *
 * @param a First text (soft-wrap '\r' markers already normalized to ' ').
 * @param b Second text (normalized likewise).
 * @param aOnly Receives the ranges of @p a missing from @p b, merged and sorted.
 * @param bOnly Receives the ranges of @p b missing from @p a, merged and sorted.
 */
void InlineDiff(const wxString &a, const wxString &b,
                std::vector<CharRange> &aOnly,
                std::vector<CharRange> &bOnly);

} // namespace Diff

#endif // DIFFALGORITHM_H
