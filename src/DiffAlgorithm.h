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

} // namespace Diff

#endif // DIFFALGORITHM_H
