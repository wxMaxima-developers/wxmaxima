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

#include "DiffAlgorithm.h"
#include "levenshtein/levenshtein.h"
#include <algorithm>

namespace Diff {

int FindOptimalThreshold(const std::vector<CellMatchData>& s1, 
                         const std::vector<CellMatchData>& s2) {
    std::vector<int> histogram(101, 0);
    int window = 50;
    for (int i = 0; i < (int)s1.size(); ++i) {
        int start = std::max(0, i - window);
        int end = std::min((int)s2.size() - 1, i + window);
        for (int j = start; j <= end; ++j) {
            if (s1[i].type != s2[j].type) continue;
            size_t maxLen = std::max(s1[i].content.Length(), s2[j].content.Length());
            if (maxLen > 0) {
                size_t dist = LevenshteinDistance(s1[i].content, s2[j].content);
                int percent = (int)(100 * dist / maxLen);
                if (percent <= 100) histogram[percent]++;
            } else if (s1[i].type == s2[j].type) {
                histogram[0]++;
            }
        }
    }
    
    // Find first local minimum after the peak at 0.
    for (int i = 5; i < 50; ++i) {
        if (histogram[i] < histogram[i-1] && histogram[i] < histogram[i+1])
            return i;
    }
    return 20; // Default fallback
}

std::vector<std::pair<int, int>> Align2(const std::vector<CellMatchData>& s1, 
                                        const std::vector<CellMatchData>& s2,
                                        int threshold) {
    int n = s1.size();
    int m = s2.size();
    
    auto is_match = [&](int i, int j) {
        const auto& c1 = s1[i];
        const auto& c2 = s2[j];
        
        // If both have UUIDs, they MUST match for the cells to match
        if (!c1.uuid.IsEmpty() && !c2.uuid.IsEmpty()) {
            if (c1.uuid == c2.uuid) return true;
            // Both have UUIDs but they differ - they are different cells.
            return false;
        }
        
        // Content-based fuzzy match (used if at least one cell lacks a UUID)
        if (c1.type != c2.type) return false;
        if (c1.content == c2.content) return true;
        
        size_t maxLen = std::max(c1.content.Length(), c2.content.Length());
        if (maxLen == 0) return true;
        
        size_t dist = LevenshteinDistance(c1.content, c2.content);
        return (int)(100 * dist / maxLen) <= threshold;
    };

    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(m + 1, 0));
    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            if (is_match(i - 1, j - 1))
                dp[i][j] = dp[i - 1][j - 1] + 1;
            else
                dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
        }
    }

    // Backtrack to find the actual alignment path
    std::vector<std::pair<int, int>> alignment;
    int i = n, j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && is_match(i - 1, j - 1)) {
            alignment.push_back({i - 1, j - 1});
            i--; j--;
        } else if (j > 0 && (i == 0 || dp[i][j - 1] >= dp[i - 1][j])) {
            alignment.push_back({-1, j - 1});
            j--;
        } else {
            alignment.push_back({i - 1, -1});
            i--;
        }
    }
    std::reverse(alignment.begin(), alignment.end());
    return alignment;
}

} // namespace Diff
