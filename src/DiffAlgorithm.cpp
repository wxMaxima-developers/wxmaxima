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
#include <set>

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

    // We normally match cells by UUID when both sides have them. But a file can
    // lose its UUIDs and get fresh, unrelated ones (e.g. it was edited by an old
    // wxMaxima that dropped unknown XML attributes, then re-saved by a current
    // one). The two files then share no UUIDs at all even though they describe
    // the same worksheet, and UUID matching would mark everything as different.
    // Detect that case (no UUID in common) and fall back to content matching.
    bool useUuids = false;
    {
        std::set<wxString> uuids1;
        for (const auto &c : s1)
            if (!c.uuid.IsEmpty())
                uuids1.insert(c.uuid);
        for (const auto &c : s2)
            if (!c.uuid.IsEmpty() && uuids1.count(c.uuid)) {
                useUuids = true;
                break;
            }
    }

    auto is_match = [&](int i, int j) {
        const auto& c1 = s1[i];
        const auto& c2 = s2[j];

        // If both have UUIDs (and the two files share some), they MUST match for
        // the cells to match.
        if (useUuids && !c1.uuid.IsEmpty() && !c2.uuid.IsEmpty()) {
            if (c1.uuid == c2.uuid) return true;
            // Both have UUIDs but they differ - they are different cells.
            return false;
        }

        // Content-based fuzzy match (used if at least one cell lacks a UUID)
        if (c1.type != c2.type) return false;
        if (c1.content == c2.content) return true;
        
        size_t len1 = c1.content.Length();
        size_t len2 = c2.content.Length();
        size_t maxLen = std::max(len1, len2);
        if (maxLen == 0) return true;

        // Fast path: if length difference is greater than threshold, they can't match
        size_t diffLen = (len1 > len2) ? (len1 - len2) : (len2 - len1);
        if ((int)(100 * diffLen / maxLen) > threshold) return false;
        
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

std::vector<CharRange> MergeRanges(std::vector<CharRange> ranges) {
    std::sort(ranges.begin(), ranges.end());
    std::vector<CharRange> merged;
    for (const auto &r : ranges) {
        if (r.first >= r.second)
            continue; // empty
        if (!merged.empty() && r.first <= merged.back().second)
            merged.back().second = std::max(merged.back().second, r.second);
        else
            merged.push_back(r);
    }
    return merged;
}

namespace {

// The line- and token-level LCS below are quadratic in memory; blocks whose
// DP table would exceed this many entries fall back to whole-block ranges.
constexpr std::size_t kLcsCellCap = 1000 * 1000;

//! A token (or line) of the tokenized input: its start offset in the full
//! string and its text.
struct Span {
    std::size_t start;
    std::size_t end; // one past the last character
    wxString text;
};

bool IsWordChar(wxUniChar c) {
    return wxIsalnum(c) || c == wxS('_') || c == wxS('%');
}

//! Splits s[from, to) into word / whitespace-run / single-character tokens.
std::vector<Span> Tokenize(const wxString &s, std::size_t from, std::size_t to) {
    std::vector<Span> tokens;
    wxString::const_iterator it = s.begin() + from;
    std::size_t pos = from;
    while (pos < to) {
        std::size_t start = pos;
        wxUniChar c = *it;
        if (IsWordChar(c)) {
            while (pos < to && IsWordChar(*it)) { ++it; ++pos; }
        } else if (wxIsspace(c)) {
            while (pos < to && wxIsspace(*it)) { ++it; ++pos; }
        } else {
            ++it; ++pos;
        }
        tokens.push_back({start, pos, s.SubString(start, pos - 1)});
    }
    return tokens;
}

//! Splits the whole string into lines. The trailing '\n' belongs to no line,
//! so an unmatched line's range does not extend into the line break.
std::vector<Span> SplitLines(const wxString &s) {
    std::vector<Span> lines;
    std::size_t start = 0;
    std::size_t pos = 0;
    for (wxString::const_iterator it = s.begin(); it != s.end(); ++it, ++pos) {
        if (*it == wxS('\n')) {
            lines.push_back({start, pos, s.SubString(start, pos).RemoveLast()});
            start = pos + 1;
        }
    }
    lines.push_back({start, s.Length(),
                     start < s.Length() ? s.SubString(start, s.Length() - 1)
                                        : wxString()});
    return lines;
}

//! Plain LCS alignment of spans[aFrom, aTo) vs spans[bFrom, bTo) by exact text
//! equality; same output convention as Align2 (-1 = gap).
std::vector<std::pair<int, int>> AlignSpans(const std::vector<Span> &a,
                                            std::size_t aFrom, std::size_t aTo,
                                            const std::vector<Span> &b,
                                            std::size_t bFrom, std::size_t bTo) {
    const int n = static_cast<int>(aTo - aFrom);
    const int m = static_cast<int>(bTo - bFrom);
    auto eq = [&](int i, int j) {
        return a[aFrom + i].text == b[bFrom + j].text;
    };
    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(m + 1, 0));
    for (int i = 1; i <= n; ++i)
        for (int j = 1; j <= m; ++j) {
            if (eq(i - 1, j - 1))
                dp[i][j] = dp[i - 1][j - 1] + 1;
            else
                dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
        }
    std::vector<std::pair<int, int>> alignment;
    int i = n, j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && eq(i - 1, j - 1)) {
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

//! Token-diffs the changed block a[aFrom, aTo) vs b[bFrom, bTo) (line index
//! ranges) and appends the unmatched tokens' character ranges to the outputs.
void DiffBlock(const wxString &a, const std::vector<Span> &aLines,
               std::size_t aFrom, std::size_t aTo,
               const wxString &b, const std::vector<Span> &bLines,
               std::size_t bFrom, std::size_t bTo,
               std::vector<CharRange> &aOnly, std::vector<CharRange> &bOnly) {
    if (aFrom >= aTo && bFrom >= bTo)
        return;
    // Only one side has lines here: the whole block is missing from the other
    // side (internal line breaks included, hence back().end not per-line spans).
    if (aFrom >= aTo) {
        bOnly.push_back({bLines[bFrom].start, bLines[bTo - 1].end});
        return;
    }
    if (bFrom >= bTo) {
        aOnly.push_back({aLines[aFrom].start, aLines[aTo - 1].end});
        return;
    }
    const std::size_t aStart = aLines[aFrom].start, aEnd = aLines[aTo - 1].end;
    const std::size_t bStart = bLines[bFrom].start, bEnd = bLines[bTo - 1].end;
    std::vector<Span> aTokens = Tokenize(a, aStart, aEnd);
    std::vector<Span> bTokens = Tokenize(b, bStart, bEnd);
    if (aTokens.size() * bTokens.size() > kLcsCellCap) {
        // Too big for the quadratic DP: mark the whole block on both sides.
        aOnly.push_back({aStart, aEnd});
        bOnly.push_back({bStart, bEnd});
        return;
    }
    for (const auto &p : AlignSpans(aTokens, 0, aTokens.size(),
                                    bTokens, 0, bTokens.size())) {
        if (p.second == -1)
            aOnly.push_back({aTokens[p.first].start, aTokens[p.first].end});
        else if (p.first == -1)
            bOnly.push_back({bTokens[p.second].start, bTokens[p.second].end});
    }
}

//! Merges consecutive ranges whose gap is whitespace only. Whitespace tokens
//! are near-universal, so the LCS happily matches a stray space between two
//! otherwise completely different chunks - which would punch odd-looking
//! unhighlighted holes into the marking.
std::vector<CharRange> BridgeWhitespaceGaps(const wxString &s,
                                            std::vector<CharRange> ranges) {
    std::vector<CharRange> bridged;
    for (const auto &r : ranges) {
        bool gapIsWhitespace = !bridged.empty();
        if (gapIsWhitespace) {
            wxString::const_iterator it = s.begin() + bridged.back().second;
            wxString::const_iterator end = s.begin() + r.first;
            for (; it != end; ++it)
                if (!wxIsspace(*it)) {
                    gapIsWhitespace = false;
                    break;
                }
        }
        if (gapIsWhitespace)
            bridged.back().second = r.second;
        else
            bridged.push_back(r);
    }
    return bridged;
}

} // anonymous namespace

void InlineDiff(const wxString &a, const wxString &b,
                std::vector<CharRange> &aOnly,
                std::vector<CharRange> &bOnly) {
    aOnly.clear();
    bOnly.clear();
    if (a == b)
        return;

    std::vector<Span> aLines = SplitLines(a);
    std::vector<Span> bLines = SplitLines(b);
    if (aLines.size() * bLines.size() > kLcsCellCap) {
        // Degenerate input; give up on detail and mark both cells wholesale.
        aOnly.push_back({0, a.Length()});
        bOnly.push_back({0, b.Length()});
        return;
    }

    // Line-level alignment first: identical lines drop out cheaply, and the
    // quadratic token diff only runs inside each changed block.
    auto lineAlignment = AlignSpans(aLines, 0, aLines.size(),
                                    bLines, 0, bLines.size());
    std::size_t aBlockFrom = 0, bBlockFrom = 0;
    for (const auto &p : lineAlignment) {
        if (p.first != -1 && p.second != -1) {
            DiffBlock(a, aLines, aBlockFrom, static_cast<std::size_t>(p.first),
                      b, bLines, bBlockFrom, static_cast<std::size_t>(p.second),
                      aOnly, bOnly);
            aBlockFrom = static_cast<std::size_t>(p.first) + 1;
            bBlockFrom = static_cast<std::size_t>(p.second) + 1;
        }
    }
    DiffBlock(a, aLines, aBlockFrom, aLines.size(),
              b, bLines, bBlockFrom, bLines.size(), aOnly, bOnly);

    aOnly = BridgeWhitespaceGaps(a, MergeRanges(std::move(aOnly)));
    bOnly = BridgeWhitespaceGaps(b, MergeRanges(std::move(bOnly)));
}

} // namespace Diff
