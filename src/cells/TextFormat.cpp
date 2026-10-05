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
  Character formatting of text cells; see TextFormat.h.
*/

#include "TextFormat.h"
#include <algorithm>
#include <wx/tokenzr.h>
#include <wx/xml/xml.h>

namespace TextFormat {

namespace {
//! The attribute name each format is saved under, and the inline tags that
//! mean the same.
struct FormatName {
  Format flag;
  const wxChar *attribute;
};
constexpr FormatName g_formatNames[] = {
  {Bold, wxS("bold")},
  {Italic, wxS("italic")},
  {Underline, wxS("underline")},
  {Strikethrough, wxS("strikethrough")}};

Format FlagForAttribute(const wxString &name) {
  for (const auto &f : g_formatNames)
    if (name == f.attribute)
      return f.flag;
  return None;
}

Format FlagForTag(const wxString &tag) {
  const wxString t = tag.Lower();
  if ((t == wxS("b")) || (t == wxS("strong")))
    return Bold;
  if ((t == wxS("i")) || (t == wxS("em")))
    return Italic;
  if (t == wxS("u"))
    return Underline;
  if ((t == wxS("s")) || (t == wxS("strike")) || (t == wxS("del")))
    return Strikethrough;
  return None;
}

bool IsHighSurrogate(wxUniChar c) {
  return (c.GetValue() >= 0xD800) && (c.GetValue() <= 0xDBFF);
}
bool IsLowSurrogate(wxUniChar c) {
  return (c.GetValue() >= 0xDC00) && (c.GetValue() <= 0xDFFF);
}

//! Escape a value for use inside a double-quoted XML attribute.
wxString EscapeAttribute(const wxString &value) {
  wxString retval;
  for (const auto &c : value) {
    switch (c.GetValue()) {
    case '&':
      retval += wxS("&amp;");
      break;
    case '<':
      retval += wxS("&lt;");
      break;
    case '>':
      retval += wxS("&gt;");
      break;
    case '"':
      retval += wxS("&quot;");
      break;
    default:
      retval += c;
    }
  }
  return retval;
}

/*! The format a character inserted at newPos gets if nothing it replaced
  tells otherwise: the rule of FormatForInsertionAt(), applied while the
  formats of newText are still being worked out.

  \param result     The formats of newText determined so far (everything
                    before newPos is known).
  \param following  The format of the old character that now follows the
                    insertion, or None if there is none.
*/
Format InheritedFormat(const wxString &newText, const Formats &result,
                       std::size_t newPos, bool hasFollowing,
                       Format following) {
  if ((newPos > 0) && (newText[newPos - 1] != wxS('\n')))
    return result[newPos - 1];
  if (hasFollowing)
    return following;
  if (newPos > 0)
    return result[newPos - 1];
  return None;
}

//! The biggest changed region (old length * new length) that is still diffed
//! character by character. A 1000 x 1000 table is 2MB and takes a few
//! milliseconds.
constexpr std::size_t g_maxDiffCells = 1000000;

/*! Map formats across a changed region by a longest common subsequence.

  oldText[oldStart, oldEnd) became newText[newStart, newEnd). Characters
  that are part of the common subsequence keep their format; the others take
  the format of the first character deleted in the same gap, or else inherit
  it.
*/
void DiffRegion(const wxString &oldText, const Formats &oldFormats,
                std::size_t oldStart, std::size_t oldEnd,
                const wxString &newText, Formats &result,
                std::size_t newStart, std::size_t newEnd) {
  const std::size_t om = oldEnd - oldStart;
  const std::size_t nm = newEnd - newStart;
  // lcs[i * (nm + 1) + j]: length of the longest common subsequence of
  // old[oldStart + i, oldEnd) and new[newStart + j, newEnd). Fits into 16 bits
  // since om * nm <= g_maxDiffCells makes min(om, nm) <= 1000.
  std::vector<std::uint16_t> lcs((om + 1) * (nm + 1), 0);
  auto at = [&](std::size_t i, std::size_t j) -> std::uint16_t & {
    return lcs[i * (nm + 1) + j];
  };
  for (std::size_t i = om; i-- > 0;)
    for (std::size_t j = nm; j-- > 0;) {
      if (oldText[oldStart + i] == newText[newStart + j])
        at(i, j) = at(i + 1, j + 1) + 1;
      else
        at(i, j) = std::max(at(i + 1, j), at(i, j + 1));
    }

  std::size_t i = 0, j = 0;
  bool gapHasDeleted = false;
  Format gapFormat = None;
  while (j < nm) {
    if ((i < om) && (oldText[oldStart + i] == newText[newStart + j]) &&
        (at(i, j) == at(i + 1, j + 1) + 1)) {
      result[newStart + j] = At(oldFormats, oldStart + i);
      gapHasDeleted = false;
      ++i;
      ++j;
    } else if ((i < om) && (at(i + 1, j) >= at(i, j + 1))) {
      // oldText[oldStart + i] was deleted
      if (!gapHasDeleted) {
        gapHasDeleted = true;
        gapFormat = At(oldFormats, oldStart + i);
      }
      ++i;
    } else {
      // newText[newStart + j] was inserted
      if (gapHasDeleted)
        result[newStart + j] = gapFormat;
      else
        result[newStart + j] =
          InheritedFormat(newText, result, newStart + j, oldStart + i < oldFormats.size(),
                          At(oldFormats, oldStart + i));
      ++j;
    }
  }
}
} // namespace

Format FormatForInsertionAt(const wxString &text, const Formats &formats,
                            std::size_t pos) {
  pos = std::min(pos, text.Length());
  if ((pos > 0) && (text[pos - 1] != wxS('\n')))
    return At(formats, pos - 1);
  if (pos < text.Length())
    return At(formats, pos);
  if (pos > 0)
    return At(formats, pos - 1);
  return None;
}

bool IsPlain(const Formats &formats) {
  return std::all_of(formats.begin(), formats.end(),
                     [](Format f) { return f == None; });
}

Formats Reconcile(const wxString &oldText, const Formats &oldFormats,
                  const wxString &newText, bool hasPending,
                  std::size_t pendingPos, Format pendingFormat) {
  if (IsPlain(oldFormats) && !(hasPending && (pendingFormat != None)))
    return {};

  const std::size_t oldLen = oldText.Length();
  const std::size_t newLen = newText.Length();
  Formats old(oldFormats);
  old.resize(oldLen, None);
  Formats result(newLen, None);

  // A pure insertion at the place the pending format was set for: typing
  // after "bold" was switched on with nothing selected. This is checked
  // before the general prefix/suffix match below, which can't tell where
  // inside a run of equal characters something was inserted ("ab" -> "aab"
  // looks like an insertion after the first "a" even if it was typed in
  // front of it).
  if (hasPending && (newLen > oldLen) && (pendingPos <= oldLen)) {
    const std::size_t inserted = newLen - oldLen;
    if ((oldText.compare(0, pendingPos, newText, 0, pendingPos) == 0) &&
        (oldText.compare(pendingPos, wxString::npos, newText,
                         pendingPos + inserted, wxString::npos) == 0)) {
      std::copy(old.begin(), old.begin() + static_cast<std::ptrdiff_t>(pendingPos),
                result.begin());
      std::fill(result.begin() + static_cast<std::ptrdiff_t>(pendingPos),
                result.begin() + static_cast<std::ptrdiff_t>(pendingPos + inserted),
                pendingFormat);
      std::copy(old.begin() + static_cast<std::ptrdiff_t>(pendingPos), old.end(),
                result.begin() + static_cast<std::ptrdiff_t>(pendingPos + inserted));
      if (IsPlain(result))
        return {};
      return result;
    }
  }

  // The common prefix and suffix are unchanged.
  const std::size_t maxCommon = std::min(oldLen, newLen);
  std::size_t prefix = 0;
  while ((prefix < maxCommon) && (oldText[prefix] == newText[prefix]))
    ++prefix;
  std::size_t suffix = 0;
  while ((suffix < maxCommon - prefix) &&
         (oldText[oldLen - 1 - suffix] == newText[newLen - 1 - suffix]))
    ++suffix;

  for (std::size_t i = 0; i < prefix; ++i)
    result[i] = old[i];
  for (std::size_t k = 0; k < suffix; ++k)
    result[newLen - 1 - k] = old[oldLen - 1 - k];

  const std::size_t oldEnd = oldLen - suffix;
  const std::size_t newEnd = newLen - suffix;
  const std::size_t om = oldEnd - prefix;
  const std::size_t nm = newEnd - prefix;

  if (nm > 0) {
    if (om == 0) {
      // A pure insertion
      const Format f = InheritedFormat(newText, result, prefix, oldEnd < oldLen,
                                       At(old, oldEnd));
      std::fill(result.begin() + static_cast<std::ptrdiff_t>(prefix),
                result.begin() + static_cast<std::ptrdiff_t>(newEnd), f);
    } else if (om == nm) {
      // Characters replaced one by one (or the same number of them): keep
      // the formats where they were.
      for (std::size_t i = 0; i < nm; ++i)
        result[prefix + i] = old[prefix + i];
    } else if (om * nm <= g_maxDiffCells)
      DiffRegion(oldText, old, prefix, oldEnd, newText, result, prefix, newEnd);
    else {
      // Too big to diff: keep what is where it was, and let the rest
      // continue the last format.
      for (std::size_t i = 0; i < nm; ++i)
        result[prefix + i] = (i < om) ? old[prefix + i] : result[prefix + i - 1];
    }
  }

  if (IsPlain(result))
    return {};
  return result;
}

std::size_t CodePointsBefore(const wxString &text, std::size_t start,
                             std::size_t index) {
  std::size_t count = 0;
  for (std::size_t i = start; (i < index) && (i < text.Length()); ++i) {
    // The second half of a surrogate pair doesn't start a new code point.
    if (IsLowSurrogate(text[i]) && (i > start) && IsHighSurrogate(text[i - 1]))
      continue;
    ++count;
  }
  return count;
}

std::size_t IndexOfCodePoint(const wxString &text, std::size_t start,
                             std::size_t codePoints) {
  std::size_t i = start;
  while ((codePoints > 0) && (i < text.Length())) {
    if (IsHighSurrogate(text[i]) && (i + 1 < text.Length()) &&
        IsLowSurrogate(text[i + 1]))
      ++i;
    ++i;
    --codePoints;
  }
  return i;
}

wxString LineAttributes(const wxString &text, const Formats &formats,
                        std::size_t lineStart, std::size_t lineEnd) {
  wxString retval;
  if (formats.empty())
    return retval;
  lineEnd = std::min(lineEnd, formats.size());
  for (const auto &f : g_formatNames) {
    wxString ranges;
    std::size_t pos = lineStart;
    while (pos < lineEnd) {
      if (!(formats[pos] & f.flag)) {
        ++pos;
        continue;
      }
      std::size_t end = pos;
      while ((end < lineEnd) && (formats[end] & f.flag))
        ++end;
      if (!ranges.IsEmpty())
        ranges += wxS(",");
      ranges += wxString::Format(wxS("%lu-%lu"),
                                 static_cast<unsigned long>(CodePointsBefore(text, lineStart, pos)),
                                 static_cast<unsigned long>(CodePointsBefore(text, lineStart, end)));
      pos = end;
    }
    if (!ranges.IsEmpty())
      retval += wxS(" ") + wxString(f.attribute) + wxS("=\"") + ranges + wxS("\"");
  }
  return retval;
}

std::vector<std::pair<std::size_t, std::size_t>> ParseRanges(const wxString &ranges) {
  std::vector<std::pair<std::size_t, std::size_t>> retval;
  wxStringTokenizer tokens(ranges, wxS(","));
  while (tokens.HasMoreTokens()) {
    const wxString token = tokens.GetNextToken().Trim().Trim(false);
    const wxString startString = token.BeforeFirst(wxS('-'));
    const wxString endString = token.AfterFirst(wxS('-'));
    unsigned long start, end;
    if (startString.ToULong(&start) && endString.ToULong(&end) && (end > start))
      retval.emplace_back(start, end);
  }
  return retval;
}

namespace {
void ReadChildren(const wxXmlNode *node, Format format, wxString &text,
                  Formats &formats) {
  for (; node != nullptr; node = node->GetNext()) {
    switch (node->GetType()) {
    case wxXML_TEXT_NODE:
    case wxXML_CDATA_SECTION_NODE: {
      const wxString content = node->GetContent();
      text += content;
      formats.resize(text.Length(), format);
      break;
    }
    case wxXML_ELEMENT_NODE:
      // An unknown tag adds no format, but its text is kept.
      ReadChildren(node->GetChildren(), format | FlagForTag(node->GetName()),
                   text, formats);
      break;
    default:
      break;
    }
  }
}
} // namespace

void ReadLine(const wxXmlNode *line, wxString &text, Formats &formats,
              wxString *unknownAttributes) {
  if (!line)
    return;
  const std::size_t lineStart = text.Length();
  formats.resize(lineStart, None);
  ReadChildren(line->GetChildren(), None, text, formats);

  for (const wxXmlAttribute *attr = line->GetAttributes(); attr != nullptr;
       attr = attr->GetNext()) {
    const Format flag = FlagForAttribute(attr->GetName());
    if (flag == None) {
      if (unknownAttributes)
        *unknownAttributes += wxS(" ") + attr->GetName() + wxS("=\"") +
          EscapeAttribute(attr->GetValue()) + wxS("\"");
      continue;
    }
    for (const auto &range : ParseRanges(attr->GetValue())) {
      const std::size_t start = IndexOfCodePoint(text, lineStart, range.first);
      const std::size_t end = IndexOfCodePoint(text, lineStart, range.second);
      for (std::size_t i = start; i < end; ++i)
        formats[i] |= flag;
    }
  }
}

} // namespace TextFormat
