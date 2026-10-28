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
#include <wx/wxcrt.h>
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
  {Strikethrough, wxS("strikethrough")},
  {Superscript, wxS("superscript")},
  {Subscript, wxS("subscript")}};

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
  if (t == wxS("sup"))
    return Superscript;
  if (t == wxS("sub"))
    return Subscript;
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

std::vector<Run> Runs(const wxString &text, const Formats &formats,
                      std::size_t start, std::size_t end) {
  std::vector<Run> runs;
  end = std::min(end, text.Length());
  for (std::size_t pos = start; pos < end; ++pos) {
    const bool lineBreak = (text[pos] == wxS('\n'));
    const Format format = lineBreak ? Format(None) : At(formats, pos);
    if (lineBreak || runs.empty() || (runs.back().format != format) ||
        (text[pos - 1] == wxS('\n')))
      runs.push_back({pos, pos + 1, format});
    else
      runs.back().end = pos + 1;
  }
  return runs;
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
      const wxString& content = node->GetContent();
      text += content;
      formats.resize(text.Length(), format);
      break;
    }
    case wxXML_ELEMENT_NODE:
      // An unknown tag adds no format, but its text is kept.
      ReadChildren(node->GetChildren(), WithFlag(format, FlagForTag(node->GetName())),
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
        formats[i] = WithFlag(formats[i], flag);
    }
  }
}

namespace {
//! What an element does to the format of the text inside it
struct FormatChange {
  wxString tag;
  Format set = None;
  Format clear = None;
};

//! The format text inside all of these elements gets
Format FormatOf(const std::vector<FormatChange> &stack) {
  Format format = None;
  for (const auto &change : stack) {
    format &= static_cast<Format>(~change.clear);
    for (Format flag = 1; flag != 0 && flag <= Subscript; flag <<= 1)
      if (change.set & flag)
        format = WithFlag(format, flag);
  }
  return format;
}

//! Adds what a CSS style attribute says about our formats to change
void ReadStyle(const wxString &style, FormatChange &change) {
  auto set = [&change](Format flag) {
    change.set |= flag;
    change.clear &= static_cast<Format>(~flag);
  };
  auto clear = [&change](Format flag) {
    change.clear |= flag;
    change.set &= static_cast<Format>(~flag);
  };
  wxStringTokenizer declarations(style, wxS(";"));
  while (declarations.HasMoreTokens()) {
    const wxString declaration = declarations.GetNextToken();
    const wxString property = declaration.BeforeFirst(wxS(':')).Trim().Trim(false).Lower();
    wxString value = declaration.AfterFirst(wxS(':')).Trim().Trim(false).Lower();
    value.Replace(wxS("!important"), wxEmptyString);
    value.Trim();
    if (property == wxS("font-weight")) {
      long weight = 0;
      if ((value == wxS("bold")) || (value == wxS("bolder")) ||
          (value.ToLong(&weight) && (weight >= 600)))
        set(Bold);
      else if ((value == wxS("normal")) || (value == wxS("lighter")) ||
               value.ToLong(&weight))
        clear(Bold);
    } else if (property == wxS("font-style")) {
      if (value.StartsWith(wxS("italic")) || value.StartsWith(wxS("oblique")))
        set(Italic);
      else if (value == wxS("normal"))
        clear(Italic);
    } else if ((property == wxS("text-decoration")) ||
               (property == wxS("text-decoration-line"))) {
      if (value.StartsWith(wxS("none")))
        clear(Underline | Strikethrough);
      if (value.Contains(wxS("underline")))
        set(Underline);
      if (value.Contains(wxS("line-through")))
        set(Strikethrough);
    } else if (property == wxS("vertical-align")) {
      if (value == wxS("super"))
        set(Superscript);
      else if (value == wxS("sub"))
        set(Subscript);
      else if (value == wxS("baseline"))
        clear(VerticalPosition);
    }
  }
}

//! The character a named or numeric entity (without & and ;) stands for
bool DecodeEntity(const wxString &name, wxString &result) {
  if (name.StartsWith(wxS("#"))) {
    unsigned long code = 0;
    const bool ok = (name.Length() > 1) && ((name[1] == 'x') || (name[1] == 'X'))
      ? name.Mid(2).ToULong(&code, 16)
      : name.Mid(1).ToULong(&code, 10);
    if (!ok || (code == 0) || (code > 0x10FFFF))
      return false;
    result = wxString(wxUniChar(static_cast<wxUint32>(code)));
    return true;
  }
  static const struct {
    const wxChar *name;
    wxUint32 code;
  } entities[] = {
    {wxS("amp"), '&'},       {wxS("lt"), '<'},         {wxS("gt"), '>'},
    {wxS("quot"), '"'},      {wxS("apos"), '\''},      {wxS("nbsp"), 0xA0},
    {wxS("ndash"), 0x2013},  {wxS("mdash"), 0x2014},   {wxS("hellip"), 0x2026},
    {wxS("euro"), 0x20AC},   {wxS("copy"), 0xA9},      {wxS("reg"), 0xAE},
    {wxS("laquo"), 0xAB},    {wxS("raquo"), 0xBB},     {wxS("lsquo"), 0x2018},
    {wxS("rsquo"), 0x2019},  {wxS("ldquo"), 0x201C},   {wxS("rdquo"), 0x201D},
    {wxS("bdquo"), 0x201E},  {wxS("times"), 0xD7},     {wxS("divide"), 0xF7},
    {wxS("deg"), 0xB0},      {wxS("middot"), 0xB7},    {wxS("plusmn"), 0xB1},
    {wxS("minus"), 0x2212},  {wxS("bull"), 0x2022},    {wxS("shy"), 0xAD}};
  for (const auto &entity : entities)
    if (name == entity.name) {
      result = wxString(wxUniChar(entity.code));
      return true;
    }
  return false;
}

bool IsHTMLSpace(wxUniChar c) {
  return (c == ' ') || (c == '\t') || (c == '\n') || (c == '\r') || (c == '\f');
}

//! The part of a clipboard HTML document that holds what was copied
wxString HTMLFragment(const wxString &html) {
  const wxString lower = html.Lower();
  // Everything between start and the end marker, or else nothing
  auto between = [&](const wxString &start, const wxChar *startEnd,
                     const wxString &end, wxString &result) {
    const std::size_t startPos = lower.find(start);
    if (startPos == wxString::npos)
      return false;
    const std::size_t contentStart = lower.find(startEnd, startPos);
    if (contentStart == wxString::npos)
      return false;
    const std::size_t from = contentStart + wxStrlen(startEnd);
    const std::size_t endPos = lower.find(end, from);
    result = html.Mid(from, (endPos == wxString::npos) ? wxString::npos : endPos - from);
    return true;
  };
  wxString result;
  if (between(wxS("<!--startfragment"), wxS("-->"), wxS("<!--endfragment"), result))
    return result;
  if (between(wxS("<body"), wxS(">"), wxS("</body"), result))
    return result;
  // A CF_HTML header without fragment markers: skip to the first tag
  if (html.StartsWith(wxS("Version:"))) {
    const std::size_t firstTag = html.find(wxS('<'));
    if (firstTag != wxString::npos)
      return html.Mid(firstTag);
  }
  return html;
}

bool IsVoidElement(const wxString &tag) {
  static const wxString voids[] = {wxS("br"),  wxS("img"),  wxS("hr"),    wxS("meta"),
                                   wxS("link"), wxS("input"), wxS("wbr"),  wxS("col"),
                                   wxS("area"), wxS("base"), wxS("param"), wxS("source")};
  for (const auto &v : voids)
    if (tag == v)
      return true;
  return false;
}

//! Elements whose start and end end the current line
bool IsBlockElement(const wxString &tag) {
  static const wxString blocks[] = {
    wxS("p"),  wxS("div"), wxS("h1"), wxS("h2"), wxS("h3"), wxS("h4"),
    wxS("h5"), wxS("h6"),  wxS("li"), wxS("tr"), wxS("ul"), wxS("ol"),
    wxS("table"), wxS("blockquote"), wxS("pre"), wxS("hr"), wxS("dt"), wxS("dd")};
  for (const auto &b : blocks)
    if (tag == b)
      return true;
  return false;
}
} // namespace

void ReadHTML(const wxString &html, wxString &text, Formats &formats) {
  text.clear();
  formats.clear();
  const wxString fragment = HTMLFragment(html);
  std::vector<FormatChange> stack;
  Format format = None;
  bool pendingSpace = false;
  Format pendingSpaceFormat = None;
  int preDepth = 0;

  auto append = [&](const wxString &chars, Format f) {
    text += chars;
    formats.insert(formats.end(), chars.Length(), f);
  };
  auto atLineStart = [&]() { return text.IsEmpty() || text.Last() == '\n'; };
  auto lineBreak = [&]() {
    pendingSpace = false;
    if (!atLineStart())
      append(wxS("\n"), None);
  };
  auto emit = [&](const wxString &chars) {
    if (pendingSpace && !atLineStart())
      append(wxS(" "), pendingSpaceFormat);
    pendingSpace = false;
    append(chars, format);
  };

  const std::size_t len = fragment.Length();
  std::size_t i = 0;
  while (i < len) {
    const wxUniChar c = fragment[i];
    if (c == '<') {
      // Not a tag, like the "<" in "a < b"
      const wxUniChar next = (i + 1 < len) ? fragment[i + 1] : wxUniChar(' ');
      if (!wxIsalpha(next.GetValue()) && (next != '/') && (next != '!') && (next != '?')) {
        emit(wxS("<"));
        ++i;
        continue;
      }
      if (fragment.compare(i, 4, wxS("<!--")) == 0) {
        const std::size_t end = fragment.find(wxS("-->"), i + 4);
        i = (end == wxString::npos) ? len : end + 3;
        continue;
      }
      if ((i + 1 < len) && ((fragment[i + 1] == '!') || (fragment[i + 1] == '?'))) {
        const std::size_t end = fragment.find(wxS('>'), i);
        i = (end == wxString::npos) ? len : end + 1;
        continue;
      }
      // A tag: find its end, minding quoted attribute values
      std::size_t end = i + 1;
      wxUniChar quote = 0;
      while (end < len) {
        const wxUniChar d = fragment[end];
        if (quote != 0) {
          if (d == quote)
            quote = 0;
        } else if ((d == '"') || (d == '\''))
          quote = d;
        else if (d == '>')
          break;
        ++end;
      }
      wxString tagText = fragment.Mid(i + 1, end - i - 1);
      i = (end >= len) ? len : end + 1;
      const bool closing = tagText.StartsWith(wxS("/"));
      if (closing)
        tagText.Remove(0, 1);
      std::size_t nameEnd = 0;
      while ((nameEnd < tagText.Length()) && !IsHTMLSpace(tagText[nameEnd]) &&
             (tagText[nameEnd] != '/'))
        ++nameEnd;
      const wxString tag = tagText.Left(nameEnd).Lower();
      if (tag.IsEmpty())
        continue;
      if (closing) {
        if (IsBlockElement(tag))
          lineBreak();
        if ((tag == wxS("td")) || (tag == wxS("th"))) {
          pendingSpace = true;
          pendingSpaceFormat = None;
        }
        for (std::size_t n = stack.size(); n > 0; --n)
          if (stack[n - 1].tag == tag) {
            if (tag == wxS("pre"))
              preDepth = std::max(0, preDepth - 1);
            stack.resize(n - 1);
            format = FormatOf(stack);
            break;
          }
        continue;
      }
      if ((tag == wxS("script")) || (tag == wxS("style")) ||
          (tag == wxS("head")) || (tag == wxS("title"))) {
        if (tagText.EndsWith(wxS("/")))
          continue;
        const std::size_t close = fragment.Lower().find(wxS("</") + tag, i);
        if (close == wxString::npos)
          i = len;
        else {
          const std::size_t closeEnd = fragment.find(wxS('>'), close);
          i = (closeEnd == wxString::npos) ? len : closeEnd + 1;
        }
        continue;
      }
      if (tag == wxS("br")) {
        pendingSpace = false;
        append(wxS("\n"), None);
        continue;
      }
      if (IsBlockElement(tag))
        lineBreak();
      if (IsVoidElement(tag) || tagText.EndsWith(wxS("/")))
        continue;
      FormatChange change;
      change.tag = tag;
      change.set = FlagForTag(tag);
      // The style attribute, if there is one
      const wxString lowerTag = tagText.Lower();
      std::size_t pos = 0;
      while ((pos = lowerTag.find(wxS("style"), pos)) != wxString::npos) {
        const bool startsWord = (pos > 0) && IsHTMLSpace(lowerTag[pos - 1]);
        pos += 5;
        std::size_t eq = pos;
        while ((eq < lowerTag.Length()) && IsHTMLSpace(lowerTag[eq]))
          ++eq;
        if (!startsWord || (eq >= lowerTag.Length()) || (lowerTag[eq] != '='))
          continue;
        std::size_t valueStart = eq + 1;
        while ((valueStart < tagText.Length()) && IsHTMLSpace(tagText[valueStart]))
          ++valueStart;
        if (valueStart >= tagText.Length())
          break;
        std::size_t valueEnd;
        if ((tagText[valueStart] == '"') || (tagText[valueStart] == '\'')) {
          valueEnd = tagText.find(tagText[valueStart], valueStart + 1);
          ++valueStart;
        } else {
          valueEnd = valueStart;
          while ((valueEnd < tagText.Length()) && !IsHTMLSpace(tagText[valueEnd]))
            ++valueEnd;
        }
        wxString style = tagText.Mid(valueStart, (valueEnd == wxString::npos)
                                                   ? wxString::npos
                                                   : valueEnd - valueStart);
        style.Replace(wxS("&quot;"), wxS("\""));
        ReadStyle(style, change);
        break;
      }
      if (tag == wxS("pre"))
        ++preDepth;
      stack.push_back(change);
      format = FormatOf(stack);
      if (tag == wxS("li"))
        emit(wxS("* "));
      continue;
    }
    if (c == '&') {
      const std::size_t semicolon = fragment.find(wxS(';'), i);
      wxString decoded;
      if ((semicolon != wxString::npos) && (semicolon - i <= 10) &&
          DecodeEntity(fragment.Mid(i + 1, semicolon - i - 1), decoded)) {
        // A non-breaking space is a space to us
        if (decoded == wxString(wxUniChar(0xA0)))
          decoded = wxS(" ");
        // ...and a soft hyphen nothing
        if (decoded != wxString(wxUniChar(0xAD)))
          emit(decoded);
        i = semicolon + 1;
        continue;
      }
      emit(wxS("&"));
      ++i;
      continue;
    }
    if (IsHTMLSpace(c)) {
      if (preDepth > 0) {
        if (c == '\n')
          append(wxS("\n"), None);
        else if (c != '\r')
          append(wxString(c), format);
      } else if (!pendingSpace) {
        pendingSpace = true;
        pendingSpaceFormat = format;
      }
      ++i;
      continue;
    }
    if (c == wxUniChar(0xA0))
      emit(wxS(" "));
    else
      emit(wxString(c));
    ++i;
  }
  // What the browser wouldn't show either: trailing white space and blank lines
  while (!text.IsEmpty() && IsHTMLSpace(text.Last())) {
    text.RemoveLast();
    formats.pop_back();
  }
}

} // namespace TextFormat
