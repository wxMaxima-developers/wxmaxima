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
  Tests for TextFormat (GH #492): how the bold/italic/... formatting of a text
  cell follows an edit of its text, and how it is written to and read from the
  \<line\> elements of a .wxmx file.
*/

#include "TextFormat.h"
#include <wx/init.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace TextFormat;

namespace {
//! Formats from a string of digits, one per character: "0011" = two plain,
//! two bold characters.
Formats F(const char *digits) {
  Formats retval;
  for (const char *c = digits; *c; ++c)
    retval.push_back(static_cast<Format>(*c - '0'));
  return retval;
}

//! Parse one \<line\> element and return its text and formats.
std::pair<wxString, Formats> ReadXmlLine(const wxString &xml,
                                         wxString *unknown = nullptr) {
  wxStringInputStream stream(xml);
  wxXmlDocument doc;
  REQUIRE(doc.Load(stream));
  wxString text;
  Formats formats;
  ReadLine(doc.GetRoot(), text, formats, unknown);
  return {text, formats};
}
} // namespace

SCENARIO("Formats follow a single edit of the text") {
  GIVEN("\"abcdef\" with \"cd\" bold") {
    const wxString text = wxS("abcdef");
    const Formats formats = F("001100");
    WHEN("a character is typed inside the bold part") {
      THEN("it is bold too, and everything else keeps its format") {
        REQUIRE(Reconcile(text, formats, wxS("abcXdef")) == F("0011100"));
      }
    }
    WHEN("a character is typed right after the bold part") {
      THEN("it continues the bold, as in a word processor") {
        REQUIRE(Reconcile(text, formats, wxS("abcdXef")) == F("0011100"));
      }
    }
    WHEN("a character is typed right in front of the bold part") {
      THEN("it continues the plain text before it") {
        REQUIRE(Reconcile(text, formats, wxS("abXcdef")) == F("0001100"));
      }
    }
    WHEN("the bold part is deleted") {
      THEN("nothing bold is left, which is plain text") {
        REQUIRE(Reconcile(text, formats, wxS("abef")).empty());
      }
    }
    WHEN("the bold part is replaced by something longer") {
      THEN("the replacement takes the format of what it replaced") {
        REQUIRE(Reconcile(text, formats, wxS("abXYZef")) == F("0011100"));
      }
    }
    WHEN("text is pasted in front of everything") {
      THEN("the formats move along with their characters") {
        REQUIRE(Reconcile(text, formats, wxS("123abcdef")) == F("000001100"));
      }
    }
  }
  GIVEN("a line that starts bold") {
    WHEN("a character is typed at the very start") {
      THEN("there is nothing in front of it, so it takes the bold after it") {
        REQUIRE(Reconcile(wxS("ab"), F("11"), wxS("Xab")) == F("111"));
      }
    }
    WHEN("a character is typed at the start of the second line") {
      THEN("the plain end of the line above doesn't decide its format") {
        REQUIRE(Reconcile(wxS("a\nb"), F("001"), wxS("a\nXb")) == F("0011"));
      }
    }
  }
  GIVEN("plain text") {
    THEN("any edit leaves it plain, without allocating formats") {
      REQUIRE(Reconcile(wxS("abc"), Formats(), wxS("abXc")).empty());
    }
  }
}

SCENARIO("Formats follow several edits at once") {
  GIVEN("a text where two words are bold") {
    const wxString text = wxS("one two three four");
    //                          "one two three four"
    const Formats formats = F("000011100000001111");
    WHEN("every \"o\" is replaced by \"oo\" (as a replace-all does)") {
      const wxString changed = wxS("oone twoo three foour");
      THEN("each character keeps its format, not just the ones before the "
           "first replacement and after the last") {
        REQUIRE(Reconcile(text, formats, changed) ==
                F("000001111000000011111"));
      }
    }
  }
}

SCENARIO("Switching a format on with nothing selected affects what is typed") {
  GIVEN("plain text and bold switched on at position 1") {
    WHEN("a character is typed there") {
      THEN("it is bold") {
        REQUIRE(Reconcile(wxS("ab"), Formats(), wxS("aXb"), true, 1, Bold) ==
                F("010"));
      }
    }
    WHEN("the character typed equals the one in front of it") {
      THEN("it is still the typed one that becomes bold") {
        // "ab" -> "aab": typed at 1, but a prefix match alone would say the
        // second "a" was typed and the first one was there before.
        REQUIRE(Reconcile(wxS("ab"), Formats(), wxS("aab"), true, 0, Bold) ==
                F("100"));
      }
    }
    WHEN("the character is typed somewhere else") {
      THEN("the pending format doesn't apply") {
        REQUIRE(Reconcile(wxS("ab"), Formats(), wxS("abX"), true, 1, Bold).empty());
      }
    }
  }
  GIVEN("bold text and bold switched off at its end") {
    THEN("what is typed there is plain") {
      REQUIRE(Reconcile(wxS("ab"), F("11"), wxS("abX"), true, 2, None) == F("110"));
    }
  }
}

SCENARIO("Formats are saved as <line> attributes") {
  GIVEN("a line with a bold and an italic part") {
    const wxString text = wxS("bold, italic");
    Formats formats(text.Length(), None);
    for (size_t i = 0; i < 4; ++i)
      formats[i] = Bold;
    for (size_t i = 6; i < 12; ++i)
      formats[i] = Italic;
    THEN("each format is a list of half-open ranges") {
      REQUIRE(LineAttributes(text, formats, 0, text.Length()) ==
              wxS(" bold=\"0-4\" italic=\"6-12\""));
    }
    THEN("reading them back gives the same text and formats") {
      const auto parsed = ReadXmlLine(wxS("<line") +
                                    LineAttributes(text, formats, 0, text.Length()) +
                                    wxS(">") + text + wxS("</line>"));
      REQUIRE(parsed.first == text);
      REQUIRE(parsed.second == formats);
    }
  }
  GIVEN("the second line of a cell") {
    const wxString text = wxS("ab\ncd");
    const Formats formats = F("00010");
    THEN("its ranges count from the line's own start") {
      REQUIRE(LineAttributes(text, formats, 3, 5) == wxS(" bold=\"0-1\""));
    }
  }
  GIVEN("a plain line") {
    THEN("it gets no attributes at all") {
      REQUIRE(LineAttributes(wxS("abc"), F("000"), 0, 3).IsEmpty());
      REQUIRE(LineAttributes(wxS("abc"), Formats(), 0, 3).IsEmpty());
    }
  }
  GIVEN("formats that overlap") {
    THEN("each is saved on its own") {
      REQUIRE(LineAttributes(wxS("abc"), F("131"), 0, 3) ==
              wxS(" bold=\"0-3\" italic=\"1-2\""));
    }
  }
}

SCENARIO("Superscript and subscript are saved like the other formats") {
  GIVEN("a line with a superscript and a subscript") {
    const wxString text = wxS("x2 a_i");
    const Formats formats = {None, Superscript, None, None, None, Subscript};
    THEN("each gets an attribute of its own") {
      REQUIRE(LineAttributes(text, formats, 0, text.Length()) ==
              wxS(" superscript=\"1-2\" subscript=\"5-6\""));
    }
    THEN("reading the attributes back restores the formats") {
      const auto parsed = ReadXmlLine(wxS("<line") +
                                    LineAttributes(text, formats, 0, text.Length()) +
                                    wxS(">") + text + wxS("</line>"));
      REQUIRE(parsed.second == formats);
    }
  }
  GIVEN("a bold superscript") {
    const Formats formats = {Format(Bold | Superscript)};
    THEN("it keeps both formats through saving and reading") {
      const auto parsed = ReadXmlLine(wxS("<line") + LineAttributes(wxS("2"), formats, 0, 1) +
                                    wxS(">2</line>"));
      REQUIRE(parsed.second == formats);
    }
  }
  GIVEN("a file claiming a character is both raised and lowered") {
    const auto parsed = ReadXmlLine(wxS("<line superscript=\"0-1\" subscript=\"0-1\">a</line>"));
    THEN("it is only one of them") {
      REQUIRE(((parsed.second[0] == Superscript) || (parsed.second[0] == Subscript)));
    }
  }
}

SCENARIO("Superscript and subscript exclude each other") {
  THEN("switching one on switches the other off") {
    REQUIRE(WithFlag(Superscript, Subscript) == Subscript);
    REQUIRE(WithFlag(Format(Bold | Subscript), Superscript) == Format(Bold | Superscript));
  }
  THEN("the other formats are combined as usual") {
    REQUIRE(WithFlag(Superscript, Bold) == Format(Bold | Superscript));
    REQUIRE(WithFlag(Italic, Underline) == Format(Italic | Underline));
  }
  THEN("each combination of font-changing formats gets a font of its own") {
    std::vector<bool> seen(FontVariants, false);
    for (Format f = 0; f <= WidthAffecting; ++f) {
      if ((f & ~WidthAffecting) || ((f & Superscript) && (f & Subscript)))
        continue;
      REQUIRE(FontIndex(f) < FontVariants);
      REQUIRE_FALSE(seen[FontIndex(f)]);
      seen[FontIndex(f)] = true;
    }
  }
}

SCENARIO("Ranges count Unicode code points") {
  GIVEN("a line with a character outside the Basic Multilingual Plane") {
    // U+1D44E MATHEMATICAL ITALIC SMALL A: one code point, but two wxString
    // characters where wxString stores UTF-16 (MS Windows).
    const wxString text = wxString::FromUTF8("\xF0\x9D\x91\x8E" "b");
    Formats formats(text.Length(), None);
    formats[text.Length() - 1] = Bold;
    THEN("the bold \"b\" is code point 1 on every platform") {
      REQUIRE(LineAttributes(text, formats, 0, text.Length()) == wxS(" bold=\"1-2\""));
    }
    THEN("reading the attribute back finds the \"b\"") {
      const auto parsed = ReadXmlLine(wxS("<line bold=\"1-2\">") + text + wxS("</line>"));
      REQUIRE(parsed.second == formats);
    }
  }
}

SCENARIO("Inline formatting tags are read, for a future file format") {
  GIVEN("a line with known inline tags") {
    const auto parsed = ReadXmlLine(wxS("<line>a<b>b</b><i>c<u>d</u></i><s>e</s></line>"));
    THEN("their text is kept and their formats are applied") {
      REQUIRE(parsed.first == wxS("abcde"));
      REQUIRE(parsed.second == F("01268"));
    }
  }
  GIVEN("superscript and subscript tags") {
    const auto parsed = ReadXmlLine(wxS("<line>x<sup>2</sup>a<sub>i</sub></line>"));
    THEN("their text is kept and their formats are applied") {
      REQUIRE(parsed.first == wxS("x2ai"));
      REQUIRE(parsed.second == Formats({None, Superscript, None, Subscript}));
    }
  }
  GIVEN("a subscript tag inside a superscript tag") {
    const auto parsed = ReadXmlLine(wxS("<line><sup>a<sub>b</sub></sup></line>"));
    THEN("the inner one wins: a character can't be raised and lowered at once") {
      REQUIRE(parsed.second == Formats({Superscript, Subscript}));
    }
  }
  GIVEN("a line with a tag this version doesn't know") {
    const auto parsed = ReadXmlLine(wxS("<line>a<mark>2</mark>b</line>"));
    THEN("its text is kept, only its formatting is lost") {
      REQUIRE(parsed.first == wxS("a2b"));
      REQUIRE(IsPlain(parsed.second));
    }
  }
  GIVEN("an unknown tag inside a known one") {
    const auto parsed = ReadXmlLine(wxS("<line><b>x<mark>2</mark></b></line>"));
    THEN("the known format still applies to its text") {
      REQUIRE(parsed.first == wxS("x2"));
      REQUIRE(parsed.second == F("11"));
    }
  }
}

SCENARIO("Attributes this version doesn't know are kept") {
  GIVEN("a line with an attribute from a newer version") {
    wxString unknown;
    const auto parsed = ReadXmlLine(
      wxS("<line bold=\"0-1\" overline=\"1-2\" note=\"a&quot;b\">ab</line>"),
      &unknown);
    THEN("the known one is applied") { REQUIRE(parsed.second == F("10")); }
    THEN("the unknown ones are returned, ready to be written out again") {
      REQUIRE(unknown == wxS(" overline=\"1-2\" note=\"a&quot;b\""));
    }
  }
}

SCENARIO("Malformed ranges are skipped") {
  THEN("only the well-formed ones are used") {
    const auto ranges = ParseRanges(wxS("0-2, x-3,5-4,,7-9"));
    REQUIRE(ranges.size() == 2);
    REQUIRE(ranges[0] == std::make_pair(std::size_t(0), std::size_t(2)));
    REQUIRE(ranges[1] == std::make_pair(std::size_t(7), std::size_t(9)));
  }
  THEN("a range past the end of the line is clipped to it") {
    const auto parsed = ReadXmlLine(wxS("<line bold=\"1-99\">abc</line>"));
    REQUIRE(parsed.second == F("011"));
  }
}

SCENARIO("Text is split into runs for other programs' formats") {
  GIVEN("two lines, partly bold, the bold part spanning the line break") {
    const wxString text = wxS("ab\ncd");
    const Formats formats = {None, Bold, Bold, Bold, None};
    WHEN("the whole text is split") {
      const std::vector<Run> runs = Runs(text, formats, 0, text.Length());
      THEN("the line break is a plain run of its own") {
        REQUIRE(runs.size() == 5);
        REQUIRE(runs[0].start == 0); REQUIRE(runs[0].end == 1);
        REQUIRE(runs[0].format == None);
        REQUIRE(runs[1].start == 1); REQUIRE(runs[1].end == 2);
        REQUIRE(runs[1].format == Bold);
        REQUIRE(runs[2].start == 2); REQUIRE(runs[2].end == 3);
        REQUIRE(runs[2].format == None);
        REQUIRE(runs[3].start == 3); REQUIRE(runs[3].end == 4);
        REQUIRE(runs[3].format == Bold);
        REQUIRE(runs[4].start == 4); REQUIRE(runs[4].end == 5);
        REQUIRE(runs[4].format == None);
      }
    }
    WHEN("only part of it is split") {
      const std::vector<Run> runs = Runs(text, formats, 1, 2);
      THEN("only that part is covered") {
        REQUIRE(runs.size() == 1);
        REQUIRE(runs[0].start == 1);
        REQUIRE(runs[0].end == 2);
      }
    }
  }
  GIVEN("plain text") {
    THEN("it is one run") {
      const std::vector<Run> runs = Runs(wxS("abc"), Formats(), 0, 3);
      REQUIRE(runs.size() == 1);
      REQUIRE(runs[0].format == None);
      REQUIRE(runs[0].end == 3);
    }
  }
  GIVEN("a range past the end") {
    THEN("it is cut off at the end of the text") {
      const std::vector<Run> runs = Runs(wxS("abc"), Formats(), 1, 99);
      REQUIRE(runs.size() == 1);
      REQUIRE(runs[0].end == 3);
    }
  }
}

int main(int argc, char *argv[]) {
  wxInitializer initializer;
  return Catch::Session().run(argc, argv);
}

namespace {
/*! The format of every character of word in text, if they all share one.

  Returns 255 if word isn't in text or its characters are formatted
  differently, which no format test expects.
*/
Format FormatOfWord(const wxString &text, const Formats &formats, const wxString &word) {
  const auto pos = text.find(word);
  if (pos == wxString::npos)
    return 255;
  const Format format = At(formats, pos);
  for (std::size_t i = pos; i < pos + word.Length(); ++i)
    if (At(formats, i) != format)
      return 255;
  return format;
}
} // namespace

SCENARIO("Formatted HTML from other programs is read (GH #2475)") {
  wxString text;
  Formats formats;
  GIVEN("what a browser copies: a whole document with a fragment marker") {
    const wxString html = wxS(
      "<html><head><meta charset=\"utf-8\"><style>p {color: red}</style>"
      "<title>Not this</title></head><body>\n<!--StartFragment-->"
      "Plain <b>bold</b> and <em>italic</em>,\n   x<sup>2</sup> "
      "&amp; H<sub>2</sub>O<!--EndFragment-->\n</body></html>");
    WHEN("it is read") {
      ReadHTML(html, text, formats);
      THEN("the text is what the fragment shows, white space collapsed") {
        REQUIRE(text == wxS("Plain bold and italic, x2 & H2O"));
        REQUIRE(formats.size() == text.Length());
      }
      THEN("each word has its format") {
        REQUIRE(FormatOfWord(text, formats, wxS("Plain ")) == None);
        REQUIRE(FormatOfWord(text, formats, wxS("bold")) == Bold);
        REQUIRE(FormatOfWord(text, formats, wxS("italic")) == Italic);
        REQUIRE(At(formats, text.find(wxS("x2")) + 1) == Superscript);
        REQUIRE(At(formats, text.find(wxS("H2O")) + 1) == Subscript);
        REQUIRE(FormatOfWord(text, formats, wxS("O")) == None);
      }
    }
  }
  GIVEN("what Google Docs copies: CSS on spans, inside a bold that isn't") {
    const wxString html = wxS(
      "<meta charset=\"utf-8\"><b style=\"font-weight:normal;\" id=\"docs-internal-guid-1\">"
      "<span style=\"font-weight:400;font-style:normal\">Plain </span>"
      "<span style=\"font-weight:700\">bold</span> "
      "<span style=\"font-style:italic;text-decoration:underline\">both</span> "
      "<span style='text-decoration: line-through'>gone</span> "
      "<span style=\"vertical-align:super\">up</span></b>");
    WHEN("it is read") {
      ReadHTML(html, text, formats);
      THEN("the CSS decides, the wrapping <b> not") {
        REQUIRE(text == wxS("Plain bold both gone up"));
        REQUIRE(FormatOfWord(text, formats, wxS("Plain")) == None);
        REQUIRE(FormatOfWord(text, formats, wxS("bold")) == Bold);
        REQUIRE(FormatOfWord(text, formats, wxS("both")) == (Italic | Underline));
        REQUIRE(FormatOfWord(text, formats, wxS("gone")) == Strikethrough);
        REQUIRE(FormatOfWord(text, formats, wxS("up")) == Superscript);
      }
    }
  }
  GIVEN("paragraphs, line breaks and a list") {
    const wxString html = wxS(
      "<p>First <strong>para</p><p>graph</strong></p><p>a<br>b<br/></p>"
      "<ul><li>one</li><li>two</li></ul><div>end</div>");
    WHEN("it is read") {
      ReadHTML(html, text, formats);
      THEN("they become line breaks, a list item a bullet") {
        REQUIRE(text == wxS("First para\ngraph\na\nb\n* one\n* two\nend"));
      }
      THEN("an element left open is closed by the one around it") {
        REQUIRE(FormatOfWord(text, formats, wxS("para")) == Bold);
        REQUIRE(FormatOfWord(text, formats, wxS("graph")) == None);
      }
    }
  }
  GIVEN("entities and a lone less-than sign") {
    WHEN("they are read") {
      ReadHTML(wxS("a&nbsp;&lt;&nbsp;b &#8594; c&#x2192;d &unknown; e < f"), text, formats);
      THEN("they become the characters they stand for") {
        REQUIRE(text == wxString::FromUTF8("a < b \xe2\x86\x92 c\xe2\x86\x92" "d &unknown; e < f"));
      }
    }
  }
  GIVEN("a Windows CF_HTML header without fragment markers") {
    WHEN("it is read") {
      ReadHTML(wxS("Version:0.9\r\nStartHTML:0000000105\r\n<i>it</i>"), text, formats);
      THEN("the header isn't text") {
        REQUIRE(text == wxS("it"));
        REQUIRE(FormatOfWord(text, formats, wxS("it")) == Italic);
      }
    }
  }
  GIVEN("preformatted text") {
    WHEN("it is read") {
      ReadHTML(wxS("<pre>a  b\nc</pre>"), text, formats);
      THEN("its white space stays") { REQUIRE(text == wxS("a  b\nc")); }
    }
  }
  GIVEN("HTML without text") {
    WHEN("it is read") {
      ReadHTML(wxS("<img src=\"x.png\"><script>var a = '<b>';</script>"), text, formats);
      THEN("there is none") {
        REQUIRE(text.IsEmpty());
        REQUIRE(formats.empty());
      }
    }
  }
}
