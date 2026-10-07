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
      const auto read = ReadXmlLine(wxS("<line") +
                                    LineAttributes(text, formats, 0, text.Length()) +
                                    wxS(">") + text + wxS("</line>"));
      REQUIRE(read.first == text);
      REQUIRE(read.second == formats);
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
      const auto read = ReadXmlLine(wxS("<line") +
                                    LineAttributes(text, formats, 0, text.Length()) +
                                    wxS(">") + text + wxS("</line>"));
      REQUIRE(read.second == formats);
    }
  }
  GIVEN("a bold superscript") {
    const Formats formats = {Format(Bold | Superscript)};
    THEN("it keeps both formats through saving and reading") {
      const auto read = ReadXmlLine(wxS("<line") + LineAttributes(wxS("2"), formats, 0, 1) +
                                    wxS(">2</line>"));
      REQUIRE(read.second == formats);
    }
  }
  GIVEN("a file claiming a character is both raised and lowered") {
    const auto read = ReadXmlLine(wxS("<line superscript=\"0-1\" subscript=\"0-1\">a</line>"));
    THEN("it is only one of them") {
      REQUIRE(((read.second[0] == Superscript) || (read.second[0] == Subscript)));
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
      const auto read = ReadXmlLine(wxS("<line bold=\"1-2\">") + text + wxS("</line>"));
      REQUIRE(read.second == formats);
    }
  }
}

SCENARIO("Inline formatting tags are read, for a future file format") {
  GIVEN("a line with known inline tags") {
    const auto read = ReadXmlLine(wxS("<line>a<b>b</b><i>c<u>d</u></i><s>e</s></line>"));
    THEN("their text is kept and their formats are applied") {
      REQUIRE(read.first == wxS("abcde"));
      REQUIRE(read.second == F("01268"));
    }
  }
  GIVEN("superscript and subscript tags") {
    const auto read = ReadXmlLine(wxS("<line>x<sup>2</sup>a<sub>i</sub></line>"));
    THEN("their text is kept and their formats are applied") {
      REQUIRE(read.first == wxS("x2ai"));
      REQUIRE(read.second == Formats({None, Superscript, None, Subscript}));
    }
  }
  GIVEN("a subscript tag inside a superscript tag") {
    const auto read = ReadXmlLine(wxS("<line><sup>a<sub>b</sub></sup></line>"));
    THEN("the inner one wins: a character can't be raised and lowered at once") {
      REQUIRE(read.second == Formats({Superscript, Subscript}));
    }
  }
  GIVEN("a line with a tag this version doesn't know") {
    const auto read = ReadXmlLine(wxS("<line>a<mark>2</mark>b</line>"));
    THEN("its text is kept, only its formatting is lost") {
      REQUIRE(read.first == wxS("a2b"));
      REQUIRE(IsPlain(read.second));
    }
  }
  GIVEN("an unknown tag inside a known one") {
    const auto read = ReadXmlLine(wxS("<line><b>x<mark>2</mark></b></line>"));
    THEN("the known format still applies to its text") {
      REQUIRE(read.first == wxS("x2"));
      REQUIRE(read.second == F("11"));
    }
  }
}

SCENARIO("Attributes this version doesn't know are kept") {
  GIVEN("a line with an attribute from a newer version") {
    wxString unknown;
    const auto read = ReadXmlLine(
      wxS("<line bold=\"0-1\" overline=\"1-2\" note=\"a&quot;b\">ab</line>"),
      &unknown);
    THEN("the known one is applied") { REQUIRE(read.second == F("10")); }
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
    const auto read = ReadXmlLine(wxS("<line bold=\"1-99\">abc</line>"));
    REQUIRE(read.second == F("011"));
  }
}

int main(int argc, char *argv[]) {
  wxInitializer initializer;
  return Catch::Session().run(argc, argv);
}
