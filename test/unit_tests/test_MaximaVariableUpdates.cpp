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
  Tests for ParseMaximaVariableUpdates(), the GUI-free parser for the
  <variables> XML documents Maxima sends (extracted from
  wxMaxima::ReadVariables so the parsing is unit-testable).
*/

#define CATCH_CONFIG_RUNNER
#include "MaximaVariableUpdates.cpp"
#include <catch2/catch.hpp>

#include <wx/init.h>
#include <wx/sstream.h>

// Parses an XML string the way wxMaxima::MaximaEvent does before calling
// ReadVariables.
static std::vector<MaximaVariableUpdate> Parse(const wxString &xml) {
  wxXmlDocument xmldoc;
  wxStringInputStream xmlStream(xml);
  xmldoc.Load(xmlStream);
  return ParseMaximaVariableUpdates(xmldoc);
}

SCENARIO("A bound variable is parsed with its raw value") {
  GIVEN("a <variables> document with one name/value pair") {
    auto updates = Parse(wxS("<variables><variable><name>numer</name>"
                             "<value>true</value></variable></variables>"));
    THEN("one bound update with that name and value results") {
      REQUIRE(updates.size() == 1);
      REQUIRE(updates[0].m_bound);
      REQUIRE(updates[0].m_name == wxS("numer"));
      REQUIRE(updates[0].m_value == wxS("true"));
    }
  }

  GIVEN("a string value that stringdisp:true wrapped in quotes") {
    auto updates = Parse(wxS("<variables><variable><name>gentranlang</name>"
                             "<value>\"c\"</value></variable></variables>"));
    THEN("the value keeps its quotes - stripping them is the consumer's job") {
      REQUIRE(updates.size() == 1);
      REQUIRE(updates[0].m_value == wxS("\"c\""));
    }
  }
}

SCENARIO("An unbound variable is parsed as not bound") {
  GIVEN("a <variable> element without a <value>") {
    auto updates = Parse(wxS("<variables><variable><name>sinnpiflag</name>"
                             "</variable></variables>"));
    THEN("the update is unbound") {
      REQUIRE(updates.size() == 1);
      REQUIRE_FALSE(updates[0].m_bound);
      REQUIRE(updates[0].m_name == wxS("sinnpiflag"));
    }
  }

  GIVEN("a <variable> element with an EMPTY <value/>") {
    auto updates = Parse(wxS("<variables><variable><name>x</name>"
                             "<value></value></variable></variables>"));
    THEN("it also counts as unbound, like in the original ReadVariables") {
      REQUIRE(updates.size() == 1);
      REQUIRE_FALSE(updates[0].m_bound);
    }
  }
}

SCENARIO("Several variables in one document all arrive") {
  GIVEN("a document mixing bound and unbound variables") {
    auto updates = Parse(wxS("<variables>"
                             "<variable><name>a</name><value>1</value></variable>"
                             "<variable><name>b</name></variable>"
                             "<variable><name>c</name><value>3</value></variable>"
                             "</variables>"));
    THEN("three updates result, in document order, with correct boundness") {
      REQUIRE(updates.size() == 3);
      REQUIRE(updates[0].m_name == wxS("a"));
      REQUIRE(updates[0].m_bound);
      REQUIRE(updates[1].m_name == wxS("b"));
      REQUIRE_FALSE(updates[1].m_bound);
      REQUIRE(updates[2].m_name == wxS("c"));
      REQUIRE(updates[2].m_value == wxS("3"));
    }
  }
}

SCENARIO("Degenerate documents yield no updates instead of garbage") {
  GIVEN("an empty document") {
    wxXmlDocument xmldoc; // never loaded -> no root
    THEN("the parser returns an empty list") {
      REQUIRE(ParseMaximaVariableUpdates(xmldoc).empty());
    }
  }

  GIVEN("a <variable> element without a <name>") {
    auto updates = Parse(wxS("<variables><variable>"
                             "<value>orphan</value></variable></variables>"));
    THEN("it is dropped - there is nothing to dispatch it to") {
      REQUIRE(updates.empty());
    }
  }

  GIVEN("whitespace and unknown elements between the variables") {
    auto updates = Parse(wxS("<variables>\n  "
                             "<variable>\n    <name>a</name>\n    "
                             "<value>1</value>\n  </variable>\n  "
                             "<unrelated/>\n"
                             "</variables>"));
    THEN("only the real variable is reported") {
      REQUIRE(updates.size() == 1);
      REQUIRE(updates[0].m_name == wxS("a"));
      REQUIRE(updates[0].m_bound);
    }
  }
}

int main(int argc, char **argv) {
  wxInitializer initializer;
  if (!initializer.IsOk())
    return 1;
  return Catch::Session().run(argc, argv);
}
