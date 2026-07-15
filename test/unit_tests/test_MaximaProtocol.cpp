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
  Tests for the pure Maxima-protocol helpers (MaximaProtocol): classifying the
  <PROMPT> labels Maxima sends, and detecting commands that would put nothing
  but a bare newline on the wire. No GUI, no Maxima - the helpers are fed
  captured prompt/command strings.
*/

#define CATCH_CONFIG_RUNNER
#include "MaximaProtocol.cpp"
#include <catch2/catch.hpp>

using namespace MaximaProtocol;

SCENARIO("Normal numbered input prompts are recognized") {
  GIVEN("the (%iN)/(%oN) prompts Maxima prints") {
    THEN("they are numbered input prompts") {
      REQUIRE(IsNumberedInputPrompt(wxS("(%i1) ")));
      REQUIRE(IsNumberedInputPrompt(wxS("(%i42) ")));
      REQUIRE(IsNumberedInputPrompt(wxS("(%o7) ")));
      // High ibase: the digit before ')' can be A-Z.
      REQUIRE(IsNumberedInputPrompt(wxS("(%iF) ")));
    }
    THEN("they are main prompts (queue-advancing), never questions") {
      REQUIRE(IsMainInputPrompt(wxS("(%i1) "), false));
      REQUIRE(IsMainInputPrompt(wxS("(%o7) "), false));
    }
    THEN("they are neither debugger nor Lisp-REPL prompts") {
      REQUIRE_FALSE(IsDebuggerPrompt(wxS("(%i1) ")));
      REQUIRE_FALSE(IsLispReplPrompt(wxS("(%i1) ")));
    }
  }
}

SCENARIO("Questions are not mistaken for main prompts") {
  GIVEN("the free-form text Maxima uses when it asks something") {
    THEN("a question is not a main prompt while in Maxima mode") {
      REQUIRE_FALSE(IsMainInputPrompt(wxS("Is x positive, negative or zero?"),
                                      false));
      REQUIRE_FALSE(IsNumberedInputPrompt(wxS("Is x positive?")));
    }
  }
}

SCENARIO("The Lisp REPL prompt (after to_lisp()) is recognized") {
  GIVEN("the MAXIMA> prompt, possibly preceded by a newline") {
    THEN("it is a Lisp-REPL prompt") {
      REQUIRE(IsLispReplPrompt(wxS("MAXIMA> ")));
      REQUIRE(IsLispReplPrompt(wxS("\nMAXIMA> ")));
    }
    THEN("it counts as a main prompt (Lisp forms advance the queue)") {
      REQUIRE(IsMainInputPrompt(wxS("MAXIMA> "), false));
    }
    THEN("it is not a numbered input or debugger prompt") {
      REQUIRE_FALSE(IsNumberedInputPrompt(wxS("MAXIMA> ")));
      REQUIRE_FALSE(IsDebuggerPrompt(wxS("MAXIMA> ")));
    }
  }
}

SCENARIO("The Maxima debugger prompt is recognized") {
  GIVEN("the (dbm:N) prompt") {
    THEN("it is a debugger prompt") {
      REQUIRE(IsDebuggerPrompt(wxS("(dbm:1) ")));
      REQUIRE(IsDebuggerPrompt(wxS("(dbm:3) ")));
    }
    THEN("it is not a numbered input prompt (even though it parenthesizes)") {
      REQUIRE_FALSE(IsNumberedInputPrompt(wxS("(dbm:1) ")));
    }
  }
}

SCENARIO("A debugger prompt is always a question, regardless of Lisp mode") {
  // A debugger session turns Lisp mode on (for the pictogram + tokenizer), so
  // ReadPrompt sees the first (dbm:N) prompt with inLispMode == false but every
  // subsequent one with inLispMode == true. A (dbm:N) prompt must be classified
  // as a question either way - otherwise the second and later debugger commands
  // would advance the eval queue and be recorded as ordinary worksheet input.
  GIVEN("the first debugger prompt (Lisp mode not yet on)") {
    THEN("it is a question, not a main prompt") {
      REQUIRE_FALSE(IsMainInputPrompt(wxS("(dbm:1) "), false));
    }
  }
  GIVEN("a later debugger prompt (Lisp mode now on)") {
    THEN("it is still a question, not a main prompt") {
      REQUIRE_FALSE(IsMainInputPrompt(wxS("(dbm:1) "), true));
    }
  }
}

SCENARIO("A blank command would transmit only a bare newline") {
  // SendMaxima trims trailing whitespace and appends "\n"; anything that trims
  // to empty becomes a lone blank line on the wire. At the (dbm:N) prompt a
  // blank line means "repeat the last command", so such sends must be skipped.
  GIVEN("empty or all-whitespace strings") {
    THEN("they are blank commands") {
      REQUIRE(CommandIsBlank(wxEmptyString));
      REQUIRE(CommandIsBlank(wxS("   ")));
      REQUIRE(CommandIsBlank(wxS("\n")));
      REQUIRE(CommandIsBlank(wxS("\t\n ")));
    }
  }
  GIVEN("strings with any non-whitespace content") {
    THEN("they are not blank") {
      REQUIRE_FALSE(CommandIsBlank(wxS(":continue")));
      REQUIRE_FALSE(CommandIsBlank(wxS("1+1;")));
      // Trailing whitespace is trimmed, but leading content remains.
      REQUIRE_FALSE(CommandIsBlank(wxS(":h   \n")));
      REQUIRE_FALSE(CommandIsBlank(wxS("  x")));
    }
  }
}

int main(int argc, char *argv[]) {
  return Catch::Session().run(argc, argv);
}
