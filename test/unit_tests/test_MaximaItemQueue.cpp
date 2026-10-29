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
  Tests for MaximaItemQueue: how Maxima's output gets from the socket thread
  to the GUI without ever keeping the GUI from handling input (GH #2594).
  GUI-free, so tested directly.
*/

#include "MaximaItemQueue.h"
#include <wx/hashmap.h>
#include <chrono>
#include <thread>
#include <unordered_map>

// Not CATCH_CONFIG_MAIN: with _UNICODE defined (as wxWidgets does on Windows)
// Catch then defines wmain(), which MinGW's startup code does not look for --
// the link fails with "undefined symbol: WinMain". Every test here therefore
// supplies its own main(), at the end of this file.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using Item = MaximaItemQueue::Item;

namespace {
// Stand-ins for Maxima::EventCause
constexpr int Text = 1;
constexpr int Prompt = 2;
constexpr int Maths = 10;

const std::unordered_map<wxString, int, wxStringHash> &Tags() {
  static const std::unordered_map<wxString, int, wxStringHash> tags = {
    {wxS("PROMPT"), Prompt}, {wxS("math"), Maths}, {wxS("mth"), Maths}};
  return tags;
}

//! What disp(i) in a loop sends
wxString DispOutput(int first, int last) {
  wxString output;
  for (int i = first; i <= last; i++)
    output += wxString::Format(wxS("<math><mn>%i</mn></math>\n"), i);
  return output;
}

//! Splits data that arrives in chunks of the given sizes, as the socket thread does
std::vector<Item> SplitInChunks(const wxString &data, const std::vector<size_t> &chunkSizes) {
  std::vector<Item> items;
  wxString buffer;
  size_t pos = 0;
  size_t chunk = 0;
  while (pos < data.length()) {
    const size_t size = chunkSizes[chunk++ % chunkSizes.size()];
    buffer += data.Mid(pos, size);
    pos += size;
    buffer.erase(0, SplitIntoItems(buffer, Tags(), Text, items));
  }
  return items;
}
} // namespace

SCENARIO("Maxima's output is split into tags and the text between them") {
  GIVEN("text, a known tag, text with a \"<\", and a prompt") {
    const wxString data =
      wxS("Hello\n<math><mn>1</mn></math>\na<b\n<PROMPT>(%i2) </PROMPT>");
    std::vector<Item> items;
    const size_t consumed = SplitIntoItems(data, Tags(), Text, items);
    THEN("everything is handed on") { REQUIRE(consumed == data.length()); }
    THEN("as four items, the newline after a tag belonging to the tag") {
      REQUIRE(items.size() == 4);
      CHECK(items[0].cause == Text);
      CHECK(items[0].data == wxS("Hello\n"));
      CHECK(items[1].cause == Maths);
      CHECK(items[1].data == wxS("<math><mn>1</mn></math>"));
      CHECK(items[2].cause == Text);
      CHECK(items[2].data == wxS("a<b\n"));
      CHECK(items[3].cause == Prompt);
    }
  }
  GIVEN("a tag whose end hasn't arrived yet") {
    const wxString data = wxS("x<math><mn>1</mn>");
    std::vector<Item> items;
    const size_t consumed = SplitIntoItems(data, Tags(), Text, items);
    THEN("only the text before it is handed on") {
      REQUIRE(consumed == 1);
      REQUIRE(items.size() == 1);
      REQUIRE(items[0].data == wxS("x"));
    }
  }
  GIVEN("data that ends in the middle of a start tag") {
    // This used to make the "<" text, and with it the whole tag: a line of
    // raw XML in the worksheet whenever a chunk of the socket's data ended
    // there.
    const wxString data = wxS("x<mat");
    std::vector<Item> items;
    const size_t consumed = SplitIntoItems(data, Tags(), Text, items);
    THEN("the start of the tag waits for the rest") {
      REQUIRE(consumed == 1);
      REQUIRE(items.size() == 1);
      REQUIRE(items[0].data == wxS("x"));
    }
  }
  GIVEN("a flood of disp() output arriving in chunks of odd sizes") {
    const wxString data = DispOutput(1, 3000);
    std::vector<Item> whole;
    REQUIRE(SplitIntoItems(data, Tags(), Text, whole) == data.length());
    const auto chunked = SplitInChunks(data, {1, 7, 4096, 3, 65536, 13});
    THEN("every line is one maths item, wherever the chunks ended") {
      REQUIRE(whole.size() == 3000);
      std::vector<Item> maths;
      for (const auto &item : chunked)
        if (item.cause == Maths)
          maths.push_back(item);
      REQUIRE(maths.size() == 3000);
      for (size_t i = 0; i < maths.size(); i++)
        REQUIRE(maths[i].data == whole[i].data);
    }
  }
  GIVEN("a split that is asked to stop") {
    std::vector<Item> items;
    const size_t consumed = SplitIntoItems(DispOutput(1, 10), Tags(), Text, items,
                                           [] { return true; });
    THEN("it hands on nothing") {
      REQUIRE(consumed == 0);
      REQUIRE(items.empty());
    }
  }
}

SCENARIO("The GUI is told about new items only when it ran out of them") {
  GIVEN("an empty queue") {
    MaximaItemQueue queue(Text);
    THEN("the first push asks for a notification") {
      REQUIRE(queue.Push({{Maths, wxS("a")}}));
      AND_THEN("further pushes don't, while the items haven't all been taken") {
        REQUIRE_FALSE(queue.Push({{Maths, wxS("b")}}));
        Item item;
        REQUIRE(queue.Take(item));
        REQUIRE_FALSE(queue.Push({{Maths, wxS("c")}}));
        AND_THEN("once the GUI found the queue empty, the next push does") {
          while (queue.Take(item)) {
          }
          REQUIRE(queue.Push({{Maths, wxS("d")}}));
        }
      }
    }
    THEN("pushing nothing asks for nothing") { REQUIRE_FALSE(queue.Push({})); }
  }
}

SCENARIO("Consecutive pieces of text are taken as one") {
  GIVEN("text in three pieces, a maths item, and more text") {
    MaximaItemQueue queue(Text);
    queue.Push({{Text, wxS("a")}, {Text, wxS("b")}, {Text, wxS("c")},
                {Maths, wxS("m")}, {Text, wxS("d")}});
    THEN("they are taken as three items") {
      Item item;
      REQUIRE(queue.Take(item));
      CHECK(item.data == wxS("abc"));
      REQUIRE(queue.Take(item));
      CHECK(item.cause == Maths);
      REQUIRE(queue.Take(item));
      CHECK(item.data == wxS("d"));
      REQUIRE_FALSE(queue.Take(item));
      REQUIRE(queue.Empty());
      REQUIRE(queue.QueuedChars() == 0);
    }
  }
}

SCENARIO("The socket thread pauses while the GUI has a big backlog") {
  GIVEN("a queue that allows 100 characters to wait") {
    MaximaItemQueue queue(Text, 100);
    WHEN("80 characters wait") {
      queue.Push({{Maths, wxString(wxS('x'), 80)}});
      THEN("there is no back pressure") { REQUIRE_FALSE(queue.BackPressure()); }
      AND_WHEN("40 more arrive") {
        queue.Push({{Maths, wxString(wxS('x'), 40)}});
        THEN("there is") { REQUIRE(queue.BackPressure()); }
        AND_WHEN("the GUI takes the first item") {
          Item item;
          queue.Take(item);
          THEN("there is none any more") { REQUIRE_FALSE(queue.BackPressure()); }
        }
      }
    }
  }
}

SCENARIO("Draining the queue returns to the event loop when the time is up") {
  GIVEN("a queue holding 1000 items that each take 5 ms to handle") {
    MaximaItemQueue queue(Text);
    std::vector<Item> items;
    for (int i = 0; i < 1000; i++)
      items.push_back({Maths, wxS("m")});
    queue.Push(std::move(items));
    const auto take = [&queue](Item &item) { return queue.Take(item); };
    int handled = 0;
    const auto slowDispatch = [&handled](const Item &) {
      std::this_thread::sleep_for(std::chrono::milliseconds(5));
      handled++;
      return true;
    };
    WHEN("they are drained for 30 ms") {
      const auto result = DrainForAtMost(std::chrono::milliseconds(30), take, slowDispatch);
      THEN("the time was up long before the queue was empty") {
        REQUIRE(result == DrainResult::TimeUp);
        REQUIRE(handled >= 1);
        // Generous: a loaded CI machine may oversleep, but nowhere near 1000 x
        REQUIRE(handled < 100);
        REQUIRE_FALSE(queue.Empty());
      }
    }
    WHEN("they are drained with no time at all") {
      DrainForAtMost(std::chrono::milliseconds(0), take, slowDispatch);
      THEN("one item is handled anyway, so a slow item can't stall the queue") {
        REQUIRE(handled == 1);
      }
    }
  }
  GIVEN("a queue holding three quick items") {
    MaximaItemQueue queue(Text);
    queue.Push({{Maths, wxS("a")}, {Maths, wxS("b")}, {Maths, wxS("c")}});
    const auto take = [&queue](Item &item) { return queue.Take(item); };
    THEN("draining it empties it") {
      int handled = 0;
      REQUIRE(DrainForAtMost(std::chrono::milliseconds(1000), take,
                             [&handled](const Item &) { handled++; return true; }) ==
              DrainResult::Empty);
      REQUIRE(handled == 3);
    }
    THEN("dispatch() can stop the drain") {
      REQUIRE(DrainForAtMost(std::chrono::milliseconds(1000), take,
                             [](const Item &) { return false; }) == DrainResult::Stopped);
      REQUIRE_FALSE(queue.Empty());
    }
  }
}

int main(int argc, char *argv[]) { return Catch::Session().run(argc, argv); }
