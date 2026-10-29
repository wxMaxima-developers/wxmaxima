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
  The hand-over of Maxima's output from the socket thread to the GUI (GH #2594).

  The socket thread splits what Maxima sends into items (one XML tag, or a run
  of plain text) and the GUI thread works through them. Until GH #2594 every
  item became a wx event of its own, and wxWidgets handles *all* pending events
  before it looks at the keyboard, the mouse or a repaint again. A command that
  printed faster than wxMaxima could lay the output out, such as
  `for i thru i+1 do disp(i)`, therefore kept that queue from ever running
  empty: nothing was drawn, and not even Ctrl+G got through.

  So the items now wait in this queue, and the GUI thread takes them one by
  one, for a limited time per turn of the event loop (DrainForAtMost()). Two
  more rules keep a flood under control:

  - The consumer is only told about new items when it has run out of them
    (Push() returns true then). While it still has a backlog it comes back by
    itself, so the notifications can't pile up in the event queue either.
  - Once the backlog exceeds a limit, the socket thread stops reading
    (BackPressure()). Maxima then blocks writing to the socket, which pauses
    the calculation until the GUI has caught up, instead of wxMaxima buffering
    output without limit -- it used to grow by hundreds of megabytes a minute.
*/

#ifndef WXM_MAXIMAITEMQUEUE_H
#define WXM_MAXIMAITEMQUEUE_H

#include <wx/string.h>
#include <chrono>
#include <cstddef>
#include <deque>
#include <functional>
#include <mutex>
#include <vector>

/*! Items Maxima sent, on their way from the socket thread to the GUI thread.

  Push() is called from the socket thread, everything else from the GUI
  thread; BackPressure() is safe from either.
*/
class MaximaItemQueue
{
public:
  //! One piece of Maxima's output: what it is, and its text
  struct Item {
    //! What kind of data this is (a Maxima::EventCause)
    int cause = 0;
    //! The data itself
    wxString data;
  };

  /*! \param mergeableCause Consecutive items of this kind are handed out as
      one (plain text that only got split because it arrived in pieces).
      \param backPressureChars How many characters may wait before the
      socket thread should stop reading.
  */
  explicit MaximaItemQueue(int mergeableCause,
                           std::size_t backPressureChars = DefaultBackPressureChars)
    : m_mergeableCause(mergeableCause), m_backPressureChars(backPressureChars) {}

  /*! Appends items. Called by the socket thread.

    \returns true if the consumer has to be told that there is something to
    take: that is the case only if it found the queue empty last time it
    looked, and hasn't been told since. */
  bool Push(std::vector<Item> &&items);

  /*! Takes the oldest item, merged with the mergeable items that follow it.

    \returns false if there was nothing to take. From then on the next
    Push() asks for a notification again. */
  bool Take(Item &item);

  //! Whether nothing is waiting
  bool Empty() const;

  //! The number of characters waiting
  std::size_t QueuedChars() const;

  //! Whether the socket thread should stop reading until the GUI caught up
  bool BackPressure() const { return QueuedChars() > m_backPressureChars; }

  //! Drops everything that waits
  void Clear();

  /*! The default for how much output may wait.

    Enough for a GUI turn's worth of output even of the fastest flood seen
    (disp() in a loop, about 40 characters a line), and little enough that
    what is left to display after the user pressed Ctrl+G takes seconds, not
    minutes. A single item larger than this (a huge matrix) still gets
    through: the limit only stops the socket thread from reading more. */
  static constexpr std::size_t DefaultBackPressureChars = 64 * 1024;

private:
  const int m_mergeableCause;
  const std::size_t m_backPressureChars;
  mutable std::mutex m_mutex;
  std::deque<Item> m_items;
  std::size_t m_queuedChars = 0;
  //! Whether the consumer was told about items it hasn't found empty since
  bool m_consumerNotified = false;
};

//! Why DrainForAtMost() returned
enum class DrainResult {
  //! Nothing left to take
  Empty,
  //! The time was used up and items may be left: come back later
  TimeUp,
  //! dispatch() asked to stop
  Stopped
};

/*! Hands items to dispatch() until none is left or the time is used up.

  Always handles at least one item, so a single item that takes longer than
  the whole budget can't stall the queue.

  \param budget How long to keep dispatching
  \param take Fetches the next item; false if there is none
  \param dispatch Handles one item; false stops the loop (for example
         because the connection the items came from is gone)
*/
DrainResult DrainForAtMost(
  std::chrono::milliseconds budget,
  const std::function<bool(MaximaItemQueue::Item &)> &take,
  const std::function<bool(const MaximaItemQueue::Item &)> &dispatch);

/*! Splits what Maxima sent into items

  An item is either a complete known XML tag (its start tag, its contents and
  its end tag), or the plain text between such tags. A newline directly after
  a tag's end belongs to the tag. A known tag whose end hasn't arrived yet
  stays in the buffer, as does everything after it -- and so does a "<" at
  the very end that may turn out to start a known tag.

  \param buffer What was received, and not handed on yet
  \param knownTags Maps the names of the known tags to the cause their items get
  \param textCause The cause the plain text gets
  \param items Receives the items
  \param stopRequested Ends the split early if it returns true
  \returns How many characters at the start of \p buffer were handed on

  Walks the buffer once. It used to cut each item off the buffer as it found
  it, copying the whole rest of the buffer each time, which made a buffer full
  of short items (disp() in a loop, GH #2594) quadratically slow to split.
*/
template <class TagMap>
std::size_t SplitIntoItems(const wxString &buffer, const TagMap &knownTags,
                           int textCause,
                           std::vector<MaximaItemQueue::Item> &items,
                           const std::function<bool()> &stopRequested = {}) {
  const std::size_t length = buffer.length();
  wxString text;
  std::size_t pos = 0;

  while (pos < length) {
    if (stopRequested && stopRequested())
      break;
    const std::size_t tagPos = buffer.find(wxS('<'), pos);
    if (tagPos == wxString::npos) {
      text.append(buffer, pos, wxString::npos);
      pos = length;
      break;
    }
    if (tagPos > pos) {
      text.append(buffer, pos, tagPos - pos);
      pos = tagPos;
      continue;
    }

    auto tag = knownTags.begin();
    bool mayBeTagStart = false;
    for (; tag != knownTags.end(); ++tag) {
      const wxString tagStartName = wxS("<") + tag->first + wxS(">");
      if (buffer.compare(pos, tagStartName.length(), tagStartName) == 0)
        break;
      // The data ends in the middle of this start tag, for example "<mat".
      if ((length - pos < tagStartName.length()) &&
          tagStartName.StartsWith(buffer.substr(pos)))
        mayBeTagStart = true;
    }
    if ((tag == knownTags.end()) && mayBeTagStart)
      // Decide once the rest has arrived. Taking the "<" for text here used
      // to turn the whole tag into text: a line of raw XML in the worksheet.
      break;
    if (tag == knownTags.end()) {
      // A "<" that doesn't start a known tag is just text
      text += buffer[pos];
      pos++;
      continue;
    }

    const wxString tagEndName = wxS("</") + tag->first + wxS(">");
    const std::size_t end = buffer.find(tagEndName, pos);
    if (end == wxString::npos)
      // Not complete yet: wait for the rest of it.
      break;
    if (!text.IsEmpty()) {
      items.push_back({textCause, text});
      text.Clear();
    }
    const std::size_t tagFullLength = end + tagEndName.length() - pos;
    items.push_back({static_cast<int>(tag->second), buffer.substr(pos, tagFullLength)});
    pos += tagFullLength;
    if ((pos < length) && (buffer[pos] == wxS('\n')))
      pos++;
  }

  if (!text.IsEmpty())
    items.push_back({textCause, std::move(text)});
  return pos;
}

#endif // WXM_MAXIMAITEMQUEUE_H
