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
  The hand-over of Maxima's output from the socket thread to the GUI.
*/

#include "MaximaItemQueue.h"

bool MaximaItemQueue::Push(std::vector<Item> &&items) {
  if (items.empty())
    return false;
  std::lock_guard<std::mutex> lock(m_mutex);
  for (auto &item : items) {
    m_queuedChars += item.data.Length();
    m_items.push_back(std::move(item));
  }
  if (m_consumerNotified)
    return false;
  m_consumerNotified = true;
  return true;
}

bool MaximaItemQueue::Take(Item &item) {
  std::lock_guard<std::mutex> lock(m_mutex);
  if (m_items.empty()) {
    // Deciding this under the same lock Push() takes is what makes it safe:
    // an item pushed after this point is guaranteed to ask for a notification.
    m_consumerNotified = false;
    return false;
  }
  item = std::move(m_items.front());
  m_items.pop_front();
  m_queuedChars -= item.data.Length();
  if (item.cause == m_mergeableCause) {
    while (!m_items.empty() && (m_items.front().cause == m_mergeableCause)) {
      m_queuedChars -= m_items.front().data.Length();
      item.data += m_items.front().data;
      m_items.pop_front();
    }
  }
  return true;
}

bool MaximaItemQueue::Empty() const {
  std::lock_guard<std::mutex> lock(m_mutex);
  return m_items.empty();
}

std::size_t MaximaItemQueue::QueuedChars() const {
  std::lock_guard<std::mutex> lock(m_mutex);
  return m_queuedChars;
}

void MaximaItemQueue::Clear() {
  std::lock_guard<std::mutex> lock(m_mutex);
  m_items.clear();
  m_queuedChars = 0;
}

DrainResult DrainForAtMost(
  std::chrono::milliseconds budget,
  const std::function<bool(MaximaItemQueue::Item &)> &take,
  const std::function<bool(const MaximaItemQueue::Item &)> &dispatch) {
  const auto deadline = std::chrono::steady_clock::now() + budget;
  MaximaItemQueue::Item item;
  do {
    if (!take(item))
      return DrainResult::Empty;
    if (!dispatch(item))
      return DrainResult::Stopped;
  } while (std::chrono::steady_clock::now() < deadline);
  return DrainResult::TimeUp;
}
