// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2024 the wxMaxima team
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
  A priority work-queue thread pool for offloading work to background threads.
*/

#include "BackgroundQueue.h"

void BackgroundTask::Run()
{
  {
    std::lock_guard<std::mutex> lock(m_mutex);
    if (m_state != State::Queued)
      return; // already dropped by RequestStop()
    m_state = State::Running;
  }

  // The work runs without any lock held, so RequestStop()/Wait() stay
  // responsive. Cancellation is already visible through m_stopSource.
  if (m_fn)
    m_fn(m_stopSource.get_token());

  {
    std::lock_guard<std::mutex> lock(m_mutex);
    m_state = State::Done;
    m_fn = nullptr;
  }
  m_doneCond.notify_all();
}

void BackgroundTask::RequestStop()
{
  m_stopSource.request_stop();
  // If we are still in the queue, drop us so a following Wait() returns at once.
  BackgroundQueue::Get().Drop(this);
}

void BackgroundTask::Wait()
{
  std::unique_lock<std::mutex> lock(m_mutex);
  m_doneCond.wait(lock, [this] { return m_state == State::Done; });
}

bool BackgroundTask::Pending() const
{
  std::lock_guard<std::mutex> lock(m_mutex);
  return m_state != State::Done;
}

BackgroundQueue &BackgroundQueue::Get()
{
  static BackgroundQueue instance;
  return instance;
}

int BackgroundQueue::Workers()
{
  // Mirror the heuristic the old ThreadNumberLimiter used: if we cannot tell
  // how many cores we have, assume there are plenty; never run fewer than 4.
  int maxThreads = static_cast<int>(std::thread::hardware_concurrency());
  if (maxThreads < 2)
    maxThreads = 8;
  if (maxThreads < 4)
    maxThreads = 4;
  return maxThreads;
}

BackgroundQueue::BackgroundQueue()
{
  const int workers = Workers();
  m_workers.reserve(workers);
  for (int i = 0; i < workers; ++i)
    m_workers.emplace_back([this] { WorkerLoop(); });
}

BackgroundQueue::~BackgroundQueue()
{
  std::deque<std::shared_ptr<BackgroundTask>> pending;
  {
    std::lock_guard<std::mutex> lock(m_mutex);
    m_shutdown = true;
    // Hand the not-yet-started tasks out so we can release their waiters below.
    pending.insert(pending.end(), m_high.begin(), m_high.end());
    pending.insert(pending.end(), m_low.begin(), m_low.end());
    m_high.clear();
    m_low.clear();
  }
  m_cond.notify_all();

  // Wake anyone still waiting on a task that will now never run.
  for (const auto &task : pending) {
    {
      std::lock_guard<std::mutex> lock(task->m_mutex);
      task->m_state = BackgroundTask::State::Done;
      task->m_fn = nullptr;
    }
    task->m_doneCond.notify_all();
  }

  for (auto &worker : m_workers)
    if (worker.joinable())
      worker.join();
}

std::shared_ptr<BackgroundTask>
BackgroundQueue::Add(BackgroundTask::Priority priority,
                     std::function<void(stop_token)> task)
{
  std::shared_ptr<BackgroundTask> handle(new BackgroundTask(std::move(task)));
  Get().Enqueue(handle, priority);
  return handle;
}

void BackgroundQueue::Enqueue(const std::shared_ptr<BackgroundTask> &task,
                              BackgroundTask::Priority priority)
{
  {
    std::lock_guard<std::mutex> lock(m_mutex);
    if (m_shutdown) {
      // Nothing will run any more: mark the task done right away.
      std::lock_guard<std::mutex> tlock(task->m_mutex);
      task->m_state = BackgroundTask::State::Done;
      task->m_fn = nullptr;
      return;
    }
    if (priority == BackgroundTask::Priority::High)
      m_high.push_back(task);
    else
      m_low.push_back(task);
  }
  m_cond.notify_one();
}

void BackgroundQueue::Drop(BackgroundTask *task)
{
  std::shared_ptr<BackgroundTask> removed;
  {
    std::lock_guard<std::mutex> lock(m_mutex);
    auto tryRemove = [&](std::deque<std::shared_ptr<BackgroundTask>> &queue) {
      for (auto it = queue.begin(); it != queue.end(); ++it)
        if (it->get() == task) {
          removed = *it;
          queue.erase(it);
          return true;
        }
      return false;
    };
    if (!tryRemove(m_high))
      tryRemove(m_low);
  }
  if (!removed)
    return; // already running or finished: let it run to completion

  {
    std::lock_guard<std::mutex> lock(removed->m_mutex);
    removed->m_state = BackgroundTask::State::Done;
    removed->m_fn = nullptr;
  }
  removed->m_doneCond.notify_all();
}

void BackgroundQueue::WorkerLoop()
{
  for (;;) {
    std::shared_ptr<BackgroundTask> task;
    {
      std::unique_lock<std::mutex> lock(m_mutex);
      m_cond.wait(lock, [this] {
        return m_shutdown || !m_high.empty() || !m_low.empty();
      });
      if (m_high.empty() && m_low.empty()) {
        if (m_shutdown)
          return;
        continue;
      }
      // Always prefer the high-priority queue.
      if (!m_high.empty()) {
        task = std::move(m_high.front());
        m_high.pop_front();
      } else {
        task = std::move(m_low.front());
        m_low.pop_front();
      }
    }
    task->Run();
  }
}
