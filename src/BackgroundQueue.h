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

#ifndef BACKGROUNDQUEUE_H
#define BACKGROUNDQUEUE_H

#include "Version.h" // stop_token / stop_source (real ones or shims)
#include <condition_variable>
#include <deque>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>
#include <utility>
#include <vector>

/*! A unit of work that has been handed to the BackgroundQueue.

  The submitter keeps a std::shared_ptr to it and uses it to wait for the work
  to finish (Wait()) or to request that it be cancelled (RequestStop()). It is
  the thread-pool equivalent of the jthread handle we used to store per Image.
*/
class BackgroundTask
{
public:
  //! The two priority levels the queue serves. High is always served first.
  enum class Priority
  {
    High, //!< e.g. loading an image: we need its size for the layout
    Low   //!< e.g. compressing the gnuplot source/data behind an image
  };

  //! Ask the task to stop at its next cancellation point. If it has not been
  //! started yet it is dropped from the queue, so a following Wait() returns at
  //! once. A task that is already running keeps running until it notices the
  //! request.
  void RequestStop();

  //! Block until the task has finished (or was dropped before it started).
  //! Returns immediately if that already happened.
  void Wait();

  //! Whether the task is still waiting in the queue or currently running.
  bool Pending() const;

  ~BackgroundTask() = default;

private:
  friend class BackgroundQueue;
  explicit BackgroundTask(std::function<void(stop_token)> fn)
    : m_fn(std::move(fn)) {}

  //! Run the work (called by a worker thread).
  void Run();

  enum class State { Queued, Running, Done };

  std::function<void(stop_token)> m_fn;
  stop_source m_stopSource;
  mutable std::mutex m_mutex;
  std::condition_variable m_doneCond;
  State m_state = State::Queued;
};

/*! A process-wide thread pool that runs BackgroundTasks, highest priority first.

  There is exactly one pool, created on first use and holding as many worker
  threads as we want to run background jobs concurrently (roughly the number of
  CPU cores). Submitting work via Add() never blocks the calling (usually the
  GUI) thread: it only appends to a queue and wakes a worker. Workers always
  empty the high-priority queue before touching the low-priority one, so e.g.
  the images of a long animation are all loaded before we start compressing the
  gnuplot sources behind them.
*/
class BackgroundQueue
{
public:
  /*! Enqueue a task at the given priority and return a handle to it.

    Does not block. If threading is impossible the task is still queued and run
    by a worker; callers that must run synchronously should do so themselves
    instead of calling Add().
  */
  static std::shared_ptr<BackgroundTask>
    Add(BackgroundTask::Priority priority, std::function<void(stop_token)> task);

  //! The number of worker threads the pool runs.
  static int Workers();

private:
  BackgroundQueue();
  ~BackgroundQueue();
  BackgroundQueue(const BackgroundQueue &) = delete;
  BackgroundQueue &operator=(const BackgroundQueue &) = delete;

  static BackgroundQueue &Get();

  void Enqueue(const std::shared_ptr<BackgroundTask> &task,
               BackgroundTask::Priority priority);
  //! If the task is still queued, remove it and mark it done. Called by
  //! BackgroundTask::RequestStop().
  void Drop(BackgroundTask *task);
  void WorkerLoop();

  friend class BackgroundTask;

  std::mutex m_mutex;
  std::condition_variable m_cond;
  std::deque<std::shared_ptr<BackgroundTask>> m_high;
  std::deque<std::shared_ptr<BackgroundTask>> m_low;
  std::vector<std::thread> m_workers;
  bool m_shutdown = false;
};

#endif // BACKGROUNDQUEUE_H
