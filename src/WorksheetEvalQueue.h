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
  The document-pure core of the worksheet's "schedule cells for evaluation"
  commands.

  Worksheet::Add*ToEvaluationQueue split into two concerns: deciding which group
  cells are eligible and appending them to the queue (this file, no view or
  cursor state), and the surrounding view side effects - following the
  evaluation with the viewport and moving the h-caret - which stay in Worksheet.
  Keeping the eligibility rule and the range walk here lets them be unit-tested
  without a Worksheet window (see test_WorksheetEvalQueue) and gives the
  document/view split a single home for the enqueue logic.
*/

#ifndef WORKSHEETEVALQUEUE_H
#define WORKSHEETEVALQUEUE_H

class GroupCell;
class EvaluationQueue;

//! The document-pure "add cells to the evaluation queue" helpers.
namespace WorksheetEvalQueue {

/*! Is this group cell eligible for evaluation?

  Only a visible code cell that actually owns an editor can be evaluated; text
  and sectioning cells, hidden (folded-away) cells and the odd editor-less code
  cell are skipped.
*/
bool ShouldEnqueue(const GroupCell &cell);

/*! Append one group cell to the queue if it is eligible.

  Marks the cell's editor as containing (not-yet-evaluated) changes - which
  greys out its stale output - and then queues the cell. A no-op for cells
  ShouldEnqueue() rejects.
*/
void Enqueue(GroupCell &cell, EvaluationQueue &queue);

/*! Enqueue every eligible cell in the inclusive range [start, end].

  Walks the group-cell list forward from \p start, calling Enqueue() on each
  cell, and stops after the cell equal to \p end. A null \p end means "to the
  end of the document".
*/
void EnqueueRange(GroupCell *start, const GroupCell *end, EvaluationQueue &queue);

} // namespace WorksheetEvalQueue

#endif // WORKSHEETEVALQUEUE_H
