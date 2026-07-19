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
  Implementation of the document-pure evaluation-queue helpers - see
  WorksheetEvalQueue.h.
*/

#include "WorksheetEvalQueue.h"
#include "EvaluationQueue.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"

namespace WorksheetEvalQueue {

bool ShouldEnqueue(const GroupCell &cell) {
  return (cell.GetGroupType() == GC_TYPE_CODE) && !cell.IsHidden() &&
         (cell.GetEditable() != nullptr);
}

void Enqueue(GroupCell &cell, EvaluationQueue &queue) {
  if (!ShouldEnqueue(cell))
    return;
  // Gray out the output of the cell in order to mark it as "not current"...
  cell.GetEditable()->ContainsChanges(true);
  // ...and add it to the evaluation queue.
  queue.AddToQueue(&cell);
}

void EnqueueRange(GroupCell *start, const GroupCell *end,
                  EvaluationQueue &queue) {
  for (GroupCell *cell = start; cell; cell = cell->GetNext()) {
    Enqueue(*cell, queue);
    if (cell == end)
      break;
  }
}

} // namespace WorksheetEvalQueue
