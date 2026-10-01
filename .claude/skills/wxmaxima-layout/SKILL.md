---
name: wxmaxima-layout
description: How wxMaxima lays the worksheet out - the schedule/recalculate/resize pipeline in WorksheetLayout, the invariants that layout bugs keep violating, and the recurring bug shapes (stale widths, parens too narrow, repaint storms, a group that won't grow). Use when touching Recalculate/Reposition/Draw, cell geometry, scrolling, or when something renders at the wrong size or the wrong time.
---

# The wxMaxima layout engine

Everything about how a worksheet turns into pixels. Read this before changing
anything that computes a size or a position: the same handful of mistakes have
caused most of the layout bugs in this codebase, and they are all invisible at
the call site.

## The pipeline

`WorksheetLayout` (`src/worksheet/WorksheetLayout.{h,cpp}`) owns the whole
thing, behind a narrow view interface so it can be driven with no `Worksheet`
window at all (which is what `test_WorksheetLayout` does).

| Phase | Entry point | What it does |
|---|---|---|
| SCHEDULE | `RequestRecalculation(start)` / `RequestFullRecalculation()` | Marks a group dirty and moves the resume point. **Lays nothing out.** |
| EXECUTE | `RecalculateIfNeeded(timeout, timeSliceMs)` | Walks from the resume point. Per group: dirty → `Recalculate()` (sizes), else `Reposition()` (positions). |
| SIZE | `AdjustSize()` → `GetMaxPoint()` | Reads the last cell's position, sets the virtual size and scroll rate. |
| SCROLL | `ScheduleScrollToCell` / `ScrollToCellIfNeeded` / `ScrollToCaretIfNeeded` | |
| CLIENT SIZE | `UpdateConfigurationClientSize()` → `Configuration::SetCanvasSize` | |

Inside a single group, sizing drops into the cell-layer break pipeline
(`Cell.cpp`): `UnBreakUpCells()` → `BreakUpCells()` → `BreakLines_List()`.

**One cell's width depends on where the lines break** (GH #2263): a hidden
multiplication sign at a soft line break is drawn (`Cell::IsAtLineBreak()`,
`IsDrawnHidden()`), so it takes the visible sign's width. `BreakLines_List()`
sets that flag (only ever on hidden multiplication signs) and fits such a sign
on a line by `GetWidthAtLineBreak()`, so the dot never pushes a line past the
window. `Unbreak()` clears it, as a cell that leaves the draw list (its parent
went back to 2D) is never visited by the line breaker again and would keep
drawing a stale dot.

The `timeout` path time-slices (50 ms by default) so a huge worksheet stays
responsive; a cell whose layout is cancelled mid-flight is the subject of one of
the traps below.

## The five invariants

**1. Scheduling is not doing.** `RequestRecalculation()` only marks work
pending; `RecalculateIfNeeded()` performs it. Never read a size or position on
the line after requesting a recalculation - you will read the *previous*
layout. `AdjustSize()` deliberately defers while positions are stale, and there
is a guard plus an assert to catch it. The name is the trap: this was called
`Recalculate()` until 2026-07-08, so older code reads as though it were
synchronous.

**2. A composite `Recalculate()` override must recurse unconditionally.**
About twenty composite cells (`FracCell`, `SqrtCell`, `ParenCell`, ...) once
skipped recursing into children when they judged *themselves* unchanged. A
child can be dirty for a reason the parent cannot see - typically a font-size
change during partial breakup - and the parent's guard then strands it at a
stale width. That is precisely how parentheses ended up drawn too narrow for
their contents. Recurse every time; let each child's own changed-flag decide.

**3. Cached list geometry must be stamped with the configuration counter.**
Per-list caches (`m_listCacheCfgCnt` and friends) otherwise survive a font,
zoom or style change and hand back widths computed under the old settings. This
is the "stale spacing" bug family.

**4. Cancelled layout must invalidate what it did not compute.** When the
layout deadline fires mid-cell, the base `Cell::Recalculate()` has already
marked the cell as recalculated. If the override then returns early without
computing its widths, `NeedsRecalculation()` is false forever after and the
cell renders at a stale or first-pass size. `MatrCell::Recalculate()` shows the
fix: `m_width.Invalidate()` before the early return. Test-drive this with
`Configuration::SetLayoutDeadline(0)`, which makes `IsLayoutCancelled()` true
immediately - see the matrix scenario in `test_GroupCellLayout.cpp`.

**5. A cell may only be broken into lines if every cell around it is.** Only a
broken cell hands its contents to the flattened draw list (`OnDrawList()`);
a cell drawn in 2D positions and draws its contents itself, and cannot draw a
broken (zero-width, linear) child. `SubCell`, `SubSupCell`, `AtCell`,
`MatrCell` and a `diff()` `FracCell` never break up, yet `CollectWideCells()`
used to descend into them anyway, so a wide fraction inside a subscript was
linearized inside a 2D cell: invisible, or partly drawn, and never unbroken
again (`UnBreakUpCells()` walks the same draw list). `BreakUpCells()` now
breaks a nested cell only if the wide cell it was found in actually broke.
Debug builds assert this in `Cell::Draw()`; `test_LayoutInvariants`'
"Nothing inside a subscript is broken into lines" pins it.

## Recurring bug shapes

Recognise these before starting a fresh investigation.

- **Something renders at the wrong width, and touching one cell fixes it.**
  Layout state surviving when it should not: invariant 2, 3 or 4. The "one cell
  heals when poked" signature specifically points at a cache or a
  never-cleared dirty flag, not at the width formula.
- **A `GroupCell` doesn't grow with its `EditorCell`.** Historically a stale
  list cache plus an input-height loop that ignored the second cell on a line.
- **Repaint storms on mouse motion.** The big one was GTK's composited overlay
  scrollbar, plus `OnPaint` collapsing the update box, a sidebar `OnSize` that
  re-triggered itself, and status-bar `SetLabel` churn. The performance monitor
  has repaint counters; use them before optimising anything.
- **Geometry read too early.** Invariant 1. Symptom: correct after the next
  interaction, wrong on the first paint.

## Keeping the cursor still while output arrives

`Worksheet::InsertLine()` calls `WorksheetLayout::ArmScrollCompensation()`
before appending, if the cursor is on screen. That remembers the cursor's
group cell (its top, or its bottom for the horizontal cursor) and where it is
now; once a layout pass has *finished* (both the one-shot tail and the
time-sliced "reached the end" early return), `ApplyScrollCompensation()`
scrolls by however far that point moved. Three things worth knowing:

- **The view scrolls in whole units (>= 10 px), so pixel exactness comes from
  `Configuration::GetWorksheetTopOffset()`**, an extra top margin in
  `[0, scrollUnit)` that only the first `GroupCell::Reposition()` adds. The
  arithmetic is `ComputeScrollCompensation()` in `WorksheetSizeMath.h`.
  Anything else that computes "the top of the document" by hand (the
  horizontal cursor above the first cell, in `Worksheet::OnPaint`) must add
  it too. It is not copied with the configuration, so printing and export
  never see it.
- **Changing the offset only needs repositioning, not recalculation**: every
  cell's size is still valid, so `ApplyScrollCompensation()` walks the list
  calling `Reposition()` and then `AdjustSize()` before scrolling, or
  wxWidgets would clamp the scroll position to the old virtual size.
- **The compensation is dropped if the cursor moved** between arming and the
  pass finishing (`SetScrollAnchorCallback()` is re-asked), and it arms only
  once per pass: a second append would measure positions not on screen yet.

## Testing layout without a window

`test_WorksheetLayout` drives the pipeline with no `Worksheet` window.
`test_GroupCellLayout` and `test_LayoutInvariants` cover cell-level geometry and
the invariants above. A layout bug that can be expressed as "these sizes should
be equal" belongs in one of those rather than in a GUI test - several of the
bugs listed here were only pinned down once they had a windowless reproduction.

For the display/`xvfb` mechanics of running GUI tests on a developer machine,
see the `run-wxmaxima` skill; the environment traps are local, not project
knowledge.
