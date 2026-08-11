---
name: wxmaxima-architecture
description: Where wxMaxima's code lives and why - the friend-class decomposition of the wxMaxima god class, the Worksheet document/view split, CellPointers, and the recipe for peeling another cluster off safely. Use when looking for where a piece of functionality lives, when a class feels too big, or before starting any extraction/refactor of wxMaxima.cpp, Worksheet.cpp or Configuration.
---

# wxMaxima's architecture, and how it got that way

Two classes used to hold almost everything: `wxMaxima` (the app/frame) and
`Worksheet` (the document view). Both have been decomposed in behaviour-
preserving slices. This is where things ended up, and the recipe for doing the
next one without breaking anything.

## Where functionality lives now

`wxMaxima` keeps the frame and the glue; the clusters were peeled into **friend
classes** (declared in `wxMaxima.h`), each owning one concern:

| Class | Owns |
|---|---|
| `MaximaProcessManager` | spawn / kill / connect, the data pump, gnuplot children |
| `MaximaEvaluator` | the evaluation queue driver and the command protocol |
| `MaximaResponseReader` | the incoming-XML handlers and their dispatch maps |
| `MaximaFileIO` | worksheet open/save (`.wxm`, `.wxmx`, `.mac`), autosave |
| `MaximaCommandMenus` | the bound menu handlers |
| `MaximaIPC`, `MaximaOutputAppender` | IPC, output appending |

So: **look in the friend class before `wxMaxima.cpp`.** The god class went from
~11,100 lines to ~3,700 this way.

`Worksheet` was split along a different axis - document state versus view - into
`WorksheetDocument`, `WorksheetLayout` (see the `wxmaxima-layout` skill),
`WorksheetExport`, `WorksheetSearch`, `WorksheetEvalQueue` and
`WorksheetContextMenu`. What remains in `Worksheet.cpp` is genuinely
view/OS-native: input handling, painting, accessibility, autocomplete,
clipboard. **That arc is closed**; don't go looking for more of the same shape
there without a new reason.

## The extraction recipe

Both arcs used the same method, and it works:

1. **Map first, cut second.** Write down the cluster's members, its callers, and
   its coupling in three directions (up to the wx view, down to the cell tree,
   sideways to `Configuration`) *before* moving a line. Most of the risk is in
   coupling you did not notice.
2. **Small slices, each green.** Move one coherent group at a time, build and
   test between slices. Never one big move.
3. **Friend class, not inheritance.** Keeps the private state reachable during
   the transition without inventing accessors you will regret.
4. **Behaviour-preserving means byte-identical where you can measure it.** The
   export cluster had `test_WorksheetExport` dump its whole output tree so the
   before/after could be `diff -r`'d. Build that lever *first* if the cluster
   has any serialisable output.

### Gotchas that actually bit

- **`sed`-style bulk renames are the main hazard.** A local variable that merely
  *looks* like a member (`m_something`) gets wrongly prefixed; qualified callers
  (`->foo()`, `.foo()`) need hand-fixing; a scoped name like
  `wxMaxima::m_exitCode` needs its own pass. Read every hunk.
- **`CallAfter` and `Close` need the real `wxEvtHandler`.** In an extracted
  class that is `m_wxMaxima`, not `this` - the friend class is not an event
  handler.
- **Event binds** from an extracted class use the functor form.
- **Relinking is slow.** Expect a long build between slices; budget for it.

## CellPointers, and the split that is *not* what it looks like

Cells reach the shared pointers (selection, active cell, last-clicked, ...)
through `Configuration`, not through `Worksheet`. So separating document from
view state there is an **internal `CellPointers` reorganisation**, not a
relocation into `WorksheetDocument`. The selection alone is ~189 call sites; do
it last, if at all.

## Long-lived references: `CellPtr`, always

Anything holding a `Cell`/`GroupCell` beyond the current call - undo/redo, the
evaluation queue, the selection, sidebars, a cached "last clicked" - **must**
use `CellPtr<...>`, which nulls itself when the cell dies. A raw `Cell *` is
fine only within a single function or event. This is also in AGENTS.md because
it is the single easiest way to introduce a use-after-free here.

## EditorCell

Split in slices 1-4 and 6; slices 5 and 7 (relocating Maxima-code-only state
onto a `CodeEditorCell`) are deliberately **deferred** as the risky ones. Soft
line breaks live in a side table (`m_softBreaks`), *not* as `\r` characters in
the content - that representation change killed a whole bug family where soft
breaks leaked into copy/paste as hard newlines. Do not reintroduce in-string
break markers.

## Related

`wxmaxima-layout` for the geometry pipeline. AGENTS.md's "Key Subsystems Map"
for the one-paragraph version.
