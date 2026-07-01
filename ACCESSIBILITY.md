# wxMaxima accessibility audit

Status of screen-reader / assistive-technology support, and the plan to improve
it. Written 2026-07-01.

## How to read this document

wxMaxima's accessibility is exposed through `wxAccessible` (MSAA/UI Automation on
Windows, ATK/AT-SPI on Linux when wxWidgets is built with
`--enable-accessibility`). **The primary target is Windows** — standard wxWidgets
distributions enable accessibility there, whereas the common Linux packages ship
`wxUSE_ACCESSIBILITY=0`, which compiles all of this out.

**Testing loop.** Behavioural validation needs an actual screen reader (NVDA /
JAWS) on Windows, done by our blind tester, who reaches the code only through the
once-a-day nightly build — a multi-day feedback loop. Therefore:

- The *logic* of our `wxAccessible` code (names, roles, child enumeration) is
  covered by `test/unit_tests/test_Accessibility.cpp`, which runs in the Windows
  CI in minutes. Extend it whenever we touch accessibility code.
- Reserve the tester's slow feedback for the *subjective* questions marked
  **[ASK TESTER]** below.
- Batch fixes so each nightly he receives covers as much as possible.

## Legend
- ✅ Done / believed good.  ⚠️ Partial or unverified.  ❌ Known gap.
- **[ASK TESTER]** = needs a real screen reader to judge; can't be unit-tested.

---

## 1. Worksheet (the document) — ✅ (logic), **[ASK TESTER]** (quality)

- ✅ The custom `Worksheet::AccessibilityInfo` is now attached via
  `SetAccessible()` (it previously was created but never attached, so screen
  readers saw only a generic panel + scrollbars). Root role is
  `wxROLE_SYSTEM_DOCUMENT`; cells are exposed as children; the caret is a child.
- ✅ Each cell has a `CellAccessible`; the accessible **name of a math cell is
  its `ToString()`** — i.e. Maxima linear syntax (`(a+b)/c`,
  `integrate(sin(x),x)`, `matrix([1,2],[3,4])`). For a CAS user this is arguably
  ideal (it is the syntax they type), but:
  - **[ASK TESTER]** Is linear Maxima syntax the representation you want read
    aloud, or would you prefer verbalised math ("the fraction a plus b over c")?
  - **[ASK TESTER]** Can you navigate the worksheet cell by cell and hear each
    input and its output?

## 2. New output announcement — ⚠️ **[ASK TESTER]**

- `wxAccessible::NotifyEvent` is fired in exactly one place
  (`Worksheet.cpp`, `wxACC_EVENT_OBJECT_FOCUS` when the caret moves to a cell).
  There is **no notification when Maxima produces new output**, so a screen
  reader likely will not announce a result automatically — the user must
  navigate to it.
  - **[ASK TESTER]** After you evaluate a cell, does your screen reader announce
    the result, or do you have to go looking for it? Do you *want* auto
    announcement (a live region), or is that noisy?
  - Candidate fix if wanted: fire an `wxACC_EVENT_*` (e.g. an alert / focus /
    value-change) on the new output cell when it arrives.

## 3. Toolbar — ✅ (logic), **[ASK TESTER]** (quality)

- ✅ The main toolbar is a `wxAuiToolBar` (owner-drawn) and had **no**
  accessibility — its tools were invisible. `ToolBarAccessible` now exposes each
  tool with a name (tooltip/label), role (push/check/radio button, separator,
  static text), state (focusable / disabled / checked), screen location, and a
  "Press" default action. Real control items (the animation slider, the text
  field) are delegated to their own accessibles.
  - **[ASK TESTER]** Can you reach the toolbar, hear each tool's name and state,
    and activate one?

## 4. Sidebars — ✅ container names; ⚠️ contents

- ✅ Each docked sidebar pane now carries an accessible **name** (its caption:
  "Table of Contents", "Statistics", "Greek Letters", …). Previously every one
  was an anonymous "panel".
- ✅ **The Greek / Symbols / General-Math sidebars' buttons** (`CharButton`, a
  `wxPanel` holding a `wxStaticText`) now carry a `CharButtonAccessible` that
  presents each as a single push button, named by its symbol description ("Greek
  small letter alpha", …), with a "Press" default action — instead of an unnamed
  pane containing a static text. **[ASK TESTER]** confirm they read as buttons
  and can be activated.
- ✅/⚠️ **UnicodeSidebar** is a `wxGrid` (not custom-drawn, as first assumed):
  `wxGrid` has built-in accessibility, so its cells — including the character's
  name column ("GREEK SMALL LETTER ALPHA") — are readable, and the grid is
  arrow-key navigable. The gap was **activation was mouse-only** (double-click);
  Enter now inserts the focused character too. **[ASK TESTER]** confirm reading
  and Enter-to-insert.
- ⚠️ Table-of-Contents, Variables (grid), History (list) use standard wx
  controls → accessible by default, but unverified. **[ASK TESTER]** spot-check.

## 5. Menus — ⚠️ (believed OK)

- Native menus are accessible through the OS by default, and the wxWidgets 3.3
  `GetHelpString` assert that broke menu use was fixed. Menu item **names** come
  from the item labels (present) and their help strings show in the status bar.
  **[ASK TESTER]** spot-check that menus read correctly.

## 6. Dialogs & wizards — ⚠️ (mostly standard controls)

- The config dialog, find/replace, and the code-generating wizards are built
  from standard wx controls (`wxTextCtrl`, `wxButton`, `wxChoice`, `wxSpinCtrl`),
  which carry native accessibility. Main risks: (a) controls whose *label* is a
  separate `wxStaticText` not associated with the field (screen reader may not
  tie label to input), (b) any custom-drawn preview panels (`SvgPanel` shows an
  image with no accessible name). Lower priority than the worksheet/sidebars.
  **[ASK TESTER]** as they come up.

## 7. Status bar — ⚠️ (believed OK, standard control)

---

## Priority order

1. **[ASK TESTER] batch** — validate the fixes already shipped (worksheet as a
   document + cell navigation, toolbar tools, sidebar names) and answer the
   output-announcement and math-representation questions. These shape everything
   else and need the most lead time.
2. **CharButton** Greek/Symbols/Math sidebar buttons → real accessible buttons
   with descriptive names. (High impact: many buttons, and `Definition` already
   has the text.)
3. **Output announcement** live region (if the tester wants it).
4. **UnicodeSidebar** custom-draw accessibility.
5. Dialog/wizard label-to-field association + `SvgPanel` names, as reported.

## Things for the tester to try (fastest way to move this forward)

Once a nightly with these changes reaches you, in NVDA/JAWS:
1. Tab to the worksheet — is it a "document"? Can you arrow through the cells and
   hear each input and its result?
2. After evaluating a cell, is the new result announced, or must you navigate to
   it? Do you want it announced automatically?
3. Is Maxima's linear syntax (e.g. `integrate(sin(x),x)`) the right thing to hear
   for math, or do you want spoken math?
4. Reach the toolbar — are the tools named and operable?
5. Reach a sidebar — does it announce its name? Can you use its buttons (Greek
   letters, symbols)?

Any "still reads as an unnamed panel / I can't get to it" reports are the most
useful — they point straight at the next fix.
