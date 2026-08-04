# Project Instructions

This file contains architectural insights, conventions, and operational knowledge to assist AI agents working on the wxMaxima codebase. **Agents are explicitly permitted and encouraged to update this file with new findings that improve context and safety.**

## Build System

Configure once (a Debug build is the default), then build and run without
installing:

```sh
cmake -S . -B build -G Ninja
ninja -C build
./build/wxmaxima-local
```

For a release build (more optimization, log window hidden by default) add
`-DCMAKE_BUILD_TYPE=Release` to the `cmake` line.

Run the tests -- most of them need Maxima installed:

```sh
ctest --test-dir build                        # everything
ctest --test-dir build -R <name-of-the-test>  # one test; names in test/CMakeLists.txt
xvfb-run ctest --test-dir build               # headless, no X server
```

The sanitizer build (ASan + UBSan) is what CI runs on every push. Run it
locally before merging changes to cell lifetime, layout or parsing:

```sh
cmake -S . -B build-asan -G Ninja -DWXM_SANITIZE=address,undefined
ninja -C build-asan
ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=print_stacktrace=1 \
    xvfb-run ctest --test-dir build-asan
```

Leak detection stays off (`detect_leaks=0`) because GTK/Pango leak noise drowns
out real findings.

Other useful targets: `ninja -C build Doxygen` builds the source documentation
(note the capital D, and the target only exists when Doxygen is installed), and
`ninja -C build update-locale` refreshes the translation files.

`CheckPo4aVersion.cmake` (included from `info/CMakeLists.txt` and
`locales/manual/CMakeLists.txt`, the only two places that invoke `po4a`)
refuses `po4a` older than 0.70 -- pre-0.70 parses text encodings loosely and
can silently corrupt non-ASCII translated text with no warning of its own,
confirmed directly with Ubuntu 24.04's own `po4a` 0.69 package turning a
German manual paragraph into mangled English on nothing more than a plain
reconfigure. `PO4A` ends up `PO4A-NOTFOUND` (falsy) in that case, same
contract `find_program()` itself has, so existing `if(PO4A)` guards keep
working without extra checks.

- **Sandbox: missing `maxima-index.lisp` and its knock-on ctest failures.**
  In this sandbox's container image, `/etc/dpkg/dpkg.cfg.d/*` has
  `path-exclude=/usr/share/doc/*` (a common image-slimming policy), so every
  package's `/usr/share/doc/*` content is silently dropped on install --
  `apt-get install --reinstall maxima maxima-doc` does not bring it back.
  `maxima`'s own package ships `/usr/share/doc/maxima/info/maxima-index.lisp.gz`
  (confirmed by extracting the real `.deb` with `dpkg-deb -x`), so its absence
  here is this sandbox's doc-stripping, not a missing dependency -- the actual
  wxMaxima `.deb` (`CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc"` in
  `src/CMakeLists.txt`) already hard-`Depends:` on both, and a normal install
  on a normal system is unaffected. Workaround for this sandbox only (not a
  repo change): `apt-get download maxima && dpkg-deb -x maxima_*.deb
  /tmp/x && gunzip -c /tmp/x/usr/share/doc/maxima/info/maxima-index.lisp.gz >
  /usr/share/doc/maxima/info/maxima-index.lisp`. Without it, Maxima logs
  `Warning: SIMPLE-WARNING: Maxima is unable to set up the help system` on
  every startup, and `ctest` targets that use `--exit-on-error`
  (`openMacFiles`, `openMacFiles2`, and most of the `*_cmdline_wxmathml`/
  `tutorial_*`/similar batch tests in `test/CMakeLists.txt`) fail near-instantly
  on that warning alone -- with the workaround applied, those specific two
  tests (`openMacFiles`/`openMacFiles2`) instead *time out* (confirmed to
  reproduce identically on an unmodified `main` checkout in an isolated
  worktree, so it's pre-existing and unrelated to any particular change) --
  not yet root-caused. Don't burn time re-diagnosing either symptom from
  scratch; both are sandbox/pre-existing, not something a code change here
  broke.

## Architecture & GUI

wxMaxima is a GUI front-end to the Maxima CAS; it talks to a Maxima process over
a local TCP socket.

- **wxAuiManager:** The application uses `wxAuiManager` for its complex layout (sidebars, toolbars, worksheet).
  - **Linux/GTK Timing:** On Linux (especially KDE Plasma with Global Menus), calling `m_manager.Update()` can disrupt the menu bar if it's already attached. This is a known environmental issue in the interaction between wxWidgets, GTK3, and the KDE Global Menu proxy.
    - **Automated Fix:** On systems with wxWidgets <= 3.2 running on KDE, Unity, or with `appmenu-gtk-module` enabled, wxMaxima automatically sets `UBUNTU_MENUPROXY=0` at startup in `main.cpp` to force menus to remain within the window and prevent disappearance.
    - If the menu still disappears, clearing `GTK_MODULES` (e.g., `GTK_MODULES=""`) can also restore local menus.
- **Cursors:** The worksheet has 2 types of Cursor: A standard cursor in an EditorCell or a hCaret between two worksheet cells (`m_hCaretPosition`, the horizontal bar that marks a position *between* group cells, used for inserting and for selecting whole cells). Only one cursor is active at a time.
- **Key Classes:**
  - `wxMaxima` (`src/wxMaxima.cpp`): The main application class (subclass of `wxMaximaFrame`). Holds most of the program logic -- Maxima process management, parsing incoming XML, menu and toolbar actions, file I/O.
  - `wxMaximaFrame` (`src/wxMaximaFrame.cpp`): The base frame class handling layout and sidebars (TOC, variables, history, symbols, draw), the toolbars and the central worksheet.
  - `Worksheet` (`src/worksheet/Worksheet.cpp`): The scrollable document view. Owns the cell tree (`m_tree`) and handles drawing, keyboard and mouse input, the cursors, the selection and the evaluation queue.
  - `GroupCell` (`src/cells/GroupCell.cpp`): The top-level container cell that bundles an input `EditorCell` with its output. The worksheet is a linked list of `GroupCell`s.
  - `Cell` (`src/cells/Cell.cpp`): Base class of all maths display cells -- `TextCell`, `FracCell`, `SqrtCell`, `IntCell`, `MatrCell`, `AnimationCell` and friends.
  - `EditorCell`: Handles text and code input, including Markdown-like formatting (bullet lists).
  - `MathParser` (`src/MathParser.cpp`): Parses the MathML-like XML Maxima produces (via `wxMathML.lisp`) into a tree of `Cell` objects.
  - `Maxima` (`src/Maxima.cpp`): Owns the TCP socket to the Maxima process and emits `EVT_MAXIMA` events carrying incoming data.
  - `Variablespane`: Manages the list of defined variables and their values.
  - `AutoComplete`: Handles the autocomplete logic for commands, variables, and files.
- **The draw list is computed, not stored (2026-08, closes GH #1445):** `Cell`
  used to carry a `mutable CellPtr<Cell> m_nextToDraw` member -- a second
  always-present `CellPtr` on *every* cell, threaded by hand via a virtual
  `SetNextToDraw()` override on each 2D-capable compound cell (`FracCell`,
  `ParenCell`, `SqrtCell`, `AbsCell`, `BoxCell`, `ConjugateCell`, `ListCell`,
  `ExptCell`, `SumCell`, `IntCell`, `LimitCell`, `IntervalCell`, `DiffCell`,
  `FunCell`, `NamedBoxCell`, `LongNumberCell`) whenever `BreakUp()`/`Unbreak()`
  ran. `CellDrawListIterator` (`src/cells/CellIterators.h`) now computes the
  same flattened "line" sequence on the fly instead: it walks `GetNext()` for
  ordinary siblings, and when a cell `IsBrokenIntoLines()` it descends into
  `GetBrokenCellCount()`/`GetBrokenCell()` (an explicit stack in the iterator
  remembers where to resume once a nested expansion is exhausted -- normal
  documents nest only a few levels deep, so this stays empty, with zero
  allocation, for any line containing no broken cell). `GetBrokenCellCount()`/
  `GetBrokenCell()` default to the existing `GetInnerCellCount()`/
  `GetInnerCell()` (the *semantic*-children interface, previously used only
  for `ResetSize_Recursively()`/`CollectWideCells()`/tooltip fallback), which
  turned out to already match the draw sequence exactly for 11 of the 15
  classes above (confirmed by direct comparison against each `BreakUp()`,
  not assumed). **Four classes needed a real, separate override**, because
  their structural inner-cell set and their actual linear draw sequence
  diverge under runtime conditions: `IntCell` (the linear form omits the
  lower/upper limit slots entirely when `HasLimits()`), `SumCell` (shows
  `Base()` -- the bare, unwrapped content -- instead of the `ParenCell`
  wrapper `GetInnerCell()` reports, and conditionally omits the upper-limit
  pieces when `over` is empty), `IntervalCell` and `LimitCell` (both have
  structural slots -- bracket glyphs, the "lim" name label -- that exist only
  for the 2D form and are never part of the linear one). Getting one of
  these four wrong is a real rendering bug, not just a wrong tree-shape for
  an unrelated recursive walk, since there is no longer a separate
  hand-threaded pointer chain to cross-check the sequence against -- treat
  any future change to a class's `GetInnerCellCount()`/`GetInnerCell()` (or
  `GetBrokenCellCount()`/`GetBrokenCell()` override) as a rendering-order
  change and re-verify it against that class's actual `BreakUp()` logic.
  A 2020 attempt at this same removal (branch
  `feature/KubaO/remove-nexttodraw`, never merged) shipped visible
  regressions in exactly this class of nested-breaking scenario (a broken
  fraction inside a broken fraction/paren/diff cell) because it didn't
  account for this divergence; this attempt was verified against it
  directly -- both with a dedicated nested-broken-cell unit test
  (`test/unit_tests/test_CellPtr.cpp`, `SCENARIO("DrawListIterator works")`)
  and by running the real batch tests (`absCells`, `boxCells`, `diffCells`,
  `conjugateCells`, `exptCells`, `fracCells`, `intCells`, `intervals`,
  `limitCells`, `matrixCells`, `parenthesisCells`, `sumCells`) against a real
  Maxima, plus a manual Xvfb+ImageMagick screenshot of
  `diff(abs(f(x)/g(x)),x)` at a narrow width (a broken `diff` containing a
  broken `abs` containing a broken nested fraction, all at once) to visually
  confirm correct rendering -- this sandbox can install `maxima`/`maxima-doc`
  (see the sandbox note under Build System for the doc-stripping workaround)
  and `Xvfb`/`xdotool`/`imagemagick` for exactly this kind of check when a
  change is rendering-sensitive and the automated test suite's XML/structural
  assertions aren't enough on their own. `CellList.cpp`'s `SetNext()`/
  `AppendCell()`/`SpliceInAfter()`/`TearOut()` no longer need any
  draw-list-mirroring bookkeeping, since there's nothing stored to keep in
  sync.

### Communication with Maxima

wxMaxima sends Lisp and Maxima commands over the socket; Maxima answers with XML
wrapped in known tags. `Maxima` reads that data on a worker thread and posts
`EVT_MAXIMA` events to the main thread, where `wxMaxima` handles them.

`src/wxMathML.lisp` is compiled into the binary (through CMake's bin2h) and is
what tells Maxima to format its output as MathML-like XML. For development,
`--wxmathml-lisp=<path>` overrides it with an external file, so a change can be
tried without rebuilding.

- **`m_configCommands` (`wxMaxima.cpp`):** the string of startup/config commands
  sent to Maxima on connect (and again whenever settings change while it's
  running). **Every entry in it MUST be a `:lisp-quiet (...)` directive --
  never a plain Maxima statement ending in `$`/`;`.** `m_configCommands` is
  sent bundled immediately ahead of the evaluation queue's next real per-cell
  command (`MaximaEvaluator::TriggerEvaluation()`), and
  `EvaluationQueue::RemoveFirst()` has no way to tell "a prompt answering a
  config command" from "a prompt answering a real queued cell" -- it advances
  the queue by one cell for *every* main `(%iN)` prompt it sees. A plain
  statement here makes Maxima print its own extra prompt, and
  `RemoveFirst()` then silently drops one real (never-sent) queued cell for
  each such prompt -- confirmed live with `tcpdump` on the raw
  wxMaxima<->Maxima socket to drop the whole evaluation queue (21 cells to 0)
  in one shot, root-causing an intermittent hang/failure in
  `automatic_test_files/lisp_mode.wxm`. This bit `wxdirs`, the Maxima struct
  exposing wxMaxima's own paths: it has to be built via genuine Maxima syntax
  (`defstruct`, `wxdirs@field: "value"`, ...) since Maxima's `defstruct` does
  not evaluate named-field initializers to the field's value -- it silently
  stores the unevaluated `field = value` equation instead (confirmed against
  a real Maxima 5.46), and `new(wxdirs(field=value, ...))` therefore doesn't
  work either. The fix: build the Maxima-syntax statement (still going
  through `wxMaxima::EscapeForLisp()` per value -- despite the name it is
  exactly the escaping Maxima string literals need too, for `"`/`\` in any
  filesystem path), then wrap the *whole* statement text as the argument to
  `:lisp-quiet (with-input-from-string (wxst "...") (meval (caddr (mread
  wxst 0))))` -- reads and evaluates it from Lisp with no separate prompt of
  its own, same as every other `m_configCommands` entry. The statement text
  needs `EscapeForLisp()` applied a *second* time at that point, since it is
  now itself the content of a Lisp string literal (each individual field
  value was already escaped once, for the Maxima string literal it sits
  inside).
  - **Debugging technique note:** when in doubt about what actually crossed
    the wxMaxima<->Maxima socket (vs. what a log line *claims* was sent),
    `wxLogMessage()`-based tracing inside wxMaxima can itself be misleading --
    `Maxima::Write()` only enqueues to `m_outputQueue`; the worker thread
    flushes it to the socket separately and asynchronously, so a logged "sent"
    call is not proof the bytes ever left the process (e.g. if the app exits
    first). `tcpdump -i lo -w file.pcap 'tcp portrange 49000-49999'` plus a
    small manual pcap parser (this sandbox's Python has no working `scapy` --
    `cryptography`'s Rust backend panics on import here -- so parse the
    classic pcap format directly: 24-byte global header, then repeated
    16-byte-record-header + packet frames; skip the 14-byte Ethernet header,
    read the IP header's IHL for its length, then the TCP header's data
    offset for its length) gives ground truth immune to any wxMaxima-internal
    misattribution. Also: `wxLogMessage()` is not safe to call from
    `Maxima::WorkerThread()` (a non-GUI thread) -- it crashed with a
    `wxArgNormalizer` format-specifier assertion the first time it was tried
    there for debugging (triggered by `%zu` specifically; the crash went away
    switching to `%lu` + an explicit `(unsigned long)` cast, but the
    thread-safety of logging from that thread at all remains unverified --
    treat any such tracing as temporary/debug-only, never ship it).

- **ASCII-art 2D display (`set_display('ascii)`) and `*alt-display2d*`:**
  when `$display2d` is on, Maxima's evaluator checks the special variable
  `*alt-display2d*` before printing a result: if it's a function symbol,
  that function is called *instead of* Maxima's own stock printer (this is
  how `mydispla` in `wxMathML.lisp` produces the normal `<mth>`/XML output);
  if it's `nil`, Maxima falls through to its own built-in ASCII-art printer,
  which pads a result's lines with literal spaces so a multi-line fraction/
  matrix/etc. lines up correctly under the `(%oN)` label -- but that padding
  assumes every line, including the label, ends up rendered in one uniform
  monospace font. `wxMathML.lisp`'s `wx-ascii-displa` wraps that stock
  printer in `<wxxml-asciimath>`/`</wxxml-asciimath>` markers *without*
  reimplementing it: it dynamically rebinds `*alt-display2d*` to `nil` for
  just the duration of `(displa x)`, which re-enters Maxima's own dispatch
  and this time takes the stock ASCII path (confirmed live: this is
  correctly reentrant-safe through Maxima's own recursive sub-expression
  `displa` calls, e.g. matrix rows, since they run inside the same dynamic
  extent). `Maxima::ProcessData()` only fires the corresponding
  `XML_ASCIIMATH` event once it has seen the *complete* matching closing
  tag, so `MaximaResponseReader::ReadAsciiMath()` always receives one whole
  block in one piece and can render all of it in one uniform style --
  before this, `ReadMiscText()` guessed a chunk's style from whether it
  happened to start with `"(%"`, and since chunks are split by socket/timer
  batching (not by where Maxima's actual output boundaries are), a block's
  label line could land in a separate batch than its neighbors and get
  misclassified into a different (proportional) font, breaking the
  alignment Maxima's padding assumed -- root-caused with a raw
  `:lisp-quiet (with-input-from-string ...)` / socket-level reproduction
  (see the debugging technique note above) before the fix, not guessed.

### File Formats

- **`.wxmx`** -- a ZIP archive holding `content.xml` (the MathML-like XML) plus
  the embedded images. The format version lives in `src/WXMXformat.h`.
- **`.wxm`** -- the plain-text format, read by `Format::ParseWXMFile()`.

### Translations (`locales/`)

- **One combined `.po` per language, in `locales/wxMaxima/`, is the file a
  translator edits.** It covers both wxMaxima's own UI strings
  (`xgettext`-extracted from `src/**/*.cpp`/`*.h`) and the manual's prose
  (`po4a`-extracted from `info/wxmaxima.md`). `locales/wxMaxima/wxMaxima.pot`,
  the template both `msgmerge` and Crowdin work against, is regenerated as
  the union of a fresh source scan and `locales/manual/wxmaxima.md.pot`
  (`msgcat --use-first`, preferring `wxMaxima.pot`'s own header) by the
  `update-locale` CMake target.
- **`po4a` must never be pointed at `locales/wxMaxima/<lang>.po` directly.**
  It looks like the obvious way to keep the manual's translations inside the
  combined file (`po4a.cfg`'s `$lang:` path *was* set to
  `locales/wxMaxima/$lang.po` at one point), but `po4a` doesn't treat a `.po`
  file as something to add/update entries in - it treats it as *its own*,
  and **rewrites it wholesale to contain only the entries it itself
  extracted from `info/wxmaxima.md`, silently discarding everything else**.
  Confirmed live: a language with 1000 translated UI strings and 69
  translated manual strings dropped to 69 (the UI strings gone) after one
  `po4a` run against the combined file - and this shipped merged to `main`
  before being caught. `po4a` therefore keeps writing its own
  `locales/manual/<lang>.po` (`po4a.cfg`'s `$lang:` path), exactly as before
  the two catalogs were combined; `locales/wxMaxima/CMakeLists.txt`'s
  `${LANG}_po` target runs `merge_manual_po.cmake` (`msgcat --use-first`) to
  fold `locales/manual/<lang>.po` into `locales/wxMaxima/<lang>.po` *before*
  `msgmerge`, every time - that script is the only place allowed to write
  manual content into the combined file. `locales/manual/*.po` only exists
  for the languages that actually have a manual translation (not the full
  `locales/wxMaxima/*.po` language list) - `info/CMakeLists.txt` expects a
  matching `info/wxmaxima.<lang>.md` to already exist for every language
  `po4a.cfg`'s language list names, and doesn't create a fresh empty one, so
  don't widen that language list to languages that have no manual
  translation yet without also handling that.
- **`po4a.cfg`'s manual `[type: text]` line needs `opt:"-o markdown"`
  explicitly** - `Locale::Po4a::Text`'s own default for that option is `1`,
  but that default does not take effect through `[type: text]`'s normal
  invocation; confirmed live by extracting the same heading with and without
  an explicit `-o markdown`. Without it, every markdown structural element
  (`##` headings, list items, ...) is extracted as generic wrapped "Plain
  text" instead of being recognized as its own no-wrap markdown construct,
  which is what caused #2047: a translated heading long enough to wrap got a
  literal newline inserted mid-heading when `po4a` wrote it back out,
  turning the second half into a normal paragraph in the rendered manual.
  The tempting broader fix, `opt:"-o neverwrap"` (disables wrapping
  entirely), is a trap: it doesn't just change *output* wrapping, it changes
  how `po4a` *segments source paragraphs into msgids* (each source line
  becomes a literal embedded `\n` in the msgid instead of the paragraph
  being one reflowed string) - confirmed live it turns 293 cleanly-matched
  German translations into 2 clean matches + 328 fuzzy, i.e. it invalidates
  the translation of nearly every multi-line paragraph in the whole manual,
  for a bug that's specifically about headings. `-o markdown` alone fixes
  the reported bug (headings/lists become their own no-wrap entries) with a
  much smaller, semantically-justified cost: only headings, list items and
  fenced code blocks need re-confirming as fuzzy (e.g. 293 clean -> 183
  clean + 156 fuzzy for German - about 89 of those are headings whose old
  msgstr still has the now-redundant leading `## ` baked in, since `po4a`
  auto-prepends it from the `Title ##` type instead of storing it in the
  translated text; the rest are fenced-code-block delimiters `po4a` now
  reconstructs itself instead of storing literally, plus a handful of
  entries that were already fuzzy for unrelated reasons - real source-text
  edits, not a `markdown` side effect), not full paragraphs. **The old
  translation text is not deleted** (`.po` keeps the fuzzy msgstr plus the
  previous msgid in a `#|` comment) **but it stops appearing in the
  generated manual** until a translator re-confirms it - `po4a-translate`
  skips fuzzy entries by default the same way `msgfmt` does for a compiled
  `.mo`, falling back to the untranslated English source. Don't describe
  this fix as lossless to a translator without that caveat: a previously
  fully-translated heading really does render in English again in
  `info/wxmaxima.<lang>.md` until someone reviews the (mostly mechanical:
  strip the leading `#+ `) fuzzy diff. Fixing the wrapping bug and keeping
  every translation rendering are in tension - there's no `po4a` option that
  gets both, since the whole point of `-o markdown` is to change what a
  heading's msgid *is*.
- **`locales/wxMaxima/CMakeLists.txt`'s `${LANG}_po` target needs
  `wxMaxima.pot` in its own `DEPENDS`, not just as a plain path string
  inside a `COMMAND` argument.** `add_custom_command(OUTPUT wxMaxima.pot
  ...)` only creates a file-level build rule; a *different* custom command
  (or target) that merely references that output path in a shell argument
  gets no ordering guarantee from it. Confirmed live: after fixing the
  manual's extraction to `-o markdown`, `make update-locale` kept producing
  stale results (matching the pre-fix fuzzy/translated counts exactly)
  because `wxMaxima.pot` itself hadn't been rebuilt - `${LANG}_po`'s
  `PRE_BUILD` command still consumed yesterday's `wxMaxima.pot` on disk.
  Fixed by declaring `DEPENDS ${LANG}.po wxMaxima.pot` on the
  `add_custom_target(${LANG}_po ...)` line.
- **Committing a `make update-locale` run's output means committing
  *every* language's drift against the current C++ source, not just the
  fix you're testing.** Running it live in this sandbox (to verify the two
  bugs above) also picked up ~1100 real UI strings that exist in
  `src/**/*.cpp`/`*.h` today but were missing from every committed
  `locales/wxMaxima/*.po` and `wxMaxima.pot` (confirmed genuine, not a
  sandbox artifact: `grep`-verified several, e.g. `Configuration.cpp`'s
  `_("  Font cache hits: %ld")`, actually exist in the source at `HEAD` -
  `xgettext` just hadn't been re-run against the source in a while before
  `wxMaxima.pot` was last committed). That's real, legitimate drift, but
  it's a separate concern from a `po4a`-pipeline bug fix - bundling ~1100
  new untranslated strings across 24 languages into a bugfix commit buries
  the actual fix and forces reviewers (translators included) to wade
  through unrelated noise. After verifying a `po4a`/CMake fix works
  end-to-end in the build directory, `git checkout --
  locales/wxMaxima/*.po locales/wxMaxima/wxMaxima.pot` to drop that
  incidental drift back to the committed state before committing, keeping
  only the actual pipeline files (`po4a.cfg.in`, the `CMakeLists.txt`s,
  `merge_manual_po.cmake`) plus whatever's scoped to the manual itself
  (`locales/manual/*.po`, `locales/manual/wxmaxima.md.pot`,
  `info/wxmaxima.<lang>.md`). The UI-string staleness is a legitimate
  follow-up `make update-locale` run of its own, on its own commit.
- **Don't run this sandbox's stock `po4a` (0.69) against real translated
  content.** See `CheckPo4aVersion.cmake`'s corruption warning above --
  verifying a change to the translation *pipeline*
  (`po4a.cfg.in`/`merge_manual_po.cmake`/the CMake wiring) is fine without
  running `po4a` itself, but regenerating actual `.po`/`.md` output needs a
  real `po4a` >= 0.70. It wasn't reachable via any of apt (only has 0.69),
  Debian's package archive, or po4a's own GitHub releases (both blocked by
  this sandbox's proxy) - but a source tarball of a current release, fetched
  outside the sandbox and handed to the agent as a file, runs perfectly well
  unpacked with no install step beyond `PERL5LIB=<unpacked-dir>/lib`
  pointing at its `Locale::Po4a::*` modules; the `po4a`/`po4a-updatepo`/
  `po4a-translate`/etc. scripts at the tarball's top level need nothing else
  to work standalone, e.g. `PERL5LIB=lib ./po4a --version`. Regenerating
  translated content live and diffing it (`msgfmt --statistics` before/after
  per language, matching translated-message counts) is how both bugs above
  were actually caught, not guessed from reading `Locale::Po4a::Text`'s
  source.
- **`git clean -fd` after `git checkout --` on a directory wipes untracked
  files in it too, including ones you meant to keep** (e.g. a new
  `locales/manual/<lang>.po` restored from a scratch copy, or a brand new
  `.cmake` helper script that was never committed yet) - it doesn't
  distinguish "test-run droppings" from "uncommitted new work" by intent,
  only by whether `git add` has seen the path yet. Prefer reverting only the
  specific files that are actually wrong (or committing work-in-progress to
  a scratch commit first) over a blanket `git checkout -- <dir> && git clean
  -fd <dir>` once a directory has a mix of both kinds of changes in it.

## Conventions & Standards

- **Git Environment:** Note that running `git diff` might launch the visual diff tool `meld` instead of outputting to the terminal. Always use `git diff --no-ext-diff` if you need terminal output.
- **String Literals & Translations:** Use the `wxS()` macro for all string literals and `_()` for user-facing translatable strings.
- **Logging:** Use `wxLogMessage()` for debugging; messages are visible in **View -> Toggle Log Window** or by using the option `--logtostderr`.
- **Asynchronous Sidebars & Safety:** Sidebars (TOC, Variables Pane) update asynchronously. Always validate `GroupCell` pointers (using `m_tree->Contains()`) before use.
- **Long-Lived Cell References:** Anything that keeps a reference to a `Cell` or `GroupCell` beyond the current call -- undo/redo actions, the evaluation queue, the selection, the sidebars, a cached "last clicked" cell -- MUST hold it as a `CellPtr<...>`, never as a raw pointer. `CellPtr` derives from `Observed` and nulls itself when the cell is destroyed, so a stale reference reads as `nullptr` instead of dangling; null-check it when consuming it, because the cell may have died since it was stored. A raw `Cell *` is fine only for the duration of a single function or event.
- **Cell UUIDs & Navigation:** Cells have unique `m_uuid`. Filenames support `#UUID` fragments.
- **Forward Compatibility:** `ToXML()` implementations MUST call `GetXMLFlags()` and include its output in the opening tag to preserve unknown attributes.
- **Serialization Tags:** Some cells use shortened tags (e.g., `LimitCell` uses `<lm>`). Verify in `MathParser.cpp` before modifying.
- **Gnuplot Probe:** MUST be done asynchronously (e.g., `wxEXEC_ASYNC`). Synchronous execution blocks the UI and can disrupt the Linux global menu system.
- **Variable Escaping:** Use `Maxima::EscapeVarnameForMaxima` for characters like `,`, `°`, and special symbols. A digit at the *start* of a variable name must be escaped (e.g., `\1a`).
- **Maxima Restart (Windows):** Restarting requires a manual reset of the network client (`m_client.reset()`) and streams in `KillMaxima` (which lives in `MaximaProcessManager`, not in `wxMaxima` any more) to avoid socket state errors.
- **Worksheet Search Logic:** Traverse in visual order: Prompt → Editor → Output (Forward) or Output → Editor → Prompt (Reverse). Resume from current caret position.
- **Layout Timeout:** Complex output can trigger a timeout (configurable in Options), replacing slow-to-render cells with a warning.
- **C++ Standard:** The project uses **C++20**. To support users on older operating systems (like Debian-oldstable or RHEL), wxMaxima aims to stay approximately 10 years behind the current C++ standard.
- **wxWidgets Version:** Maintain compatibility with wxWidgets 3.0.5 where possible. Avoid features only available in 3.1+ (e.g., use `MakeAbsolute()` + `GetFullPath()` instead of `GetAbsolutePath()`).
- **Sizer Flags Are Different Enum Types:** `wxDirection` (`wxLEFT`/`wxRIGHT`/`wxALL`/...), `wxAlignment` (`wxALIGN_*`) and `wxStretch` (`wxEXPAND`/...) are three distinct unscoped enums; OR'ing two of them directly (e.g. `wxALIGN_CENTER_VERTICAL | wxALL`) is deprecated in C++20 and GCC warns `-Wdeprecated-enum-enum-conversion`. Fix by casting the *first* operand of the OR-chain to `int` (e.g. `static_cast<int>(wxALIGN_CENTER_VERTICAL) | wxALL`) -- since `|` is left-associative, this makes every subsequent operation `int | enum`, which is unambiguous and unwarned, without needing to touch the rest of the chain. Only the leftmost token needs the cast, however many differently-typed flags follow.
- **`[[maybe_unused]]` on data members and GCC < 12:** GCC before version 12 doesn't support `[[maybe_unused]]` on non-static data members at all and warns `'maybe_unused' attribute ignored [-Wattributes]` regardless of whether the member is actually used (reproduced directly against `g++-11`; fixed by `g++-12`). Since the attribute is still needed for Clang (`-Wunused-private-field`), don't just delete it -- wrap the declaration in `#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ < 12` / `#pragma GCC diagnostic push/ignored "-Wattributes"` ... `#pragma GCC diagnostic pop` / `#endif` (see `SvgBitmap.h`, `wxMathml.h`, `graphical_io/Printout.h`).
- **CI Warnings Live On the Non-`-Werror` Jobs:** `compile_latest_and_test` and `compile_without_webview` (Ubuntu) build with `-Werror`, so they can't show warnings by construction -- check `compile_2204` (Ubuntu 22.04, plain `-Wall -Wextra`, GCC 11) for real warnings that survive to a release build. Don't assume that job's warning list is exhaustive, though: e.g. the `[[maybe_unused]]`-on-a-data-member GCC<12 warning above showed up for `Printout.h` in one such log but not for the identical pattern in `SvgBitmap.h`/`wxMathml.h` in the same run, for reasons that weren't tracked down (not precompiled headers -- `WXM_ENABLE_PRECOMPILED_HEADERS` defaults `OFF`) -- a clean local build with `g++-11 -Wall -Wextra` is the more reliable check for this specific class of warning.
- **`Cell` Bitfields Use C++20 Default Member Initializers, Not `InitBitFields_ClassName()`:** Every per-class flag bit-field (`Cell`, `EditorCell`, `GroupCell`, `TextCell`, `MatrCell`, and the rest of `src/cells/`) declares its default inline, e.g. `bool m_foo : 1 = false;`. The older pattern -- an `InitBitFields_ClassName()` method called from the constructor body, with each field tagged `/* InitBitFields_ClassName */` -- predated C++20 support for bit-field default member initializers and has been fully removed (2026-08); don't reintroduce it for new flags. Classes with zero bit-fields of their own no longer carry an empty stub either. Before folding an existing full-size `bool m_foo;` into a class's bitfield, check (1) nothing takes its address (`&m_foo` doesn't work on a bit-field member) and (2) it's only touched from the GUI thread (no cross-thread `bool` atomicity/tearing expectations) -- worksheet cells are not thread-shared, but double-check call sites rather than assuming. **Declaration order matters more than usual here**: C++ initializes members in declaration order, not constructor-init-list order, so a bit-field read by a *later*-declared member's own initializer (e.g. `IntervalCell::m_leftBracketOpensLeft`/`m_rightBracketOpensRight`, read by the `m_openBracket`/`m_closeBracket` initializers) must stay declared *before* those members -- relocating it next to an unrelated bitfield group to save a byte is undefined behavior (reading the bit-field before it's initialized), not just a style choice, caught before it shipped by tracing the actual initialization order rather than trusting the mem-initializer-list order. When a field can't move, bit-fielding it in place still works: two adjacent `: 1` declarations pack into a shared byte regardless of position.
- **Tab Characters in `EditorCell`:** A `'\t'` is a real, single character in `m_text` (see `EditorCell::NormalizeLineEndings()`, which replaced the old `TabExpand()` that irreversibly rewrote every tab to 1-4 spaces on input/paste/load). It is expanded to the next 4-column tab stop -- one column being the width of a space glyph in the current font -- only where text becomes pixels, via `EditorCell::NextTabStop(startX)`/`MeasureTextWidth(startX, text)`. Tab width is **position-dependent**, the one thing `GetTextExtent()`/`GetTextSize()` cannot compute on their own (unlike every other character), so it can never be cached the way `StyledText::SetWidth()` caches other tokens' widths. `MaximaTokenizer` guarantees a tab is always its own isolated, single-character token (never merged into a space run, mirroring how a newline is already its own token) -- this is *load-bearing*: every `m_styledText`-based site (`Draw()`, `Recalculate()`, `GetLineWidth()`, `SelectPointText()`'s code-cell branch, `StyleTextCode()`) only needs a `text == wxS("\t")` equality check as a result, never substring splitting. Prose/text cells don't go through `MaximaTokenizer` at all, so `EditorCell::StyleTextTexts()` uses its own splitter, `PushTextLine()`, to get the same isolation guarantee for a tab embedded in an otherwise plain line of text. Sites that measure a raw `m_text` substring instead of a single token (`MarkSelection()`, `MixedDirectionOffset()`, `StyleTextTexts()`'s wrap check) go through `MeasureTextWidth()` instead, which splits on `'\t'` internally since a substring can still have one embedded anywhere. Left/Right arrow and Delete need **no special-casing** for tabs -- they already move/delete exactly one `m_text` character, which is now correct automatically. The plain `WXK_BACK` case's old "gobble up to 4 trailing spaces" shim was a workaround for the old space-expanded-tab world and is gone; a real tab deletes in one plain single-character backspace like anything else.

## Layout & Compatibility

- **Mathematical Cell Padding:** Use `MC_TEXT_PADDING` (in `Configuration.h`) for text-based cells. **Exception:** `DigitCell` does not include padding to ensure visual consistency in broken-up numbers.
- **Three-Step Layout Process:**
  1. `UnBreakUpCells()`: Reset to 2D.
  2. `BreakUpCells()`: Convert wide 2D objects to 1D fallback.

     **Recursive Strategy:** If a 2D object is too wide, `CollectWideCells` recursively identifies sub-cells that are already >80% of the available width. These sub-cells are also converted to linear form in the same pass. This heuristic accounts for font size increases that occur when a parent object is linearized, preventing redundant O(N^2) size resets and recalculations in deeply nested structures.
  3. `BreakLines_List()`: Final line wrapping.
- **High-DPI / wxBitmapBundle:** Use `wxBitmapBundle` for SVG rendering.
- **Windows Focus Management:** Use `CallAfter` for focus transitions (e.g., `m_searchText->SetFocus()`) to prevent the worksheet from "stealing" focus back.
- **Graphical export temp files (`src/graphical_io/OutCommon.cpp`):** `wxSVGFileDC`/EMF's DC only write to a real path, so the SVG/EMF representation rendered for the clipboard needs a temp file (unlike a real "Export as..." target file, which is the user's own chosen path and untouched by this). `PrivateTempDir()` puts it in a mode-0700 `tmp/` subdirectory of `Dirstructure::UserConfDir()` instead of the shared system temp dir, so another unprivileged user can't win a race between the file's creation and its being opened by name (the classic symlink-swap window any path-only API leaves open). Falls back to `wxFileName::CreateTempFileName()`'s own default location if that directory can't be created.
- **Bidi (`src/Bidi.h`/`.cpp`):** Reorders a line of text per the Unicode Bidirectional Algorithm (UAX #9), using `libfribidi` when it's available at build time (`USE_FRIBIDI` in `BuildConfig.h`, optional, `WXM_USE_FRIBIDI` CMake option, on by default when `pkg-config fribidi` is found) and falling back to a single-run approximation otherwise. No wxWidgets backend exposes this reordering itself -- Pango/CoreText/DirectWrite compute it internally to shape glyphs but never hand it back to the app. `EditorCell::GetLineBidiRuns()` wraps it as absolute `m_text` positions; `MixedDirectionOffset()` (used by `PositionToPoint()`, hence also `MarkSelection()` and `SelectPointText()`'s click search) and `HandleSpecialKey()`'s arrow-key handling are the consumers. wxmTestApp is an OBJECT library (`test/unit_tests/CMakeLists.txt`): it compiles `Bidi.cpp` itself and needs `PkgConfig::FRIBIDI` linked directly to *it* (not just to `wxmaxima`) to get fribidi's include path at that compile step; separately, its own `target_link_libraries()` don't propagate through `$<TARGET_OBJECTS:wxmTestApp>`, so anything it needs must *also* be linked into each consuming test executable directly (`WXM_TESTAPP_EXTRA_LIBS`) for the final link. The imported target has to be declared `GLOBAL` since `test/` is a sibling directory of `src/`, not a descendant. `#include <fribidi.h>`, not `<fribidi/fribidi.h>`: pkg-config's own `-I` already points *at* fribidi's header directory (confirmed on both Debian's and Homebrew's `.pc` files), so the extra `fribidi/` prefix only "worked" on Linux by accident, via `/usr/include` being an implicit compiler search path Homebrew's non-default prefix doesn't share -- caught by a real macOS CI failure, not by this sandbox.
- **ConfigDialogue Tabs Must Scroll:** Every tab panel in `src/dialogs/ConfigDialogue.cpp` is a `wxScrolled<wxPanel>` with `SetScrollRate(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor())` and `SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth, GetContentScaleFactor() * mMinPanelHeight))`. Without this, a tab's natural size (which grows with font size/DPI/translation length) can make the whole dialog taller than a hi-DPI screen with no way to reach what's cut off. When adding a new tab, copy this pattern (see `CreateWorksheetPanel()`) rather than a plain `wxPanel`.
- **Constructor Initialization:** Order initialization lists to match header declaration order to prevent `-Wreorder` warnings.

## Performance & Documentation Mandates

- **NEWS.md Updates:** Every non-trivial change MUST be documented in `NEWS.md` under the "# Current development version" section.
- **Doxygen Comments:** Include descriptions for all new classes and public methods. Complex algorithms (e.g., LCS alignment) require detailed architectural comments.
- **Background Tasks:** Use `jthread` for automatic joining, protect data with `std::mutex`, check for abort flags regularly, and update `Doxygen/Readme.md`.
- **Lisp Performance (`wxMathML.lisp`):**
  - Use `with-output-to-string` instead of recursive concatenation for large inputs.
  - Use `unwind-protect` when modifying global state like `$lmxchar`.
  - Prefer `(intern ...)` over `read-from-string` for dynamic symbol generation.
- **Strict XML Mandate:** To avoid duplicate attributes, add any new manually-handled XML attribute (e.g., `noneParens="true"`) to the filter list in `MathParser.cpp`.

## Visual Documentation (`art/Doxygen/`)

- **Geometry Awareness:** If modifying `Recalculate()` or `Draw()` geometry (padding, center alignment), you MUST update the corresponding SVG diagrams in `art/Doxygen/`.
- **Consistency:** New cell types should have `*Geometry.svg` and `*LinearGeometry.svg` (if applicable) diagrams.

## Key Subsystems Map

- **Layout Engine:** `src/cells/` and `src/worksheet/` (`Worksheet.cpp` and its siblings moved into that subdirectory).
- **MathML Formatting:** `src/wxMathML.lisp` and `src/MathParser.cpp`.
- **Main Logic:** `src/wxMaxima.cpp` and `src/wxMaximaFrame.cpp` -- but much of what used to sit in `wxMaxima` has been peeled off into friend classes, so look there first: `MaximaProcessManager` (spawn/kill/connect and the data pump), `MaximaEvaluator` (evaluation queue and command protocol), `MaximaResponseReader` (the incoming-XML handlers), `MaximaFileIO` (worksheet open/save) and `MaximaCommandMenus` (the menu handlers).
- **Configuration:** `src/Configuration.cpp`.

## Backlog / Future Work

Items the maintainer has flagged as worth doing but hasn't asked for yet -- don't
start on these without checking in first, but pick them up if asked for "what's
next" style work.

- **Real tab handling in `EditorCell`:** tabs are currently just replaced by
  spaces on input instead of being handled as their own character/column-stop
  concept.

## Error resilience

- To err is human => If your instructions don't seem to make sense feel free to ask.
