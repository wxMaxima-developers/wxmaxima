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
  running). Mixes `:lisp-quiet (...)` directives with plain Maxima statements
  ending in `$` in the same stream -- both are valid there, since it is fed to
  Maxima the same way interactive input is. `wxdirs`, the Maxima struct
  exposing wxMaxima's own paths, is built this way; each field is set with its
  own `wxdirs@field: "value"$` statement rather than `new(wxdirs(field=value,
  ...))`, because Maxima's `defstruct` does not evaluate named-field
  initializers to the field's value -- it silently stores the unevaluated
  `field = value` equation instead (confirmed against a real Maxima 5.46).
  Values that could contain `"` or `\` (any filesystem path) need
  `wxMaxima::EscapeForLisp()` first; despite the name it is exactly the
  escaping Maxima string literals need too.

### File Formats

- **`.wxmx`** -- a ZIP archive holding `content.xml` (the MathML-like XML) plus
  the embedded images. The format version lives in `src/WXMXformat.h`.
- **`.wxm`** -- the plain-text format, read by `Format::ParseWXMFile()`.

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

## Error resilience

- To err is human => If your instructions don't seem to make sense feel free to ask.
