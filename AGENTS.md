# Project Instructions

This file contains architectural insights, conventions, and operational knowledge to assist AI agents working on the wxMaxima codebase. **Agents are explicitly permitted and encouraged to update this file with new findings that improve context and safety.**

## Build System
- **Build Tool:** `ninja`
- **Build Directory:** `build`
- **Compilation Command:** `ninja -C build`

## Architecture & GUI
- **wxAuiManager:** The application uses `wxAuiManager` for its complex layout (sidebars, toolbars, worksheet). 
  - **Linux/GTK Timing:** On Linux (especially KDE Plasma with Global Menus), calling `m_manager.Update()` can disrupt the menu bar if it's already attached. This is a known environmental issue in the interaction between wxWidgets, GTK3, and the KDE Global Menu proxy.
    - **Automated Fix:** On systems with wxWidgets <= 3.2 running on KDE, Unity, or with `appmenu-gtk-module` enabled, wxMaxima automatically sets `UBUNTU_MENUPROXY=0` at startup in `main.cpp` to force menus to remain within the window and prevent disappearance.
    - If the menu still disappears, clearing `GTK_MODULES` (e.g., `GTK_MODULES=""`) can also restore local menus.
- **Cursors:** The worksheet has 2 types of Cursor: A standard cursor in an EditorCell or a hCaret between two worksheet cells. Only one cursor is active at a time.
- **Key Classes:**
  - `wxMaxima`: The main application class (subclass of `wxMaximaFrame`).
  - `wxMaximaFrame`: The base frame class handling layout and sidebars.
  - `EditorCell`: Handles text and code input, including Markdown-like formatting (bullet lists).
  - `Variablespane`: Manages the list of defined variables and their values.
  - `AutoComplete`: Handles the autocomplete logic for commands, variables, and files.

## Conventions & Standards
- **Git Environment:** Note that running `git diff` might launch the visual diff tool `meld` instead of outputting to the terminal. Always use `git diff --no-ext-diff` if you need terminal output.
- **String Literals & Translations:** Use the `wxS()` macro for all string literals and `_()` for user-facing translatable strings.
- **Logging:** Use `wxLogMessage()` for debugging; messages are visible in **View -> Toggle Log Window** or by using the option `--logtostderr`.
- **Asynchronous Sidebars & Safety:** Sidebars (TOC, Variables Pane) update asynchronously. Always validate `GroupCell` pointers (using `m_tree->Contains()`) before use.
- **Cell UUIDs & Navigation:** Cells have unique `m_uuid`. Filenames support `#UUID` fragments.
- **Forward Compatibility:** `ToXML()` implementations MUST call `GetXMLFlags()` and include its output in the opening tag to preserve unknown attributes.
- **Serialization Tags:** Some cells use shortened tags (e.g., `LimitCell` uses `<lm>`). Verify in `MathParser.cpp` before modifying.
- **Gnuplot Probe:** MUST be done asynchronously (e.g., `wxEXEC_ASYNC`). Synchronous execution blocks the UI and can disrupt the Linux global menu system.
- **Variable Escaping:** Use `Maxima::EscapeVarnameForMaxima` for characters like `,`, `°`, and special symbols. A digit at the *start* of a variable name must be escaped (e.g., `\1a`).
- **Maxima Restart (Windows):** Restarting requires a manual reset of the network client (`m_client.reset()`) and streams in `KillMaxima` to avoid socket state errors.
- **Worksheet Search Logic:** Traverse in visual order: Prompt $\to$ Editor $\to$ Output (Forward) or Output $\to$ Editor $\to$ Prompt (Reverse). Resume from current caret position.
- **Layout Timeout:** Complex output can trigger a timeout (configurable in Options), replacing slow-to-render cells with a warning.
- **C++ Standard:** The project uses **C++20**. To support users on older operating systems (like Debian-oldstable or RHEL), wxMaxima aims to stay approximately 10 years behind the current C++ standard.
- **wxWidgets Version:** Maintain compatibility with wxWidgets 3.0.5 where possible. Avoid features only available in 3.1+ (e.g., use `MakeAbsolute()` + `GetFullPath()` instead of `GetAbsolutePath()`).

## Layout & Compatibility
- **Mathematical Cell Padding:** Use `MC_TEXT_PADDING` (in `Configuration.h`) for text-based cells. **Exception:** `DigitCell` does not include padding to ensure visual consistency in broken-up numbers.
- **Three-Step Layout Process:** 
  1. `UnBreakUpCells()`: Reset to 2D.
  2. `BreakUpCells()`: Convert wide 2D objects to 1D fallback.
  3. `BreakLines_List()`: Final line wrapping.
- **High-DPI / wxBitmapBundle:** Use `wxBitmapBundle` for SVG rendering. Avoid unnecessary `GetPreferredBitmapSizeFor` calls on macOS to prevent `nodiscard` warnings.
- **Windows Focus Management:** Use `CallAfter` for focus transitions (e.g., `m_searchText->SetFocus()`) to prevent the worksheet from "stealing" focus back.
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
- **Layout Engine:** `src/cells/` and `src/Worksheet.cpp`.
- **MathML Formatting:** `src/wxMathML.lisp` and `src/MathParser.cpp`.
- **Main Logic:** `src/wxMaxima.cpp` and `src/wxMaximaFrame.cpp`.
- **Configuration:** `src/Configuration.cpp`.

## Error resilience
- To err is human => If your instructions don't seem to make sense feel free to ask.
