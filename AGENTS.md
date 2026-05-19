# Project Instructions

This file contains architectural insights, conventions, and operational knowledge to assist AI agents working on the wxMaxima codebase. **Agents are explicitly permitted and encouraged to update this file with new findings that improve context and safety.**

## Build System
- **Build Tool:** `ninja`
- **Build Directory:** `build`
- **Compilation Command:** `ninja -C build`

## Architecture & GUI
- **wxAuiManager:** The application uses `wxAuiManager` for its complex layout (sidebars, toolbars, worksheet). 
  - **Linux/GTK Timing:** On Linux (especially KDE Plasma with Global Menus), calling `m_manager.Update()` can disrupt the menu bar if it's already attached. The recommended practice is to check if the menu bar is still attached using `if (GetMenuBar() != m_MenuBar)` and re-assert it with `SetMenuBar(m_MenuBar)` within the `AuiManagerUpdate()` helper. This ensures the menu bar remains visible after layout changes without disrupting active user interactions.
- **Key Classes:**
  - `wxMaxima`: The main application class (subclass of `wxMaximaFrame`).
  - `wxMaximaFrame`: The base frame class handling layout and sidebars.
  - `EditorCell`: Handles text and code input, including Markdown-like formatting (bullet lists).
  - `Variablespane`: Manages the list of defined variables and their values.
  - `AutoComplete`: Handles the autocomplete logic for commands, variables, and files.

## Conventions & Standards
- **String Literals:** Use the `wxS()` macro for all string literals for cross-platform character encoding compatibility.
- **Translations:** Use the `_()` macro for user-facing strings that need to be translatable.
- **Logging:** Use `wxLogMessage()` for debugging and operational status. These messages are visible in the internal log window (Help -> Show Log Window).
- If the relative position of subcells inside a cell is changed please update the diagrams in 'art/Doxygen/' to reflect it.
- **Asynchronous Sidebars & Safety:** Sidebars like the Table of Contents (TOC) and Variables Pane are often updated in the idle loop or via asynchronous events. Because the worksheet structure can change between an update trigger and a user action (e.g., double-clicking a TOC item), always validate that a `GroupCell` pointer is still valid and present in the worksheet tree (using `m_tree->Contains()`) before use.
- **Cell UUIDs & Navigation:** Every cell can have a unique identifier (`m_uuid`). 
  - **Fragment Support:** Filenames support `#UUID` fragments (e.g., `file.wxmx#ID`) to automatically scroll to a specific cell upon opening.
  - **Copy/Jump:** The UI supports copying UUIDs to the clipboard and jumping to a specific UUID via **File > Jump to UUID...**.
- **Forward Compatibility:** To prevent data loss when older versions of wxMaxima save files created with newer versions, `MathParser` collects unknown XML attributes into a map (`m_extraAttributes`) in each `Cell`. To ensure these (and other standard metadata like `uuid`, `altCopy`, and `tooltip`) are preserved, **all `ToXML()` implementations must call `GetXMLFlags()`** and include its output in the opening tag.
- **Serialization Tags:** Some cells use shortened tag names in XML for historical reasons or compatibility (e.g., `LimitCell` uses `<lm>`, `SqrtCell` uses `<q>`). Always verify the expected tag and child order in `MathParser.cpp` before modifying `ToXML()` methods.

## Conventions & Standards
- **Gnuplot Probe:** Probing Gnuplot for supported terminals MUST be done asynchronously (e.g., using `wxEXEC_ASYNC` with a `wxProcess` and event handler). Synchronous execution blocks the UI thread and can cause the Linux global menu system to lose track of the application.
- **Variable Escaping:** Characters like `,`, `°`, emojis, and special symbols (`:`, `.`, `=`, `|`, `&`) in variable names must be escaped with a backslash before being sent to Maxima. 
  - **Digits:** A digit at the *start* of a variable name must be escaped (e.g., `\1a`), but digits elsewhere do not need escaping.
  - `Maxima::EscapeVarnameForMaxima` is the central point for this logic. Autocomplete should use these escaped names to ensure user-readable input.
- **Maxima Restart & Connection (Windows):** On Windows, restarting Maxima requires a manual reset of the network client (`m_client.reset()`) and all streams in `KillMaxima`. Failure to do so can cause "New connection attempt whilst already connected" errors because the socket state is not automatically cleaned up when the process is killed via `taskkill`.
- **Windows Taskbar Progress:** To correctly clear the "busy" state from the Windows taskbar icon, use `wxTASKBAR_BUTTON_NOPROGRESS`. Using `wxTASKBAR_BUTTON_NORMAL` with zero progress may still display a green bar overlay on some systems. 
  - **Overlays:** When Maxima asks a question, a "question mark" overlay icon is set via `SetOverlayIcon`. This overlay MUST be cleared (using `wxNullIcon`) when transitioning to any other state to prevent stale notifications.
- **Worksheet Search Logic:** Searching (FindNext) traverses GroupCell components in a specific order to match the visual layout and avoid infinite loops.
  - **Forward Order:** Prompt $\to$ Editor $\to$ Output.
  - **Reverse Order:** Output (bottom-to-top) $\to$ Editor $\to$ Prompt.
  - **Resume Logic:** Always identify the component where the current selection/cursor is and resume search from there. When searching backwards from an Editor or Prompt, always skip the Output of that same group.
- **Table of Contents (TOC):** Indentation and section numbering in the TOC are mutually exclusive. Choosing "Section numbers" via the context menu disables visual indentation to maintain readability.
- **Layout Timeout:** Extremely complex output cells can cause the UI to hang during layout. wxMaxima includes a timeout mechanism (configurable via **Max. layout time** in Options) that replaces outputs taking too long to layout with a warning marker.
- **Exports (MathML/RTF):**
  - **MathML:** Modern MathML 4 grouping standards must be followed. Use `<mrow><mo>...</mo>...</mrow>` for fenced expressions instead of the deprecated `<mfenced>`. For missing scripts in `mmultiscripts` or `msubsup`, use the standard `<none/>` tag.
  - **RTF:** RTF uses **twips** (1/1440th of an inch) for dimensions. When exporting images, always include `\picwgoal` and `\pichgoal` in twips (calculated using the image's PPI) to ensure correct scaling in external word processors. Character escaping via `RTFescape` must handle surrogate pairs for Unicode characters outside the BMP and convert `\r` to `\line` for soft line breaks.
- **Auto-Answer UI:** When Maxima asks a question that wxMaxima can answer automatically (via saved answers in `GroupCell`), intrusive UI indicators like desktop notifications and the "userinput" status bar state (including the question mark overlay) are suppressed to avoid distracting the user.
- **StyleTextTexts:** This function in `EditorCell.cpp` handles syntax highlighting and formatting for text cells. Be extremely careful with iterator/index increments when adding soft line breaks to avoid skipping characters.
- **C++ standard:** wxMaxima tries to be about 9 years behind the current C++ standard to allow users with old operating systems to compile it. This means in 2026 C++17 is fine, but C++20 should still be a TODO, not something that can be switched to right now. 
- **wxWidgets version:** If possible it would be fine if we supported a fallback to wxWidgets 3.0.5 as some old operating systems still come with that library version. Note that `wxFileName::GetAbsolutePath` is only available from wxWidgets 3.1 onwards; use `MakeAbsolute()` followed by `GetFullPath()` for compatibility with 3.0.5.

## Layout & Compatibility
- **Mathematical Cell Padding:** Nearly all text-based mathematical cells (including `DigitCell` used in broken-up long numbers) MUST include `MC_TEXT_PADDING` (defined in `Configuration.h`) in their size calculations. 
  - **Recalculate:** `m_width` should be `sz.GetWidth() + 2 * MC_TEXT_PADDING`.
  - **Draw:** Text should be drawn at `point.x + MC_TEXT_PADDING`.
  - **Failure Mode:** Skipping this padding leads to cumulative horizontal errors, especially in long numbers broken across many lines, causing them to exceed worksheet margins.
- **Three-Step Layout Process:** The layout engine follows a strict sequence:
  1. `UnBreakUpCells()`: Resets all objects to their compact 2D form.
  2. `BreakUpCells()`: Recursively converts over-wide 2D objects (like fractions or long numbers) into linearized 1D forms.
  3. `BreakLines_List()`: Performs final line wrapping and inserts soft line breaks.
- **High-DPI / wxBitmapBundle:** For modern wxWidgets (3.1.6+), use `wxBitmapBundle` for SVG rendering. 
  - **Avoid:** Do not call `GetPreferredBitmapSizeFor` explicitly if the result is not used, as it triggers `nodiscard` warnings on macOS. 
  - **Mandate:** `wxBitmapBundle::GetBitmap(size)` correctly handles scale factors internally for the given logical size.
- **Windows Focus Management:** Focus transitions to dialogues (like the Find dialogue) from the worksheet on Windows often require `CallAfter` to ensure focus is not immediately "stolen" back by the worksheet.
  - **Dialogue Focus:** Always use `m_searchText->SetFocus()` within `CallAfter` when opening or updating a dialogue string from a worksheet event.
- **Constructor Initialization Order:** Always order members in the constructor initialization list to match their declaration order in the header file. This prevents `-Wreorder` warnings and potential uninitialized member access issues.

## Extending Maxima via Lisp (`wxMathML.lisp`)
- **Central Formatting Hub:** `src/wxMathML.lisp` is the primary location for extending Maxima's output formatting for wxMaxima. It is compiled into the binary as a header file.
- **Display Wrappers:** New user-facing formatting commands (like `wx_matrix`) should be implemented here to leverage Maxima's expression handling.
- **Safety:** Always use `unwind-protect` when temporarily modifying global Maxima/Lisp variables (like `$lmxchar`) to ensure they are restored even if the formatting process fails.
- **Loading:** Wrap new functions in `(no-warning ...)` to prevent "redefined" warnings during the initialization phase.

## Visual Documentation (`art/Doxygen/`)
- **Geometry Awareness:** The SVG diagrams in `art/Doxygen/` are critical for maintaining the complex layout engine. 
- **Update Mandate:** If you modify the `Recalculate()` or `Draw()` logic of a cell in a way that changes its internal geometry (padding, center alignment, sub-cell placement), you MUST update the corresponding SVG files.
- **Visual Consistency:** New cell types should be accompanied by a `*Geometry.svg` diagram and, if they have a linearized fallback, a `*LinearGeometry.svg` diagram.

## Lisp Performance & Safety
- **String Manipulation:** Avoid recursive string substitution or concatenation for large inputs. Use iterative loops with `with-output-to-string` for safety and performance.
- **Symbol Lookups:** Use `(intern (concatenate 'string "$" name) :maxima)` for dynamic symbol generation. Avoid `read-from-string` as it is less safe and slower.
- **Global State:** When wrapping Maxima commands, use `unwind-protect` to ensure global variables like `$lmxchar` are restored to their previous state even if an error occurs.

## Key Subsystems Map
- **Layout Engine:** `src/cells/` (individual cell logic), `src/Worksheet.cpp` (global orchestration).
- **MathML Formatting:** `src/wxMathML.lisp` (Lisp-to-XML), `src/MathParser.cpp` (XML-to-Cell objects).
- **Main Application Logic:** `src/wxMaxima.cpp` (event handling, Maxima communication), `src/wxMaximaFrame.cpp` (GUI structure).
- **Configuration:** `src/Configuration.cpp` (global settings and geometric constants).
