---
name: wxmaxima-export
description: How wxMaxima turns worksheets into HTML, LaTeX, images and .wxmx/.wxm - the equation-rendering flavours, the single source of accessible label text, the round-trip guarantees, and the traps (double scaling, ASCII-art alignment, forward-compatible XML). Use when touching WorksheetExport, the graphical_io classes, ToXML/MathParser, or any exporter.
---

# Exporting worksheets

Every exporter shares two obligations: it must be **deterministic** (exporting
the same document twice yields byte-identical output) and it must not **lose**
anything it cannot reproduce.

## The pieces

- `src/worksheet/WorksheetExport.cpp` - HTML, LaTeX, MAC, RTF, and the
  selection-to-string converters.
- `src/graphical_io/` - `BitmapOut` (PNG/BMP/XPM/JPEG), `Svgout`, `EMFout`,
  `Printout`, and `OutCommon`, which holds what they share.
- `Cell::ToXML()` / `MathParser` - the `.wxmx` `content.xml` round trip.
- `Format::CellsToXMLDocument()` / `TreeFromXMLDocument()` (`WXMXformat.cpp`) -
  the same XML as one self-contained document, which is what copied cells put
  on the clipboard (`application/x-wxmaxima-xml`, GH #2474; a paste prefers it
  over the `.wxm`, which has no text formatting, output or images).

HTML equations have several flavours (`Configuration::htmlExportFormat`): native
MathML, MathML+MathJax fill-in, bitmap and SVG. Each is a separate code path, so
a change to "the HTML export" usually needs checking in all four -
`test_WorksheetExport` loops over them for exactly this reason.

## Accessible labels have ONE source

An exported equation is a picture: an SVG draws it as anonymous paths, a PNG as
pixels. Screen readers get nothing unless we hand them the text.

`OutCommon::AccessibleText(const Cell *tree)` is the single place that derives
it - `ListToString()` with every run of whitespace folded to one space. All of
these must agree, and do, because they call it:

- the SVG document `<title>` and its `role="math"` group's `aria-label`
- the PNG's `Description` text chunk (written as `iTXt`, not `tEXt`)
- the HTML export's `<img alt="...">`

Whitespace folding is not cosmetic: 2-D ASCII-art maths pads its lines with long
space runs and tabs to keep fraction bars aligned, which is meaningless once the
text is spoken and makes the label a stuttering mess.

## Traps

- **Double scaling.** `BitmapOut::Layout()` once called `CreateScaled(size,
  scale)` *and* `SetUserScale(scale)`, magnifying everything twice - at the
  default 3x zoom only the upper-left third of an equation survived. Allocate
  the canvas at the already-scaled device size and let `SetUserScale` do the
  magnification, once. Suspect this shape whenever something is exactly 2x or
  3x wrong.
- **ASCII-art must stay verbatim.** `TS_ASCIIMATHS` output only reads correctly
  if its column alignment survives, so the LaTeX exporter emits a run of art
  lines as one `verbatim` block rather than as an equation, and the HTML export
  deliberately keeps it as an image rather than as text.
- **LaTeX markup is decided where it is generated, not cleaned up after.**
  A cell's `ToTeX()` braces a base or a script only through `Cell::TeXGroup()`
  (single tokens and whole groups stay bare, so `x^2`, but `{x_1}^2`), and
  uses `\left`/`\right` only if `Cell::TeXNeedsGrowingDelimiters()` finds a
  fraction, big operator, root or matrix inside. `<mo>` and `<fnm>` share
  `TS_FUNCTION`: only a name gets `\mathop{}`, an operator stays bare (a
  `\mathop{=}` has the spacing of a `\sum`). A label doesn't switch math mode
  itself: `Cell::ListToTeX()`, `GroupCell::ToTeXCodeCell()` and
  `Worksheet::CopyTeX()` each end the previous equation, and when the label
  did so as well an empty `\[\displaystyle \]` was left, which a string
  replacement then removed together with the second equation's
  `\displaystyle`. `test_TeXRedundancy` pins all of this.
- **Forward compatibility is mandatory.** `ToXML()` implementations MUST include
  `GetXMLFlags()` output in the opening tag, so attributes written by a *newer*
  wxMaxima survive a round trip through an older one. Any attribute you handle
  manually must also be added to `MathParser`'s filter list, or it gets emitted
  twice.
- **Shortened tags.** Some cells serialise under abbreviated names (`LimitCell`
  → `<lm>`). Check `MathParser.cpp` before assuming a tag name.
- **`ToXML()` doesn't write images, it names them.** `ImgCell`/`AnimationCell`
  push their bytes (and gnuplot sources) to `Configuration::PushFileToSave()`
  and put only a file name in the XML. A `.wxmx` stores those files next to
  `content.xml` in the zip; a self-contained document carries them base64
  encoded in an `<attachments>` element instead. Reading goes through
  `AttachmentSource` either way, so `Image` never knows which it was. Anything
  that calls `ToXML()` outside a save must take the file list out
  (`TakeFilesToSave()`) and put the previous one back, as the clipboard XML is
  rendered whenever a program asks for it - possibly while a save is running.
- **Temp files for DC-based formats.** `wxSVGFileDC`/EMF only write to a real
  path, so clipboard rendering needs a temp file - `OutCommon::PrivateTempDir()`
  puts it in a mode-0700 directory rather than the shared system temp, closing a
  symlink-swap window.

## The safety nets

- `test_WorksheetExport` exports a rich corpus through **every** exporter
  **twice** and requires the two trees to be byte-identical - which is also the
  lever that made the export-cluster extraction reviewable. Set
  `WXM_EXPORT_DUMP_DIR=<dir>` to keep the output and `diff -r` two revisions.
  It normalises the two things that legitimately differ between runs: wxSVG's
  process-global image ids, and zip entry timestamps in `.wxmx`.
- `test_WXMRoundtrip` / `test_WXMXRoundtrip` - `.wxm` byte round trip and
  `content.xml` parse/serialise idempotency. A cell class that loses information
  in `ToXML()`/`MathParser` shows up as a non-fixed-point here.
- Image assertions live alongside: bitmaps must not be clipped, SVGs must be
  labelled, PNGs must carry a description.

When adding an exporter feature, add the assertion to the existing loop rather
than a new test - it already has the corpus, the two-run determinism check and
the per-flavour iteration.
