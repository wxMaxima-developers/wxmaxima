# The manual's screenshots

Most images in `info/*.png` are made by wxMaxima itself, from the scenes in
`scenes/`, so that they can be regenerated whenever the program's look
changes instead of being redone by hand.

## Regenerating them

```sh
cmake -S . -B build -G Ninja
ninja -C build screenshots
```

This needs Maxima, gnuplot and `xvfb-run`, and overwrites `info/<scene>.png`
in the source tree. Look at the result before committing it: a scene that
failed leaves its old image in place and makes the target fail.

To make a few scenes only, or to write them elsewhere, run the script
directly:

```sh
xvfb-run -a -s "-screen 0 2400x1600x24" \
    sh info/screenshots/make-screenshots.sh build/src/wxmaxima /tmp/shots InputCell SidePanes
```

The virtual screen has to be larger than the biggest scene's window: parts of
a window outside the screen come out black.

## How a scene works

A scene is `scenes/<name>.ini`, plus `scenes/<name>.wxm` or `.wxmx` if it
shows a worksheet. The script runs

    wxmaxima --ini=<common.ini + name.ini> --screenshot=<name>.png [<name>.wxm]

with a fresh `HOME` and `MAXIMA_USERDIR`, so that nobody's own settings end
up in the picture. wxMaxima evaluates the worksheet, waits until Maxima is
idle, captures the image and exits without asking to save.

- `common.ini` holds the settings every scene shares. Its keys are top-level
  keys, so they come before the scene's own groups.
- The scene's `[Screenshot]` group says what to capture:
  - `show=window` (the whole window), `worksheet`, `cells` (only the
    worksheet's content, trimmed to it), `pane:<name>` (one sidebar) or
    `config:<page>` (a page of the configuration dialog).
  - `sidebars=` a comma-separated list of the sidebars to open, and
    `sidebarWidth=` their width.
  - `width=` and `height=` the window size.
  Any other configuration key can be set in the scene's `.ini` too.
- `wxmaxima-init.mac` is loaded before the scene starts, so that loading
  packages (and the compiler notes some Lisps print meanwhile) doesn't show.
- Files a scene loads (`Quadratic.mac`, `wxMaximaLogo.png`) are copied into
  the directory the scene runs in, which has a short name because a scene
  like `BatchImage` shows the path.

## Still made by hand

`horizontal-cursor-only.png`, `horizontal-cursor-between-cells.png`,
`ezUnits.png` and `Sidepane-TOC-convert-headings.png` show the program in the
middle of an interaction, which a scene can't express yet. The logo,
`maxima_screenshot.png` (plain Maxima in a terminal) and the locale warning
aren't wxMaxima windows at all.
