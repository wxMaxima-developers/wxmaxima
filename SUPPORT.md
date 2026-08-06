# Getting help & reporting problems

wxMaxima is the graphical **front-end** that displays math and handles user input
in a worksheet. **Maxima** is the computer algebra system — the "math engine" that
does the actual calculations. They are two separate projects, so where to go depends
on what your question or problem is about.

## A quick test: wxMaxima or Maxima?

Run the same command in a plain terminal with `maxima` (no wxMaxima). If you get
the same wrong or missing **result** there, it is a **Maxima** issue. If the
problem only happens **inside the wxMaxima window** (a crash, a display glitch, a
menu, file loading/saving, …), it is a **wxMaxima** issue.

If unsure just report the problem to the wxMaxima project so its members will find
out where the problem lies.

**Note:** this quick test can be misleading for the macOS quarantine issue below --
a plain terminal session doesn't need the local socket wxMaxima uses to talk to
Maxima, so `maxima` can look completely fine there even when wxMaxima cannot
reach it at all.

## Known issues

### macOS: wxMaxima never evaluates anything (stuck on "Reading Maxima output")

The GUI opens and is responsive, but every cell just sits in the evaluation
queue forever, with the status bar stuck on "Reading Maxima output" (see
[#1761](https://github.com/wxMaxima-developers/wxmaxima/issues/1761)). This is
usually **macOS Gatekeeper quarantining the `maxima` binary** -- a binary
installed via Homebrew, MacPorts, or downloaded directly can be flagged
`com.apple.quarantine`, which then silently blocks the local socket connection
wxMaxima uses to talk to it. Fix it from a Terminal:

```sh
xattr -dr com.apple.quarantine /path/to/maxima
```

If the exact binary path is unclear, running it against the whole install
prefix also works, e.g. `$(brew --prefix)` for Homebrew or `/opt/local` for
MacPorts.

## The wxMaxima GUI (this project)

- **Questions, ideas, general discussion** →
  [GitHub Discussions](https://github.com/wxMaxima-developers/wxmaxima/discussions)
- **Bugs in the user interface** (crashes, rendering, menus, file handling, …) →
  [GitHub Issues](https://github.com/wxMaxima-developers/wxmaxima/issues)

## Maxima, the computer algebra system

If the problem is about the **mathematics** — a wrong or missing result, an
integral Maxima cannot solve, a function that misbehaves — it comes from Maxima
itself, not from the GUI:

- **Bugs in Maxima's mathematics** →
  [Maxima bug tracker on SourceForge](https://sourceforge.net/p/maxima/bugs/)
- **Questions about using Maxima** →
  the [Maxima mailing list](https://maxima.sourceforge.io/maximalist.html)

More about Maxima itself: <https://maxima.sourceforge.io/>.

## Crash reports contents

wxMaxima contains a crash report generator that bundles all the information about
what lead to the crash.

The only information from the crash report that is really helpful is

- the backtrace that tells what line number the crash was in and what
  function calls with what parameters lead there.
- The operating system and
- The version of wxMaxima since the line number the line that caused the crash is
  in might have changed since the release of that version.
