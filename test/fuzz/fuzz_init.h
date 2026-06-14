// Shared helper for the fuzz harnesses.
//
// wxGTK routes font/DC work through GTK, which needs an X display. Locally we run
// the fuzzers under `xvfb-run` (so DISPLAY is already set). In a headless
// environment such as OSS-Fuzz there is no display, so we start our own Xvfb
// once during initialization. No-op when a display is already available.
#ifndef WXM_FUZZ_INIT_H
#define WXM_FUZZ_INIT_H

#include <cstdlib>
#include <unistd.h>

static inline void EnsureDisplay() {
  if (getenv("DISPLAY") || getenv("WAYLAND_DISPLAY"))
    return;
  // Best-effort: requires Xvfb to be installed (the OSS-Fuzz Dockerfile does).
  if (system("Xvfb :99 -screen 0 1280x1024x24 >/dev/null 2>&1 &") == 0) {
    setenv("DISPLAY", ":99", 1);
    sleep(1);   // let Xvfb come up before GTK connects
  }
}

#endif
