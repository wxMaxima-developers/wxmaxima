// -*- mode: C++; c-file-style: "bsd"; -*-
//
// Headless-CI safety net for the Windows test runners.
//
// On Windows a crash or a wxWidgets assertion otherwise pops up a *modal*
// dialog (Windows Error Reporting's "<exe> has stopped working", or the wx
// assert box). In CI nobody is there to click it away, so the test process
// blocks until ctest's watchdog kills it after the per-test timeout -- which is
// exactly the "every unit test times out at 300 s, even the trivial ones"
// symptom seen on the first Windows test run.
//
// The static initializer below runs before main() in every test executable (the
// object file is linked directly into each one, so its constructor is never
// stripped) and turns those modal dialogs off: a real crash terminates the
// process immediately and a failed wx assertion becomes a no-op. Either way a
// broken test now fails *fast* and its reason shows up in the CI log instead of
// hanging the whole job.
//
// On every other platform this file compiles to nothing.

#ifdef _WIN32

#include <wx/debug.h>
#include <windows.h>

namespace {
struct DisableModalErrorDialogs {
  DisableModalErrorDialogs() {
    // No "application error" / "stopped working" pop-up on a crash; the process
    // just dies and ctest reports the failure.
    SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX |
                 SEM_NOOPENFILEERRORBOX);
#if wxDEBUG_LEVEL
    // No modal wx assertion dialog: a tripped assertion becomes a no-op so the
    // test keeps running and Catch2 can report the genuine outcome.
    wxSetAssertHandler(nullptr);
#endif
  }
};
[[maybe_unused]] const DisableModalErrorDialogs g_disableModalErrorDialogs;
} // namespace

#endif // _WIN32
