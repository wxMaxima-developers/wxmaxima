// -*- mode: C++; c-file-style: "bsd"; -*-
//
// Headless-CI safety net for the test runners.
//
// A wxWidgets assertion otherwise pops up a *modal* dialog (the wx assert box,
// and on Windows additionally Windows Error Reporting's "<exe> has stopped
// working" on a crash). In CI - and in a headless or unattended run on any
// platform - nobody is there to click it away, so the test process blocks
// until ctest's watchdog kills it after the per-test timeout: the "every unit
// test times out at 300 s" symptom seen on the first Windows test run, and
// again when a Linux test first tripped an assertion.
//
// The static initializer below runs before main() in every test executable
// (the object file is linked directly into each one, so its constructor is
// never stripped) and replaces the modal dialog with a message on stderr: a
// failed wx assertion is reported in the test log and the test keeps running,
// so Catch2 can report the genuine outcome. On Windows a real crash
// additionally terminates the process immediately instead of blocking.

#include <wx/debug.h>
#include <wx/string.h>
#include <cstdio>
#ifdef _WIN32
#include <windows.h>
#endif

namespace {

#if wxDEBUG_LEVEL
void StderrAssertHandler(const wxString &file, int line, const wxString &func,
                         const wxString &cond, const wxString &msg) {
    fprintf(stderr, "wxASSERT failed: %s:%d [%s]: %s %s\n",
            static_cast<const char *>(file.utf8_str()), line,
            static_cast<const char *>(func.utf8_str()),
            static_cast<const char *>(cond.utf8_str()),
            static_cast<const char *>(msg.utf8_str()));
}
#endif

struct DisableModalErrorDialogs {
    DisableModalErrorDialogs() {
#ifdef _WIN32
        // No "application error" / "stopped working" pop-up on a crash; the
        // process just dies and ctest reports the failure.
        SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX |
                     SEM_NOOPENFILEERRORBOX);
#endif
#if wxDEBUG_LEVEL
        // No modal wx assertion dialog: a tripped assertion is printed to
        // stderr (visible in the test log) and the test keeps running.
        wxSetAssertHandler(StderrAssertHandler);
#endif
    }
};
[[maybe_unused]] const DisableModalErrorDialogs g_disableModalErrorDialogs;
} // namespace
