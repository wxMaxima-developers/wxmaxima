// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  A console-subsystem front end for the GUI-subsystem wxmaxima.exe.

  On Windows an executable's subsystem is a bit in its PE header, fixed at
  link time: a binary is either GUI-subsystem or console-subsystem, never
  both. wxmaxima.exe has to be GUI-subsystem (a console-subsystem GUI app
  pops up a console window next to every worksheet), and that has two
  consequences for anything run from a command prompt:

    - it starts with no stdio wired up at all, which is what
      RedirectStdioToParent() in main.cpp exists to paper over, and
    - cmd.exe does not *wait* for a GUI-subsystem process. It returns to the
      prompt immediately, so even output that does reach the console lands
      after the next prompt has already been printed.

  The second one no amount of code inside wxmaxima.exe can fix. So this: a
  tiny console-subsystem launcher that starts wxmaxima.exe with our own
  standard handles, waits for it, and passes its exit code back. Because
  *this* process is console-subsystem, cmd.exe waits for it, and --version,
  --help and --batch behave the way a command-line tool is expected to.

  Deliberately plain Win32 + CRT, with no wxWidgets and none of wxMaxima's
  own code linked in: it needs neither, and staying dependency-free keeps it
  a few tens of kilobytes that link statically without dragging a second copy
  of a statically-linked wxWidgets along. If this ever grows into a genuinely
  headless CLI that evaluates worksheets, share the core through a CMake
  OBJECT library (the pattern wxmTestApp/wxmFuzzApp already use) rather than
  a DLL -- a DLL would duplicate statically-linked wxWidgets' global state
  (the wxModule registry, the wxApp instance, the art provider table) across
  the boundary.
*/

#include <windows.h>

#include <cstdio>
#include <string>

namespace {

//! The directory this executable lives in, with a trailing separator.
std::wstring ExeDirectory() {
  std::wstring path(MAX_PATH, L'\0');
  for (;;) {
    DWORD len = GetModuleFileNameW(nullptr, path.data(),
                                   static_cast<DWORD>(path.size()));
    if (len == 0)
      return std::wstring();
    // On truncation GetModuleFileNameW() returns exactly the buffer size it
    // was given (and still null-terminates), so "shorter than the buffer" is
    // what distinguishes a complete path from a truncated one.
    if (len < path.size()) {
      path.resize(len);
      break;
    }
    path.resize(path.size() * 2);
  }
  std::wstring::size_type slash = path.find_last_of(L"\\/");
  if (slash == std::wstring::npos)
    return std::wstring();
  return path.substr(0, slash + 1);
}

/*! Everything after the program name in our own raw command line.

  Deliberately not rebuilt by re-quoting a parsed argv[]: re-quoting is
  exactly the class of bug that already cost this project a multi-session
  investigation on the Windows CI (see AGENTS.md's wxmaxima_version_string
  entry, whose actual root cause turned out to be one pair of redundant
  quotes). Handing the original characters through untouched cannot
  introduce a quoting error that was not already in what the user typed.

  Windows parses the program-name portion of a command line with a simpler
  rule than the arguments: no backslash escapes, a quoted program name just
  ends at the next quote.
*/
const wchar_t *ArgumentTail() {
  const wchar_t *p = GetCommandLineW();
  while ((*p == L' ') || (*p == L'\t'))
    ++p;
  if (*p == L'"') {
    ++p;
    while (*p && (*p != L'"'))
      ++p;
    if (*p == L'"')
      ++p;
  } else {
    while (*p && (*p != L' ') && (*p != L'\t'))
      ++p;
  }
  while ((*p == L' ') || (*p == L'\t'))
    ++p;
  return p;
}

/*! Let the child decide what Ctrl+C means.

  Without this the launcher dies on Ctrl+C while the GUI process it started
  keeps running, orphaned, and the shell sees the launcher's exit code rather
  than the child's. Returning TRUE swallows the event here only: the console
  delivers it to every process attached to it, so wxmaxima.exe still gets its
  own copy and still terminates -- at which point the wait below ends
  normally and the real exit code propagates.
*/
BOOL WINAPI CtrlHandler(DWORD ctrlType) {
  switch (ctrlType) {
  case CTRL_C_EVENT:
  case CTRL_BREAK_EVENT:
    return TRUE;
  default:
    return FALSE;
  }
}

} // namespace

// Plain main(), not wmain(): the command line is read through
// GetCommandLineW() rather than argv, so there is no reason to require
// -municode from every toolchain that builds this.
int main() {
  std::wstring dir = ExeDirectory();
  if (dir.empty()) {
    std::fprintf(stderr, "wxmaxima-cli: cannot determine my own location.\n");
    return 1;
  }
  std::wstring exe = dir + L"wxmaxima.exe";

  // Refuse to launch ourselves: without this, a copy of this launcher
  // renamed to wxmaxima.exe would spawn an unbounded chain of processes.
  std::wstring self(MAX_PATH, L'\0');
  DWORD selfLen =
    GetModuleFileNameW(nullptr, self.data(), static_cast<DWORD>(self.size()));
  if ((selfLen > 0) && (selfLen < self.size())) {
    self.resize(selfLen);
    // lstrcmpiW() rather than _wcsicmp(): it comes from <windows.h>, which is
    // already included, so this does not depend on which CRT header happens
    // to declare the case-insensitive wide compare on a given toolchain.
    if (lstrcmpiW(self.c_str(), exe.c_str()) == 0) {
      std::fprintf(stderr,
                   "wxmaxima-cli: I am installed as wxmaxima.exe myself -- "
                   "refusing to launch a copy of myself.\n");
      return 1;
    }
  }

  std::wstring cmd = L"\"" + exe + L"\"";
  const wchar_t *tail = ArgumentTail();
  if (*tail != L'\0') {
    cmd += L' ';
    cmd += tail;
  }

  STARTUPINFOW si = {};
  si.cb = sizeof(si);
  // A GUI-subsystem child inherits no usable standard handles unless they are
  // handed to it explicitly. With these set, main.cpp's BindStdStreamToParent()
  // finds a valid handle on its very first GetStdHandle() call and never needs
  // its AttachConsole(ATTACH_PARENT_PROCESS) fallback -- and this works
  // unchanged whether our own stdout is a console, a pipe or a redirect to a
  // file, because whatever we were given is simply passed along.
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  SetConsoleCtrlHandler(CtrlHandler, TRUE);

  PROCESS_INFORMATION pi = {};
  // CreateProcessW() may write to the command-line buffer, so it gets a
  // writable one -- hence std::wstring::data() rather than a literal.
  if (!CreateProcessW(exe.c_str(), cmd.data(), nullptr, nullptr, TRUE, 0,
                      nullptr, nullptr, &si, &pi)) {
    DWORD err = GetLastError();
    std::fprintf(stderr, "wxmaxima-cli: cannot start %ls (error %lu).\n",
                 exe.c_str(), static_cast<unsigned long>(err));
    return 1;
  }
  CloseHandle(pi.hThread);

  WaitForSingleObject(pi.hProcess, INFINITE);
  DWORD exitCode = 1;
  GetExitCodeProcess(pi.hProcess, &exitCode);
  CloseHandle(pi.hProcess);
  return static_cast<int>(exitCode);
}
