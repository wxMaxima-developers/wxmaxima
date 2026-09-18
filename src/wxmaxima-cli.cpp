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
#include <cwchar>
#include <string>

namespace {

/*! Append one line to whatever file WXM_STDIO_DEBUG_LOG names, if anything.

  The same opt-in switch, the same log file and the same reasoning as
  main.cpp's StdioDebugLog(): entirely inert unless that variable is set,
  which only the one CI step that reproduces these failures does. A normal
  build and a normal user's run pay a single cached environment lookup.

  Deliberately raw CreateFileW()/WriteFile() rather than the CRT: this traces
  how standard handles are handed from one process to the next, so routing it
  through the very stdio layer under investigation would be self-defeating --
  and it must not write a byte to our own stdout, which belongs entirely to
  the child we launch.

  FILE_APPEND_DATA alone (no GENERIC_WRITE) makes Windows position every
  WriteFile() at end-of-file atomically, which is what lets the launcher, the
  child it starts and the other wxmaxima processes ctest runs in parallel
  share one log file without a seek-then-write race. Do not add an explicit
  SetFilePointer().
*/
void CliDebugLog(const std::wstring &msg) {
  static bool checked = false;
  static std::wstring path;
  if (!checked) {
    checked = true;
    wchar_t buf[MAX_PATH];
    DWORD len = GetEnvironmentVariableW(L"WXM_STDIO_DEBUG_LOG", buf, MAX_PATH);
    if ((len > 0) && (len < MAX_PATH))
      path.assign(buf, len);
  }
  if (path.empty())
    return;

  HANDLE h = CreateFileW(path.c_str(), FILE_APPEND_DATA,
                         FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                         OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);
  if (h == INVALID_HANDLE_VALUE)
    return;

  std::wstring line = L"[wxmaxima-cli pid " +
    std::to_wstring(GetCurrentProcessId()) + L"] " + msg + L"\r\n";
  // The log is plain text shared with main.cpp's own UTF-8/ANSI lines, so
  // narrow it rather than writing UTF-16 into the middle of the file.
  int need = WideCharToMultiByte(CP_UTF8, 0, line.c_str(),
                                 static_cast<int>(line.size()), nullptr, 0,
                                 nullptr, nullptr);
  if (need > 0) {
    std::string narrow(static_cast<size_t>(need), '\0');
    WideCharToMultiByte(CP_UTF8, 0, line.c_str(),
                        static_cast<int>(line.size()), narrow.data(), need,
                        nullptr, nullptr);
    DWORD written = 0;
    WriteFile(h, narrow.data(), static_cast<DWORD>(narrow.size()), &written,
              nullptr);
  }
  CloseHandle(h);
}

/*! "handle=... type=... inherit=..." for one of the std handles.

  inherit= is the interesting one: a handle listed in STARTUPINFO reaches the
  child only if it carries HANDLE_FLAG_INHERIT, and nothing guarantees that
  the handles our own parent gave us do (see InheritableDuplicate() below).
*/
std::wstring DescribeStdHandle(DWORD stdHandleId) {
  HANDLE h = GetStdHandle(stdHandleId);
  if ((h == nullptr) || (h == INVALID_HANDLE_VALUE))
    return L"none";
  const wchar_t *type = L"unknown";
  switch (GetFileType(h)) {
  case FILE_TYPE_PIPE:
    type = L"pipe";
    break;
  case FILE_TYPE_CHAR:
    type = L"char";
    break;
  case FILE_TYPE_DISK:
    type = L"disk";
    break;
  default:
    break;
  }
  DWORD flags = 0;
  const wchar_t *inherit = GetHandleInformation(h, &flags)
    ? ((flags & HANDLE_FLAG_INHERIT) ? L"yes" : L"no")
    : L"unqueryable";
  wchar_t buf[96];
  swprintf(buf, 96, L"handle=%p type=%ls inherit=%ls", h, type, inherit);
  return buf;
}

/*! An explicitly inheritable duplicate of a handle, or the handle itself.

  A handle named in STARTUPINFO is only actually placed in the child's handle
  table if it carries HANDLE_FLAG_INHERIT (and CreateProcess() is called with
  bInheritHandles = TRUE, which it is below). Get that wrong and the failure is
  quiet rather than loud: CreateProcess() still succeeds, and the child still
  finds the handle *values* in its PEB, so GetStdHandle() hands back
  plausible-looking numbers -- but they name nothing the child owns. Writes
  through them are then lost, or land on whatever unrelated object happens to
  occupy that slot in the child's own table, which is how a pipe the parent
  sees can look like a character device to the child.

  Nothing guarantees the handles we ourselves were given are inheritable.
  A parent that captures our output has every reason to clear the flag on its
  own copy so that no grandchild can hold its pipe open past our exit --
  wxmaxima.exe does exactly that to its own standard handles, for exactly that
  reason (see the SetHandleInformation() call in main.cpp's MyApp::OnInit()).
  So hand the child duplicates we know are inheritable rather than betting on
  the originals, the way MSDN's own redirected-child example does.

  The duplicates are not inherited further than the child (only its own copy
  matters, and it is free to clear the flag again), and the caller closes them
  as soon as CreateProcess() has returned.
*/
HANDLE InheritableDuplicate(HANDLE h) {
  if ((h == nullptr) || (h == INVALID_HANDLE_VALUE))
    return h;
  HANDLE duplicate = INVALID_HANDLE_VALUE;
  if (!DuplicateHandle(GetCurrentProcess(), h, GetCurrentProcess(), &duplicate,
                       0, TRUE, DUPLICATE_SAME_ACCESS))
    return h; // Nothing better to offer: pass the original along unchanged.
  return duplicate;
}

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
  CliDebugLog(L"start: raw GetCommandLineW()=[" +
              std::wstring(GetCommandLineW()) + L"]");
  CliDebugLog(L"start: stdin " + DescribeStdHandle(STD_INPUT_HANDLE) +
              L", stdout " + DescribeStdHandle(STD_OUTPUT_HANDLE) +
              L", stderr " + DescribeStdHandle(STD_ERROR_HANDLE));

  std::wstring dir = ExeDirectory();
  if (dir.empty()) {
    CliDebugLog(L"ExeDirectory() came back empty");
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
      CliDebugLog(L"refusing to launch myself: self=[" + self + L"]");
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

  // A GUI-subsystem child inherits no usable standard handles unless they are
  // handed to it explicitly. With these set, main.cpp's BindStdStreamToParent()
  // finds a valid handle on its very first GetStdHandle() call and never needs
  // its AttachConsole(ATTACH_PARENT_PROCESS) fallback -- and this works
  // unchanged whether our own stdout is a console, a pipe or a redirect to a
  // file, because whatever we were given is passed along. They have to be
  // inheritable duplicates rather than the originals, though; see
  // InheritableDuplicate().
  HANDLE stdIn = GetStdHandle(STD_INPUT_HANDLE);
  HANDLE stdOut = GetStdHandle(STD_OUTPUT_HANDLE);
  HANDLE stdErr = GetStdHandle(STD_ERROR_HANDLE);
  HANDLE childIn = InheritableDuplicate(stdIn);
  HANDLE childOut = InheritableDuplicate(stdOut);
  HANDLE childErr = InheritableDuplicate(stdErr);

  STARTUPINFOW si = {};
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = childIn;
  si.hStdOutput = childOut;
  si.hStdError = childErr;

  SetConsoleCtrlHandler(CtrlHandler, TRUE);

  CliDebugLog(L"argument tail=[" + std::wstring(tail) + L"]");
  CliDebugLog(L"CreateProcessW: exe=[" + exe + L"] cmd=[" + cmd + L"]");

  PROCESS_INFORMATION pi = {};
  // CreateProcessW() may write to the command-line buffer, so it gets a
  // writable one -- hence std::wstring::data() rather than a literal.
  BOOL started = CreateProcessW(exe.c_str(), cmd.data(), nullptr, nullptr, TRUE,
                                0, nullptr, nullptr, &si, &pi);
  DWORD startError = started ? 0 : GetLastError();

  // The child has its own copies now, so drop ours -- and drop them whether or
  // not the child started, since nothing else uses them. Leaving a duplicate
  // of a pipe open here would leave this process holding a second write end,
  // which is precisely what stops whoever is capturing our output from ever
  // seeing end-of-file.
  if (childIn != stdIn)
    CloseHandle(childIn);
  if (childOut != stdOut)
    CloseHandle(childOut);
  if (childErr != stdErr)
    CloseHandle(childErr);

  if (!started) {
    CliDebugLog(L"CreateProcessW failed, error " + std::to_wstring(startError));
    std::fprintf(stderr, "wxmaxima-cli: cannot start %ls (error %lu).\n",
                 exe.c_str(), static_cast<unsigned long>(startError));
    return 1;
  }
  CliDebugLog(L"CreateProcessW ok, child pid " + std::to_wstring(pi.dwProcessId));
  CloseHandle(pi.hThread);

  WaitForSingleObject(pi.hProcess, INFINITE);
  DWORD exitCode = 1;
  GetExitCodeProcess(pi.hProcess, &exitCode);
  CloseHandle(pi.hProcess);
  CliDebugLog(L"child exited with code " + std::to_wstring(exitCode));
  return static_cast<int>(exitCode);
}
