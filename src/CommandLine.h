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
  Turns wxMaxima's command line into settings, without needing a GUI.

  main.cpp used to read each option straight out of its wxCmdLineParser in
  the middle of starting the application, which made the parsing impossible
  to test or fuzz on its own. Everything between "here are the arguments"
  and "here is what the user asked for" now lives here and needs nothing but
  wxBase, which is what test_CommandLine and the libFuzzer target
  fuzz_cmdline (test/fuzz/) build it with. main.cpp only acts on the result.

  The options themselves are defined in CommandLineOptions.h.
*/

#ifndef WXM_COMMANDLINE_H
#define WXM_COMMANDLINE_H

#include <wx/cmdline.h>
#include <wx/string.h>

#include <optional>
#include <vector>

namespace wxm::cmdline {

/*! The wxCmdLineParser description, made from wxm::cmdline::Options

  Points to a static, wxCMD_LINE_NONE-terminated array that lives as long as
  the program.
*/
const wxCmdLineEntryDesc *ParserDescription();

/*! The arguments wxMaxima acts on, if it was started under this name

  The first entry is the program itself. If that is called wxmxdiff and no
  --diff/-d was given, a -d is inserted right behind it, so "wxmxdiff a b"
  compares a and b. Otherwise the arguments are returned unchanged. A "-d"
  after "--" is a file name, not the switch, and so is one that is the
  value of an option ("--ini -d").

  This used to re-join the arguments into one string and have
  wxCmdLineParser split that again. That string's first word was the
  program, which wxCmdLineParser takes as a parameter (it supplies the
  program name itself), so wxmxdiff compared the wxMaxima executable with
  the files; and an argument containing a quote was mangled on the way.
  Passing the vector through keeps every argument exactly as it was.
*/
std::vector<wxString> DiffAliasArguments(const std::vector<wxString> &args);

//! Hands an argument vector (program first) to the parser, unmodified
void SetArguments(wxCmdLineParser &parser, const std::vector<wxString> &args);

/*! Parses the arguments (program first) with wxMaxima's options

  \param parser A parser that has no description yet: this sets it.
  \param args The arguments, the program name first.
  \param showUsage Print the usage text along with an error message.
  \return What wxCmdLineParser::Parse() returns: 0 on success, -1 if help
  was requested, a positive number on an error, which has been printed.

  The same as SetDesc(), SetArguments() and Parse(), except for one argument
  wxCmdLineParser can't cope with: an option starting with "--" and a
  character that can't be part of an option name, such as "--+" or "--=x".
  wxWidgets (3.2 up to at least 3.3) looks up the option's name, finds it
  empty, and asks the empty name for its last character -- a failed
  assertion, followed by a read in front of the string. Found by
  fuzz_cmdline. Such an argument is harmless where it is a value (after
  "--", or after an option that takes one), so whether it is in the place
  of an option is asked of wxCmdLineParser itself: the arguments are parsed
  once with it replaced by an option name of our own, and if the parser
  complains about that name, the argument is reported as an unknown option
  instead of being handed to the real parse.
*/
int Parse(wxCmdLineParser &parser, const std::vector<wxString> &args,
          bool showUsage = true);

/*! A file name from the command line, made absolute

  A "#uuid" suffix, which addresses a cell inside a worksheet, is split off
  before the path is made absolute and appended again afterwards.
*/
wxString CanonicalFileArgument(const wxString &arg);

//! What the user asked for on the command line
struct Settings {
  bool version = false;       //!< --version
  bool batch = false;         //!< --batch
  bool eval = false;          //!< --eval
  bool diff = false;          //!< --diff
  bool singleProcess = false; //!< --single_process
  bool singleThread = false;  //!< --single_thread
  bool logToStderr = false;   //!< --logtostderr
  bool debug = false;         //!< --debug
  bool pipe = false;          //!< --pipe
  bool exitOnError = false;   //!< --exit-on-error
  bool enableIpc = false;     //!< --enableipc
  //! --ini: the configuration file, made absolute
  std::optional<wxString> ini;
  //! --wxmathml-lisp
  std::optional<wxString> wxMathMLLisp;
  //! --maxima: the Maxima binary
  std::optional<wxString> maxima;
  //! --screenshot: the PNG file to write, made absolute
  std::optional<wxString> screenshot;
  //! What --lisp, --extra-args and --use-version add to Maxima's command line
  wxString extraMaximaArgs;
  //! The positional arguments, as given (the files --diff compares)
  std::vector<wxString> params;
  /*! The files to open, each in a window of its own, made absolute

    --open's file first, then the positional arguments unless --diff uses
    them.
  */
  std::vector<wxString> filesToOpen;
};

/*! Reads the settings out of a parser that has already run Parse()

  Only meaningful if Parse() returned 0; on an error main.cpp still looks at
  --single_process and --single_thread before it exits, so this doesn't
  refuse to run then.
*/
Settings Interpret(const wxCmdLineParser &parser);

} // namespace wxm::cmdline

#endif // WXM_COMMANDLINE_H
