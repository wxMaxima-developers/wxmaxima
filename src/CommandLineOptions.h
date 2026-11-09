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
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  The one definition of wxMaxima's command-line options.

  Everything that describes the command line is made from this table:
  - the wxCmdLineParser description main.cpp parses with (and whose usage
    text is what --help prints),
  - the bash, zsh and fish completion scripts and
  - the OPTIONS section of the English man page.
  The last three are written by wxmaxima-genopts (src/genopts.cpp) at build
  time. That is why this header uses nothing but plain C++: the generator
  must build and run without wxWidgets and without a display.

  The documents that cannot be generated -- the German man page and the
  manual's "Command-line arguments" chapter, which po4a translates -- are
  checked against this table by the ctest command_line_options_documented,
  so an option missing from them fails CI instead of going unnoticed.
*/
#ifndef WXM_COMMANDLINEOPTIONS_H
#define WXM_COMMANDLINEOPTIONS_H

#include <array>
#include <string_view>

namespace wxm::cmdline {

//! Whether an entry is a bare switch or takes a value
enum class Kind { Switch, Option };

//! What a shell should offer to complete the value of an option with
enum class Complete {
  Nothing,    //!< Free text: offer nothing
  Openable,   //!< A file wxMaxima opens (see OpenableExtensions)
  AnyFile     //!< Any file (or program: a path, too)
};

//! One command-line option
struct Option {
  Kind kind;
  //! One-letter name, or empty if there is none
  std::string_view shortName;
  //! Long name, used without its leading "--"
  std::string_view longName;
  //! The text --help, the man page and the completions show (not translated)
  std::string_view description;
  //! For an Option: what the value is called in the man page
  std::string_view valueName = {};
  //! For an Option: what the shell completes its value with
  Complete complete = Complete::Nothing;
  //! True for the entry wxCmdLineParser treats as "show the help"
  bool isHelp = false;
};

//! Every option wxMaxima understands, in the order --help lists them
inline constexpr std::array Options{
  Option{Kind::Switch, "v", "version", "Output the version info."},
  Option{Kind::Switch, "h", "help", "Show this help message.", {},
         Complete::Nothing, true},
  Option{Kind::Option, "o", "open", "Open a file.", "file",
         Complete::Openable},
  Option{Kind::Switch, "e", "eval", "Evaluate the file after opening it."},
  Option{Kind::Switch, "d", "diff",
         "Compare 2 or 3 files side-by-side using the Diff viewer."},
  Option{Kind::Switch, "", "single_process",
         "Open all worksheets from within the same process."},
  Option{Kind::Switch, "", "single_thread",
         "Run all background tasks from within the main thread."},
  Option{Kind::Switch, "b", "batch",
         "Run the file and exit afterwards. Halts on questions and stops on "
         "errors."},
  Option{Kind::Switch, "", "logtostderr",
         "Log all \"debug messages\" sidebar messages to stderr, too."},
  Option{Kind::Switch, "", "debug", "Enable costly debug checks."},
  Option{Kind::Switch, "", "pipe", "Pipe messages from Maxima to stderr."},
  Option{Kind::Switch, "", "exit-on-error",
         "Close the program on any Maxima error."},
  Option{Kind::Option, "f", "ini",
         "Allows specifying a file to store the configuration in.", "file",
         Complete::AnyFile},
  Option{Kind::Option, "u", "use-version", "Use Maxima version <str>.",
         "str"},
  Option{Kind::Option, "l", "lisp",
         "Use a Maxima compiled with lisp compiler <str>.", "str"},
  Option{Kind::Option, "X", "extra-args",
         "Allows specifying extra Maxima arguments.", "args"},
  Option{Kind::Option, "m", "maxima",
         "Allows specifying the location of the Maxima binary.", "file",
         Complete::AnyFile},
  Option{Kind::Switch, "", "enableipc",
         "Lets Maxima control wxMaxima via interprocess communications. Use "
         "this option with care."},
  Option{Kind::Option, "", "wxmathml-lisp",
         "Location of wxMathML.lisp (if not the built-in should be used, "
         "mainly for developers).",
         "file", Complete::AnyFile},
};

//! The description of the positional arguments (the files to open)
inline constexpr std::string_view ParamDescription = "input file";

/*! The extensions (without the dot, lower case) of the files wxMaxima opens

  This is what the shells offer for the positional arguments and for --open.
  It mirrors ClassifyFileForOpening() (FileOpenKind.cpp); test_FileOpenKind
  checks that each of them is accepted there. Backup files (".wxm~") are
  deliberately left out: nobody wants them offered by Tab.
*/
inline constexpr std::array<std::string_view, 10> OpenableExtensions{
  "wxm", "wxmx", "zip", "mac", "out", "xml", "dem", "lisp", "lsp", "mc"};

} // namespace wxm::cmdline

#endif // WXM_COMMANDLINEOPTIONS_H
