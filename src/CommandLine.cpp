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
  Turns wxMaxima's command line into settings; see CommandLine.h.
*/

#include "CommandLine.h"
#include "CommandLineOptions.h"

#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/msgout.h>

#include <algorithm>
#include <cctype>
#include <string>

namespace wxm::cmdline {

const wxCmdLineEntryDesc *ParserDescription() {
  // Every string in the Options table is a literal, so its data() is
  // NUL-terminated, which is what wxCmdLineEntryDesc's const char * members
  // need.
  static const std::vector<wxCmdLineEntryDesc> desc = [] {
    std::vector<wxCmdLineEntryDesc> entries;
    for (const auto &opt : Options) {
      const bool takesValue = opt.kind == Kind::Option;
      entries.push_back({takesValue ? wxCMD_LINE_OPTION : wxCMD_LINE_SWITCH,
                         opt.shortName.data(), opt.longName.data(),
                         opt.description.data(),
                         takesValue ? wxCMD_LINE_VAL_STRING : wxCMD_LINE_VAL_NONE,
                         opt.isHelp ? wxCMD_LINE_OPTION_HELP : 0});
    }
    entries.push_back({wxCMD_LINE_PARAM, nullptr, nullptr,
                       ParamDescription.data(), wxCMD_LINE_VAL_STRING,
                       wxCMD_LINE_PARAM_OPTIONAL | wxCMD_LINE_PARAM_MULTIPLE});
    entries.push_back({wxCMD_LINE_NONE, nullptr, nullptr, nullptr,
                       wxCMD_LINE_VAL_NONE, 0});
    return entries;
  }();
  return desc.data();
}

std::vector<wxString> DiffAliasArguments(const std::vector<wxString> &args) {
  if (args.empty() ||
      !wxFileName(args.front()).GetName().IsSameAs(wxS("wxmxdiff"), false))
    return args;
  // Everything after "--" is a file name, even if it looks like an option.
  const auto optionsEnd = std::find(args.begin() + 1, args.end(), wxS("--"));
  const bool hasDiff = std::any_of(args.begin() + 1, optionsEnd,
                                   [](const wxString &arg) {
                                     return arg == wxS("-d") ||
                                            arg == wxS("--diff");
                                   });
  if (hasDiff)
    return args;
  std::vector<wxString> withDiff = args;
  withDiff.insert(withDiff.begin() + 1, wxS("-d"));
  return withDiff;
}

void SetArguments(wxCmdLineParser &parser, const std::vector<wxString> &args) {
  // wxCmdLineParser wants a C-style argv. Our own copies of the strings keep
  // the pointers valid until it has copied them.
  std::vector<std::wstring> storage;
  storage.reserve(args.size());
  for (const auto &arg : args)
    storage.push_back(arg.ToStdWstring());
  std::vector<wchar_t *> argv;
  argv.reserve(storage.size() + 1);
  for (auto &arg : storage)
    argv.push_back(arg.data());
  argv.push_back(nullptr);
  parser.SetCmdLine(static_cast<int>(storage.size()), argv.data());
}

namespace {
/*! True if wxCmdLineParser would find an empty name in this argument if it
  were an option

  wxCmdLineParser takes a long option's name to be the letters, digits and
  "_?-" after the "--". Non-ASCII characters count as letters depending on
  the locale, so they are treated as suspicious, too: that costs nothing but
  a probe parse.
*/
bool MayHaveEmptyLongOptionName(const wxString &arg) {
  if (arg.length() < 3 || !arg.StartsWith(wxS("--")))
    return false;
  const wxUniChar c = arg[2];
  if (!c.IsAscii())
    return true;
  const char ascii = static_cast<char>(c.GetValue());
  return !(std::isalnum(static_cast<unsigned char>(ascii)) || ascii == '_' ||
           ascii == '?' || ascii == '-');
}

//! The option name an argument is replaced by in the probe parse
const wxString probeName = wxS("wxm-probe-for-argument-");

//! Collects what wxCmdLineParser prints while it is in place
class CapturingMessageOutput : public wxMessageOutput {
public:
  CapturingMessageOutput() : m_previous(wxMessageOutput::Set(this)) {}
  ~CapturingMessageOutput() override { wxMessageOutput::Set(m_previous); }
  CapturingMessageOutput(const CapturingMessageOutput &) = delete;
  CapturingMessageOutput &operator=(const CapturingMessageOutput &) = delete;
  void Output(const wxString &str) override { m_text += str; }
  const wxString &Text() const { return m_text; }

private:
  wxMessageOutput *m_previous;
  wxString m_text;
};
} // namespace

int Parse(wxCmdLineParser &parser, const std::vector<wxString> &args,
          bool showUsage) {
  parser.SetDesc(ParserDescription());

  std::vector<wxString> probeArgs = args;
  bool needsProbe = false;
  for (size_t i = 1; i < probeArgs.size(); ++i)
    if (MayHaveEmptyLongOptionName(probeArgs[i])) {
      probeArgs[i] = wxS("--") + probeName + wxString::Format(wxS("%zu"), i);
      needsProbe = true;
    }

  if (needsProbe) {
    wxString complaint;
    {
      CapturingMessageOutput capture;
      wxCmdLineParser probe;
      probe.SetDesc(ParserDescription());
      SetArguments(probe, probeArgs);
      probe.Parse(false);
      complaint = capture.Text();
    }
    // Parsing stops at the first error, so at most one of our names can be
    // complained about, and that one is where the real parse would assert.
    const int pos = complaint.Find(probeName);
    if (pos != wxNOT_FOUND) {
      unsigned long index = 0;
      complaint.Mid(pos + probeName.length()).ToULong(&index);
      if (index > 0 && index < args.size()) {
        wxString usage;
        if (showUsage)
          usage = parser.GetUsageString();
        // wxWidgets' own message, so its translations apply.
        const wxString error = wxString::Format(
          wxGetTranslation(wxS("Unknown long option '%s'")),
          args[index].Mid(2));
        if (wxMessageOutput *out = wxMessageOutput::Get())
          out->Printf(wxS("%s%s\n"), usage, error);
        return 1;
      }
    }
  }

  SetArguments(parser, args);
  return parser.Parse(showUsage);
}

wxString CanonicalFileArgument(const wxString &arg) {
  wxString realFile = arg;
  wxString uuid;
  if (arg.Contains(wxS("#"))) {
    realFile = arg.BeforeFirst(wxS('#'));
    uuid = arg.AfterFirst(wxS('#'));
  }
  wxFileName fileName(realFile);
  fileName.MakeAbsolute();
  wxString canonical = fileName.GetFullPath();
  if (!uuid.IsEmpty())
    canonical += wxS("#") + uuid;
  return canonical;
}

Settings Interpret(const wxCmdLineParser &parser) {
  Settings settings;
  settings.version = parser.Found(wxS("v"));
  settings.batch = parser.Found(wxS("b"));
  settings.eval = parser.Found(wxS("e"));
  settings.diff = parser.Found(wxS("d"));
  settings.singleProcess = parser.Found(wxS("single_process"));
  settings.singleThread = parser.Found(wxS("single_thread"));
  settings.logToStderr = parser.Found(wxS("logtostderr"));
  settings.debug = parser.Found(wxS("debug"));
  settings.pipe = parser.Found(wxS("pipe"));
  settings.exitOnError = parser.Found(wxS("exit-on-error"));
  settings.enableIpc = parser.Found(wxS("enableipc"));

  wxString value;
  if (parser.Found(wxS("f"), &value)) {
    wxFileName configFile(value);
    configFile.MakeAbsolute();
    settings.ini = configFile.GetFullPath();
  }
  if (parser.Found(wxS("wxmathml-lisp"), &value))
    settings.wxMathMLLisp = value;
  if (parser.Found(wxS("m"), &value))
    settings.maxima = value;

  // Appended to Maxima's command line as they are: --extra-args exists to
  // pass several arguments at once.
  if (parser.Found(wxS("l"), &value))
    settings.extraMaximaArgs += wxS(" -l ") + value;
  if (parser.Found(wxS("X"), &value))
    settings.extraMaximaArgs += wxS(" -X ") + value;
  if (parser.Found(wxS("u"), &value))
    settings.extraMaximaArgs += wxS(" -u ") + value;

  for (size_t i = 0; i < parser.GetParamCount(); ++i)
    settings.params.push_back(parser.GetParam(i));

  if (parser.Found(wxS("o"), &value))
    settings.filesToOpen.push_back(CanonicalFileArgument(value));
  if (!settings.diff)
    for (const auto &param : settings.params)
      settings.filesToOpen.push_back(CanonicalFileArgument(param));
  return settings;
}

} // namespace wxm::cmdline
