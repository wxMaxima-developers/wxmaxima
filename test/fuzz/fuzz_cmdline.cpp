// Coverage-guided libFuzzer harness for wxMaxima's command-line handling
// (src/CommandLine.{h,cpp}): wxCmdLineParser with wxMaxima's option table,
// the wxmxdiff alias, Interpret() and what main.cpp then does with the file
// names (making them absolute, classifying them, quoting them for Maxima).
//
// Needs nothing but wxBase: no display, no Xvfb, and it builds in seconds,
// unlike fuzz_wxm/fuzz_mathparser, which compile the whole application.
//
// Input layout: the first byte selects how the rest is turned into a command
// line, the rest is that command line.
//   bit 0: 0 = the rest is an argument vector, arguments separated by NUL
//              bytes (what main() gets on Linux and macOS);
//          1 = the rest is one string wxCmdLineParser splits itself (what
//              WinMain() gets on Windows)
//   bit 1: the program is called "wxmxdiff" instead of "wxmaxima"
//
// Besides crashes, ASan reports and wxWidgets assertions (which this harness
// turns into aborts) it checks properties a correct parser must have; a
// violated one aborts with a message saying which.
#include "CommandLine.h"
#include "CommandLineOptions.h"
#include "FileOpenKind.h"

#include <wx/init.h>
#include <wx/log.h>
#include <wx/msgout.h>
#include <wx/string.h>

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <vector>

namespace {

//! Swallows what wxCmdLineParser prints (usage, errors)
class NullMessageOutput : public wxMessageOutput {
public:
  void Output(const wxString &) override {}
};

[[noreturn]] void Fail(const char *what) {
  fprintf(stderr, "fuzz_cmdline: property violated: %s\n", what); // flawfinder: ignore -- constant format string
  abort();
}

void Check(bool condition, const char *what) {
  if (!condition)
    Fail(what);
}

//! A failed wxASSERT/wxCHECK is a finding, not something to log and go on
void AssertHandler(const wxString &file, int line, const wxString &func,
                   const wxString &cond, const wxString &msg) {
  fprintf(stderr, "fuzz_cmdline: wx assertion failed: %s:%d %s: %s %s\n", // flawfinder: ignore -- constant format string
          static_cast<const char *>(file.utf8_str()), line,
          static_cast<const char *>(func.utf8_str()),
          static_cast<const char *>(cond.utf8_str()),
          static_cast<const char *>(msg.utf8_str()));
  abort();
}

//! The Maxima string literal MaximaStringLiteral() makes, read back
wxString UnquoteMaximaString(const wxString &literal) {
  Check(literal.length() >= 2 && literal.StartsWith(wxS("\"")) &&
            literal.EndsWith(wxS("\"")),
        "MaximaStringLiteral() result is not enclosed in quotes");
  wxString result;
  const wxString body = literal.Mid(1, literal.length() - 2);
  for (auto it = body.begin(); it != body.end(); ++it) {
    if (*it == wxS('\\')) {
      ++it;
      Check(it != body.end(), "MaximaStringLiteral() ends in a lone backslash");
    } else
      Check(*it != wxS('"'), "MaximaStringLiteral() has an unescaped quote");
    result += *it;
  }
  return result;
}

//! Parses args the way main.cpp does; returns Parse()'s result
int ParseArgs(wxCmdLineParser &parser, const std::vector<wxString> &args) {
  return wxm::cmdline::Parse(parser, args, false);
}

} // namespace

extern "C" int LLVMFuzzerInitialize(int *, char ***) {
  static wxInitializer init;
  if (!init.IsOk())
    abort();
  wxSetAssertHandler(AssertHandler);
  wxLog::EnableLogging(false);
  delete wxMessageOutput::Set(new NullMessageOutput);
  return 0;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
  if (size == 0)
    return 0;
  const uint8_t mode = data[0];
  ++data;
  --size;
  const bool asOneString = mode & 1;
  const wxString program = (mode & 2) ? wxS("/usr/bin/wxmxdiff")
                                      : wxS("/usr/bin/wxmaxima");

  std::vector<wxString> args{program};
  if (asOneString) {
    const wxString cmdLine =
      wxString::FromUTF8(reinterpret_cast<const char *>(data), size);
    // Both splitting rules, whichever platform this runs on
    for (const auto &arg : wxCmdLineParser::ConvertStringToArgs(
             cmdLine, wxCMD_LINE_SPLIT_DOS))
      args.push_back(arg);
    const wxArrayString unixArgs =
      wxCmdLineParser::ConvertStringToArgs(cmdLine, wxCMD_LINE_SPLIT_UNIX);
    (void)unixArgs;
  } else {
    const char *begin = reinterpret_cast<const char *>(data);
    const char *end = begin + size;
    const char *start = begin;
    for (const char *p = begin; p <= end; ++p)
      if (p == end || *p == '\0') {
        args.push_back(wxString::FromUTF8(start, p - start));
        start = p + 1;
      }
  }

  const std::vector<wxString> diffArgs = wxm::cmdline::DiffAliasArguments(args);
  Check(diffArgs.size() == args.size() || diffArgs.size() == args.size() + 1,
        "DiffAliasArguments() did more than insert one -d");

  wxCmdLineParser parser;
  const int result = ParseArgs(parser, diffArgs);
  const wxm::cmdline::Settings settings = wxm::cmdline::Interpret(parser);

  if (result != 0) {
    // What main.cpp prints after a failed Parse()
    (void)parser.GetUsageString();
    return 0;
  }

  // The program itself is never one of the files.
  Check(settings.params.size() + 1 <= diffArgs.size(),
        "more positional arguments than arguments");
  if (mode & 2) {
    Check(settings.diff, "wxmxdiff did not switch on --diff");
    // wxmxdiff must see the same files the same command line gives
    // "wxmaxima --diff".
    std::vector<wxString> asDiff = args;
    asDiff.front() = wxS("/usr/bin/wxmaxima");
    asDiff.insert(asDiff.begin() + 1, wxS("--diff"));
    wxCmdLineParser reference;
    if (ParseArgs(reference, asDiff) == 0) {
      const auto referenceSettings = wxm::cmdline::Interpret(reference);
      Check(referenceSettings.params == settings.params,
            "wxmxdiff sees different files than wxmaxima --diff");
    }
  }
  if (settings.diff)
    Check(settings.filesToOpen.size() <= 1,
          "--diff opens its files in windows of their own, too");
  else
    Check(settings.filesToOpen.size() >= settings.params.size(),
          "a file from the command line was lost");

  // What happens to every file name on its way to a window and a Maxima
  for (const auto &file : settings.filesToOpen) {
    (void)wxm::ClassifyFileForOpening(file);
    Check(UnquoteMaximaString(wxm::MaximaStringLiteral(file)) == file,
          "MaximaStringLiteral() does not round-trip");
  }
  if (settings.ini && settings.ini.value_or(wxString()).empty())
    Fail("--ini resolved to an empty path");
  return 0;
}
