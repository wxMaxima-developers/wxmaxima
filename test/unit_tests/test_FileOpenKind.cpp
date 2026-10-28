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
  Tests for FileOpenKind: which files wxMaxima agrees to open. Opening
  wxmaxima.exe and its DLLs used to start a window and a Maxima per file,
  each asked to load() an executable.
*/

#include "FileOpenKind.h"

#include <wx/ffile.h>
#include <wx/filename.h>
#include <wx/init.h>

#include <string>

// Not CATCH_CONFIG_MAIN: see test_SyntaxErrorLocation.cpp for why.
#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using wxm::ClassifyFileForOpening;
using wxm::FileLooksBinary;
using wxm::FileOpenKind;
using wxm::MaximaStringLiteral;
using wxm::WhyFileCannotBeOpened;

namespace {
//! A temporary file with the given name suffix and contents, removed again
//! when the object goes out of scope
class TempFile {
public:
  TempFile(const wxString &suffix, const std::string &contents) {
    m_name = wxFileName::CreateTempFileName(wxS("wxmopen"));
    wxRemoveFile(m_name);
    m_name += suffix;
    wxFFile out(m_name, wxS("wb"));
    out.Write(contents.data(), contents.size());
  }
  ~TempFile() { wxRemoveFile(m_name); }
  const wxString &Name() const { return m_name; }

private:
  wxString m_name;
};
} // namespace

SCENARIO("Files are classified by their extension") {
  GIVEN("worksheet files") {
    THEN("they are read into the worksheet") {
      CHECK(ClassifyFileForOpening(wxS("a.wxm")) == FileOpenKind::WxmWorksheet);
      CHECK(ClassifyFileForOpening(wxS("a.wxm~")) == FileOpenKind::WxmWorksheet);
      CHECK(ClassifyFileForOpening(wxS("A.WXMX")) == FileOpenKind::WxmxWorksheet);
      CHECK(ClassifyFileForOpening(wxS("a.wxmx~")) == FileOpenKind::WxmxWorksheet);
      CHECK(ClassifyFileForOpening(wxS("a.zip")) == FileOpenKind::WxmxWorksheet);
      CHECK(ClassifyFileForOpening(wxS("a.mac")) == FileOpenKind::MacFile);
      CHECK(ClassifyFileForOpening(wxS("a.out")) == FileOpenKind::MacFile);
      CHECK(ClassifyFileForOpening(wxS("content.xml")) == FileOpenKind::XmlWorksheet);
    }
  }
  GIVEN("a cell address after the file name") {
    THEN("it is ignored") {
      CHECK(ClassifyFileForOpening(wxS("a.wxmx#1234-abcd")) ==
            FileOpenKind::WxmxWorksheet);
    }
  }
  GIVEN("demos and packages") {
    THEN("demos are demos and Lisp and Maxima packages are packages") {
      CHECK(ClassifyFileForOpening(wxS("a.dem")) == FileOpenKind::Demo);
      CHECK(ClassifyFileForOpening(wxS("a.lisp")) == FileOpenKind::Package);
      CHECK(ClassifyFileForOpening(wxS("a.lsp")) == FileOpenKind::Package);
      CHECK(ClassifyFileForOpening(wxS("a.mc")) == FileOpenKind::Package);
    }
  }
  GIVEN("anything else") {
    THEN("it is refused") {
      CHECK(ClassifyFileForOpening(wxS("wxmaxima.exe")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("wxbase32u_gcc_custom.dll")) ==
            FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("libwx_baseu-3.2.so.0")) ==
            FileOpenKind::Unsupported);
      // Compiled Lisp is native code; GCL links any object file in.
      CHECK(ClassifyFileForOpening(wxS("compiled.o")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("compiled.fasl")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("compiled.fas")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("picture.png")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("noextension")) == FileOpenKind::Unsupported);
      CHECK(ClassifyFileForOpening(wxS("a.mac.exe")) == FileOpenKind::Unsupported);
    }
  }
}

SCENARIO("Binary contents are recognised") {
  GIVEN("plain text") {
    TempFile f(wxS(".mac"), "MZ: 3$\nf(x):=x^2;\n");
    THEN("it is text, even if it starts with a Windows executable's magic") {
      CHECK_FALSE(FileLooksBinary(f.Name()));
      CHECK(WhyFileCannotBeOpened(f.Name()).IsEmpty());
    }
  }
  GIVEN("a Windows executable renamed to .wxm") {
    TempFile f(wxS(".wxm"), std::string("MZ\x90\0\x03\0\0\0\x04\0\0\0", 12));
    THEN("it is binary and refused") {
      CHECK(FileLooksBinary(f.Name()));
      CHECK_FALSE(WhyFileCannotBeOpened(f.Name()).IsEmpty());
    }
  }
  GIVEN("the start of an ELF file without a NUL byte") {
    TempFile f(wxS(".mac"), "\x7f" "ELF");
    THEN("it is binary") { CHECK(FileLooksBinary(f.Name())); }
  }
  GIVEN("UTF-16 text with a byte order mark") {
    TempFile f(wxS(".mac"), std::string("\xff\xfe" "a\0:\0" "1\0;\0", 10));
    THEN("its NUL bytes don't make it binary") {
      CHECK_FALSE(FileLooksBinary(f.Name()));
    }
  }
  GIVEN("a .wxmx, which is a zip archive") {
    TempFile f(wxS(".wxmx"), std::string("PK\x03\x04\0\0\0\0", 8));
    THEN("being binary is fine") {
      CHECK(WhyFileCannotBeOpened(f.Name()).IsEmpty());
    }
  }
  GIVEN("a Lisp package that is binary") {
    TempFile f(wxS(".lisp"), std::string("\x7f" "ELF\x02\x01\x01\0", 8));
    THEN("it is refused") {
      CHECK_FALSE(WhyFileCannotBeOpened(f.Name()).IsEmpty());
    }
  }
  GIVEN("a file that doesn't exist") {
    THEN("it doesn't count as binary") {
      CHECK_FALSE(FileLooksBinary(wxS("/nonexistent/dir/file.mac")));
    }
  }
}

SCENARIO("File names are quoted for Maxima") {
  GIVEN("a plain file name") {
    THEN("it is just put in quotes") {
      CHECK(MaximaStringLiteral(wxS("/home/u/a.mac")) == wxS("\"/home/u/a.mac\""));
    }
  }
  GIVEN("a file name containing a double quote") {
    THEN("the quote can't end the string") {
      CHECK(MaximaStringLiteral(wxS("data.csv\"),system(\"rm x")) ==
            wxS("\"data.csv\\\"),system(\\\"rm x\""));
    }
  }
  GIVEN("a Windows path") {
    THEN("its backslashes survive") {
      CHECK(MaximaStringLiteral(wxS("C:\\data\\x.csv")) ==
            wxS("\"C:\\\\data\\\\x.csv\""));
    }
  }
  GIVEN("a backslash right before a quote") {
    THEN("both are escaped, so the quote still can't end the string") {
      CHECK(MaximaStringLiteral(wxS("a\\\"b")) == wxS("\"a\\\\\\\"b\""));
    }
  }
}

int main(int argc, char *argv[]) {
  wxInitializer init;
  return Catch::Session().run(argc, argv);
}
