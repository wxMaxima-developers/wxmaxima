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
  Declares MaximaSessionInfo: what we know about the connected Maxima.

  Another step of splitting the Configuration god class: the facts wxMaxima
  learns about the running Maxima instance (its version, architecture and
  lisp, its directories, whether it is in lisp mode and which operator names
  it knows) are session state, not settings - they are never persisted and
  become stale when Maxima restarts. They now live in this class;
  Configuration owns a MaximaSessionInfo and delegates its existing
  accessors, so its many dependents are unaffected.
*/

#ifndef MAXIMASESSIONINFO_H
#define MAXIMASESSIONINFO_H

#include <wx/string.h>
#include <wx/hashmap.h>
#include <unordered_map>

/*! What we know about the Maxima instance we are talking to.

  Pure state: this class knows nothing about the configuration store or the
  process/socket handling. Copying it (for printing, the diff viewer etc.)
  copies all of it.
*/
class MaximaSessionInfo
{
public:
  //! Constructs the session info with the built-in operator names seeded.
  MaximaSessionInfo()
    {
      // The list elements are plain string literals: a `const wxString &`
      // loop variable would bind to a temporary constructed per element,
      // which gcc's -Wrange-loop-construct rejects under -Werror.
      for (const wxChar *op : {
             wxS("("), wxS("/"), wxS("{"), wxS("-"), wxS("^"), wxS("#"),
             wxS("="), wxS(":"), wxS("["), wxS("'"), wxS("!"), wxS("+"),
             wxS("*"), wxS("or"), wxS("and"), wxS("do_in"), wxS(">"),
             wxS("$SUBVAR"), wxS("<"), wxS("if"), wxS("::="), wxS("::"),
             wxS("@"), wxS("."), wxS("-->"), wxS("^^"), wxS("not"),
             wxS("<="), wxS(":="), wxS(">="), wxS("$BFLOAT"), wxS("do")})
        m_operators[op] = 1;
      wxString operators(wxS("\u221A\u22C0\u22C1\u22BB\u22BC\u22BD\u00AC\u222b"
                             "\u2264\u2265\u2211\u2260+-*/^:=#'!()[]{}"));
      for (wxString::const_iterator it = operators.begin();
           it != operators.end(); ++it)
        m_operators[wxString(*it)] = 1;
    }

  //! Is this name an operator known to maxima?
  bool IsOperator(const wxString &name) const
    { return m_operators.find(name) != m_operators.end(); }
  //! Registers name as an operator known to maxima.
  void AddOperator(const wxString &name) { m_operators[name] = 1; }

  //! The version of the connected Maxima.
  wxString GetMaximaVersion() const { return m_maximaVersion; }
  //! Sets the version of the connected Maxima.
  void SetMaximaVersion(const wxString &version) { m_maximaVersion = version; }
  //! The processor architecture the connected Maxima runs on.
  wxString GetMaximaArch() const { return m_maximaArch; }
  //! Sets the processor architecture the connected Maxima runs on.
  void SetMaximaArch(const wxString &arch) { m_maximaArch = arch; }
  //! The version of the lisp the connected Maxima runs on.
  wxString GetLispVersion() const { return m_lispVersion; }
  //! Sets the version of the lisp the connected Maxima runs on.
  void SetLispVersion(const wxString &version) { m_lispVersion = version; }
  //! The type of the lisp the connected Maxima runs on.
  wxString GetLispType() const { return m_lispType; }
  //! Sets the type of the lisp the connected Maxima runs on.
  void SetLispType(const wxString &type) { m_lispType = type; }

  //! Maxima's working directory.
  wxString GetWorkingDirectory() const { return m_workingDirectory; }
  //! Sets maxima's working directory.
  void SetWorkingDirectory(wxString dir) { m_workingDirectory = std::move(dir); }
  //! The directory maxima's shared files (e.g. the manual) live in.
  wxString ShareDir() const { return m_shareDir; }
  //! Sets the directory maxima's shared files live in.
  void ShareDir(wxString dir) { m_shareDir = std::move(dir); }
  //! The directory maxima's demo files live in.
  wxString DemoDir() const { return m_demoDir; }
  //! Sets the directory maxima's demo files live in.
  void DemoDir(wxString dir) { m_demoDir = std::move(dir); }

  //! Is maxima currently in lisp mode (where commands need no trailing ";")?
  bool InLispMode() const { return m_inLispMode; }
  //! Tells us whether maxima currently is in lisp mode.
  void InLispMode(bool lisp) { m_inLispMode = lisp; }

private:
  //! All operator names the connected Maxima knows
  std::unordered_map<wxString, int, wxStringHash> m_operators;
  //! The version of the connected Maxima
  wxString m_maximaVersion;
  //! The processor architecture the connected Maxima runs on
  wxString m_maximaArch;
  //! The version of the lisp the connected Maxima runs on
  wxString m_lispVersion;
  //! The type of the lisp the connected Maxima runs on
  wxString m_lispType;
  //! Maxima's working directory
  wxString m_workingDirectory;
  //! The directory maxima's shared files live in
  wxString m_shareDir;
  //! The directory maxima's demo files live in
  wxString m_demoDir;
  //! Is maxima currently in lisp mode?
  bool m_inLispMode = false;
};

#endif // MAXIMASESSIONINFO_H
