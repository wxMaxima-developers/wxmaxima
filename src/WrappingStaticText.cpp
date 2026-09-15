// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2020      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file contains code to create a wxPanel containing image data.
*/

#include "WrappingStaticText.h"
#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <algorithm>

namespace {
//! Never wrap narrower than this, however little room we are given -- below
//! a few words per line the text stops being readable at all, and a
//! near-zero wrap width makes the control absurdly tall instead.
constexpr int MIN_WRAP_WIDTH = 50;
//! The line length we assume before anyone has told us how wide we are.
//!
//! Only ever a starting point: the first size event re-wraps to the width
//! actually available. But it has to be a *plausible* one, because a parent
//! that sizes itself to fit its contents (wxSizer::Fit(), as WizardHelp
//! uses) asks before that ever happens, and would otherwise lay itself out
//! around either a single enormously long line or a column a few characters
//! wide.
constexpr int DEFAULT_WRAP_WIDTH = 400;
} // namespace

WrappingStaticText::WrappingStaticText(wxWindow *parent, int id, wxString text)
  : wxPanel(parent, wxID_ANY), m_label(text) {
  m_textCtrl = new wxStaticText(this, id, text);
  wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);
  sizer->Add(m_textCtrl, wxSizerFlags(1).Expand());
  SetSizer(sizer);
  // Establish a sane best size before anything asks for one -- see
  // DEFAULT_WRAP_WIDTH. Deliberately before the size handler is bound: this
  // one is not a reaction to a size we were given.
  RewrapTo(DEFAULT_WRAP_WIDTH * GetContentScaleFactor());
  Bind(wxEVT_SIZE, &WrappingStaticText::OnSize, this);
}

void WrappingStaticText::RewrapTo(int width) {
  width = std::max(width, static_cast<int>(MIN_WRAP_WIDTH * GetContentScaleFactor()));
  // The loop-breaker. Wrapping changes our best size; the enclosing sizer
  // reacts by giving us a different size; that arrives as a size event and
  // brings us back here. Without this the two never agree: measured on
  // wxWidgets 3.3.4/GTK4, a paragraph oscillated through ten layout passes
  // (890 px tall, then 91, then 19, ...) and came to rest fully unwrapped --
  // a single 1883 px line inside a 592 px panel, i.e. invisible.
  if (width == m_wrappedAt)
    return;
  m_wrappedAt = width;

  // wxStaticText::Wrap() edits the label it is given rather than remembering
  // the original, so wrapping an already-wrapped label keeps its existing
  // line breaks. Restore the text we were constructed with first, or a
  // paragraph can only ever get narrower.
  m_textCtrl->SetLabel(m_label);
  m_textCtrl->Wrap(width);

  // Measure the wrapped text ourselves and become that size, rather than
  // letting the enclosing sizer ask wxStaticText how big it wants to be.
  //
  // It cannot usefully answer. wxGTK's wxStaticText::DoGetBestSize()
  // deliberately reports the size the text would have if it were NOT
  // wrapped ("GetBestSize is supposed to return unwrapped size", see
  // src/gtk/stattext.cpp) -- one enormously long line. A GtkLabel that is
  // wrapping reports the opposite extreme as its minimum: the width of its
  // longest single word. Neither is the height this paragraph needs at the
  // width it was just wrapped to, and laying out on either of them is what
  // made these paragraphs come out as one clipped line or as a column a few
  // characters wide.
  int height = 0;
  wxStringTokenizer lines(m_textCtrl->GetLabel(), wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (lines.HasMoreTokens()) {
    // A blank line still occupies a line: measuring it gives height 0.
    height += std::max(m_textCtrl->GetTextExtent(lines.GetNextToken()).GetHeight(),
                       m_textCtrl->GetCharHeight());
  }

  // The width matters as much as the height here: without it a call site
  // that doesn't stretch us (wxSizerFlags() with no Expand(), which two of
  // them use) would lay us out at the longest-word width described above.
  SetMinSize(wxSize(width, height));
  InvalidateBestSize();
}

void WrappingStaticText::SetLabel(wxString const &value) {
  m_label = value;
  // Force a re-wrap even at an unchanged width: it is the *text* that
  // changed, so the height that goes with this width has changed too.
  m_wrappedAt = -1;
  RewrapTo(GetSize().GetWidth());
  if (GetParent())
    GetParent()->Layout();
}

void WrappingStaticText::OnSize(wxSizeEvent &event) {
  const int heightBefore = GetBestSize().GetHeight();
  RewrapTo(event.GetSize().GetWidth());
  // Only when the height we need actually changed: this asks our parent to
  // lay out again, and doing that unconditionally is the other half of the
  // loop described in RewrapTo().
  if ((GetBestSize().GetHeight() != heightBefore) && GetParent())
    GetParent()->Layout();
  event.Skip();
}
