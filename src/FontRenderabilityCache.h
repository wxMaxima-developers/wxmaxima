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
  Declares FontRenderabilityCache: which chars can each font render?

  Another step of splitting the Configuration god class: finding out whether
  a font actually renders a character (rather than drawing nothing or a
  placeholder glyph) means rasterizing it and comparing bitmaps, which is way
  too slow to do on every use - so the verdicts are cached per font, and the
  cache is persisted in the configuration storage. That probe-and-cache
  logic now lives in this class; Configuration owns one and delegates its
  existing accessors, so its dependents are unaffected.
*/

#ifndef FONTRENDERABILITYCACHE_H
#define FONTRENDERABILITYCACHE_H

#include <wx/confbase.h>
#include <wx/font.h>
#include <wx/string.h>
#include <unordered_map>

/*! Caches which characters each font can actually render.

  wxWidgets doesn't offer such a test, and no perfect one is known: missing
  glyphs can end up with size 0 (or draw nothing), as identical blank boxes,
  or as boxes containing the character's codepoint digits - depending on the
  font and the platform's renderer. What we can do instead: draw the
  character to a bitmap and see if any pixel changed (catches the
  draws-nothing case), and compare its rendering against characters that are
  guaranteed to have no real glyph (if they are identical we most probably
  got the font's missing-glyph box). The digit-box case is the remaining
  blind spot: such a box contains the probed character's own codepoint, so
  there is no reference rendering to compare it against.

  The per-platform glyph-coverage APIs wouldn't help, by the way: they
  answer "does this font contain the glyph", but what the user sees is what
  appears after the renderer's font fallback, which is what the bitmap probe
  measures.

  Both probes are costly, hence the per-font cache, which is also persisted
  in the configuration storage between runs.
*/
class FontRenderabilityCache
{
public:
  //! True if we are confident that the font renders this char.
  bool FontRendersChar(wxUniChar ch, const wxFont &font);

  //! Forgets the cached verdicts (the known font names stay as empty entries).
  void ClearValues();

  //! Reads the persisted verdicts from the configuration storage.
  void ReadFrom(wxConfigBase *config);
  //! Persists the verdicts to the configuration storage.
  void WriteTo(wxConfigBase *config) const;

private:
  //! Maps a font name to the string of chars with a cached verdict
  using RenderableCharsHash =
    std::unordered_map<wxString, wxString, wxStringHash>;

  //! True if drawing this char alters at least one pixel.
  static bool FontDisplaysChar(wxUniChar ch, const wxFont &font);
  //! True if drawing this char differs visibly from drawing otherChar.
  static bool CharVisiblyDifferent(wxChar ch, wxChar otherChar,
                                   const wxFont &font);

  //! The chars each font is known to render
  RenderableCharsHash m_renderableChars;
  //! The chars each font is known not to render
  RenderableCharsHash m_nonRenderableChars;
};

#endif // FONTRENDERABILITYCACHE_H
