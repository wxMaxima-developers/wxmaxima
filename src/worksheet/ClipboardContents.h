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

#ifndef CLIPBOARDCONTENTS_H
#define CLIPBOARDCONTENTS_H

/*! \file
  Clipboard formats that are only rendered once a program asks for them (GH #2030).

  A copy used to render every format it offered -- RTF, MathML, a bitmap, an
  SVG, ... -- before it put anything on the clipboard, so a big selection cost
  seconds and hundreds of megabytes for formats nobody might ever paste. The
  config dialogue therefore made the user pick which formats to offer at all.

  The clipboard does not need the data up front: it only needs to know which
  formats are available, and asks the owner for the actual bytes when a
  program pastes in one of them (GTK answers a SelectionRequest, Windows'
  OLE clipboard calls IDataObject::GetData()). wxWidgets forwards both to
  wxDataObject::GetDataSize()/GetDataHere(), so a data object that renders in
  there is all delayed rendering needs. (macOS' wxClipboard writes every format
  to the pasteboard as soon as it is set, so there everything still renders
  at copy time.)

  The pieces:
  - ClipboardSnapshot: a private copy of the copied cells, so that what is
    pasted is what was copied, whatever happens to the worksheet afterwards.
  - LazyValue: renders one payload on first use and keeps the result, so a
    format offered under several names (RTF has three) renders only once.
  - ClipboardContents: everything one copy offers.
  - LazyDataObject, LazyBitmapDataObject, LazyEnhMetaFileDataObject: the
    wxDataObjects that render on request.

  The catch is that a program that has exited can no longer answer requests:
  see Worksheet::RenderClipboardContents(), which replaces the lazy data by
  the formats worth keeping when the worksheet that owns them goes away.
*/

#include "precomp.h"
#include <wx/bitmap.h>
#include <wx/dataobj.h>
#include <wx/string.h>
#if wxUSE_ENH_METAFILE
#include <wx/metafile.h>
#endif
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <utility>

class Cell;
class GroupCell;
class Configuration;

/*! A value that is computed the first time it is asked for.

  An empty renderer means "not offered"; see IsOffered(). The renderer is
  dropped once it has run, so whatever it captured (typically the shared
  ClipboardSnapshot) is freed as soon as nothing needs it any more.
*/
template <class T> class LazyValue
{
public:
  LazyValue() = default;
  explicit LazyValue(std::function<T()> renderer) : m_renderer(std::move(renderer)) {}

  //! Is there a value, or a way to compute one?
  bool IsOffered() const { return m_value.has_value() || static_cast<bool>(m_renderer); }
  //! Has the value been computed yet?
  bool IsRendered() const { return m_value.has_value(); }

  //! The value, computed now if this is the first request
  const T &Get() const {
    if (!m_value) {
      if (m_renderer)
        m_value = m_renderer();
      else
        m_value = T();
      m_renderer = nullptr;
    }
    return *m_value;
  }

private:
  mutable std::function<T()> m_renderer;
  mutable std::optional<T> m_value;
};

/*! A private copy of the cells a copy was made from.

  The renderers run whenever another program pastes, which may be long after
  the copy. By then the user may have edited, re-evaluated or deleted what
  was copied, so they work on this copy instead of the worksheet. The copy
  hangs off a GroupCell of its own for the same reason: Cell::Copy() would
  otherwise leave every copied cell pointing at the worksheet's GroupCell.
*/
class ClipboardSnapshot
{
public:
  /*! \param configuration The configuration the copied cells are drawn with.
      It must outlive the snapshot -- see Worksheet::RenderClipboardContents().
      \param cells The list of cells to copy. Copied, not taken over.
  */
  ClipboardSnapshot(Configuration *configuration, const Cell *cells);
  ~ClipboardSnapshot();

  //! The snapshot's cells. Renderers that don't consume their input use this.
  const Cell *GetCells() const { return m_cells.get(); }
  //! A fresh copy of the cells, for renderers that take ownership of theirs
  std::unique_ptr<Cell> CopyCells() const;
  //! The configuration, in the form BitmapOut, Svgout and Emfout want it
  Configuration *const *GetConfigurationPointer() const { return &m_configuration; }
  Configuration *GetConfiguration() const { return m_configuration; }

private:
  Configuration *m_configuration;
  //! The group the copied cells belong to. Declared before m_cells: it has to
  //! outlive them.
  std::unique_ptr<GroupCell> m_group;
  std::unique_ptr<Cell> m_cells;
};

/*! Everything one copy operation puts on the clipboard.

  The text and .wxm flavours are cheap, needed by nearly every paste and
  needed again by Worksheet::RenderClipboardContents(), so they are computed
  right away. Every other format is a LazyValue; one that is not offered
  stays empty.

  The data objects share ownership of this, so it lives exactly as long as
  some data object built from it is still on the clipboard: a
  std::weak_ptr to it tells whether that is the case.
*/
struct ClipboardContents
{
  //! The selection as .wxm batch code
  wxString wxm;
  //! The selection as plain text
  wxString text;
  //! A MathML document, UTF-8 encoded
  LazyValue<std::string> mathML;
  //! An RTF document with OMML maths, UTF-8 encoded
  LazyValue<std::string> rtf;
  //! A SVG image
  LazyValue<std::string> svg;
  //! A bitmap image. Invalid if the image would exceed the size limit.
  LazyValue<wxBitmap> bitmap;
#if wxUSE_ENH_METAFILE
  //! A Windows enhanced metafile
  LazyValue<wxEnhMetaFile> emf;
#endif
};

/*! A clipboard format whose bytes are rendered when a program asks for them.

  Several of these may share one LazyValue: the same RTF is offered under
  three format names, but rendered only once.
*/
class LazyDataObject final : public wxDataObjectSimple
{
public:
  LazyDataObject(const wxDataFormat &format,
                 std::shared_ptr<const LazyValue<std::string>> data);

  size_t GetDataSize() const override;
  bool GetDataHere(void *buf) const override;
  //! Pasting into a LazyDataObject is not supported
  bool SetData(size_t len, const void *buf) override;

  // Don't hide the base class' per-format overloads, which forward to the
  // ones above.
  using wxDataObjectSimple::GetDataSize;
  using wxDataObjectSimple::GetDataHere;
  using wxDataObjectSimple::SetData;

private:
  std::shared_ptr<const LazyValue<std::string>> m_data;
};

/*! A bitmap that is only drawn when a program asks for it.

  wxBitmapDataObject is platform-specific (PNG on GTK, a HBITMAP or DIB on
  Windows), so rather than reimplementing any of that this hands the
  rendered bitmap to the base class the first time any of its data is asked
  for, and lets it do the rest.
*/
class LazyBitmapDataObject final : public wxBitmapDataObject
{
public:
  explicit LazyBitmapDataObject(std::shared_ptr<const LazyValue<wxBitmap>> bitmap);

  wxBitmap GetBitmap() const override;
  size_t GetDataSize() const override;
  bool GetDataHere(void *buf) const override;
  size_t GetDataSize(const wxDataFormat &format) const override;
  bool GetDataHere(const wxDataFormat &format, void *buf) const override;

private:
  //! Hands the bitmap to the base class. Returns false if there is none.
  bool Render() const;
  std::shared_ptr<const LazyValue<wxBitmap>> m_bitmap;
  mutable bool m_rendered = false;
};

/*! The same bitmap, as PNG bytes, for ports that can only hand over bytes.

  wxQt turns a bitmap into clipboard image data in
  wxBitmapDataObject::QtAddDataTo(), which only runs when the bitmap is the
  whole clipboard contents. Inside a composite, wxDataObject::QtAddDataTo()
  asks every format for its bytes instead, and a bitmap has none to give, so
  the image reaches no other program at all. PNG bytes are handed over
  unchanged, and every image editor reads them.
*/
class LazyPngDataObject final : public wxDataObjectSimple
{
public:
  explicit LazyPngDataObject(std::shared_ptr<const LazyValue<wxBitmap>> bitmap);

  //! The format this is offered under: image/png
  static wxDataFormat GetDataFormat();

  size_t GetDataSize() const override;
  bool GetDataHere(void *buf) const override;
  //! Pasting into a LazyPngDataObject is not supported
  bool SetData(size_t len, const void *buf) override;

  // Don't hide the base class' per-format overloads, which forward to the
  // ones above.
  using wxDataObjectSimple::GetDataSize;
  using wxDataObjectSimple::GetDataHere;
  using wxDataObjectSimple::SetData;

private:
  //! The encoded bytes, encoded on the first request. Empty if there is no
  //! bitmap to encode.
  const std::string &Png() const;
  std::shared_ptr<const LazyValue<wxBitmap>> m_bitmap;
  mutable std::optional<std::string> m_png;
};

#if wxUSE_ENH_METAFILE
//! A Windows enhanced metafile that is only drawn when a program asks for it.
class LazyEnhMetaFileDataObject final : public wxEnhMetaFileDataObject
{
public:
  explicit LazyEnhMetaFileDataObject(
    std::shared_ptr<const LazyValue<wxEnhMetaFile>> metafile);

  wxEnhMetaFile GetMetafile() const override;
  size_t GetDataSize(const wxDataFormat &format) const override;
  bool GetDataHere(const wxDataFormat &format, void *buf) const override;

private:
  //! Hands the metafile to the base class. Returns false if there is none.
  bool Render() const;
  std::shared_ptr<const LazyValue<wxEnhMetaFile>> m_metafile;
  mutable bool m_rendered = false;
};
#endif

#endif // CLIPBOARDCONTENTS_H
