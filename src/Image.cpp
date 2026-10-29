// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class Image

  Image stores compressed images and handles scaling and uncompressing them.
*/
#include "precomp.h"
#include "cells/Cell.h"
#include "Compat.h"
#include <wx/image.h>
#include <wx/rawbmp.h>

#include <wx/fs_arc.h>
#include <wx/buffer.h>
#include <Image.h>
#include <vector>
#include <utility>
#include <wx/log.h>
#include "StringUtils.h"
#include "SvgBitmap.h"
#include "SvgSize.h"
#include <wx/mstream.h>
#include <wx/regex.h>
#include <wx/stdpaths.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/sstream.h>
#include <wx/bmpbndl.h>

Image::Image(Configuration *config) {
  m_configuration = config;
  InvalidBitmap();
}

Image::Image(Configuration *config, const wxMemoryBuffer &image, const wxString &type) {
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_compressedImage = image;
  m_extension = type;

  wxLogBuffer errorAggregator;
  if (m_compressedImage.GetDataLen() > 0) {
    if (IsSvg()) {
      // Rendered by SvgBundle() when drawn, at whatever size it is drawn
      // at; only its own size is needed here (GH #2087).
      wxMemoryInputStream memIn(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
      wxZlibInputStream gzipInput(memIn, wxZLIB_GZIP);
      wxStringOutputStream stringOut;
      stringOut.Write(gzipInput);
      m_ppi = m_configuration->GetPPI().x;
      const wxSize size =
        wxm::SvgIntrinsicSize(stringOut.GetString(), m_ppi);
      if (size.IsFullySpecified()) {
        m_originalWidth = size.x;
        m_originalHeight = size.y;
      }
      else
        InvalidBitmap(_("Could not parse the SVG image data."));
    } else {
      wxImage Image;
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
      if(Image.IsOk())
      {
        m_originalWidth = Image.GetWidth();
        m_originalHeight = Image.GetHeight();
      }
      else
      {
        InvalidBitmap(errorAggregator.GetBuffer());
      }
    }
  } else
    InvalidBitmap(_("Image data had zero length!"));
}

Image::Image(Configuration *config, const wxBitmap &bitmap) {
  m_configuration = config;
  LoadImage(bitmap);
}

// constructor which loads an image
Image::Image(Configuration *config, const wxString &image,
             const AttachmentSource &attachments, bool remove)
{
  m_configuration = config;
  m_ppi = m_configuration->GetPPI().x;
  LoadImage(image, attachments, remove);
}

Image::Image(Configuration *config, const Image &image) {
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_maxWidth = image.m_maxWidth;
  m_maxHeight = image.m_maxHeight;
  m_userScale = image.m_userScale;
  m_fitToWindow = image.m_fitToWindow;
  m_originalWidth = image.m_originalWidth;
  m_originalHeight = image.m_originalHeight;
  m_compressedImage = image.m_compressedImage;
  m_ppi = image.m_ppi;
  m_extension = image.m_extension;
  m_svgBundle = image.m_svgBundle;
}

Image::~Image() {
  // Make sure no worker thread is still touching this object before we tear it
  // down: request a stop (which drops the task if it has not started yet) and
  // then wait for it to actually finish.
  if(m_loadImageTask) {
    m_loadImageTask->RequestStop();
    m_loadImageTask->Wait();
  }
  if(m_loadGnuplotSourceTask) {
    m_loadGnuplotSourceTask->RequestStop();
    m_loadGnuplotSourceTask->Wait();
  }
  if (!m_gnuplotSource.IsEmpty() && IsSafePath(m_gnuplotSource)) {
    if (wxFileExists(m_gnuplotSource))
    {
      if(!wxRemoveFile(m_gnuplotSource))
        wxLogMessage(_("Failed to delete gnuplot file %s"),
                     m_gnuplotSource.utf8_str());
    }
    wxString popoutname = m_gnuplotSource + wxS(".popout");
    if (wxFileExists(popoutname))
    {
      if(!wxRemoveFile(popoutname))
        wxLogMessage(_("Failed to delete gnuplot file %s"),
                     popoutname.utf8_str());
    }
  }

  if (!m_gnuplotData.IsEmpty() && IsSafePath(m_gnuplotData)) {
    if (wxFileExists(m_gnuplotData))
    {
      if(!wxRemoveFile(m_gnuplotData))
        wxLogMessage(_("Failed to delete gnuplot data file %s"),
                     m_gnuplotData.utf8_str());
    }
  }
}

wxMemoryBuffer Image::ReadCompressedImage(const stop_token& stopToken, wxInputStream *data) {
  wxMemoryBuffer retval;
  std::vector<char> buf(8192);

  while (data->CanRead() && !stopToken.stop_requested()) {
    data->Read(buf.data(), buf.size());
    retval.AppendData(buf.data(), data->LastRead());
  }

  return retval;
}

wxBitmap Image::GetUnscaledBitmap() {
  if(m_loadImageTask)
    m_loadImageTask->Wait();

  if (m_isInvalid)
    GenerateInvalidBitmap();

  if (IsSvg()) {
    return SvgBitmapAt(wxSize(m_originalWidth, m_originalHeight));
  } else {
    wxMemoryInputStream istream(m_compressedImage.GetData(),
                                m_compressedImage.GetDataLen());
    wxImage img(istream, wxBITMAP_TYPE_ANY);
    wxBitmap bmp;
    if (img.Ok())
      bmp = wxBitmap(img);
    return bmp;
  }
}

const wxMemoryBuffer Image::GetCompressedImage() const {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  return m_compressedImage;
}

std::size_t Image::GetOriginalWidth() const {
  if(m_loadImageTask)
    m_loadImageTask->Wait();

  return m_originalWidth;
}

std::size_t Image::GetOriginalHeight() const {
  if(m_loadImageTask)
    m_loadImageTask->Wait();

  return m_originalHeight;
}

void Image::GnuplotSource(const wxString& gnuplotFilename, const wxString& dataFilename,
                          const AttachmentSource &attachments) {
  if(m_loadGnuplotSourceTask)
  {
    m_loadGnuplotSourceTask->RequestStop();
    m_loadGnuplotSourceTask->Wait();
  }
  m_gnuplotSource = gnuplotFilename;
  m_gnuplotData = dataFilename;
  // Loading/compressing the gnuplot source and data is low priority: it is the
  // image bitmap (loaded at high priority) that the worksheet layout needs.
  if(m_configuration->UseThreads())
    m_loadGnuplotSourceTask = BackgroundQueue::Add(
      BackgroundTask::Priority::Low,
      [this, gnuplotFilename, dataFilename,
       attachments](stop_token stopToken) mutable {
        LoadGnuplotSource_Backgroundtask(stopToken,
                                         gnuplotFilename, dataFilename,
                                         attachments);
      });
  else
    LoadGnuplotSource_Backgroundtask({},
      m_gnuplotSource, m_gnuplotData, attachments);
}

void Image::LoadGnuplotSource_Backgroundtask(
  const stop_token& stopToken,
  const wxString& gnuplotFile, const wxString& dataFile, const AttachmentSource& attachments)
{
  if (stopToken.stop_requested()) return;
  if(attachments.IsEmpty())
  {
    {
      wxFileInputStream source(gnuplotFile);
      LoadGnuplotSource(&source);
    }
    if (stopToken.stop_requested()) return;
    {
      wxFileInputStream data(dataFile);
      LoadGnuplotData(&data);
    }
  }
  else
  {
    {
      const wxMemoryBuffer bytes = attachments.Read(gnuplotFile);
      if (bytes.GetDataLen() > 0) {
        wxMemoryInputStream source(bytes.GetData(), bytes.GetDataLen());
        LoadGnuplotSource(&source);
      }
    }
    if (stopToken.stop_requested()) return;
    {
      const wxMemoryBuffer bytes = attachments.Read(dataFile);
      if (bytes.GetDataLen() > 0) {
        wxMemoryInputStream data(bytes.GetData(), bytes.GetDataLen());
        LoadGnuplotData(&data);
      }
    }
  }
}

void Image::CompressedGnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                                     const AttachmentSource &attachments) {
  if(m_loadGnuplotSourceTask)
  {
    m_loadGnuplotSourceTask->RequestStop();
    m_loadGnuplotSourceTask->Wait();
  }
  m_gnuplotSource = std::move(gnuplotFilename);
  m_gnuplotData = std::move(dataFilename);
  if(m_configuration->UseThreads())
    m_loadGnuplotSourceTask = BackgroundQueue::Add(
      BackgroundTask::Priority::Low,
      [this, gnuplotSource = m_gnuplotSource,
       data = m_gnuplotData, attachments](stop_token stopToken) mutable {
        LoadCompressedGnuplotSource_Backgroundtask(
          stopToken, gnuplotSource, data, attachments);
      });
  else
    LoadCompressedGnuplotSource_Backgroundtask({},
      m_gnuplotSource,
      m_gnuplotData,
      attachments);
  // Store the filenames without the ".gz".
  if(m_gnuplotSource.EndsWith(".gz"))
    m_gnuplotSource = m_gnuplotSource.Left(m_gnuplotSource.Length()-3);
  if(m_gnuplotData.EndsWith(".gz"))
    m_gnuplotData = m_gnuplotData.Left(m_gnuplotData.Length()-3);
}


void Image::LoadGnuplotSource(
  wxInputStream *source)
{
  // The gnuplot source of the image is cached in a compressed form:
  //
  // as it is text-only and contains many redundancies it will get way
  // smaller this way.
  if (source->IsOk()) {
    wxTextInputStream textIn(*source, wxS('\t'),
                             wxConvAuto(wxFONTENCODING_UTF8));

    wxMemoryOutputStream mstream;
    int zlib_flags;
    if (wxZlibOutputStream::CanHandleGZip())
      zlib_flags = wxZLIB_GZIP;
    else
      zlib_flags = wxZLIB_ZLIB;
    wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION, zlib_flags);
    wxTextOutputStream textOut(zstream);
    wxString line;

    // A RegEx that matches the name of the data file (needed if we ever
    // want to move a data file into the temp directory of a new computer
    // that locates its temp data somewhere strange).
    // TODO: Manipulates a variable that isn't used at all.
    wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[_0-9]*\\.data'");
    // The IsOk() term matters: Eof() only reports wxSTREAM_EOF, so a stream
    // stuck in a read-ERROR state never reaches Eof() and the loop would spin
    // forever, growing the output by one "\n" per iteration.
    while (source->IsOk() && !source->Eof()) {
      line = textIn.ReadLine();
      if (replaceDataFileName.Matches(line)) {
        replaceDataFileName.Replace(&line, wxS("'<DATAFILENAME>'"));
      }
      textOut << line + wxS("\n");
    }
    textOut.Flush();
    zstream.Close();

    m_gnuplotSource_Compressed.Clear();
    m_gnuplotSource_Compressed.AppendData(
      mstream.GetOutputStreamBuffer()->GetBufferStart(),
      mstream.GetOutputStreamBuffer()->GetBufferSize());
  }
}

void Image::LoadGnuplotData(
  wxInputStream *data)
{
  if (data->IsOk()) {
    wxTextInputStream textIn(*data, wxS('\t'),
                             wxConvAuto(wxFONTENCODING_UTF8));

    wxMemoryOutputStream mstream;
    int zlib_flags;
    if (wxZlibOutputStream::CanHandleGZip())
      zlib_flags = wxZLIB_GZIP;
    else
      zlib_flags = wxZLIB_ZLIB;
    wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION,
                               zlib_flags);
    wxTextOutputStream textOut(zstream);
    wxString line;

    // IsOk(): see LoadGnuplotSource - don't spin on a read error.
    while (data->IsOk() && !data->Eof()) {
      line = textIn.ReadLine();
      textOut << line + wxS("\n");
    }
    textOut.Flush();
    zstream.Close();

    m_gnuplotData_Compressed.Clear();
    m_gnuplotData_Compressed.AppendData(
      mstream.GetOutputStreamBuffer()->GetBufferStart(),
      mstream.GetOutputStreamBuffer()->GetBufferSize());
  }
}

void Image::LoadCompressedGnuplotSource_Backgroundtask(
  const stop_token& stopToken,
  const wxString& sourcefile,
  const wxString& datafile,
  const AttachmentSource& attachments
  ) {
  if (stopToken.stop_requested()) return;
  // Read the gnuplot source. A file that isn't there keeps what we had.
  {
    const wxMemoryBuffer source = attachments.Read(sourcefile);
    if (source.GetDataLen() > 0)
      m_gnuplotSource_Compressed = source;
  }
  if (stopToken.stop_requested()) return;
  // Read the gnuplot data
  m_gnuplotData_Compressed = attachments.Read(datafile);
}

const wxMemoryBuffer Image::GetGnuplotSource() {
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();

  wxMemoryBuffer retval;
  if ((m_gnuplotSource_Compressed.GetDataLen() < 2) ||
      (m_gnuplotData_Compressed.GetDataLen() < 2)) {
    return retval;
  }
  wxMemoryOutputStream output;
  wxTextOutputStream textOut(output);
  if (output.IsOk()) {
    wxMemoryInputStream mstream(m_gnuplotSource_Compressed.GetData(),
                                m_gnuplotSource_Compressed.GetDataLen());
    wxZlibInputStream zstream(mstream);
    wxTextInputStream textIn(zstream);
    wxString line;

    while (!zstream.Eof()) {
      line = textIn.ReadLine();
      textOut << line + wxS("\n");
    }
    textOut.Flush();

    retval.AppendData(output.GetOutputStreamBuffer()->GetBufferStart(),
                      output.GetOutputStreamBuffer()->GetBufferSize());
  }

  return retval;
}

const wxMemoryBuffer Image::GetCompressedGnuplotSource()
{
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  return m_gnuplotSource_Compressed;
}

const wxMemoryBuffer Image::GetCompressedGnuplotData()
{
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  return m_gnuplotData_Compressed;
}

const wxMemoryBuffer Image::GetGnuplotData() {
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  wxMemoryBuffer retval;
  if ((m_gnuplotSource_Compressed.GetDataLen() < 2) ||
      (m_gnuplotData_Compressed.GetDataLen() < 2)) {
    return retval;
  }

  wxMemoryOutputStream output;
  wxTextOutputStream textOut(output);
  if (output.IsOk()) {
    wxMemoryInputStream mstream(m_gnuplotData_Compressed.GetData(),
                                m_gnuplotData_Compressed.GetDataLen());
    wxZlibInputStream zstream(mstream);
    wxTextInputStream textIn(zstream);
    wxString line;

    while (!zstream.Eof()) {
      line = textIn.ReadLine();
      textOut << line + wxS("\n");
    }
    textOut.Flush();

    retval.AppendData(output.GetOutputStreamBuffer()->GetBufferStart(),
                      output.GetOutputStreamBuffer()->GetBufferSize());
  }
  return retval;
}

wxString Image::GnuplotData() {
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  if ((!m_gnuplotData.IsEmpty()) && (!wxFileExists(m_gnuplotData))) {
    // Move the gnuplot data and data file into our temp directory
    wxFileName gnuplotSourceFile(m_gnuplotSource);
    m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" +
      gnuplotSourceFile.GetFullName();
    wxFileName gnuplotDataFile(m_gnuplotData);
    m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" +
      gnuplotDataFile.GetFullName();

    wxFileOutputStream output(m_gnuplotData);
    wxTextOutputStream textOut(output);
    if (output.IsOk()) {
      if (m_gnuplotData_Compressed.GetDataLen() <= 1) {
        wxLogMessage(_("No gnuplot data!"));
        return wxEmptyString;
      }
      wxMemoryInputStream mstream(m_gnuplotData_Compressed.GetData(),
                                  m_gnuplotData_Compressed.GetDataLen());
      wxZlibInputStream zstream(mstream);
      wxTextInputStream textIn(zstream);
      wxString line;

      while (!zstream.Eof()) {
        line = textIn.ReadLine();
        textOut << line + wxS("\n");
      }
      textOut.Flush();
    }
  }
  return m_gnuplotData;
}

wxString Image::GnuplotSource() {
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  if ((!m_gnuplotSource.IsEmpty()) && (!wxFileExists(m_gnuplotSource))) {
    // Move the gnuplot source and data file into our temp directory
    wxFileName gnuplotSourceFile(m_gnuplotSource);
    m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" +
      gnuplotSourceFile.GetFullName();
    wxFileName gnuplotDataFile(m_gnuplotData);
    m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" +
      gnuplotDataFile.GetFullName();

    wxFileOutputStream output(m_gnuplotSource);
    wxTextOutputStream textOut(output);
    if (output.IsOk()) {
      if (m_gnuplotSource_Compressed.GetDataLen() <= 1) {
        wxLogMessage(_("No gnuplot source!"));
        return wxEmptyString;
      }
      wxMemoryInputStream mstream(m_gnuplotSource_Compressed.GetData(),
                                  m_gnuplotSource_Compressed.GetDataLen());
      wxZlibInputStream zstream(mstream);
      if (zstream.IsOk()) {
        wxTextInputStream textIn(zstream);
        wxString line;

        while (!zstream.Eof()) {
          line = textIn.ReadLine();
          line.Replace(wxS("'<DATAFILENAME>'"),
                       wxS("'") + m_gnuplotData + wxS("'"));
          textOut << line + wxS("\n");
        }
        textOut.Flush();
      }
    }
  }
  // Restore the data file, as well.
  GnuplotData();
  return m_gnuplotSource;
}

wxSize Image::ToImageFile(const wxString& filename) {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  wxFileName fn(filename);
  wxString ext = fn.GetExt();
  if (filename.Lower().EndsWith(GetExtension().Lower())) {
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxDefaultSize;

    file.Write(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    if (file.Close())
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxDefaultSize;
  }

  if ((filename.Lower().EndsWith(".svg")) && (m_extension == "svgz")) {
    // Unzip the .svgz image
    wxString svgContents_string;
    wxMemoryInputStream istream(m_compressedImage.GetData(),
                                m_compressedImage.GetDataLen());
    wxZlibInputStream zstream(istream);
    if (!zstream.IsOk())
      return wxDefaultSize;
    wxTextInputStream textIn(zstream);
    wxString line;
    while (!zstream.Eof()) {
      line = textIn.ReadLine();
      svgContents_string += line + wxS("\n");
    }
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxDefaultSize;
    wxFileOutputStream output(file);
    wxTextOutputStream text(output);
    text << svgContents_string;
    if (file.Close())
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxDefaultSize;
  } else {
    wxBitmap bitmap = GetUnscaledBitmap();
    wxImage image = bitmap.ConvertToImage();
    wxBitmapType mimetype = wxBITMAP_TYPE_ANY;
    if ((ext.Lower() == wxS("jpg")) || (ext.Lower() == wxS("jpeg")))
      mimetype = wxBITMAP_TYPE_JPEG;
    else if (ext.Lower() == wxS("png"))
      mimetype = wxBITMAP_TYPE_PNG;
    else if (ext.Lower() == wxS("pcx"))
      mimetype = wxBITMAP_TYPE_PCX;
    else if (ext.Lower() == wxS("pnm"))
      mimetype = wxBITMAP_TYPE_PNM;
    else if ((ext.Lower() == wxS("tif")) || (ext.Lower() == wxS("tiff")))
      mimetype = wxBITMAP_TYPE_TIFF;
    else if (ext.Lower() == wxS("xpm"))
      mimetype = wxBITMAP_TYPE_XPM;
    else if (ext.Lower() == wxS("ico"))
      mimetype = wxBITMAP_TYPE_ICO;
    else if (ext.Lower() == wxS("cur"))
      mimetype = wxBITMAP_TYPE_CUR;
    else
      return wxDefaultSize;

    if (!image.SaveFile(filename, mimetype))
      return wxDefaultSize;
    return image.GetSize();
  }
}

wxBitmap Image::GetBitmap(double scale) {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  // Recalculate contains its own WaitForLoad object.
  Recalculate(scale);

  if (m_isInvalid)
    GenerateInvalidBitmap();

  wxLogBuffer errorAggregator;
  // Let's see if we have cached the scaled bitmap with the right size.
  // Guard the size query: GetWidth() on an invalid bitmap asserts in wxWidgets.
  if (m_scaledBitmap.IsOk() && (m_scaledBitmap.GetWidth() == m_width))
    return m_scaledBitmap;

  // Seems like we need to create a new scaled bitmap.
  if (IsSvg()) {
    // The bundle renders from the vector data at exactly this size, so the
    // image stays sharp at any zoom factor.
    m_scaledBitmap = SvgBitmapAt(wxSize(m_width, m_height));
    if (m_scaledBitmap.IsOk())
      return m_scaledBitmap;
    InvalidBitmap(_("Could not render the SVG image."));
    Recalculate();
  } else {
    wxImage img;
    if (m_compressedImage.GetDataLen() > 0) {
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());

      img = wxImage(istream, wxBITMAP_TYPE_ANY);
    }

    if (img.Ok())
      m_scaledBitmap = wxBitmap(img);
    else
    {
      wxString errorMessage = errorAggregator.GetBuffer();
      InvalidBitmap(errorMessage);
      wxLogMessage("GetBitmap(): %s", errorMessage.mb_str());
      Recalculate();
    }
  }

  // Make sure we stay within sane defaults
  if (m_width < 1)
    m_width = 1;
  if (m_height < 1)
    m_height = 1;

  // Create a scaled bitmap and return it.
  if (m_scaledBitmap.IsOk()) {
    wxImage img = m_scaledBitmap.ConvertToImage();
    // GIF and XPM mark their transparent pixels with a mask colour rather than
    // an alpha channel. A mask only takes effect where a caller explicitly
    // asks for it (DrawBitmap(..., true), Blit(..., useMask=true)); ImgCell
    // and AnimationCell Blit() without it, so the mask colour -- usually black
    // or near-black -- used to be painted where the image is meant to be
    // transparent (GH #2227). Turning the mask into alpha here makes the
    // transparency travel with the pixels themselves, which every DC honours,
    // and lets the bicubic rescale below blend the edges instead of smearing
    // the mask colour into a dark fringe around them.
    if (img.HasMask() && !img.HasAlpha())
      img.InitAlpha();
    img.Rescale(m_width, m_height, wxIMAGE_QUALITY_BICUBIC);
    // Forcing depth=24 here (as this used to) discards any alpha channel
    // Rescale() just carried over from the source image, turning previously
    // transparent pixels into solid, opaque -- typically black, since that is
    // what most encoders leave in the RGB channels of a fully transparent
    // pixel -- ones (GH #2227). Every other wxBitmap(img, ...) call in this
    // file (GetUnscaledBitmap(), the non-rescaled path a few lines above)
    // already omits the depth argument for exactly this reason; do the same
    // here so a scaled bitmap's alpha channel survives identically to an
    // unscaled one's.
    m_scaledBitmap = wxBitmap(img);
  } else
    m_scaledBitmap = wxBitmap(1, 1);
  return m_scaledBitmap;
}

void Image::InvalidBitmap(const wxString &message) {
  m_originalWidth = m_width = 1200 * m_ppi / 96;
  m_originalHeight = m_height = 900 * m_ppi / 96;
  m_isInvalid = true;
  m_invalidBitmapMessage = message;
  // Do NOT discard m_compressedImage here. Those bytes are the only copy of the
  // original image; if the current wxWidgets build simply cannot decode this
  // format we still want ToXML()/GetCompressedImage() to write the untouched
  // bytes back out, so that opening and re-saving a worksheet on such a machine
  // does not silently turn the image into a zero-length file. Rendering is
  // unaffected: while m_isInvalid is set GetBitmap() returns the placeholder
  // produced by GenerateInvalidBitmap() and never re-attempts a decode.
}

void Image::RecordDataUnavailable(const wxString &message) {
  m_loadFailureKind = LoadFailureKind::DataUnavailable;
  InvalidBitmap(message);
}

void Image::RecordDecodeFailure(const wxString &message, bool missingCodec) {
  m_loadFailureKind =
    missingCodec ? LoadFailureKind::MissingCodec : LoadFailureKind::DecodeFailed;
  InvalidBitmap(message);
}

void Image::GenerateInvalidBitmap() {
  if (!m_isInvalid)
    return;
  m_isInvalid = false;

  // Create a "image not loaded" bitmap.
  m_scaledBitmap.Create(m_width, m_height);
  wxString fileSystemMessage;
  if(m_fromWxFS)
    fileSystemMessage = _("\nImage was loaded from a .wxmx file");
  wxString error;
  if (m_imageName != wxEmptyString)
  {
    if(m_invalidBitmapMessage.IsEmpty())
      error =
        wxString::Format(_("Error: Cannot render %s."), m_imageName.utf8_str())  + fileSystemMessage;
    else
      error =
        wxString::Format(_("Error: Cannot render %s:\n%s"), m_imageName.utf8_str(), m_invalidBitmapMessage.utf8_str()) + fileSystemMessage;
  }
  else
  {
    if(m_invalidBitmapMessage.IsEmpty())
      error = wxString::Format(_("Error: Cannot render the image.")) + fileSystemMessage;
    else
      error = wxString::Format(_("Error: Cannot render the image:\n%s"), m_invalidBitmapMessage.utf8_str()) + fileSystemMessage;
  }

  {
    wxMemoryDC dc;
    dc.SelectObject(m_scaledBitmap);
    dc.SetPen(*wxBLACK_PEN);
    dc.SetBrush(*wxWHITE_BRUSH);
    dc.Clear();
    wxCoord width = 0, height = 0;
    dc.GetTextExtent(error, &width, &height);

    dc.DrawRectangle(0, 0, m_width - 1, m_height - 1);
    dc.DrawLine(0, 0, m_width - 1, m_height - 1);
    dc.DrawLine(0, m_height - 1, m_width - 1, 0);

    dc.GetTextExtent(error, &width, &height);
    dc.DrawText(error, (m_width - width) / 2, (m_height - height) / 2);
  }
  wxMemoryOutputStream mstream;
  wxASSERT(m_scaledBitmap.IsOk());
  wxImage image = m_scaledBitmap.ConvertToImage();
  wxASSERT(image.IsOk());
  image.SaveFile(mstream, wxBITMAP_TYPE_PNG);
  m_compressedImage.Clear();
  m_compressedImage.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                               mstream.GetOutputStreamBuffer()->GetBufferSize());
}

void Image::LoadImage(const wxBitmap &bitmap) {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  // Convert the bitmap to a png image we can use as m_compressedImage
  wxImage image = bitmap.ConvertToImage();
  wxMemoryOutputStream stream;
  image.SaveFile(stream, wxBITMAP_TYPE_PNG);
  m_compressedImage.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                               stream.GetOutputStreamBuffer()->GetBufferSize());

  // Set the info about the image.
  m_extension = wxS("png");
  m_originalWidth = image.GetWidth();
  m_originalHeight = image.GetHeight();
  m_scaledBitmap.Create(1, 1);
  m_width = 1;
  m_height = 1;
}

wxString Image::GetExtension() const {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  return m_extension;
}

void Image::LoadImage(wxString image, const AttachmentSource &attachments,
                      bool remove) {
  if(m_loadImageTask)
  {
    m_loadImageTask->RequestStop();
    m_loadImageTask->Wait();
  }
  m_fromWxFS = !attachments.IsEmpty();
  m_extension = wxFileName(image).GetExt();
  m_extension = m_extension.Lower();
  m_imageName = image;
  m_compressedImage.Clear();
  m_scaledBitmap.Create(1, 1);
  // Loading the image is high priority: the worksheet layout needs its size and
  // normally wants to draw it right away. We want as many images as we have
  // worker threads to load before any lower-priority work (gnuplot sources) runs.
  if(m_configuration->UseThreads())
    m_loadImageTask = BackgroundQueue::Add(
      BackgroundTask::Priority::High,
      [this, image = std::move(image),
       attachments, remove](stop_token stopToken) mutable {
        LoadImage_Backgroundtask(stopToken, image,
                                 attachments, remove);
      });
  else
    LoadImage_Backgroundtask({},
      image, attachments,
      remove
      );
}

void Image::LoadImage_Backgroundtask(const stop_token& stopToken,
                                     const wxString& image, const AttachmentSource& attachments,
                                     bool remove) {
  wxLogBuffer errorAggregator;
  // A fresh attempt starts with a clean slate: if this Image is being
  // reloaded after a previous attempt failed (e.g. a config change), a
  // successful reload must not leave the earlier failure recorded.
  m_loadFailureKind = LoadFailureKind::None;

  if (stopToken.stop_requested()) return;

  if (!attachments.IsEmpty()) {
    m_compressedImage = attachments.Read(image);
  } else {
    wxFile file;
    // Support relative and absolute paths.
    if (wxFileExists(m_configuration->GetWorkingDirectory() + wxS("/") + image))
      file.Open(m_configuration->GetWorkingDirectory() + wxS("/") + image);
    else
      file.Open(image);

    if (file.IsOpened()) {
      std::unique_ptr<wxInputStream> strm (new wxFileInputStream(file));
      bool ok = strm->IsOk();
      if (ok)
        m_compressedImage = ReadCompressedImage(stopToken, strm.get());

      file.Close();
      if (ok && remove && IsSafePath(image)) {
        wxLogNull suppressor;
        if(!wxRemoveFile(image))
        {
          wxMilliSleep(300);
          if(!wxRemoveFile(image))
          {
            wxMilliSleep(300);
            wxRemoveFile(image);
          }
        }
      }
    }
    else
      RecordDataUnavailable(errorAggregator.GetBuffer());
  }

  if (stopToken.stop_requested()) return;

  wxImage Image;
  if (m_compressedImage.GetDataLen() > 0) {
    // A gzip-compressed SVG whose file name did not end in ".svgz" (e.g. a
    // ".svg.gz" file, whose extension is "gz") would otherwise be treated as a
    // raster image and fail to decode. Detect a gzipped SVG by its content and
    // handle it as a .svgz so it renders correctly (rather than only surviving
    // as opaque bytes) after a save/load round-trip.
    if ((m_extension != wxS("svg")) && (m_extension != wxS("svgz")) &&
        (m_compressedImage.GetDataLen() > 2)) {
      const unsigned char *bytes =
        static_cast<const unsigned char *>(m_compressedImage.GetData());
      if ((bytes[0] == 0x1f) && (bytes[1] == 0x8b)) { // gzip magic number
        wxMemoryInputStream istream(m_compressedImage.GetData(),
                                    m_compressedImage.GetDataLen());
        wxZlibInputStream zstream(istream);
        if (zstream.IsOk()) {
          char head[512] = {}; // flawfinder: ignore -- Read() below is bounded by sizeof(head)-1
          zstream.Read(head, sizeof(head) - 1);
          if (wxString::FromUTF8(head, zstream.LastRead()).Contains(wxS("<svg")))
            m_extension = wxS("svgz");
        }
      }
    }
    if ((m_extension == "svg") || (m_extension == "svgz")) {
      wxString svgContents_string;

      // Read the svg file's data into the system's memory
      if (m_extension == "svg") {
        // We can read the file from memory without much ado...
        svgContents_string =
          wxString::FromUTF8(static_cast<char *>(m_compressedImage.GetData()),
                             m_compressedImage.GetDataLen());

        // ...but we want to compress the in-memory image for saving memory
        wxMemoryOutputStream mstream;
        wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION, wxZLIB_GZIP);
        wxTextOutputStream textOut(zstream);
        textOut << svgContents_string;
        textOut.Flush();
        zstream.Close();
        m_compressedImage.Clear();
        m_compressedImage.AppendData(
          mstream.GetOutputStreamBuffer()->GetBufferStart(),
          mstream.GetOutputStreamBuffer()->GetBufferSize());
        m_extension += "z";
        m_imageName += "z";
      } else {
        // Unzip the .svgz image
        wxMemoryInputStream istream(m_compressedImage.GetData(),
                                    m_compressedImage.GetDataLen());
        wxZlibInputStream zstream(istream);
        wxTextInputStream textIn(zstream);
        wxString line;
        while (zstream.IsOk() && (!zstream.Eof())) {
          line = textIn.ReadLine();
          svgContents_string += line + wxS("\n");
        }
      }
      // Only the size is needed now; the SVG itself is parsed by SvgBundle(),
      // on the GUI thread, the first time the image is drawn.
      const wxSize size =
        wxm::SvgIntrinsicSize(svgContents_string, m_configuration->GetPPI().x);
      if (size.IsFullySpecified()) {
        m_originalWidth = size.x;
        m_originalHeight = size.y;
      } else {
        // Every SVG renderer wxWidgets can use is compiled in whenever SVG
        // support is: there is no "missing codec" case, a file without a
        // usable root element is always a real failure.
        RecordDecodeFailure(_("Could not parse the SVG image data."),
                             /* missingCodec = */ false);
      }
    } else {
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
      m_originalWidth = 700;
      m_originalHeight = 300;

      if (Image.Ok()) {
        m_originalWidth = Image.GetWidth();
        m_originalHeight = Image.GetHeight();
        if (Image.HasOption(wxS("wxIMAGE_OPTION_RESOLUTION"))) {
          int resolution;
          resolution = Image.GetOptionInt(wxS("wxIMAGE_OPTION_RESOLUTION"));
          if (Image.HasOption(wxS("wxIMAGE_OPTION_RESOLUTIONUNIT"))) {
            if (Image.GetOptionInt("wxIMAGE_OPTION_RESOLUTIONUNIT") ==
                wxIMAGE_RESOLUTION_CM)
              resolution *= 2.54;
          }
          if (resolution > 10)
            m_ppi = resolution;
        }
      } else {
        wxString errorMessage = errorAggregator.GetBuffer();
        // A null handler means this wxWidgets build has no decoder registered
        // for the format at all (e.g. WebP/TIFF support not compiled in) --
        // not our bug. A registered handler that still failed to decode the
        // actual bytes is a real failure.
        bool missingCodec =
          (wxImage::FindHandler(m_extension, wxBITMAP_TYPE_ANY) == nullptr);
        RecordDecodeFailure(errorMessage, missingCodec);
        wxLogMessage("LoadImage(): %s", errorMessage.mb_str());
      }
    }
  } else {
    RecordDataUnavailable(_("No usable image data was found."));
  }
}

void Image::Recalculate(double scale) {
  if(m_loadImageTask)
    m_loadImageTask->Wait();
  if(m_loadGnuplotSourceTask)
    m_loadGnuplotSourceTask->Wait();
  wxCoord width = m_originalWidth;
  wxCoord height = m_originalHeight;

  // We want the image to get bigger if the user zooms in - and
  // if a high printing resolution requires us to scale everything up.
  // To also take care of the user's printing-scale,
  // the scale is passed by a parameter.

  // Ensure a minimum size for images.
  if (scale < 0.01)
    scale = 0.01;

  // pre-scale the image according to the current output's ppi and the image's
  // ppi;
  scale *= m_configuration->GetPPI().x;
  scale /= m_ppi;

  if ((width < 1) || (height < 1)) {
    m_width = 700;
    m_height = 300;
    return;
  }

  int viewPortHeight = m_configuration->GetCanvasSize().y - 1;
  int viewPortWidth = m_configuration->GetLineWidth();

  if (viewPortHeight < 10)
    viewPortHeight = 10;
  if (viewPortWidth < 10)
    viewPortWidth = 10;

  if (m_fitToWindow) {
    if (scale * width > .99 * viewPortWidth)
      scale = .99 * viewPortWidth / width;

    // Shrink to .9* the canvas height, if needed
    if (scale * height > .9 * viewPortHeight) {
      if (scale > .9 * viewPortHeight / height)
        scale = .9 * viewPortHeight / height;
    }
  }

  // The user's own scale factor (wxplot_scale) comes after fitting the image
  // into the window: Its whole point is to allow a plot to be wider than the
  // window, or smaller than it would be shown otherwise.
  scale *= m_userScale;

  // Shrink to be smaller than the maximum size.
  if ((m_maxWidth > 0) &&
      (static_cast<wxCoord>(scale) * width >
       m_maxWidth * static_cast<wxCoord>(m_configuration->GetPPI().x)))
    scale = static_cast<double>(m_maxWidth) * m_configuration->GetPPI().x / width;
  if ((m_maxHeight > 0) &&
      (static_cast<wxCoord>(scale) * height >
       m_maxHeight * static_cast<wxCoord>(m_configuration->GetPPI().y)))
    scale = static_cast<double>(m_maxHeight) * m_configuration->GetPPI().y / height;

  // Set the width of the scaled image
  m_height = static_cast<int>(scale * height);
  m_width = static_cast<int>(scale * width);

  if ((m_height < 1) || (m_width < 1)) {
    m_height = 1;
    m_width = 1;
  }
  // Clear this cell's image cache if it doesn't contain an image of the size
  // we need right now. Guard the size query: GetWidth() on an invalid bitmap
  // (e.g. a not-yet-rendered or broken image) asserts in wxWidgets, which under
  // a GUI assert handler hangs (this was a libFuzzer timeout on "<img/>").
  if (!m_scaledBitmap.IsOk() || (m_scaledBitmap.GetWidth() != m_width))
    ClearCache();
}

#include <wx/stdpaths.h>
#include <wx/filename.h>

bool Image::IsSafePath(const wxString &path) {
  if (path.IsEmpty())
    return false;

  wxFileName fn(path);
  fn.MakeAbsolute();
  wxString absolutePath = fn.GetFullPath();

  wxString tempDir = wxStandardPaths::Get().GetTempDir();
  wxFileName tempFn(tempDir);
  tempFn.MakeAbsolute();
  wxString absoluteTempDir = tempFn.GetFullPath();

  if (absolutePath.StartsWith(absoluteTempDir))
    return true;

  return false;
}

const wxString Image::GetBadImageToolTip() {
  return _(
    "The image could not be displayed. It may be broken, in a wrong format "
    "or "
    "be the result of gnuplot not being able to write the image or not being "
    "able to understand what maxima wanted to plot.\n"
    "One example of the latter would be: Gnuplot refuses to plot entirely "
    "empty images");
}

wxBitmap Image::SvgBitmapAt(wxSize size) {
  if (!SvgBundle().IsOk())
    return wxBitmap();
  wxBitmap bitmap = SvgBundle().GetBitmap(size);
  // GetBitmap() marks the bitmap as drawn at size / (the bundle's default
  // size) on a HiDPI screen, which would make it occupy the default size on
  // the worksheet whatever size we asked for. Our sizes are in pixels already.
  if (bitmap.IsOk())
    bitmap.SetScaleFactor(1);
  return bitmap;
}

const wxBitmapBundle &Image::SvgBundle() {
  if (m_svgBundle.IsOk() || !IsSvg())
    return m_svgBundle;
  wxMemoryInputStream istream(m_compressedImage.GetData(),
                              m_compressedImage.GetDataLen());
  wxZlibInputStream zstream(istream);
  if (!zstream.IsOk())
    return m_svgBundle;
  wxMemoryOutputStream svg;
  zstream.Read(svg);
  svg.PutC('\0');
  m_svgBundle = wxBitmapBundle::FromSVG(
    static_cast<char *>(svg.GetOutputStreamBuffer()->GetBufferStart()),
    wxSize(m_originalWidth, m_originalHeight));
  return m_svgBundle;
}
