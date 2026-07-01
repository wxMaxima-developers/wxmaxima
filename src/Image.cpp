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
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanosvg_private.h"
#include "nanosvgrast_private.h"
#include <Image.h>
#include <vector>
#include <utility>
#include <wx/log.h>
#include "StringUtils.h"
#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/regex.h>
#include <wx/stdpaths.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/sstream.h>
#if wxCHECK_VERSION(3, 1, 6)
#include <wx/bmpbndl.h>
#endif

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
  wxImage Image;
  // Remark for issue #2087:
  // That works, if the image is a usual image format (png, jpeg, ...). For "svgz" (and "svg") we must first convert them
  // image with wxBitmapBundle::FromSVG (if wxWidgets is new enough) and then load the image.
  // (Implemented now, but it does not work - why? Was my assumption false? If so, the commit can be reverted...)

  wxLogMessage(_("Image of type %s found."), type); // should output the type for every loaded image. However, I can't see any log message. Why?
  if (m_compressedImage.GetDataLen() > 0) {
#if wxCHECK_VERSION(3, 1, 6)
    if (type == wxS("svgz")) {
      wxMemoryInputStream memIn(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
      wxZlibInputStream gzipInput(memIn, wxZLIB_GZIP);
      // Output to a string and get the result as wxString
      wxStringOutputStream stringOut;
      stringOut.Write(gzipInput);
      wxString decompressedString = stringOut.GetString();
      // wxBitmapBundle::FromSVG currently does not use the sizes given in the SVG file, but requires a wxSize parameter. Am I right?
      // Maybe we should read them from the SVG data by ourselves.
      // For now, just use a constant size...
      wxBitmapBundle bmb = wxBitmapBundle::FromSVG(decompressedString.mb_str(), wxSize(200,200));
      Image = bmb.GetBitmap(wxSize(200,200)).ConvertToImage();
    } else {
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
    }
#else
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
#endif
    // Image.IsOk() is now true for svgz files too (wxWidgets >= 3.1.6). Before it was not. However, the SVG is still not displayed in WXM files... Strange!
    if(Image.IsOk())
    {
      m_originalWidth = Image.GetWidth();
      m_originalHeight = Image.GetHeight();
    }
    else
    {
      InvalidBitmap(errorAggregator.GetBuffer());
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
             const wxString &wxmxFile, bool remove)
{
  m_svgImage = NULL;
  m_configuration = config;
  m_ppi = m_configuration->GetPPI().x;
  LoadImage(image, wxmxFile, remove);
}

Image::Image(Configuration *config, const Image &image) {
  m_svgImage = NULL;
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_maxWidth = image.m_maxWidth;
  m_maxHeight = image.m_maxHeight;
  m_originalWidth = image.m_originalWidth;
  m_originalHeight = image.m_originalHeight;
  m_compressedImage = image.m_compressedImage;
  m_ppi = image.m_ppi;
  m_extension = image.m_extension;
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
  if (m_svgImage)
    free(m_svgImage);
}

wxMemoryBuffer Image::ReadCompressedImage(stop_token stopToken, wxInputStream *data) {
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

  if (m_svgRast) {
    std::vector<unsigned char> imgdata(m_originalWidth * m_originalHeight * 4);

    wxm_nsvgRasterize(m_svgRast.get(), m_svgImage, 0, 0, 1, imgdata.data(),
                      m_originalWidth, m_originalHeight, m_originalWidth * 4);
    return RGBA2wxBitmap(imgdata.data(), m_originalWidth, m_originalHeight);
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

void Image::GnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                          const wxString &wxmxFile) {
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
       wxmxFile](stop_token stopToken) mutable {
        LoadGnuplotSource_Backgroundtask(stopToken,
                                         gnuplotFilename, dataFilename,
                                         wxmxFile);
      });
  else
    LoadGnuplotSource_Backgroundtask({},
      m_gnuplotSource, m_gnuplotData, wxmxFile);
}

void Image::LoadGnuplotSource_Backgroundtask(
  stop_token stopToken,
  wxString gnuplotFile, wxString dataFile, wxString wxmxFile)
{
  if (stopToken.stop_requested()) return;
  if(wxmxFile.IsEmpty())
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
      wxFileInputStream wxmx(wxmxFile);
      WxmxStream source(wxmx, gnuplotFile);
      LoadGnuplotSource(&source);
    }
    if (stopToken.stop_requested()) return;
    {
      wxFileInputStream wxmx(wxmxFile);
      WxmxStream data(wxmx, dataFile);
      LoadGnuplotData(&data);
    }
  }
}

void Image::CompressedGnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                                     const wxString &wxmxFile) {
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
       data = m_gnuplotData, wxmxFile](stop_token stopToken) mutable {
        LoadCompressedGnuplotSource_Backgroundtask(
          stopToken, gnuplotSource, data, wxmxFile);
      });
  else
    LoadCompressedGnuplotSource_Backgroundtask({},
      m_gnuplotSource,
      m_gnuplotData,
      wxmxFile);
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
    while (!source->Eof()) {
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

    while (!data->Eof()) {
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
  stop_token stopToken,
  wxString sourcefile,
  wxString datafile,
  wxString wxmxFile
  ) {
  if (stopToken.stop_requested()) return;
  {
    // Read the gnuplot source
    {
      wxFileInputStream wxmx(wxmxFile);
      WxmxStream source(wxmx, sourcefile);
      if (!source.Eof()) {
        m_gnuplotSource_Compressed.Clear();
        wxMemoryOutputStream mstream;
        source.Read(mstream);
        if (stopToken.stop_requested()) return;
        m_gnuplotSource_Compressed.AppendData(
          mstream.GetOutputStreamBuffer()->GetBufferStart(),
          mstream.GetOutputStreamBuffer()->GetBufferSize());
      }
    }
    if (stopToken.stop_requested()) return;
    // Read the gnuplot data
    {
      m_gnuplotData_Compressed.Clear();
      wxFileInputStream wxmx(wxmxFile);
      WxmxStream data(wxmx, datafile);
      if (!data.Eof()) { // open successful
        wxMemoryOutputStream mstream;
        data.Read(mstream);
        if (stopToken.stop_requested()) return;
        m_gnuplotData_Compressed.AppendData(
          mstream.GetOutputStreamBuffer()->GetBufferStart(),
          mstream.GetOutputStreamBuffer()->GetBufferSize());
      }
    }
  }
}

Image::WxmxStream::WxmxStream(wxInputStream &wxmxFile, const wxString &fileInWxmx):
  wxZipInputStream(wxmxFile, wxConvLocal)
{
  while(!Eof())
  {
    wxZipEntry *contentsEntry = NULL;
    contentsEntry = GetNextEntry();
    if((!contentsEntry) || (contentsEntry->GetName() == fileInWxmx))
      break;
  }
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

wxSize Image::ToImageFile(wxString filename) {
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
  // Let's see if we have cached the scaled bitmap with the right size
  if (m_scaledBitmap.GetWidth() == m_width)
    return m_scaledBitmap;

  // Seems like we need to create a new scaled bitmap.
  if (m_svgRast) {
    // First create rgba data
    std::vector<unsigned char> imgdata(static_cast<std::size_t>(m_width) * m_height * 4);

    wxm_nsvgRasterize(m_svgRast.get(), m_svgImage, 0, 0,
                      static_cast<double>(m_width) / (static_cast<double>(m_originalWidth)),
                      imgdata.data(),
                      m_width, m_height, m_width * 4);
    return m_scaledBitmap =
      RGBA2wxBitmap(imgdata.data(), m_width, m_height);
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
    img.Rescale(m_width, m_height, wxIMAGE_QUALITY_BICUBIC);
    m_scaledBitmap = wxBitmap(img, 24);
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

void Image::LoadImage(wxString image, const wxString &wxmxFile,
                      bool remove) {
  if(m_loadImageTask)
  {
    m_loadImageTask->RequestStop();
    m_loadImageTask->Wait();
  }
  m_fromWxFS = !wxmxFile.IsEmpty();
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
       wxmxFile, remove](stop_token stopToken) mutable {
        LoadImage_Backgroundtask(stopToken, std::move(image),
                                 wxmxFile, remove);
      });
  else
    LoadImage_Backgroundtask({},
      std::move(image), wxmxFile,
      remove
      );
}

void Image::LoadImage_Backgroundtask(stop_token stopToken,
                                     wxString image, wxString wxmxFile,
                                     bool remove) {
  wxLogBuffer errorAggregator;

  if (stopToken.stop_requested()) return;

  if (!wxmxFile.IsEmpty()) {
    wxFileInputStream wxmx(wxmxFile);
    WxmxStream imgData(wxmx, image);
    m_compressedImage = ReadCompressedImage(stopToken, &imgData);
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
      InvalidBitmap(errorAggregator.GetBuffer());
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
          char head[512] = {};
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
      // Convert the data we have read to a modifiable char * containing the svg
      // file's contents.
      wxCharBuffer svgContents = svgContents_string.ToUTF8();

      // Parse the svg file's contents
      int ppi = m_configuration->GetPPI().x;

      if (svgContents.data()) {
        m_svgImage = wxm_nsvgParse(svgContents.data(), "px", ppi);
      }

      if (m_svgImage) {
        if (!m_svgRast)
          m_svgRast.reset(wxm_nsvgCreateRasterizer());
        m_originalWidth = m_svgImage->width;
        m_originalHeight = m_svgImage->height;
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
        InvalidBitmap(errorMessage);
        wxLogMessage("LoadImage(): %s", errorMessage.mb_str());
      }
    }
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

  if (scale * width > .99 * viewPortWidth)
    scale = .99 * viewPortWidth / width;

  // Shrink to .9* the canvas height, if needed
  if (scale * height > .9 * viewPortHeight) {
    if (scale > .9 * viewPortHeight / height)
      scale = .9 * viewPortHeight / height;
  }

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
  // we need right now.
  if (m_scaledBitmap.GetWidth() != m_width)
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

wxBitmap Image::RGBA2wxBitmap(const unsigned char imgdata[],
                              const int &width, const int &height) {
  wxBitmap retval = wxBitmap(wxSize(width, height), 32);
  const unsigned char *rgba = imgdata;
  if (!retval.Ok())
    return retval;

  wxAlphaPixelData bmpdata(retval);
  wxAlphaPixelData::Iterator dst(bmpdata);
  for (int y = 0; y < height; y++) {
    dst.MoveTo(bmpdata, 0, y);
    for (int x = 0; x < width; x++) {
      unsigned char a = rgba[3];
      dst.Red() = rgba[0] * a / 255;
      dst.Green() = rgba[1] * a / 255;
      dst.Blue() = rgba[2] * a / 255;
      dst.Alpha() = a;
      dst++;
      rgba += 4;
    }
  }
  return retval;
}
