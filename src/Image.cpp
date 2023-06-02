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
#include "Cell.h"
#include "Version.h"
#include <wx/image.h>
#include <wx/rawbmp.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>
#include <wx/buffer.h>
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"
#include <Image.h>

/* The nanosvg .h files contain both the header and the implementation.
   In exactly one file of the project need to be defined in order to
   make the implementation visible to the compiler.

   In wxWidgets >3.1.6 this is done in wxWidgets itself so we need to
   skip that step there.

   So that should work. But for Linux builds with 3.1.6 I get linking errors,
   e.g.: undefined reference to `nsvgRasterize' Therefore do not define
   NANOSVG_IMPLEMENTATION/NANOSVGRAST_IMPLEMENTATION only for Windows. That is
   probably not correct (a FIXME), but I have no idea, why Linux still requires
   that NANOSVG_IMPLEMENTATION/NANOSVGRAST_IMPLEMENTATION is defined.

*/
#if (wxCHECK_VERSION(3, 1, 6)) && defined(__WINDOWS__)
#else
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#endif

#include "ErrorRedirector.h"
#include "StringUtils.h"
#include "SvgBitmap.h"
#include <wx/mstream.h>
#include <wx/regex.h>
#include <wx/stdpaths.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>

Image::Image(Configuration *config) {
  m_configuration = config;
  m_width = 1;
  m_height = 1;
  m_originalWidth = 640;
  m_originalHeight = 480;
  m_scaledBitmap.Create(1, 1);
  m_isOk = false;
  m_maxWidth = -1;
  m_maxHeight = -1;
}

Image::Image(Configuration *config, wxMemoryBuffer image, wxString type) {
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_compressedImage = image;
  m_extension = type;
  m_isOk = false;
  m_width = 1;
  m_height = 1;
  m_originalWidth = 640;
  m_originalHeight = 480;

  wxImage Image;
  if (m_compressedImage.GetDataLen() > 0) {
    wxMemoryInputStream istream(m_compressedImage.GetData(),
                                m_compressedImage.GetDataLen());
    {
      SuppressErrorDialogs logNull;
      Image.LoadFile(istream);
    }
    m_isOk = Image.IsOk();
    m_originalWidth = Image.GetWidth();
    m_originalHeight = Image.GetHeight();
  } else
    InvalidBitmap();
  m_maxWidth = -1;
  m_maxHeight = -1;
}

Image::Image(Configuration *config, const wxBitmap &bitmap) {
  m_configuration = config;
  m_isOk = false;
  m_width = 1;
  m_height = 1;
  m_maxWidth = -1;
  m_maxHeight = -1;
  m_originalWidth = 640;
  m_originalHeight = 480;
  LoadImage(bitmap);
  m_scaledBitmap.Create(1, 1);
}

// constructor which loads an image
// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
Image::Image(Configuration *config, wxString image,
             std::shared_ptr<wxFileSystem> filesystem, bool remove)
{
  m_svgImage = NULL;
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_isOk = false;
  m_width = 1;
  m_height = 1;
  m_maxWidth = -1;
  m_maxHeight = -1;
  m_originalWidth = 640;
  m_originalHeight = 480;
  m_ppi = m_configuration->GetDC()->GetPPI().x;
  LoadImage(image, filesystem, remove);
}

Image::Image(Configuration *config, const Image &image) {
  m_svgImage = NULL;
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_isOk = image.m_isOk;
  m_width = 1;
  m_height = 1;
  m_maxWidth = image.m_maxWidth;
  m_maxHeight = image.m_maxHeight;
  m_originalWidth = image.m_originalWidth;
  m_originalHeight = image.m_originalHeight;
  m_compressedImage = image.m_compressedImage;
  m_ppi = image.m_ppi;
  m_extension = image.m_extension;
}

Image::~Image() {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
  m_isOk = false;
  if (!m_gnuplotSource.IsEmpty()) {
    SuppressErrorDialogs logNull;
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

  if (!m_gnuplotData.IsEmpty()) {
    SuppressErrorDialogs logNull;
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

wxMemoryBuffer Image::ReadCompressedImage(wxInputStream *data) {
  wxMemoryBuffer retval;
  std::vector<char> buf(8192);

  while (data->CanRead()) {
    data->Read(buf.data(), buf.size());
    retval.AppendData(buf.data(), data->LastRead());
  }

  return retval;
}

wxBitmap Image::GetUnscaledBitmap() {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  if (!m_isOk) {
    InvalidBitmap();
    return m_scaledBitmap;
  }

  SuppressErrorDialogs logNull;
  if (m_svgRast) {
    std::vector<unsigned char> imgdata(m_originalWidth * m_originalHeight * 4);

    nsvgRasterize(m_svgRast.get(), m_svgImage, 0, 0, 1, imgdata.data(),
                  m_originalWidth, m_originalHeight, m_originalWidth * 4);
    return RGBA2wxBitmap(imgdata.data(), m_originalWidth,
                                    m_originalHeight);
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
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  return m_compressedImage;
}

size_t Image::GetOriginalWidth() const {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  
  return m_originalWidth;
}

size_t Image::GetOriginalHeight() const {
    if(m_loadImageTask.joinable())
    m_loadImageTask.join();

    return m_originalHeight;
}

bool Image::IsOk() const {
    if(m_loadImageTask.joinable())
    m_loadImageTask.join();
return m_isOk;
}

// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
void Image::GnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                          std::shared_ptr<wxFileSystem> filesystem) {
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
  std::unique_ptr<ThreadNumberLimiter> limiter(new
                                               ThreadNumberLimiter(&m_gnuplotDataThreadRunning)); 
  m_loadGnuplotSourceTask = std::thread(&Image::LoadGnuplotSource_Backgroundtask,
                                        this, gnuplotFilename, dataFilename, filesystem,
                                        std::move(limiter));
}

void Image::CompressedGnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                                    std::shared_ptr<wxFileSystem> filesystem) {
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
  std::unique_ptr<ThreadNumberLimiter> limiter(new
                                               ThreadNumberLimiter(&m_gnuplotDataThreadRunning)); 
  m_loadGnuplotSourceTask = std::thread(&Image::LoadCompressedGnuplotSource_Backgroundtask,
                                        this, gnuplotFilename, dataFilename, filesystem,
                                        std::move(limiter));
}

void Image::LoadGnuplotSource_Backgroundtask(
					     wxString gnuplotFilename, wxString dataFilename,
					     std::shared_ptr<wxFileSystem> filesystem,
                                             std::unique_ptr<ThreadNumberLimiter> limiter) {
  // Error dialogues need to be created by the foreground thread.
  SuppressErrorDialogs suppressor;
  std::shared_ptr<wxFileSystem> keepalive(filesystem);
  m_gnuplotSource = gnuplotFilename;
  m_gnuplotData = dataFilename;

  if (filesystem == NULL) {
    if (wxFileExists(dataFilename)) {
      // Don't cache the data for unreasonably long files.
      wxStructStat strucStat;
      wxStat(dataFilename, &strucStat);
      if (strucStat.st_size >
          m_configuration->MaxGnuplotMegabytes() * 1000 * 1000) {
        wxLogMessage(_("Too much gnuplot data => Not storing it in the worksheet"));
        m_gnuplotData_Compressed.Clear();
        return;
      }

      // The gnuplot source of the image is cached in a compressed form:
      //
      // as it is text-only and contains many redundancies it will get way
      // smaller this way.
      {
        wxFileInputStream input(m_gnuplotSource);
        if (input.IsOk()) {
          wxTextInputStream textIn(input, wxS('\t'),
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
          wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[0-9]*\\.data'");
          while (!input.Eof()) {
            line = textIn.ReadLine();
            if (replaceDataFileName.Matches(line)) {
              wxString dataFileName;
              dataFileName = replaceDataFileName.GetMatch(line);
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

        {
          wxFileInputStream input2(m_gnuplotData);
          if (input2.IsOk()) {
            wxTextInputStream textIn(input2, wxS('\t'),
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

            while (!input2.Eof()) {
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
      }
    }
  } else {
    {
      std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(m_gnuplotSource));
      if (fsfile) { // open successful
        std::unique_ptr<wxInputStream> input(fsfile->DetachStream());
        if (input->IsOk()) {
          wxTextInputStream textIn(*input, wxS('\t'),
                                   wxConvAuto(wxFONTENCODING_UTF8));

          wxMemoryOutputStream mstream;
          int zlib_flags;
          if (wxZlibOutputStream::CanHandleGZip())
            zlib_flags = wxZLIB_GZIP;
          else
            zlib_flags = wxZLIB_ZLIB;
          if (!input->Eof()) {
            wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION,
                                       zlib_flags);
            if (zstream.IsOk()) {
              wxTextOutputStream textOut(zstream);
              wxString line;

              // A RegEx that matches the name of the data file (needed if we
              // ever want to move a data file into the temp directory of a new
              // computer that locates its temp data somewhere strange).
              wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[0-9*]\\.data'");
              while (!input->Eof()) {
                line = textIn.ReadLine();
                if (replaceDataFileName.Matches(line)) {
                  wxString dataFileName;
                  dataFileName = replaceDataFileName.GetMatch(line);
                  if (dataFileName != wxEmptyString)
                    wxLogMessage(_("Gnuplot Data File Name: ") + dataFileName);
                  replaceDataFileName.Replace(&line, wxS("'<DATAFILENAME>'"));
                }
                textOut << line + wxS("\n");
              }
              textOut.Flush();
              m_gnuplotSource_Compressed.Clear();
              m_gnuplotSource_Compressed.AppendData(
						    mstream.GetOutputStreamBuffer()->GetBufferStart(),
						    mstream.GetOutputStreamBuffer()->GetBufferSize());
            }
            zstream.Close();
          }
        }
      }
    }
    {
      std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(m_gnuplotData));
      if (fsfile) { // open successful
        std::unique_ptr<wxInputStream> input(fsfile->DetachStream());
        if (input->IsOk()) {
          wxTextInputStream textIn(*input, wxS('\t'),
                                   wxConvAuto(wxFONTENCODING_UTF8));

          wxMemoryOutputStream mstream;
          int zlib_flags;
          if (wxZlibOutputStream::CanHandleGZip())
            zlib_flags = wxZLIB_GZIP;
          else
            zlib_flags = wxZLIB_ZLIB;
          if (!input->Eof()) {
            wxZlibOutputStream zstream(mstream, wxZ_BEST_COMPRESSION,
                                       zlib_flags);
            if (zstream.IsOk()) {
              wxTextOutputStream textOut(zstream);
              wxString line;

              while (!input->Eof()) {
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
        }
      }
    }
  }
}

void Image::LoadCompressedGnuplotSource_Backgroundtask(
  wxString gnuplotFilename, wxString dataFilename,
  std::shared_ptr<wxFileSystem> filesystem,
  std::unique_ptr<ThreadNumberLimiter> limiter) {
  std::shared_ptr<wxFileSystem> keepalive(filesystem);

  // Error dialogues need to be created by the foreground thread.
  SuppressErrorDialogs suppressor;

  // Store the filenames without the ".gz".
  m_gnuplotSource = gnuplotFilename;
  m_gnuplotData = dataFilename;
  if(m_gnuplotSource.EndsWith(".gz"))
    m_gnuplotSource = m_gnuplotSource.Left(m_gnuplotSource.Length()-3);
  if(m_gnuplotData.EndsWith(".gz"))
    m_gnuplotData = m_gnuplotData.Left(m_gnuplotData.Length()-3);

  // Read the gnuplot source
  {
    std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(gnuplotFilename));
    if (fsfile) { // open successful
      std::unique_ptr<wxInputStream> input(fsfile->DetachStream());
      if (input->IsOk()) {
        m_gnuplotSource_Compressed.Clear();
        wxMemoryOutputStream mstream;
        input->Read(mstream);
        m_gnuplotSource_Compressed.AppendData(
          mstream.GetOutputStreamBuffer()->GetBufferStart(),
          mstream.GetOutputStreamBuffer()->GetBufferSize());
      }
    }
  }
  // Read the gnuplot data
  {
    std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(dataFilename));
    if (fsfile) { // open successful
      std::unique_ptr<wxInputStream> input(fsfile->DetachStream());
      if (input->IsOk()) {
        m_gnuplotData_Compressed.Clear();
        wxMemoryOutputStream mstream;
        input->Read(mstream);
        m_gnuplotData_Compressed.AppendData(
          mstream.GetOutputStreamBuffer()->GetBufferStart(),
          mstream.GetOutputStreamBuffer()->GetBufferSize());
      }
    }
  }
}

const wxMemoryBuffer Image::GetGnuplotSource() {
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();

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
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
  return m_gnuplotSource_Compressed;
}

const wxMemoryBuffer Image::GetCompressedGnuplotData()
{
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
  return m_gnuplotData_Compressed;
}

const wxMemoryBuffer Image::GetGnuplotData() {
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
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
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
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
  if(m_loadGnuplotSourceTask.joinable())
    m_loadGnuplotSourceTask.join();
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
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  wxFileName fn(filename);
  wxString ext = fn.GetExt();
  if (filename.Lower().EndsWith(GetExtension().Lower())) {
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxSize(-1, -1);

    file.Write(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    if (file.Close())
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxSize(-1, -1);
  }

  if ((filename.Lower().EndsWith(".svg")) && (m_extension == "svgz")) {
    // Unzip the .svgz image
    wxString svgContents_string;
    wxMemoryInputStream istream(m_compressedImage.GetData(),
                                m_compressedImage.GetDataLen());
    wxZlibInputStream zstream(istream);
    if (!zstream.IsOk())
      return wxSize(-1, -1);
    wxTextInputStream textIn(zstream);
    wxString line;
    while (!zstream.Eof()) {
      line = textIn.ReadLine();
      svgContents_string += line + wxS("\n");
    }
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxSize(-1, -1);
    wxFileOutputStream output(file);
    wxTextOutputStream text(output);
    text << svgContents_string;
    if (file.Close())
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxSize(-1, -1);
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
      return (wxSize(-1, -1));

    if (!image.SaveFile(filename, mimetype))
      return wxSize(-1, -1);
    return image.GetSize();
  }
}

wxBitmap Image::GetBitmap(double scale) {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  // Recalculate contains its own WaitForLoad object.
  Recalculate(scale);

  if (!m_isOk) {
    InvalidBitmap();
    return m_scaledBitmap;
  }

  // Let's see if we have cached the scaled bitmap with the right size
  if (m_scaledBitmap.GetWidth() == m_width)
    return m_scaledBitmap;

  // Seems like we need to create a new scaled bitmap.
  if (m_svgRast) {
    // First create rgba data
    std::vector<unsigned char> imgdata(m_width * m_height * 4);

    nsvgRasterize(m_svgRast.get(), m_svgImage, 0, 0,
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
    m_isOk = true;

    if (img.Ok())
      m_scaledBitmap = wxBitmap(img);
    else
      InvalidBitmap();
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

void Image::InvalidBitmap() {
  m_isOk = false;
  m_width = 800 * m_ppi / 96;
  m_height = 600 * m_ppi / 96;
  // Create a "image not loaded" bitmap.
  m_scaledBitmap.Create(m_width, m_height);

  wxString error;
  if (m_imageName != wxEmptyString)
    error =
      wxString::Format(_("Error: Cannot render %s."), m_imageName.utf8_str());
  else
    error = wxString::Format(_("Error: Cannot render the image."));

  wxMemoryDC dc;
  dc.SelectObject(m_scaledBitmap);

  int width = 0, height = 0;
  dc.GetTextExtent(error, &width, &height);

  dc.DrawRectangle(0, 0, m_width - 1, m_height - 1);
  dc.DrawLine(0, 0, m_width - 1, m_height - 1);
  dc.DrawLine(0, m_height - 1, m_width - 1, 0);

  dc.GetTextExtent(error, &width, &height);
  dc.DrawText(error, (m_width - width) / 2, (m_height - height) / 2);
}

void Image::LoadImage(const wxBitmap &bitmap) {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  // Convert the bitmap to a png image we can use as m_compressedImage
  wxImage image = bitmap.ConvertToImage();
  m_isOk = image.IsOk();
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

wxString Image::GetExtension() const { return m_extension; }

// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
void Image::LoadImage(wxString image, std::shared_ptr<wxFileSystem> filesystem,
                      bool remove) {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  m_extension = wxFileName(image).GetExt();
  m_extension = m_extension.Lower();
  m_imageName = image;
  std::unique_ptr<ThreadNumberLimiter> limiter(new ThreadNumberLimiter()); 
  m_compressedImage.Clear();
  m_scaledBitmap.Create(1, 1);
  SuppressErrorDialogs logNull;

  if (filesystem) {
    std::unique_ptr<wxFSFile> fsfile(filesystem->OpenFile(image));
    if (fsfile) { // open successful
      std::unique_ptr<wxInputStream> istream(fsfile->DetachStream());

      m_compressedImage = ReadCompressedImage(istream.get());
    }

    // Closing and deleting fsfile is important: If this line is missing
    // opening .wxmx files containing hundreds of images might lead to a
    // "too many open files" error.
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
        m_compressedImage = ReadCompressedImage(strm.get());

      file.Close();
      if (ok && remove) {
        SuppressErrorDialogs logNull;
        wxRemoveFile(image);
      }
    }
  }

  m_loadImageTask = std::thread(&Image::LoadImage_Backgroundtask,
                                this,
                                std::move(limiter));

}

void Image::LoadImage_Backgroundtask(std::unique_ptr<ThreadNumberLimiter> limiter) {
  m_isOk = false;

  wxImage Image;
  if (m_compressedImage.GetDataLen() > 0) {
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
        while (!zstream.Eof()) {
          line = textIn.ReadLine();
          svgContents_string += line + wxS("\n");
        }
      }
      // Convert the data we have read to a modifiable char * containing the svg
      // file's contents.
      wxCharBuffer svgContents = svgContents_string.ToUTF8();

      // Parse the svg file's contents
      int ppi;
      if (m_configuration->GetDC()->GetPPI().x > 50)
        ppi = m_configuration->GetDC()->GetPPI().x;
      else
        ppi = 96;

      if (svgContents.data()) {
        m_svgImage = nsvgParse(svgContents.data(), "px", ppi);
      }

      if (m_svgImage) {
        if (!m_svgRast)
          m_svgRast.reset(nsvgCreateRasterizer());
        if (m_svgRast)
          m_isOk = true;
        m_originalWidth = m_svgImage->width;
        m_originalHeight = m_svgImage->height;
      }
    } else {
      wxMemoryInputStream istream(m_compressedImage.GetData(),
                                  m_compressedImage.GetDataLen());
      {
        SuppressErrorDialogs logNull;
        Image.LoadFile(istream);
      }
      m_originalWidth = 700;
      m_originalHeight = 300;

      if (Image.Ok()) {
        m_originalWidth = Image.GetWidth();
        m_originalHeight = Image.GetHeight();
        m_isOk = true;
        if (Image.HasOption(wxS("wxIMAGE_OPTION_RESOLUTION"))) {
          int resolution;
          resolution = Image.GetOptionInt(wxS("wxIMAGE_OPTION_RESOLUTION"));
          if (Image.HasOption(wxS("wxIMAGE_OPTION_RESOLUTIONUNIT"))) {
            if (Image.GetOptionInt("wxIMAGE_OPTION_RESOLUTIONUNIT") ==
                wxIMAGE_RESOLUTION_CM)
              resolution *= 2.54;
          }
          if (resolution > 50)
            m_ppi = resolution;
        }
      } else {
        InvalidBitmap();
      }
    }
  }
}

void Image::Recalculate(double scale) {
  if(m_loadImageTask.joinable())
    m_loadImageTask.join();
  // It would better to use a jthread for joining no-more used threads.
  // But that is C++20, which now (in 2023) is still too early.
  if((!m_gnuplotDataThreadRunning) && m_loadGnuplotSourceTask.joinable())
     m_loadGnuplotSourceTask.join();
  int width = m_originalWidth;
  int height = m_originalHeight;

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

  int viewPortHeight = m_configuration->GetClientHeight();
  int viewPortWidth = m_configuration->GetClientWidth();

  if (viewPortHeight < 10)
    viewPortHeight = 10;
  if (viewPortWidth < 10)
    viewPortWidth = 10;

  // Shrink to .9* the canvas size, if needed
  if (scale * width > .9 * viewPortWidth)
    scale = .9 * viewPortWidth / width;

  if (scale * height > .9 * viewPortHeight) {
    if (scale > .9 * viewPortHeight / height)
      scale = .9 * viewPortHeight / height;
  }

  // Shrink to be smaller than the maximum size.
  if ((m_maxWidth > 0) &&
      (scale * width > m_maxWidth * m_configuration->GetPPI().x))
    scale = m_maxWidth * m_configuration->GetPPI().x / width;
  if ((m_maxHeight > 0) &&
      (scale * height > m_maxHeight * m_configuration->GetPPI().y))
    scale = m_maxHeight * m_configuration->GetPPI().y / height;

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

const wxString &Image::GetBadImageToolTip() {
  // cppcheck-suppress returnTempReference
  return T_(
	    "The image could not be displayed. It may be broken, in a wrong format "
	    "or "
	    "be the result of gnuplot not being able to write the image or not being "
	    "able to understand what maxima wanted to plot.\n"
	    "One example of the latter would be: Gnuplot refuses to plot entirely "
	    "empty images");
}

wxBitmap Image::RGBA2wxBitmap(const unsigned char imgdata[],
                              const int &width, const int &height,

#if defined __WXOSX__
                                  const int &scaleFactor
#else
                                  const int &WXUNUSED(scaleFactor)
#endif
				  ) {
#if defined __WXOSX__
  wxBitmap retval = wxBitmap(wxSize(width, height), 32, scaleFactor);
#else
  wxBitmap retval = wxBitmap(wxSize(width, height), 32);
#endif
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
