// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
  This file defines the class Image that stores compressed images and handles scaling and uncompressing them.
*/

#include "Image.h"
#define NANOSVG_ALL_COLOR_KEYWORDS
#define NANOSVG_IMPLEMENTATION
#define NANOSVGRAST_IMPLEMENTATION
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/regex.h>
#include <wx/stdpaths.h>
#include "stx/optional.hpp"
#include "Streams.h"
#include "SvgBitmap.h"
#include "ErrorRedirector.h"

Image::Image(Configuration **config) :
    m_configuration(config)
{
  #ifdef HAVE_OMP_HEADER
  omp_init_lock(&m_gnuplotLock);
  omp_init_lock(&m_imageLoadLock);
  #endif
  m_scaledBitmap.Create(1, 1);
}

Image::Image(Configuration **config, wxMemoryBuffer image, const wxString &type) :
    m_compressedImage(image),
    m_extension(type),
    m_configuration(config)
{
  #ifdef HAVE_OMP_HEADER
  omp_init_lock(&m_gnuplotLock);
  omp_init_lock(&m_imageLoadLock);
  #endif
  m_scaledBitmap.Create(1, 1);
  
  wxImage Image;
  if (!m_compressedImage.IsEmpty())
  {
    MemBufInputStream istream(m_compressedImage);
    Image.LoadFile(istream);
    m_isOk = Image.IsOk();
    m_originalWidth = Image.GetWidth();
    m_originalHeight = Image.GetHeight();
  }
  else
    InvalidBitmap();
}

Image::Image(Configuration **config, const wxBitmap &bitmap) :
    m_configuration(config)
{
  #ifdef HAVE_OMP_HEADER
  omp_init_lock(&m_gnuplotLock);
  omp_init_lock(&m_imageLoadLock);
  #endif
  LoadImage(bitmap);
  m_scaledBitmap.Create(1, 1);
}

// constructor which loads an image
// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
Image::Image(Configuration **config, const wxString &image,
             std::shared_ptr<wxFileSystem> filesystem, bool remove) :
    m_configuration(config),
    m_fs_keepalive_imagedata(filesystem)
{
  #ifdef HAVE_OMP_HEADER
  omp_init_lock(&m_gnuplotLock);
  omp_init_lock(&m_imageLoadLock);
  #endif
  m_scaledBitmap.Create(1, 1);
  LoadImage(image, filesystem, remove);
}

Image::~Image()
{
  m_isOk = false;
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  {
    if (!m_gnuplotSource.IsEmpty())
    {
      SuppressErrorDialogs logNull;
      wxLogMessage(wxString::Format(_("Trying to delete gnuplot file %s"), m_gnuplotSource.utf8_str()));
      if (wxFileExists(m_gnuplotSource))
        wxRemoveFile(m_gnuplotSource);
      wxString popoutname = m_gnuplotSource + wxT(".popout");
      wxLogMessage(wxString::Format(_("Trying to delete gnuplot file %s"), popoutname.utf8_str()));
      if (wxFileExists(popoutname))
        wxRemoveFile(popoutname);    
    }

    if (!m_gnuplotData.IsEmpty())
    {
      SuppressErrorDialogs logNull;
      wxLogMessage(wxString::Format(_("Trying to delete gnuplot file %s"), m_gnuplotData.utf8_str()));
      if (wxFileExists(m_gnuplotData))
        wxRemoveFile(m_gnuplotData);
    }
  }
}

wxBitmap Image::GetUnscaledBitmap()
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif

  if (!m_isOk)
  {
    InvalidBitmap();
    return m_scaledBitmap;    
  }

  if (m_svgRast)
  {
    std::vector<unsigned char> imgdata(m_originalWidth*m_originalHeight*4);

    nsvgRasterize(m_svgRast.get(), m_svgImage.get(), 0,0,1, imgdata.data(),
                  m_originalWidth, m_originalHeight, m_originalWidth*4);
    return SvgBitmap::RGBA2wxBitmap(imgdata.data(), m_originalWidth, m_originalHeight);
  }
  else
  {
    MemBufInputStream istream(m_compressedImage);
    wxImage img(istream, wxBITMAP_TYPE_ANY);
    wxBitmap bmp;
    if (img.Ok())
      bmp = wxBitmap(img);
    return bmp;
  }
}

wxMemoryBuffer Image::GetCompressedImage() const
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  return m_compressedImage;
}

size_t Image::GetOriginalWidth() const
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  return m_originalWidth;
}

size_t Image::GetOriginalHeight() const
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  return m_originalHeight;
}

bool Image::IsOk() const
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  return m_isOk;
}


// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
void Image::GnuplotSource(const wxString &gnuplotFilename,
                          const wxString &dataFilename, std::shared_ptr<wxFileSystem> filesystem)
{
  m_fs_keepalive_gnuplotdata = filesystem;
  #ifdef HAVE_OPENMP_TASKS
  wxLogMessage(_("Starting background task that loads the gnuplot data for a plot."));
  #pragma omp task
  #endif
  LoadGnuplotSource_Backgroundtask(gnuplotFilename, dataFilename, filesystem);
}

static void ReplaceDataFileName(wxString &line)
{
  // A RegEx that matches the name of the data file (needed if we ever want to
  // move a data file into the temp directory of a new computer that locates its
  // temp data somewhere strange).
  static const wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[0-9*]\\.data'");
  static const wxString replacement("'<DATAFILENAME>'");

  if (replaceDataFileName.Matches(line))
  {
    wxString dataFileName = replaceDataFileName.GetMatch(line);
    if (!dataFileName.empty())
      wxLogMessage(_("Gnuplot Data File Name: ") + dataFileName);
    replaceDataFileName.Replace(&line, replacement);
  }
}

/*! Compresses text from a given filesystem file into a memory buffer filtering it
 * line-by-line.
 *
 * \arg inputFilesystem is the optional filesystem to read from, or null if
 *                      the file should be read from disk
 * \returns the memory buffer on success, or nothing on failure
 */
template <typename Filter>
static stx::optional<wxMemoryBuffer>
CompressAndFilterText(const wxString &inputFileName,
                      std::shared_ptr<wxFileSystem> inputFilesystem,
                      Filter &&filter)
{
  bool proceed = false;
  std::unique_ptr<wxFSFile> fsfile;
  stx::optional<wxFileInputStream> ifstream;
  wxInputStream *istream = {};

  if (inputFilesystem)
  {
    fsfile.reset(inputFilesystem->OpenFile(inputFileName));
    proceed = fsfile && (istream = fsfile->GetStream());
  }
  else
  {
    ifstream.emplace(inputFileName);
    proceed = (istream = &*ifstream);
  }

  if (proceed && istream->IsOk())
  { // open successful
    auto zlib_flags = wxZlibOutputStream::CanHandleGZip() ? wxZLIB_GZIP : wxZLIB_ZLIB;
    wxMemoryBuffer obuf;
    MemBufOutputStream mostream(obuf);
    wxZlibOutputStream zostream(mostream, wxZ_BEST_COMPRESSION, zlib_flags);

    if (CopyAndFilterText(*istream, zostream, filter,
                          wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8)))
      return obuf;
  }
  return {};
}

void Image::LoadGnuplot_Subtask(std::shared_ptr<wxFileSystem> filesystem)
{
  // The gnuplot source of the image is cached in a compressed form:
  // as it is text-only and contains many redundancies it will get way
  // smaller this way.
  auto zSource = CompressAndFilterText(m_gnuplotSource, filesystem,
                                       ReplaceDataFileName);
  m_gnuplotSource_Compressed = zSource ? *zSource : wxMemoryBuffer();
  if (!zSource)
  {
    wxLogMessage(_("Can't read and cache gnuplot source!"));
  }

  auto zData = CompressAndFilterText(m_gnuplotData, filesystem, [](auto &){});
  m_gnuplotData_Compressed = zData ? *zData : wxMemoryBuffer();
  if (!zData)
  {
    wxLogMessage(_("Can't read and cache gnuplot data!"));
  }
}

void Image::LoadGnuplotSource_Backgroundtask(
  const wxString &gnuplotFilename, const wxString &dataFilename, std::shared_ptr<wxFileSystem> filesystem)
{
  #ifdef HAVE_OMP_HEADER
  omp_set_lock(&m_gnuplotLock);
  #endif
  // Error dialogues need to be created by the foreground thread.
  SuppressErrorDialogs suppressor;
  m_gnuplotSource = gnuplotFilename;
  m_gnuplotData = dataFilename;

  if (!filesystem)
  {
    if (wxFileExists(dataFilename))
    {
      // Don't cache the data for unreasonably long files.
      wxStructStat strucStat;
      wxStat(dataFilename, &strucStat);
      if (strucStat.st_size > (*m_configuration)->MaxGnuplotMegabytes()*1000*1000)
      {
        wxLogMessage(_("Too much gnuplot data => Not storing it in the worksheet"));
        m_gnuplotData_Compressed.Clear();
        goto done;
      }
    }
    LoadGnuplot_Subtask({});
  }
  else
  {
    #ifdef HAVE_OPENMP_TASKS
    #pragma omp critical (OpenFSFile)
    #endif
    LoadGnuplot_Subtask(filesystem);
  }
done:
  m_fs_keepalive_gnuplotdata.reset();
  #ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_gnuplotLock);
  #endif
}

wxMemoryBuffer Image::GetGnuplotSource() const
{
  #ifdef HAVE_OMP_HEADER
  omp_set_lock(&m_gnuplotLock);
  #else
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  #endif

  stx::optional<wxMemoryBuffer> source;
  if ((m_gnuplotSource_Compressed.GetDataLen() >= 2)
      && (m_gnuplotData_Compressed.GetDataLen() >= 2))
  {
    source = DecompressData(m_gnuplotSource_Compressed);
  }

  #ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_gnuplotLock);
  #endif
  return source ? *source : wxMemoryBuffer{};
}

wxMemoryBuffer Image::GetGnuplotData() const
{
  #ifdef HAVE_OMP_HEADER
  omp_set_lock(&m_gnuplotLock);
  #else
  #ifdef HAVE_OPENMP_TASKS
  #pragma omp taskwait
  #endif
  #endif

  stx::optional<wxMemoryBuffer> data;
  if ((m_gnuplotSource_Compressed.GetDataLen() >= 2)
      && (m_gnuplotData_Compressed.GetDataLen() >= 2))
  {
    data = DecompressData(m_gnuplotData_Compressed);
  }

  #ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_gnuplotLock);
  #endif
  return data ? *data : wxMemoryBuffer{};;
}

wxString Image::GnuplotData()
{
  auto retval = m_gnuplotData;
  if ((!m_gnuplotData.IsEmpty()) && (!wxFileExists(m_gnuplotData)))
  {
    #ifdef HAVE_OMP_HEADER
    omp_set_lock(&m_gnuplotLock);
    #else
    #ifdef HAVE_OPENMP_TASKS
    #pragma omp taskwait
    #endif
    #endif
    {
      // Move the gnuplot data and data file into our temp directory
      wxFileName gnuplotSourceFile(m_gnuplotSource);
      m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotSourceFile.GetFullName();
      wxFileName gnuplotDataFile(m_gnuplotData);
      m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotDataFile.GetFullName();

      retval.clear();
      wxFileOutputStream ostream(m_gnuplotData);
      if (ostream.IsOk() && m_gnuplotData_Compressed.GetDataLen() <= 1)
      {
        wxLogMessage(_("No gnuplot data!"));
        goto done;
      }
      if (!DecompressDataInto(m_gnuplotData_Compressed, ostream))
      {
        wxLogMessage(_("Can't write gnuplot data to file!"));
        goto done;
      }
      retval = m_gnuplotData;
    }
  done:;
    #ifdef HAVE_OMP_HEADER
    omp_unset_lock(&m_gnuplotLock);
    #endif
  }
  return retval;
}

wxString Image::GnuplotSource()
{
  auto retval = m_gnuplotSource;
  if ((!m_gnuplotSource.IsEmpty()) && (!wxFileExists(m_gnuplotSource)))
  {
    #ifdef HAVE_OMP_HEADER
    omp_set_lock(&m_gnuplotLock);
    #else
    #ifdef HAVE_OPENMP_TASKS
    #pragma omp taskwait
    #endif
    #endif
    {
      // Move the gnuplot source and data file into our temp directory
      wxFileName gnuplotSourceFile(m_gnuplotSource);
      m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotSourceFile.GetFullName();
      wxFileName gnuplotDataFile(m_gnuplotData);
      m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotDataFile.GetFullName();

      wxFileOutputStream ostream(m_gnuplotSource);
      MemBufInputStream mistream(m_gnuplotSource_Compressed);
      wxZlibInputStream zistream(mistream);

      retval.clear();
      static const wxString needle = wxT("'<DATAFILENAME>'");
      const auto haystack = wxString() << wxT('\'') << m_gnuplotData << wxT('\'');
      if (!CopyAndFilterText(zistream, ostream, [haystack](auto &line){
            line.Replace(needle, haystack);
          }))
      {
        wxLogMessage(_("Cannot read gnuplot source!"));
        goto done;
      }
      retval = m_gnuplotSource;
    }
  done:;
    #ifdef HAVE_OMP_HEADER
    omp_unset_lock(&m_gnuplotLock);
    #endif
  }

  // Restore the data file, as well.
  GnuplotData();
  return retval;
}
 
wxSize Image::ToImageFile(const wxString &filename)
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  wxFileName fn(filename);
  wxString ext = fn.GetExt();
  if (filename.Lower().EndsWith(GetExtension().Lower()))
  {
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxSize(-1, -1);

    file.Write(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    if (file.Close())
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxSize(-1, -1);
  }

  if ((filename.Lower().EndsWith(".svg")) && (m_extension == "svgz"))
  {
    // Unzip the .svgz image
    wxFile file(filename, wxFile::write);
    if (!file.IsOpened())
      return wxSize(-1, -1);
    wxFileOutputStream ostream(file);
    if (DecompressDataInto(m_compressedImage, ostream))
      return wxSize(m_originalWidth, m_originalHeight);
    else
      return wxSize(-1, -1);    
  }
  else
  {
    wxBitmap bitmap = GetUnscaledBitmap();
    wxImage image = bitmap.ConvertToImage();
    wxBitmapType mimetype = wxBITMAP_TYPE_ANY;
    if ((ext.Lower() == wxT("jpg")) || (ext.Lower() == wxT("jpeg")))
      mimetype = wxBITMAP_TYPE_JPEG;
    else if (ext.Lower() == wxT("png"))
      mimetype = wxBITMAP_TYPE_PNG;
    else if (ext.Lower() == wxT("pcx"))
      mimetype = wxBITMAP_TYPE_PCX;
    else if (ext.Lower() == wxT("pnm"))
      mimetype = wxBITMAP_TYPE_PNM;
    else if ((ext.Lower() == wxT("tif")) || (ext.Lower() == wxT("tiff")))
      mimetype = wxBITMAP_TYPE_TIFF;
    else if (ext.Lower() == wxT("xpm"))
      mimetype = wxBITMAP_TYPE_XPM;
    else if (ext.Lower() == wxT("ico"))
      mimetype = wxBITMAP_TYPE_ICO;
    else if (ext.Lower() == wxT("cur"))
      mimetype = wxBITMAP_TYPE_CUR;
    else
      return (wxSize(-1, -1));

    if (!image.SaveFile(filename, mimetype))
      return wxSize(-1, -1);
    return image.GetSize();
  }
}

wxBitmap Image::GetBitmap(double scale) 
{
  // Recalculate contains its own WaitForLoad object.
  Recalculate(scale);
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif

  if (!m_isOk)
  {
    InvalidBitmap();
    return m_scaledBitmap;    
  }
  
  // Let's see if we have cached the scaled bitmap with the right size
  if (m_scaledBitmap.GetWidth() == m_width)
    return m_scaledBitmap;
  
  // Seems like we need to create a new scaled bitmap.
  if (m_svgRast)
  {
    // First create rgba data
    std::vector<unsigned char> imgdata(m_width*m_height*4);

    nsvgRasterize(m_svgRast.get(), m_svgImage.get(), 0,0,
                  ((double)m_width)/((double)m_originalWidth),
                  imgdata.data(), m_width, m_height, m_width*4);
    m_scaledBitmap = SvgBitmap::RGBA2wxBitmap(imgdata.data(), m_width, m_height);
    goto done;
  }
  else
  {
    wxImage img;
    if (!m_compressedImage.IsEmpty())
    {
      MemBufInputStream istream(m_compressedImage);
      img = wxImage(istream, wxBITMAP_TYPE_ANY);
    }
    m_isOk = true;

    if (img.Ok())
      m_scaledBitmap = wxBitmap(img);
    else
      InvalidBitmap();
  }

  // Make sure we stay within sane defaults
  if (m_width < 1) m_width = 1;
  if (m_height < 1) m_height = 1;

  // Create a scaled bitmap and return it.
  if (m_scaledBitmap.IsOk())
  {
    wxImage img = m_scaledBitmap.ConvertToImage();
    img.Rescale(m_width, m_height, wxIMAGE_QUALITY_BICUBIC);
    m_scaledBitmap = wxBitmap(img, 24);
  }
  else
    m_scaledBitmap = wxBitmap(1,1);
done:
  #ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_gnuplotLock);
  #endif
  return m_scaledBitmap;
}

void Image::InvalidBitmap()
{
  m_isOk = false;
  m_width = 800; m_height = 600;
  // Create a "image not loaded" bitmap.
  m_scaledBitmap.Create(m_width, m_height);
  
  wxString error;
  if (!m_imageName.empty())
    error = wxString::Format(_("Error: Cannot render %s."), m_imageName.utf8_str());
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
  m_scaledBitmap = m_scaledBitmap.ConvertToImage();
}

void Image::LoadImage(const wxBitmap &bitmap)
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  // Convert the bitmap to a png image we can use as m_compressedImage
  wxImage image = bitmap.ConvertToImage();
  m_isOk = image.IsOk();
  m_compressedImage.Clear();
  MemBufOutputStream mostream(m_compressedImage);
  image.SaveFile(mostream, wxBITMAP_TYPE_PNG);

  // Set the info about the image.
  m_extension = wxT("png");
  m_originalWidth = image.GetWidth();
  m_originalHeight = image.GetHeight();
  m_scaledBitmap.Create(1, 1);
  m_width = 1;
  m_height = 1;
}

// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
void Image::LoadImage(const wxString &image, std::shared_ptr<wxFileSystem> filesystem, bool remove)
{
  m_fs_keepalive_imagedata = filesystem;
  m_extension = wxFileName(image).GetExt();
  m_extension = m_extension.Lower();
  // If we don't have fine-grained locking using omp.h we don't profit from sending the
  // load process to the background and therefore load images from the main thread.
  // Loading images is of rather high priority as they are needed during the
  // recalculation that follows
  #ifdef HAVE_OMP_HEADER
  wxLogMessage(_("Starting background thread that loads an image"));
  #if HAVE_OPENMP_TASKS
  #pragma omp task
  #endif
  #endif
  LoadImage_Backgroundtask(image, filesystem, remove);
}

void Image::LoadImage_Backgroundtask(const wxString &image,
                                     std::shared_ptr<wxFileSystem> filesystem, bool remove)
{
#ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
#endif

  m_imageName = image;
  m_compressedImage.Clear();
  m_scaledBitmap.Create(1, 1);

  wxMemoryBuffer inputBinary;

  if (filesystem)
  {
    auto binary = ReadAll(image, filesystem.get());
    if (binary) inputBinary = *binary;
  }
  else
  {
    wxFile file;
    // Support relative and absolute paths.
    if(wxFileExists((*m_configuration)->GetWorkingDirectory() + wxT("/") + image))
      file.Open((*m_configuration)->GetWorkingDirectory() + wxT("/") + image);
    else
      file.Open(image);

    auto binary = ReadAll(file);
    if (binary)
    {
      inputBinary = *binary;
      file.Close();
      if (remove)
      {
        SuppressErrorDialogs logNull;
        wxRemoveFile(image);
      }
    }
  }

  m_isOk = false;

  wxImage Image;
  if (!inputBinary.IsEmpty())
  {
    if ((m_extension == "svg") || (m_extension == "svgz"))
    {
      m_isOk = false;
      wxMemoryBuffer svgContents;

      // Read the svg file's data into the system's memory
      if (m_extension == "svg")
      {
        // We already got the uncompressed file...
        svgContents = inputBinary;
        // ...but we want to compress the in-memory image for saving memory
        auto compressed = CompressData(svgContents);
        if (compressed) m_compressedImage = *compressed;
        m_extension += "z";
        m_imageName += "z";
      }
      else
      {
        // We already got the compressed file...
        m_compressedImage = inputBinary;
        // ...but we need to decompress the image to parse it
        auto contents = DecompressData(m_compressedImage);
        if (contents) svgContents = *contents;
      }

      // Parse the svg file's contents
      int ppi;
      if ((*m_configuration)->GetDC()->GetPPI().x > 50)
        ppi = (*m_configuration)->GetDC()->GetPPI().x;
      else
        ppi = 96;

      if (!svgContents.IsEmpty())
      {
        // zero-terminate the buffer
        svgContents.AppendByte('\0');
        m_svgImage.reset(nsvgParse(reinterpret_cast<char*>(svgContents.GetData()), "px", ppi));
        svgContents.Clear();
      }

      if (m_svgImage)
      {
        if (!m_svgRast)
          m_svgRast.reset(nsvgCreateRasterizer());
        if (m_svgRast)
          m_isOk = true;
        m_originalWidth = m_svgImage->width;
        m_originalHeight = m_svgImage->height;
      }
    }
    else
    {
      MemBufInputStream istream(inputBinary);
      Image.LoadFile(istream);
      m_originalWidth = 700;
      m_originalHeight = 300;

      if (Image.Ok())
      {
        m_originalWidth = Image.GetWidth();
        m_originalHeight = Image.GetHeight();
        m_isOk = true;
      }
      else
        InvalidBitmap();
    }
  }
  m_fs_keepalive_imagedata.reset();
  inputBinary = {};
#ifdef HAVE_OMP_HEADER
  omp_unset_lock(&m_imageLoadLock);
#endif
}

void Image::Recalculate(double scale)
{
  #ifdef HAVE_OMP_HEADER
  WaitForLoad waitforload(&m_imageLoadLock);
  #endif
  int width = m_originalWidth;
  int height = m_originalHeight;
  Configuration *configuration = (*m_configuration);

  // We want the image to get bigger if the user zooms in - and
  // if a high printing resolution requires us to scale everything up.
  // To also take care of the user's printing-scale,
  // the scale is passed by a parameter.

  // Ensure a minimum size for images.
  if (scale < 0.01) scale = 0.01;

  if ((width < 1) || (height < 1))
  {
    m_width = 700;
    m_height = 300;
    return;
  }

  int viewPortHeight = configuration->GetClientHeight();
  int viewPortWidth = configuration->GetClientWidth();

  if (viewPortHeight < 10)
    viewPortHeight = 10;
  if (viewPortWidth < 10)
    viewPortWidth = 10;

  if (!configuration->GetPrinting())
  {
    // Change the scale only if we are not printing.
    // Resetting the scale for printing makes the images become too small.
    scale = 1.0;
  }
  
  // Shrink to .9* the canvas size, if needed
  if (scale * width > .9 * viewPortWidth)
    scale = .9 * viewPortWidth / width;
  
  if (scale * height > .9 * viewPortHeight)
  {
    if (scale > .9 * viewPortHeight / height)
      scale = .9 * viewPortHeight / height;
  }

  // Shrink to be smaller than the maximum size.
  if ((m_maxWidth > 0) && (scale * width > m_maxWidth * (*m_configuration)->GetDC()->GetPPI().x))
    scale = m_maxWidth * (*m_configuration)->GetDC()->GetPPI().x / width;
  if ((m_maxHeight > 0) && (scale * height > m_maxHeight * (*m_configuration)->GetDC()->GetPPI().y))
    scale = m_maxHeight * (*m_configuration)->GetDC()->GetPPI().y / height;
  
  // Set the width of the scaled image
  m_height = (int) (scale * height);
  m_width = (int) (scale * width);

  if((m_height < 1) || (m_width < 1))
  {
    m_height = 100;
    m_width = 100;
  }
  // Clear this cell's image cache if it doesn't contain an image of the size
  // we need right now. Printing uses unscaled bitmaps, so the cache is left
  // unchanged then.
  if (!configuration->GetPrinting() && m_scaledBitmap.GetWidth() != m_width)
    ClearCache();
}
