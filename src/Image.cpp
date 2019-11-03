// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Image that stores compressed images and handles scaling and uncompressing them.
*/

#include "Image.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/regex.h>
#include <wx/stdpaths.h>

wxMemoryBuffer Image::ReadCompressedImage(wxInputStream *data)
{
  wxMemoryBuffer retval;

  char *buf = new char[8192];

  while (data->CanRead())
  {
    data->Read(buf, 8192);
    size_t siz;
    retval.AppendData(buf, siz = data->LastRead());
  }

  delete[] buf;
  return retval;
}

wxBitmap Image::GetUnscaledBitmap()
{
  wxMemoryInputStream istream(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
  wxImage img(istream, wxBITMAP_TYPE_ANY);
  wxBitmap bmp;
  if (img.Ok())
    bmp = wxBitmap(img);
  return bmp;
}

Image::Image(Configuration **config)
{
  m_configuration = config;
  m_width = 1;
  m_height = 1;
  m_originalWidth = 1;
  m_originalHeight = 1;
  m_scaledBitmap.Create(1, 1);
  m_isOk = false;
  m_maxWidth = -1;
  m_maxHeight = -1;
}

Image::Image(Configuration **config, wxMemoryBuffer image, wxString type)
{
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_compressedImage = image;
  m_extension = type;
  m_width = 1;
  m_height = 1;
  m_originalWidth = 640;
  m_originalHeight = 480;

  wxImage Image;
  if (m_compressedImage.GetDataLen() > 0)
  {
    wxMemoryInputStream istream(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    Image.LoadFile(istream);
    m_isOk = Image.IsOk();
    m_originalWidth = Image.GetWidth();
    m_originalHeight = Image.GetHeight();
  }
  m_maxWidth = -1;
  m_maxHeight = -1;
}

Image::Image(Configuration **config, const wxBitmap &bitmap)
{
  m_configuration = config;
  m_width = 1;
  m_height = 1;
  m_maxWidth = -1;
  m_maxHeight = -1;
  LoadImage(bitmap);
}

// constructor which loads an image
Image::Image(Configuration **config, wxString image, bool remove, wxFileSystem *filesystem)
{
  m_configuration = config;
  m_scaledBitmap.Create(1, 1);
  m_width = 1;
  m_height = 1;
  m_maxWidth = -1;
  m_maxHeight = -1;
  LoadImage(image, remove, filesystem);
}

Image::~Image()
{
  if(m_gnuplotSource != wxEmptyString)
  {
    if(wxFileExists(m_gnuplotSource))
      wxRemoveFile(m_gnuplotSource);

    if(wxFileExists(m_gnuplotData))
      wxRemoveFile(m_gnuplotData);

    wxString popoutname = m_gnuplotSource + wxT(".popout");
    if(wxFileExists(popoutname))
      wxRemoveFile(popoutname);
    
  }
}

void Image::GnuplotSource(wxString gnuplotFilename, wxString dataFilename, wxFileSystem *filesystem)
{
  m_gnuplotSource = gnuplotFilename;
  m_gnuplotData = dataFilename;

  if(filesystem == NULL)
  {
    if(!wxFileExists(dataFilename))
      return;
    
    // Don't cache the data for unreasonably long files.
    wxStructStat strucStat;
    wxStat(dataFilename, &strucStat);
    if (strucStat.st_size > 25*1000*1000)
      return;

    // The gnuplot source of the image is cached in a compressed form:
    //
    // as it is text-only and contains many redundancies it will get way
    // smaller this way.
    {
      wxFileInputStream input(m_gnuplotSource);
      if(!input.IsOk())
        return;
      wxTextInputStream textIn(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      
      wxMemoryOutputStream mstream;
      int zlib_flags;
      if(wxZlibOutputStream::CanHandleGZip())
        zlib_flags = wxZLIB_GZIP;
      else
        zlib_flags = wxZLIB_ZLIB;
      wxZlibOutputStream zstream(mstream,wxZ_BEST_COMPRESSION,zlib_flags);
      wxTextOutputStream textOut(zstream);
      wxString line;
      
      // A RegEx that matches the name of the data file (needed if we ever want to
      // move a data file into the temp directory of a new computer that locates its
      // temp data somewhere strange).
      wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[0-9]*\\.data'");
      while(!input.Eof())
      {
        line = textIn.ReadLine();
        if(replaceDataFileName.Matches(line))
        {
          wxString dataFileName;
          dataFileName = replaceDataFileName.GetMatch(line);
          replaceDataFileName.Replace(&line,wxT("'<DATAFILENAME>'"));
        }
        textOut << line + wxT("\n");
      }
      textOut.Flush();
      zstream.Close();
      
      m_gnuplotSource_Compressed.Clear();
      m_gnuplotSource_Compressed.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                                            mstream.GetOutputStreamBuffer()->GetBufferSize());
    }
    
    {
      wxFileInputStream input(m_gnuplotData);
      if(!input.IsOk())
        return;
      wxTextInputStream textIn(input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
      
      wxMemoryOutputStream mstream;
      int zlib_flags;
      if(wxZlibOutputStream::CanHandleGZip())
        zlib_flags = wxZLIB_GZIP;
      else
        zlib_flags = wxZLIB_ZLIB;
      wxZlibOutputStream zstream(mstream,wxZ_BEST_COMPRESSION,zlib_flags);
      wxTextOutputStream textOut(zstream);
      wxString line;
      
      while(!input.Eof())
      {
      line = textIn.ReadLine();
      textOut << line + wxT("\n");
      }
      textOut.Flush();
      zstream.Close();
      
      m_gnuplotData_Compressed.Clear();
      m_gnuplotData_Compressed.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                                          mstream.GetOutputStreamBuffer()->GetBufferSize());
    }
  }
  else
  {
    {
      wxFSFile *fsfile = filesystem->OpenFile(m_gnuplotSource);
      if (fsfile)
      { // open successful
        wxInputStream *input = fsfile->GetStream();
          if(input->IsOk())
          {
            wxTextInputStream textIn(*input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));

            wxMemoryOutputStream mstream;
            int zlib_flags;
            if(wxZlibOutputStream::CanHandleGZip())
              zlib_flags = wxZLIB_GZIP;
            else
              zlib_flags = wxZLIB_ZLIB;
            wxZlibOutputStream zstream(mstream,wxZ_BEST_COMPRESSION,zlib_flags);
            wxTextOutputStream textOut(zstream);
            wxString line;
            
            // A RegEx that matches the name of the data file (needed if we ever want to
            // move a data file into the temp directory of a new computer that locates its
            // temp data somewhere strange).
            wxRegEx replaceDataFileName("'[^']*maxout_[^']*_[0-9*]\\.data'");
            while(!input->Eof())
            {
              line = textIn.ReadLine();
              if(replaceDataFileName.Matches(line))
              {
                wxString dataFileName;
                dataFileName = replaceDataFileName.GetMatch(line);
                if(dataFileName != wxEmptyString)
                  wxLogMessage(_("Gnuplot Data File Name: ") + dataFileName);
                replaceDataFileName.Replace(&line,wxT("'<DATAFILENAME>'"));
              }
              textOut << line + wxT("\n");
            }
            textOut.Flush();
            zstream.Close();
            m_gnuplotSource_Compressed.Clear();
            m_gnuplotSource_Compressed.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                                                  mstream.GetOutputStreamBuffer()->GetBufferSize());
            wxDELETE(input);
          }
      }
    }
    {
      wxFSFile *fsfile = filesystem->OpenFile(m_gnuplotData);
      if (fsfile)
      { // open successful
        wxInputStream *input = fsfile->GetStream();
        if(input->IsOk())
        {
          wxTextInputStream textIn(*input, wxT('\t'), wxConvAuto(wxFONTENCODING_UTF8));
            
          wxMemoryOutputStream mstream;
          int zlib_flags;
          if(wxZlibOutputStream::CanHandleGZip())
            zlib_flags = wxZLIB_GZIP;
          else
            zlib_flags = wxZLIB_ZLIB;
          wxZlibOutputStream zstream(mstream,wxZ_BEST_COMPRESSION,zlib_flags);
          wxTextOutputStream textOut(zstream);
          wxString line;
            
          while(!input->Eof())
          {
            line = textIn.ReadLine();
            textOut << line + wxT("\n");
          }
          textOut.Flush();
          zstream.Close();
            
          m_gnuplotData_Compressed.Clear();
          m_gnuplotData_Compressed.AppendData(mstream.GetOutputStreamBuffer()->GetBufferStart(),
                                              mstream.GetOutputStreamBuffer()->GetBufferSize());
        }
      }
    }
  }
}

wxMemoryBuffer Image::GetGnuplotSource()
{
  wxMemoryBuffer retval;
  
  wxMemoryOutputStream output;
  wxTextOutputStream textOut(output);
  if(!output.IsOk())
    return retval;

  wxMemoryInputStream mstream(
    m_gnuplotSource_Compressed.GetData(),
    m_gnuplotSource_Compressed.GetDataLen()
    );
  wxZlibInputStream zstream(mstream);
  wxTextInputStream textIn(zstream);
  wxString line;
  
  while(!mstream.Eof())
  {
    line = textIn.ReadLine();
    textOut << line + wxT("\n");
  }
  textOut.Flush();

  retval.AppendData(output.GetOutputStreamBuffer()->GetBufferStart(),
                    output.GetOutputStreamBuffer()->GetBufferSize());
  return retval;
}

wxMemoryBuffer Image::GetGnuplotData()
{
  wxMemoryBuffer retval;
  
  wxMemoryOutputStream output;
  wxTextOutputStream textOut(output);
  if(!output.IsOk())
    return retval;

  wxMemoryInputStream mstream(
    m_gnuplotData_Compressed.GetData(),
    m_gnuplotData_Compressed.GetDataLen()
    );
  wxZlibInputStream zstream(mstream);
  wxTextInputStream textIn(zstream);
  wxString line;
  
  while(!mstream.Eof())
  {
    line = textIn.ReadLine();
    textOut << line + wxT("\n");
  }
  textOut.Flush();

  retval.AppendData(output.GetOutputStreamBuffer()->GetBufferStart(),
                    output.GetOutputStreamBuffer()->GetBufferSize());
  return retval;
}

wxString Image::GnuplotData()
{
  if((m_gnuplotData == wxEmptyString) || (wxFileExists(m_gnuplotData)))
    return m_gnuplotData;

  // Move the gnuplot data and data file into our temp directory
  wxFileName gnuplotSourceFile(m_gnuplotSource);
  m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotSourceFile.GetFullName();
  wxFileName gnuplotDataFile(m_gnuplotData);
  m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotDataFile.GetFullName();

  wxFileOutputStream output(m_gnuplotData);
  wxTextOutputStream textOut(output);
  if(!output.IsOk())
    return m_gnuplotData;

  wxMemoryInputStream mstream(
    m_gnuplotData_Compressed.GetData(),
    m_gnuplotData_Compressed.GetDataLen()
    );
  wxZlibInputStream zstream(mstream);
  wxTextInputStream textIn(zstream);
  wxString line;
  
  while(!mstream.Eof())
  {
    line = textIn.ReadLine();
    textOut << line + wxT("\n");
  }
  textOut.Flush();
  
  return m_gnuplotData;
}

wxString Image::GnuplotSource()
{
  if((m_gnuplotSource == wxEmptyString) || (wxFileExists(m_gnuplotSource)))
    return m_gnuplotSource;

  // Move the gnuplot source and data file into our temp directory
  wxFileName gnuplotSourceFile(m_gnuplotSource);
  m_gnuplotSource = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotSourceFile.GetFullName();
  wxFileName gnuplotDataFile(m_gnuplotData);
  m_gnuplotData = wxStandardPaths::Get().GetTempDir() + "/" + gnuplotDataFile.GetFullName();
  
  wxFileOutputStream output(m_gnuplotSource);
  wxTextOutputStream textOut(output);
  if(!output.IsOk())
    return m_gnuplotSource;

  wxMemoryInputStream mstream(
    m_gnuplotSource_Compressed.GetData(),
    m_gnuplotSource_Compressed.GetDataLen()
    );
  wxZlibInputStream zstream(mstream);
  wxTextInputStream textIn(zstream);
  wxString line;
  
  while(!mstream.Eof())
  {
    line = textIn.ReadLine();
    line.Replace(wxT("'<DATAFILENAME>'"),wxT("'")+m_gnuplotData+wxT("'"));
    textOut << line + wxT("\n");
  }
  textOut.Flush();
  // Restore the data file, as well.
  GnuplotData();
  
  return m_gnuplotSource;
}

wxSize Image::ToImageFile(wxString filename)
{
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
  Recalculate(scale);

  // Let's see if we have cached the scaled bitmap with the right size
  if (m_scaledBitmap.GetWidth() == m_width)
    return m_scaledBitmap;


  // Seems like we need to create a new scaled bitmap.
  if (m_scaledBitmap.GetWidth() != m_width)
  {
    wxImage img;
    if (m_compressedImage.GetDataLen() > 0)
    {
      wxMemoryInputStream istream(m_compressedImage.GetData(), m_compressedImage.GetDataLen());

      img = wxImage(istream, wxBITMAP_TYPE_ANY);
    }
    m_isOk = true;

    if (img.Ok())
      m_scaledBitmap = wxBitmap(img);
    else
    {
      m_isOk = false;
      // Create a "image not loaded" bitmap.
      m_scaledBitmap.Create(m_width, m_height);

      wxString error;
      if(m_imageName != wxEmptyString)
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
    }
  }

  // Make sure we stay within sane defaults
  if (m_width < 1)m_width = 1;
  if (m_height < 1)m_height = 1;

  // Create a scaled bitmap and return it.
  wxImage img = m_scaledBitmap.ConvertToImage();
  img.Rescale(m_width, m_height, wxIMAGE_QUALITY_BICUBIC);
  m_scaledBitmap = wxBitmap(img, 24);
  return m_scaledBitmap;
}

void Image::LoadImage(const wxBitmap &bitmap)
{
  // Convert the bitmap to a png image we can use as m_compressedImage
  wxImage image = bitmap.ConvertToImage();
  m_isOk = image.IsOk();
  wxMemoryOutputStream stream;
  image.SaveFile(stream, wxBITMAP_TYPE_PNG);
  m_compressedImage.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                               stream.GetOutputStreamBuffer()->GetBufferSize());

  // Set the info about the image.
  m_extension = wxT("png");
  m_originalWidth = image.GetWidth();
  m_originalHeight = image.GetHeight();
  m_scaledBitmap.Create(1, 1);
  m_width = 1;
  m_height = 1;
}

void Image::LoadImage(wxString image, bool remove, wxFileSystem *filesystem)
{
  m_imageName = image;
  m_compressedImage.Clear();
  m_scaledBitmap.Create(1, 1);

  if (filesystem)
  {
    wxFSFile *fsfile = filesystem->OpenFile(image);
    if (fsfile)
    { // open successful

      wxInputStream *istream = fsfile->GetStream();

      m_compressedImage = ReadCompressedImage(istream);
    }

    // Closing and deleting fsfile is important: If this line is missing
    // opening .wxmx files containing hundreds of images might lead to a
    // "too many open files" error.
    wxDELETE(fsfile);
  }
  else
  {
    wxFile file;
    // Support relative and absolute paths.
    if(wxFileExists((*m_configuration)->GetWorkingDirectory() + wxT("/") + image))
      file.Open((*m_configuration)->GetWorkingDirectory() + wxT("/") + image);
    else
      file.Open(image);

    if (file.IsOpened())
    {
      wxFileInputStream strm(file);
      bool ok = strm.IsOk();
      if (ok)
        m_compressedImage = ReadCompressedImage(&strm);

      file.Close();
      if (ok && remove)
        wxRemoveFile(image);
    }
  }

  m_isOk = false;
  
  wxImage Image;
  if (m_compressedImage.GetDataLen() > 0)
  {
    wxMemoryInputStream istream(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    Image.LoadFile(istream);
  }

  m_extension = wxFileName(image).GetExt();

  m_originalWidth = 700;
  m_originalHeight = 300;

  if (Image.Ok())
  {
    m_originalWidth = Image.GetWidth();
    m_originalHeight = Image.GetHeight();
    m_isOk = true;
  }
  else
    m_isOk = false;

  Recalculate();

}

void Image::Recalculate(double scale)
{
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
    scale = 1;
    m_height = (int) (scale * height);
    m_width = (int) (scale * width);
  }

  if((m_height < 1) || (m_width < 1))
  {
    m_height = 100;
    m_width = 100;
  }
// Clear this cell's image cache if it doesn't contain an image of the size
  // we need right now.
  if (m_scaledBitmap.GetWidth() != m_width)
    ClearCache();
}
