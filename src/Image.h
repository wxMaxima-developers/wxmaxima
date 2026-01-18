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

#ifndef IMAGE_H
#define IMAGE_H

#include <memory>
#include <thread>
#include "ThreadNumberLimiter.h"
#include "precomp.h"
#include "Version.h"
#include "Configuration.h"
#include <wx/image.h>
#include <wx/buffer.h>
#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanosvg_private.h"
#include "nanosvgrast_private.h"


/*! Manages an auto-scaling image

  This class keeps two versions of an image:
  - The image in its original compressed format. This way the image can losslessly
  be exported later on
  - A bitmap version of the image that is scaled down to a size that makes sense
  with the current viewport.

  Storing images this way has many advantages:
  - It allows us to restrict scaling operations to only once on actually drawing
  the image after a resize of the wxMaxima window.
  - We can keep (and therefore export and save) jpeg photos in their original
  format instead of using the .png compression that is superior for line art -
  but not for depicting real-live images.
  - We save time on saving the .wxmx file since image compression, if done good,
  might need a good deal of time and this class never needs to compress images
  itself.
  - It allows images to keep their metadata, if needed
  - and if we have big images (big plots or for example photographs) we don't need
  to store them in their uncompressed form.
  - One could even delete the cached scaled images for all cells that currently
  are off-screen in order to save memory.
*/
class Image final
{
public:
  //! A constructor that generates an empty image. See LoadImage()
  explicit Image(Configuration *config);

  //! A constructor that loads the compressed file from a wxMemoryBuffer
  Image(Configuration *config, const wxMemoryBuffer &image, const wxString &type);

  /*! A constructor that loads a bitmap

    This constructor actually has to do some compression since we got
    the bitmap in an uncompressed form.
  */
  Image(Configuration *config, const wxBitmap &bitmap);

  /*! A constructor that loads an image

    \param config The pointer to the current configuration storage for the worksheet
    \param image The name of the file
    \param wxmxFile The wxmx file to load it from.
    Empty when loading from a standalone file from the filesystem.
    \param remove true = Delete the file after loading it
  */
  Image(Configuration *config, const wxString &image,
        const wxString &wxmxFile, bool remove = true);

  Image(Configuration *config, const Image &image);
  Image(const Image &image) = delete;

  virtual ~Image();

  //! Converts rgba data to a wxBitmap
  static wxBitmap RGBA2wxBitmap(const unsigned char imgdata[],
                                const int &width, const int &height);

  void SetConfiguration(Configuration *config){
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    m_configuration = config; }
  //! Return the image's resolution
  int GetPPI() const {
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    return m_ppi;}
  //! Set the image's resolution
  void SetPPI(int ppi) {
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    m_ppi = ppi;}

  //! Creates a bitmap showing an error message
  void InvalidBitmap(const wxString &message = wxEmptyString);

  /*! Sets the name of the gnuplot source and data file of this image

    Causes the files to be cached if they are not way too long; As the files
    are text-only they profit from being compressed and are stored in the
    memory in their compressed form.
  */
  void GnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                     const wxString &wxmxFile = wxEmptyString);

  /*! Loads the compressed gnuplot source and data file for this image

   */
  void CompressedGnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                               const wxString &wxmxFile = wxEmptyString);

/*! Returns the gnuplot source file name of this image

  If maxima has deleted the temporary file in the meantime or if it comes from
  a .wxmx file and has never been created from maxima the file is created by this
  function.

  If the file cannot be created (for example if no gnuplot source exists/
  is known) this function returns wxEmptyString.
*/
  wxString GnuplotSource();
  /*! Returns the gnuplot data file name of this image

    If maxima has deleted the temporary file in the meantime or if it comes from
    a .wxmx file and has never been created from maxima the file is created by this
    function.

    If the file cannot be created (for example if no gnuplot source exists/
    is known) this function returns wxEmptyString.
  */
  wxString GnuplotData();

  //! Returns the gnuplot source of this image
  const wxMemoryBuffer GetGnuplotSource();
  const wxMemoryBuffer GetCompressedGnuplotSource();
  //! Returns the gnuplot data of this image
  const wxMemoryBuffer GetGnuplotData();
  const wxMemoryBuffer GetCompressedGnuplotData();

  /*! Temporarily forget the scaled image in order to save memory

    Will recreate the scaled image as soon as needed.
  */
  void ClearCache()
    {
      if(m_loadImageTask.joinable())
        m_loadImageTask.join();
      if ((m_scaledBitmap.GetWidth() > 1) || (m_scaledBitmap.GetHeight() > 1))
        m_scaledBitmap.Create(1, 1);
    }

  //! Returns the file name extension of the current image
  wxString GetExtension() const;
  //! The maximum width this image shall be displayed with
  double GetMaxWidth() const {
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    return m_maxWidth;}
  //! The maximum height this image shall be displayed with
  double GetHeightList() const {
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    return m_maxHeight;}
  //! Set the maximum width this image shall be displayed with
  void   SetMaxWidth(double width){
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    m_maxWidth = width;
  }
  //! Set the maximum height this image shall be displayed with
  void   SetMaxHeight(double height){
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    m_maxHeight = height;
  }

  //! "Loads" an image from a bitmap
  void LoadImage(const wxBitmap &bitmap);

  //! Saves the image in its original form, or as .png if it originates in a bitmap
  wxSize ToImageFile(wxString filename);

  //! Returns the bitmap being displayed with custom scale
  wxBitmap GetBitmap(double scale = 1.0);

  //! Returns the image in its unscaled form
  wxBitmap GetUnscaledBitmap();

  //! Can be called to specify a specific scale
  void Recalculate(double scale = 1.0);

  //! The width of the scaled image
  long m_width = 1;
  //! The height of the scaled image
  long m_height = 1;

  //! Returns the original image in its compressed form
  const wxMemoryBuffer GetCompressedImage() const;

  //! Returns the original width
  std::size_t GetOriginalWidth() const;

  //! Returns the original height
  std::size_t GetOriginalHeight() const;



  //! The image in its original compressed form
  wxMemoryBuffer m_compressedImage;

  //! Can this image be exported in SVG format?
  bool CanExportSVG() const {
    if(m_loadImageTask.joinable())
      m_loadImageTask.join();
    return m_svgRast != nullptr;}

  //! The tooltip to use wherever an image that's not Ok is shown.
  static const wxString GetBadImageToolTip();

  class WxmxStream: public wxZipInputStream
  {
  public:
    WxmxStream(wxInputStream &wxmxFile, const wxString &fileInWxmx);
  };

  bool HasGnuplotSource() const {return m_gnuplotSource_Compressed.GetDataLen() > 20;}
private:
  bool m_fromWxFS = false;
  //! A zipped version of the gnuplot commands that produced this image.
  wxMemoryBuffer m_gnuplotSource_Compressed;
  //! A zipped version of the gnuplot data needed in order to create this image.
  wxMemoryBuffer m_gnuplotData_Compressed;
  //! The width of the unscaled image
  std::size_t m_originalWidth = 640;
  //! The height of the unscaled image
  std::size_t m_originalHeight = 480;
  //! The bitmap, scaled down to the screen size
  wxBitmap m_scaledBitmap;
  //! The file extension for the current image type
  wxString m_extension;
  //! The gnuplot source file for this image, if any.
  wxString m_gnuplotSource;
  //! The gnuplot data file for this image, if any.
  wxString m_gnuplotData;
  mutable jthread m_loadImageTask;
  void LoadImage_Backgroundtask(std::unique_ptr<ThreadNumberLimiter> limiter,
                                wxString image, wxString wxmxFile,
                                bool remove);
  jthread m_loadGnuplotSourceTask;
  void LoadGnuplotSource_Backgroundtask(
    std::unique_ptr<ThreadNumberLimiter> limiter,
    wxString gnuplotFile, wxString dataFile, wxString wxmxFile);
  void LoadGnuplotSource(wxInputStream *source);
  void LoadGnuplotData(wxInputStream *data);
  void LoadGnuplotSource_Backgroundtask_internal(
    wxInputStream *source,
    wxInputStream *data);


  void LoadCompressedGnuplotSource_Backgroundtask(std::unique_ptr<ThreadNumberLimiter> limiter,
                                                  wxString sourcefile,
                                                  wxString datafile,
                                                  wxString wxmxFile
    );
  //! Loads an image from a file
  void LoadImage(wxString image, const wxString &wxmxFile, bool remove = true);
  //! Reads the compressed image into a memory buffer
  static wxMemoryBuffer ReadCompressedImage(wxInputStream *data);
  Configuration *m_configuration = NULL;
  /*! The upper width limit for displaying this image
   */
  wxCoord m_maxWidth = -1;
  //! The upper height limit for displaying this image
  wxCoord m_maxHeight = -1;
  //! The name of the image, if known.
  wxString m_imageName;
  //! The image resolution
  double m_ppi = 72;
  struct free_deleter { void operator()(void *p) const { std::free(p); } };
  wxm_NSVGimage* m_svgImage = {};
  std::unique_ptr<struct wxm_NSVGrasterizer, free_deleter> m_svgRast{nullptr};
};

#endif // IMAGE_H
