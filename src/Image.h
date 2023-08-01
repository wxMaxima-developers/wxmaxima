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
#include "Cell.h"
#include "Version.h"
#include <wx/image.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>
#include <wx/buffer.h>
#define NANOSVG_ALL_COLOR_KEYWORDS
#include "nanoSVG/nanosvg.h"
#include "nanoSVG/nanosvgrast.h"


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
  Image(Configuration *config, wxMemoryBuffer image, wxString type);

  /*! A constructor that loads a bitmap

    This constructor actually has to do some compression since we got
    the bitmap in an uncompressed form.
  */
  Image(Configuration *config, const wxBitmap &bitmap);

  /*! A constructor that loads an image

    \param config The pointer to the current configuration storage for the worksheet
    \param image The name of the file
    \param filesystem The filesystem to load it from
    \param remove true = Delete the file after loading it
  */
  Image(Configuration *config, wxString image,
        std::shared_ptr<wxFileSystem> &filesystem, bool remove = true);

  Image(Configuration *config, const Image &image);
  Image(const Image &image) = delete;

  ~Image();

  //! Converts rgba data to a wxBitmap
  static wxBitmap RGBA2wxBitmap(const unsigned char imgdata[],
                                const int &width, const int &height,
                                const int &scaleFactor = 1);
  
  void SetConfiguration(Configuration *config){ m_configuration = config; }
  //! Return the image's resolution
  int GetPPI() const {return m_ppi;}
  //! Set the image's resolution
  void SetPPI(int ppi) {m_ppi = ppi;}

  //! Creates a bitmap showing an error message
  void InvalidBitmap();

  /*! Sets the name of the gnuplot source and data file of this image

    Causes the files to be cached if they are not way too long; As the files
    are text-only they profit from being compressed and are stored in the 
    memory in their compressed form.
  */
  void GnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                     std::shared_ptr<wxFileSystem> &filesystem);

  /*! Loads the compressed gnuplot source and data file for this image

   */
  void CompressedGnuplotSource(wxString gnuplotFilename, wxString dataFilename,
                               std::shared_ptr<wxFileSystem> &filesystem);

  //! Load the gnuplot source file from the system's filesystem
  void GnuplotSource(wxString gnuplotFilename, wxString dataFilename)
    {
      std::shared_ptr<wxFileSystem> filesystem;
      GnuplotSource(std::move(gnuplotFilename), std::move(dataFilename),
                    filesystem /* system fs */);
    }

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
      if ((m_scaledBitmap.GetWidth() > 1) || (m_scaledBitmap.GetHeight() > 1))
        m_scaledBitmap.Create(1, 1);
    }
  
  //! Returns the file name extension of the current image
  wxString GetExtension() const;
  //! The maximum width this image shall be displayed with
  double GetMaxWidth() const {return m_maxWidth;}
  //! The maximum height this image shall be displayed with
  double GetHeightList() const {return m_maxHeight;}
  //! Set the maximum width this image shall be displayed with
  void   SetMaxWidth(double width){m_maxWidth = width;}
  //! Set the maximum height this image shall be displayed with
  void   SetMaxHeight(double height){m_maxHeight = height;}
  
  //! "Loads" an image from a bitmap
  void LoadImage(const wxBitmap &bitmap);
  
  //! Saves the image in its original form, or as .png if it originates in a bitmap
  wxSize ToImageFile(wxString filename);

  //! Returns the bitmap being displayed with custom scale
  wxBitmap GetBitmap(double scale = 1.0);

  //! Does the image show an actual image or an "broken image" symbol?
  bool IsOk() const;
  
  //! Returns the image in its unscaled form
  wxBitmap GetUnscaledBitmap();

  //! Can be called to specify a specific scale
  void Recalculate(double scale = 1.0);

  //! The width of the scaled image
  long m_width;
  //! The height of the scaled image
  long m_height;

  //! Returns the original image in its compressed form
  const wxMemoryBuffer GetCompressedImage() const;

  //! Returns the original width
  size_t GetOriginalWidth() const;

  //! Returns the original height
  size_t GetOriginalHeight() const;


  
  //! The image in its original compressed form
  wxMemoryBuffer m_compressedImage;

  //! Can this image be exported in SVG format?
  bool CanExportSVG() const {return m_svgRast != nullptr;}

  //! The tooltip to use wherever an image that's not Ok is shown.
  static const wxString &GetBadImageToolTip();

private:
  bool m_gnuplotDataThreadRunning = false;
  //  static std::atomic<int> m_numberOfThreads;
  //! A zipped version of the gnuplot commands that produced this image.
  wxMemoryBuffer m_gnuplotSource_Compressed;
  //! A zipped version of the gnuplot data needed in order to create this image.
  wxMemoryBuffer m_gnuplotData_Compressed;
  //! The width of the unscaled image
  size_t m_originalWidth;
  //! The height of the unscaled image
  size_t m_originalHeight;
  //! The bitmap, scaled down to the screen size
  wxBitmap m_scaledBitmap;
  //! The file extension for the current image type
  wxString m_extension;
  //! Does this image contain an actual image?
  bool m_isOk;
  //! The gnuplot source file for this image, if any.
  wxString m_gnuplotSource;
  //! The gnuplot data file for this image, if any.
  wxString m_gnuplotData;
  mutable std::thread m_loadImageTask;
  void LoadImage_Backgroundtask(std::unique_ptr<ThreadNumberLimiter> limiter);
  std::thread m_loadGnuplotSourceTask;
  void LoadGnuplotSource_Backgroundtask1(
    std::unique_ptr<ThreadNumberLimiter> limiter,
    std::shared_ptr<wxFileSystem> filesystem,
    std::shared_ptr<wxInputStream> gnuplotFile,
    std::shared_ptr<wxInputStream> dataFile
    );
  void LoadGnuplotSource_Backgroundtask2(
    std::unique_ptr<ThreadNumberLimiter> limiter,
    wxString gnuplotFile, wxString dataFile);
  void LoadGnuplotSource_Backgroundtask(
    wxInputStream *source,
    wxInputStream *data);
  void LoadCompressedGnuplotSource_Backgroundtask(std::unique_ptr<ThreadNumberLimiter> limiter,
                                                  std::shared_ptr<wxFileSystem> filesystem,
                                                  std::shared_ptr<wxFSFile> sourcefile,
                                                  std::shared_ptr<wxFSFile> datafile
                                                  );
  //! Loads an image from a file
  void LoadImage(wxString image, std::shared_ptr<wxFileSystem> &filesystem, bool remove = true);
  //! Reads the compressed image into a memory buffer
  static wxMemoryBuffer ReadCompressedImage(wxInputStream *data);
  Configuration *m_configuration;
  /*! The upper width limit for displaying this image

    \todo: Why is this a float and not an integer value?
  */
  double m_maxWidth;
  //! The upper height limit for displaying this image
  double m_maxHeight;
  //! The name of the image, if known.
  wxString m_imageName;
  //! The image resolution
  double m_ppi = 72;
  struct free_deleter { void operator()(void *p) const { std::free(p); } };
  NSVGimage* m_svgImage = {};
  std::unique_ptr<struct NSVGrasterizer, free_deleter> m_svgRast{nullptr};
};

#endif // IMAGE_H
