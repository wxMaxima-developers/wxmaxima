// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file declares the class SlideShowCell

  SlideShowCell is the MathCell type that represents animations.
*/

#ifndef SLIDESHOWCELL_H
#define SLIDESHOWCELL_H

#include "MathCell.h"
#include "Image.h"
#include "CellPointers.h"
#include <wx/image.h>
#include <wx/timer.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>

#include <vector>

using namespace std;

class SlideShow : public MathCell
{
public:
  /*! The constructor

    \param framerate The individual frame rate that has to be set for this cell only. 
    If the default frame rate from the config is to be used instead this parameter 
    has to be set to -1.
   */
  SlideShow(MathCell *parent, Configuration **config, CellPointers *cellPointers, wxFileSystem *filesystem = NULL, int framerate = -1);

  ~SlideShow();

  bool IsOk(){return (m_size>0) && (m_images[m_displayed]->IsOk());}
  
  virtual wxString GetToolTip(const wxPoint &point);

  void MarkAsDeleted();

  /*! Remove all cached scaled images from memory

    To be called when the slideshow is outside of the displayed portion 
    of the screen; The bitmaps will be re-generated when needed.
   */
  virtual void ClearCache();

  void LoadImages(wxArrayString images);

  MathCell *Copy();

  void SelectInner(wxRect &rect, MathCell **first, MathCell **last)
  {
    *first = *last = this;
  }

  int GetDisplayedIndex()
  { return m_displayed; }

  wxImage GetBitmap(int n)
  { return m_images[n]->GetUnscaledBitmap().ConvertToImage(); }

  void SetDisplayedIndex(int ind);

  int Length()
  { return m_size; }

  //! Exports the image the slideshow currently displays
  wxSize ToImageFile(wxString filename);

  //! Exports the whole animation as animated gif
  wxSize ToGif(wxString filename);

  bool CopyToClipboard();

  /*! Get the frame rate of this SlideShow [in Hz].

    Returns either the frame rate set for this slide show cell individually or 
    the default frame rate chosen in the config.
   */
  int GetFrameRate();

  /*! Set the frame rate of this SlideShow [in Hz].
    
    \param Freq The requested frequency [in Hz] or -1 for: Use the default value.
    \return The frame rate that was actually set.
   */
  int SetFrameRate(int Freq);

  bool AnimationRunning() {return m_animationRunning;}
  wxTimer *AnimationRunning(bool run);
protected:
  wxTimer *m_timer;
  /*! The framerate of this cell.

    Can contain a frame rate [in Hz] or a -1, which means: Use the default frame rate.
  */
  int m_framerate;

  bool m_animationRunning;
  int m_size;
  int m_displayed;
  wxFileSystem *m_fileSystem;
  vector<Image *> m_images;

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  void Draw(wxPoint point, int fontsize);

  wxString ToString();

  wxString ToTeX();

  wxString ToRTF();

  wxString ToXML();

  virtual void DrawBoundingBox(wxDC &dc, bool all = false)
  {
    m_drawBoundingBox = true;
  }

private:
  bool m_drawBoundingBox;
  CellPointers *m_cellPointers;
};

#endif // SLIDESHOWCELL_H
