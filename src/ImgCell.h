//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef IMGCELL_H
#define IMGCELL_H

#include "MathCell.h"
#include <wx/image.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>

class ImgCell : public MathCell
{
public:
  ImgCell();
  ImgCell(wxString image, bool remove = true, wxFileSystem *filesystem = NULL);
  ~ImgCell();
  void Destroy();
  void LoadImage(wxString image, bool remove = true);
  MathCell* Copy(bool all);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last)
  {
    *first = *last = this;
  }
  friend class SlideShow;
  bool ToImageFile(wxString filename);
  void SetBitmap(wxBitmap bitmap);
  bool CopyToClipboard();
  // These methods should only be used for saving wxmx files
  // and are shared with SlideShowCell.
  static void WXMXResetCounter() { s_counter = 0; }
  static wxString WXMXGetNewFileName();
  static int WXMXImageCount() { return s_counter; }
  void DrawRectangle(bool draw) { m_drawRectangle = draw; }
protected:
  wxBitmap *m_bitmap;
  wxFileSystem *m_fileSystem;
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
	wxString ToXML(bool all);
	static int s_counter;
	bool m_drawRectangle;
};

#endif // IMGCELL_H
