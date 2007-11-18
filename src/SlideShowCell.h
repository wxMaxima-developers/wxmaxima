///
///  Copyright (C) 2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef _SLIDESHOW_H_
#define _SLIDESHOW_H_

#include "MathCell.h"
#include <wx/image.h>

class SlideShow : public MathCell
{
public:
  SlideShow();
  ~SlideShow();
  void Destroy();
  void LoadImages(wxArrayString images);
  MathCell* Copy(bool all);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last)
  {
    *first = *last = this;
  }
  int GetDisplayedIndex() { return m_displayed; }
  void SetDisplayedIndex(int ind);
  int Length() { return m_size; }
protected:
  int m_size;
  int m_displayed;
  wxBitmap *m_bitmap;
  wxImage *m_images;
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
};

#endif //_ABSCELL_H_
