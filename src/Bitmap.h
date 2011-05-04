///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _BITMAP_H_
#define _BITMAP_H_

#include "MathCell.h"

class Bitmap
{
public:
  Bitmap();
  ~Bitmap();
  void SetData(MathCell* tree);
  bool ToFile(wxString file);
  bool ToClipboard();
protected:
  void DestroyTree();
  void RecalculateWidths();
  void BreakLines();
  void RecalculateSize();
  void GetMaxPoint(int* width, int* height);
  void BreakUpCells();
  void Layout();
  void Draw();
  MathCell *m_tree;
  wxBitmap m_bmp;
};

#endif
