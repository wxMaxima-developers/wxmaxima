// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2006-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef TIPOFTHEDAY_H
#define TIPOFTHEDAY_H

#include <wx/wx.h>
#include <wx/tipdlg.h>
#include <wx/arrstr.h>
#include <wx/image.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>

extern unsigned char media_playback_start_128_png[];
extern unsigned int  media_playback_start_128_png_len;
extern unsigned char media_playback_start_192_png[];
extern unsigned int  media_playback_start_192_png_len;
extern unsigned char media_playback_start_reverse_128_png[];
extern unsigned int  media_playback_start_reverse_128_png_len;
extern unsigned char media_playback_start_reverse_192_png[];
extern unsigned int  media_playback_start_reverse_192_png_len;
extern const char * invalidImage_xpm[];

/*! A minimalistic Tip of the day dialogue

  We roll our own dialogue here as the one from wxWidgets is modal (which
  means it blocks the application until it is closed) and Ubuntu's Focus 
  Stealing Prevention makes it pop up below wxMaxima (which means the user
  has no means of finding out it needs closing).

  For details see https://trac.wxwidgets.org/ticket/17974.
 */
class TipOfTheDay : public wxDialog
{
public:
  TipOfTheDay(wxWindow *parent);
  ~TipOfTheDay();
protected:
  void OnNextButton(wxCommandEvent &dummy);
  void OnPreviousButton(wxCommandEvent &dummy);
  void OnOkButton(wxCommandEvent &dummy);

private:
  wxString GetTip(unsigned int n);
  int m_num;
  wxTextCtrl *m_tip;
  wxString GetTip();
  wxCheckBox *m_showAtStartup;
  wxImage GetImage(unsigned char *data_128, size_t len_128,
                   unsigned char *data_192, size_t len_192);
  wxArrayString m_tips;
};

#endif // TIPOFTHEDAY_H
