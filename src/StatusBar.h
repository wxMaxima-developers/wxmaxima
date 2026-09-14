// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

  This file contains the definition of the class History that handles the recently
  issued commands for the history pane.
*/
#include "precomp.h"
#include <wx/wx.h>
#include <wx/bitmap.h>
#include <wx/image.h>
#include <wx/timer.h>
#include <wx/statbmp.h>
#include <wx/statusbr.h>
#include <wx/stattext.h>
#include <memory>

#ifndef STATUSBAR_H
#define STATUSBAR_H

extern unsigned char GO_NEXT_SVG_GZ[];
extern size_t GO_NEXT_SVG_GZ_SIZE;

/*! The class that draws the status bar
 */
class StatusBar : public wxStatusBar
{
public:
  /*! \param aiChatAvailable Whether the AI Chat feature is compiled in and
    usable on this system (see AiProvider::SecretStoreAvailable()) -- this
    is a fixed, startup-time property of the running binary, not whether a
    provider happens to be configured right now (that is what UpdateAiStatus()
    is for). Only if this is true does the status bar reserve a 4th field
    for the AI status icon at all; otherwise the status bar looks and
    behaves exactly as it did before this icon existed.
  */
  StatusBar(wxWindow *parent, int id, bool aiChatAvailable = false);
  virtual ~StatusBar();
  //! The network states that can be passed to NetworkStatus()
  enum networkState
  {
    idle,
    error,
    offline,
    receive,
    transmit
  };

  //! The states the AI status icon (4th status bar field) can be in
  enum class AiStatus
  {
    //! No AI provider is configured -- the icon is hidden entirely.
    None,
    //! A provider is configured and the last request (if any) succeeded.
    Active,
    //! A request to the provider is currently in flight -- mirrors how
    //! m_maximaStatus shows a "calculating" icon while Maxima is busy.
    Busy,
    //! A provider is configured, but the last request to it failed.
    Error
  };

  //! Update the bitmaps to the Right size for the Resolution
  void UpdateBitmaps();

  //! Informs the status bar about networking events.
  void NetworkStatus(networkState status);

  wxWindow *GetNetworkStatusElement()
    { return m_networkStatus; }

  wxWindow *GetStatusTextElement()
    { return m_statusTextPanel; }

  wxWindow *GetMaximaStatusElement()
    { return m_maximaStatus; }

  /*! The AI status icon, or NULL if aiChatAvailable was false at
    construction time (the AI Chat feature isn't compiled in / usable here).
    Callers must null-check before binding events to it or querying its
    screen position.
  */
  wxWindow *GetAiStatusElement()
    { return m_aiStatus; }

  /*! Update the AI status icon.

    \param status None hides the icon entirely (no provider configured).
    Active/Error show it with the corresponding bitmap/tooltip.
    \param detail Extra text (e.g. the last error message) appended to the
    tooltip for AiStatus::Error; ignored otherwise.
  */
  void UpdateAiStatus(AiStatus status, const wxString &detail = wxString());

  //! Inform the status bar how many percents of the available CPU power maxima uses
  void SetMaximaCPUPercentage(float percentage)
    {
      m_maximaPercentage = percentage;
      NetworkStatus(m_oldNetworkState);
    }

  enum MaximaStatus
  {
    wait_for_start,
    process_wont_start,
    sending,
    waiting,
    waitingForPrompt,
    waitingForAuth,
    calculating,
    parsing,
    transferring,
    userinput,
    maximaerror,
    //! Maxima's Lisp is stopped in a debugger (e.g. sbcl's LDB).
    debugging,
    //! Maxima's reader is in Lisp mode (after to_lisp() / a MAXIMA> prompt).
    lispmode,
    disconnected
  };

  void UpdateStatusMaximaBusy(MaximaStatus status, std::size_t bytesFromMaxima);
  /*! The exact bitmap m_maximaStatus itself would show for this status --
    i.e. the same per-status icon UpdateStatusMaximaBusy()'s switch already
    assigns via m_maximaStatus->SetBitmap(...), just returned instead of
    applied to that particular wxStaticBitmap. Lets TrayIcon (GH #2286)
    reuse these bitmaps instead of re-embedding the same art a second time
    -- the generated headers under art/statusbar (one per icon) define their
    byte arrays without `static`/`extern`, so #include-ing one from a second .cpp file
    is a duplicate-symbol link error, confirmed by trying exactly that
    first. NOT the m_network_* bitmaps: those belong to the separate
    m_networkStatus icon (raw socket send/receive activity, driven by
    HandleTimerEvent()), a different concern from "what is Maxima doing" --
    using them here first, before this comment, showed as a barely-visible
    speck in the tray because the "idle" one in that family is a very
    subtle, mostly-transparent glyph.
  */
  wxBitmap GetTrayIconBitmap(MaximaStatus status) const {
    switch (status) {
    case wait_for_start:
      return m_bitmap_waitForStart;
    case process_wont_start:
    case maximaerror:
      return m_bitmap_process_wont_start;
    case sending:
      return m_bitmap_sending;
    case waiting:
      return m_bitmap_waiting;
    case waitingForPrompt:
      return m_bitmap_waitingForPrompt;
    case waitingForAuth:
      return m_bitmap_waitingForAuth;
    case calculating:
      return m_bitmap_calculating;
    case parsing:
      return m_bitmap_parsing;
    case transferring:
      return m_bitmap_transferring;
    case userinput:
      return m_bitmap_userinput;
    case debugging:
      return m_bitmap_debugging;
    case lispmode:
      return m_bitmap_lispmode;
    case disconnected:
      return m_bitmap_disconnected;
    default:
      return m_bitmap_waiting;
    }
  }
  /*! Set the left status text

    Skips unchanged text: wxStaticText::SetLabel() re-sizes the label even
    for identical text, which on wxGTK re-layouts (and repaints) the whole
    frame - too expensive for a setter that is called from the idle loop.
  */
  void SetStatusText(wxString statusText){
    if (m_statusText->GetLabel() != statusText)
      m_statusText->SetLabel(statusText);
  }
protected:
  void StatusMsgDClick(wxMouseEvent &ev);
  void OnSize(wxSizeEvent &event);
  void OnTimerEvent(wxTimerEvent &event);

  void HandleTimerEvent();

private:
  std::unique_ptr<struct wxm_NSVGrasterizer, decltype(std::free)*> m_svgRast{nullptr, std::free};
  //! The display resolution
  wxSize m_ppi = wxSize(75, 75);
  /*! How many percents of the available CPU power does maxima use?

    See m_maximaPercentage and SetMaximaCPUPercentage()
  */
  float m_maximaPercentage = -1;
  /*! How many percents of the available CPU power did maxima use when updating the network icon last?

    See m_maximaPercentage and SetMaximaCPUPercentage()
  */
  float m_oldmaximaPercentage = -1;
  networkState m_oldNetworkState = receive;
  wxString m_stdToolTip;
  wxString m_networkErrToolTip;
  wxString m_noConnectionToolTip;
  //! The basic network state we currently display without receive or transmit info
  networkState m_networkState = offline;
  //! Does the icon show that we currently receive data?
  bool m_icon_shows_receive = false;
  //! Does the icon show that we currently transmit data?
  bool m_icon_shows_transmit = false;
  bool m_overlayIconIsSet = false;
  //! The background for m_statusText;
  wxPanel *m_statusTextPanel = NULL;
  //! The currently shown network status bitmap
  wxStaticText *m_statusText = NULL;
  //! The currently shown network status bitmap
  wxStaticBitmap *m_networkStatus = NULL;
  //! The currently shown network status bitmap
  wxStaticBitmap *m_maximaStatus = NULL;
  //! Whether the AI Chat feature is available at all, see the constructor.
  bool m_aiChatAvailable = false;
  //! The AI status icon (4th status bar field), or NULL if !m_aiChatAvailable
  wxStaticBitmap *m_aiStatus = NULL;
  //! The logical state UpdateAiStatus() was last called with, so UpdateBitmaps()
  //! can re-apply it with freshly-rescaled bitmaps after a PPI change.
  AiStatus m_aiStatusState = AiStatus::None;
  //! The detail text UpdateAiStatus() was last called with, see m_aiStatusState.
  wxString m_aiStatusDetail;
  //! The bitmap shown for AiStatus::Active
  wxBitmap m_bitmap_ai_active;
  //! The bitmap shown for AiStatus::Busy
  wxBitmap m_bitmap_ai_busy;
  //! The bitmap shown for AiStatus::Error
  wxBitmap m_bitmap_ai_error;
  //! The bitmap shown on network errors
  wxBitmap m_network_error;
  //! The bitmap shown while not connected to the network
  wxBitmap m_network_offline;
  //! The bitmap shown while transmitting data
  wxBitmap m_network_transmit;
  //! The bitmap shown while not transmitting or receiving data
  wxBitmap m_network_idle;
  //! The bitmap shown while not transmitting or receiving data and maxima not using CPU power
  wxBitmap m_network_idle_inactive;
  //! The bitmap shown while receiving data
  wxBitmap m_network_receive;
  //! The bitmap shown while simultaneously receiving and transmitting data
  wxBitmap m_network_transmit_receive;
  //! The timer that prolongs the showing of the "sending" bitmap a bit.
  wxTimer SendTimer;
  //! The timer that prolongs the showing of the "receiving" bitmap a bit.
  wxTimer ReceiveTimer;

  wxBitmap m_bitmap_waitForStart;
  wxBitmap m_bitmap_process_wont_start;
  wxBitmap m_bitmap_sending;
  wxBitmap m_bitmap_waiting;
  wxBitmap m_bitmap_waitingForPrompt;
  wxBitmap m_bitmap_waitingForAuth;
  wxBitmap m_bitmap_calculating;
  wxBitmap m_bitmap_parsing;
  wxBitmap m_bitmap_transferring;
  wxBitmap m_bitmap_userinput;
  wxBitmap m_bitmap_debugging;
  wxBitmap m_bitmap_lispmode;
  wxBitmap m_bitmap_disconnected;
};

#endif

