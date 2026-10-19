// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Turns mouse wheel rotation into whole wheel steps.

  A classic mouse wheel reports one event per notch, each carrying a rotation
  of exactly one wheel delta. Touchpads and high-resolution wheels (wxGTK on
  Wayland, macOS, precision touchpads on Windows) instead report many events
  carrying a small fraction of a delta each. Anything that reacts to the wheel
  in discrete steps -- the frame of an animation, the zoom factor -- would
  advance once per event on such a device, so a light touch flicks through
  dozens of steps. WheelSteps adds the fractions up and only reports a step
  once a whole delta has come together.
*/

#ifndef WHEELSTEPS_H
#define WHEELSTEPS_H

namespace wxm {

class WheelSteps {
public:
  /*! Add one wheel event's rotation; return the number of whole steps it completes.

    The sign of the result is the sign of the rotation (wx counts a vertical
    wheel turned away from the user as positive). Reversing the direction
    drops whatever was left over from the other direction, so the wheel reacts
    to the reversal as soon as a step's worth of movement in the new direction
    has come together, not only after first winding back the remainder.

    \param rotation wxMouseEvent::GetWheelRotation()
    \param delta    wxMouseEvent::GetWheelDelta(); a delta <= 0 (nothing sane
                    reports that) counts every non-zero rotation as one step.
  */
  int Add(int rotation, int delta) {
    if (delta <= 0)
      return (rotation > 0) - (rotation < 0);
    if ((rotation > 0 && m_rotation < 0) || (rotation < 0 && m_rotation > 0))
      m_rotation = 0;
    m_rotation += rotation;
    // Integer division truncates towards zero, so the remainder keeps the
    // sign of the movement in both directions.
    const int steps = m_rotation / delta;
    m_rotation -= steps * delta;
    return steps;
  }

  //! Forget any partial step, e.g. when the wheel starts acting on something else.
  void Reset() { m_rotation = 0; }

private:
  //! The rotation collected so far that doesn't yet make up a whole step.
  int m_rotation = 0;
};

} // namespace wxm

#endif // WHEELSTEPS_H
