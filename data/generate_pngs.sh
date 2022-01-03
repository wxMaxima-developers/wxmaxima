#!/bin/sh

# convert SVG images to PNG and then to C Sourcecode (using xxd -i)

    cat >icon.h <<END
/* Automatically generated file using generate_pngs.sh                                  */
/* SVG images are converted to PNG (using inkscape) and then converted to C using xxd   */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2022 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

/* This program is free software; you can redistribute it and/or modify                 */
/* it under the terms of the GNU General Public License as published by                 */
/* the Free Software Foundation; either version 2 of the License, or                    */
/* (at your option) any later version.                                                  */

/* This program is distributed in the hope that it will be useful,                      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of                       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                        */
/* GNU General Public License for more details.                                         */

/* You should have received a copy of the GNU General Public License                    */
/* along with this program; if not, write to the Free Software                          */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA         */
END

for i in io.github.wxmaxima_developers.wxMaxima.svg; do
    PNGIMAGENAME=$(basename "$i" .svg).png
    echo "Converting image $i to $$PNGIMAGENAME"
    inkscape "--export-png=$PNGIMAGENAME" --export-width=128 "$i"
    which optipng >/dev/null && optipng -o 9 "$PNGIMAGENAME"  # optimize image, if optipng is available
    xxd -i "$PNGIMAGENAME" >> icon.h
done
