#!/bin/sh

# convert SVG images to PNG and then to C Sourcecode (using xxd -i)

for d in config statusbar toolbar; do
    cd "$d" || exit
    cat >images.h <<END
/* Automatically generated file using generate_pngs.sh                                  */
/* SVG images are converted to PNG (using inkscape) and then converted to C using xxd   */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2018 wxMaxima Team (https://andrejv.github.io/wxmaxima/)               */

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
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA            */
END

    for i in *.svg; do
        for j in 128 192; do
             PNGIMAGENAME=$(basename "$i" .svg).$j.png
             echo "Converting image $d/$i to $d/$PNGIMAGENAME"
             inkscape "--export-png=$PNGIMAGENAME" --export-width=$j "$i"
             which optipng >/dev/null && optipng -o 7 "$PNGIMAGENAME"  # optimize image, if optipng is available
             xxd -i "$PNGIMAGENAME" >> images.h
             rm -f "$PNGIMAGENAME"
        done
    done
    cd ..
done


for d in draw; do
    cd "$d" || exit
    cat >images.h <<END
/* Automatically generated file using generate_pngs.sh                                  */
/* SVG images are converted to PNG (using inkscape) and then converted to C using xxd   */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2018 wxMaxima Team (https://andrejv.github.io/wxmaxima/)               */

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
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA            */
END

    for i in *.png; do
	pngtopnm $i|pnmquant 256|pnmtopng >tmp
	optipng -o 9 tmp
	mv tmp $i
        xxd -i $i >> images.h
    done
    cd ..
done


