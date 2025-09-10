#!/bin/sh

# convert Unicodedata.txt to C Sourcecode (using xxd -i)

UNICODEURL="https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt"

cat >UnicodeData.h <<END
/* Automatically generated file using generate_unicodedata.sh                           */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2024 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

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

/* The license of UnicodeData.txt can be found at: https://www.unicode.org/license.txt  */
/* and is included in here as unicode-license.txt                                       */
/* UnicodeData.txt was downloaded from:                                                 */
/* $UNICODEURL                            */

#ifndef WXM_UNICODEDATA_H
#define WXM_UNICODEDATA_H
END

echo "Fetching UnicodeData.txt from: $UNICODEURL"
curl $UNICODEURL >UnicodeData.txt
echo "Converting UnicodeData.txt to embeddable C code UnicodeData.h"
cut -d ";" -f 1-2 <UnicodeData.txt | gzip -c -n >UnicodeData.txt.gz
xxd -i "UnicodeData.txt.gz" >>UnicodeData.h
echo "#endif" >>UnicodeData.h
rm -f UnicodeData.txt.gz UnicodeData.txt

