#!/bin/sh

# convert wxMathml.lisp and COPYING to C Sourcecode (using xxd -i)

cat >wxMathML.h <<END
/* Automatically generated file using generate_wxmathml.sh                              */
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

echo "Converting wxMathML.lisp to embeddable C code"
gzip -c -n wxMathML.lisp >wxMathML.lisp.gz
xxd -i wxMathML.lisp.gz >>wxMathML.h
rm -f wxMathML.lisp.gz


echo "Converting ../GPL.txt to embeddable C code"
cat >License.h <<END
/* Automatically generated file using generate_wxmathml.sh                              */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2020 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

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

gzip -c -n ../GPL.txt >License.gz
xxd -i "License.gz" >>License.h
rm -f License.gz

echo "Converting manual_anchors.xml to embeddable C code"
cat >manual_anchors.xml.gz.h <<END
/* Automatically generated file using generate_wxmathml.sh                              */
/* This file is part of wxMaxima.                                                       */

/* Copyright (C) 2020 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

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

gzip -c -n manual_anchors.xml >manual_anchors.xml.gz
xxd -i "manual_anchors.xml.gz" >>manual_anchors.xml.gz.h
rm -f manual_anchors.xml.gz

