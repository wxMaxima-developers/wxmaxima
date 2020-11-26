#!/bin/sh

# convert SVG images to PNG and then to C Sourcecode (using xxd -i)

cat >examples_gettext.list <<END
/* Automatically generated file using generate_code.sh from the .wxm examples           */
/* This file is part of wxMaxima, but read only by gettext allowing to translate the    */
/* example files.                                                                       */

/* Copyright (C) 2019 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

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

#include <wx/wx.h>
#include <wx/string.h>

wxString examples = 
END
cat >examples.h <<END
/* Automatically generated file using generate_code.sh from the .wxm examples           */
/* This file is part of wxMaxima and contains complessed versions of the examples       */

/* Copyright (C) 2019 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

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
for i in *.wxm; do
    echo "Packing $i"
    gzip -k "$i"
    xxd -i "$i.gz" >> examples.h
    rm $i.gz
    cat "$i" |\
	sed -e "s/\\\\/\\\\\\\\/g" |\
	sed -e "s/\\\"/\\\\\"/g" |\
	sed -e "s/^\\(.*\\)/L\"\\1\\\\n\" /g" |\
	sed -e "s#^\\(.*wxMaxima: title.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: title.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: comment.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: comment.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: section.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: section.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: subsect.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: subsect.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: subsubsect.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: subsubsect.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: heading5.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: heading5.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: heading6.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: heading6.*end.*$\\)#) + \\1 #g" |\
	sed -e "s#^\\(.*wxMaxima: caption.*start.*$\\)#\\1 + _(#g" |\
	sed -e "s#^\\(.*wxMaxima: caption.*end.*$\\)#) + \\1 #g" \
	>> examples_gettext.list
done
echo "\" \";" >> examples_gettext.list
