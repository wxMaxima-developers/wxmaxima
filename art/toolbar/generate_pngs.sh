#!/bin/sh

for i in *.svg; do
    inkscape -e `basename $i .svg`.png -w 72 $i
    optipng -o 9 `basename $i .svg`.png
done
