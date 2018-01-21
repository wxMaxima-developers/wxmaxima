#!/bin/sh

for i in *.svg; do
  for j in 128; do
    inkscape -e `basename $i .svg`.png -w $j $i
    optipng -o 9 `basename $i .svg`.png
  done
done
