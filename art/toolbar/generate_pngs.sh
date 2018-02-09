#!/bin/sh

for i in *.svg; do
  for j in 128 192; do
    inkscape -e `basename $i .svg`.$j.png -w $j $i
    optipng -o 9 `basename $i .svg`.$j.png
  done
done

rm -f images.h
for i in *.png; do
    xxd -i $i>> images.h
done
