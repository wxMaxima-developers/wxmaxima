#!/bin/sh
make html
cd ..
cp Doxygen tmp -a
git checkout gh-pages
rm Doxygen -r -f
mv tmp Doxygen
git add -f Doxygen/html/* Doxygen/html/search/*
