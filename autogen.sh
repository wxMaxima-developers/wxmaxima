#!/bin/sh
autopoint --force
aclocal
autoconf
autoheader
automake --add-missing
./configure $@
