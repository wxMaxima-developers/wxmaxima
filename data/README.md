Data
====

This directory contains the following things

wxMathml.lisp
-------------
The lisp part of wxMaxima. Note that wxMathml.lisp isn't automatically
used directly by the program: Files a program depends on may fail to be 
shipped along with the program or be installed in a folder that is logical
to a human being but might not be a folder the program doesn't search for 
them in. Instead of installing the file somewhere therefore a shell 
script named `generate_wxmathml.sh` was provided that compresses the file
and converts it to a .h file whose contents will be compiled into 
wxMaxima.

Desktop integration files
-------------------------

Stuff needed for to integrate wxMaxima into various desktop environments.

Icons that are compiled into the executable
-------------------------------------------

Like wxMathml.lisp the icons aren't shipped as separate files along with
wxMaxima. Instead `generate_pngs.sh` generates .h files that contain the
images and that are compiled into wxMaxima.
