Src
===

This directory contains the following things:

C++ and header files for wxMaxima.
wxMaxima is mostly programmed in C++, C++14 is the required C++ standard.

wxMathml.lisp
-------------
The lisp part of wxMaxima. Note that wxMathml.lisp isn't automatically
used directly by the program: Files a program depends on may fail to be
shipped along with the program or be installed in a folder that is logical
to a human being but might not be a folder the program doesn't search for
them in. Instead of installing the file somewhere, it is converted to
a header file (using CMake), and compiled in the wxMaxima binary.
There is a command line option `--wxmathml-lisp=<str>` to use an external
Lisp file instead - mainly useful for developers.
