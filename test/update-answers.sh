#!/bin/sh
../build/src/wxmaxima --batch testbench_simple.wxmx
../build/src/wxmaxima --batch automatic_test_files/all-celltypes.wxmx
../build/src/wxmaxima --batch automatic_test_files/rememberingAnswers.wxm
../build/src/wxmaxima --batch ../examples/diffEquations.wxm
../build/src/wxmaxima --batch ../examples/solvingEquations.wxm
../build/src/wxmaxima --batch automatic_test_files/10MinuteTutorial.wxm

