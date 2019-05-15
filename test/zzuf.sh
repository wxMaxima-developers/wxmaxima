#!/bin/bash
for i in {1000..3000}; do for f in testbench_simple.wxmx; do zzuf -r 0.0001 -s $i < "$f" > "$i-$f"; done; done
for i in *.wxmx; do timeout 3 wxmaxima $i; done &> fuzzing.log
grep -C2 "Segmentation fault" fuzzing.log
