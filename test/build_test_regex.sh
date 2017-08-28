# author: Guglielmo Saggiorato
# email: astyonax@gmail.com
# date: 28.08.17
# purpose: Build and run maxima to test the BetterTeX function.
# Tought to be exectuted on a debian-based setup (with aptitude)

#sudo aptitude build-dep wxmaxima
mkdir build
cd build
cmake ..
set -e
cmake  --build .
./src/wxmaxima ../test/regex.wxmx
