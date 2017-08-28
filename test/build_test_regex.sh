#sudo apt-get build-dep wxmaxima
mkdir build
cd build
cmake ..
set -e
cmake  --build .
./src/wxmaxima ../test/regex.wxmx

