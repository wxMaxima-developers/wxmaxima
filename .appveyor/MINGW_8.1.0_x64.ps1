# A script that pulls in MinGW 8.1.0 variables
echo '- Setting environment variables for MinGW 8.1.0'

$env:PATH = "C:\mingw-w64\x86_64-8.1.0-posix-seh-rt_v6-rev0\mingw64\bin;C:\msys64\usr\bin;${env:PATH}"

$env:CC = "x86_64-w64-mingw32-gcc.exe"
$env:CXX = "x86_64-w64-mingw32-g++.exe"


$env:CXXFLAGS = "-static -static-libgcc -static-libstdc++"
$env:LDFLAGS = "-static"
