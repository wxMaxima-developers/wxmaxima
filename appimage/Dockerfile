FROM debian:oldstable

RUN apt-get update && apt-get -y install git autoconf python binutils \
    texinfo gcc libtool vim desktop-file-utils pkgconf libcairo2-dev \
    libssl-dev libfuse-dev zsync wget fuse bzip2 gawk g++ gperf \
    libgtk-3-dev doxygen libwxbase3.0-dev wx3.0-headers libpng-dev

RUN wget 'https://github.com/Kitware/CMake/releases/download/v3.13.3/cmake-3.13.3.tar.gz' && \
    zcat cmake-3.13.3.tar.gz | tar xvf - && \
    cd cmake-3.13.3 && \
    ./bootstrap && \
    make && \
    make install

RUN wget 'http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.16-x86-64-linux-binary.tar.bz2' -O /tmp/sbcl.tar.bz2 && \
    mkdir /sbcl && \
    tar jxvf /tmp/sbcl.tar.bz2 --strip-components=1 -C /sbcl && \
    cd /sbcl && \
    sh install.sh && \
    rm -f /tmp/sbcl.tar.bz2

RUN git clone https://git.code.sf.net/p/gnuplot/gnuplot-main && \
    cd gnuplot-main && \
    git checkout tags/5.2.6
RUN cd gnuplot-main && \
    ./prepare && \
    ./configure --prefix=`pwd`/dist && \
    make && \
    make install

RUN git clone https://git.code.sf.net/p/maxima/code maxima-code && \
    cd maxima-code && \
    git checkout 5.42.2

RUN cd maxima-code && \
    mkdir dist && \
    ./bootstrap && \
    ./configure --enable-sbcl-exec --prefix=`pwd`/dist && \
    make && \
    make install

RUN git clone https://github.com/wxMaxima-developers/wxmaxima.git && \
    cd wxmaxima && \
    git checkout Version-19.01.2

RUN cd wxmaxima && \
    mkdir -p build && \
    cd build && \
    cmake -DCMAKE_INSTALL_PREFIX:PATH=/wxmaxima-inst .. && \
    cmake -- build . && \
    cmake --build . -- install

COPY appimagetool-x86_64.AppImage /
RUN chmod +x appimagetool-x86_64.AppImage
RUN ./appimagetool-x86_64.AppImage --appimage-extract && \
    cp -R squashfs-root/* .

RUN mkdir maxima-squashfs
WORKDIR maxima-squashfs
RUN mkdir -p usr/bin

RUN cp -ar /gnuplot-main/dist gnuplot-inst
RUN ln -s ../../gnuplot-inst/bin/gnuplot usr/bin/gnuplot

RUN (cd .. && tar cf - sbcl) | tar xf -
RUN ln -s ../../sbcl/run-sbcl.sh usr/bin/sbcl

RUN mkdir -p usr/lib
RUN cp -a /usr/local/lib/libwx* /usr/local/lib/libpng* usr/lib

RUN mkdir maxima-inst && \
    (cd ../maxima-code/dist && tar cf - *) | (cd maxima-inst && tar xf -)
RUN ln -s share/info maxima-inst/info
RUN ln -s ../../maxima-inst/bin/maxima usr/bin/maxima

RUN (cd .. && tar cf - wxmaxima-inst) | tar xf -
RUN ln -s ../../wxmaxima-inst/bin/wxmaxima usr/bin/wxmaxima

RUN mkdir -p usr/share/metainfo
COPY wxmaxima.appdata.xml usr/share/metainfo/

COPY AppRun .
RUN chmod +x AppRun
COPY wxmaxima.desktop .
COPY maxima.png .

WORKDIR /
RUN ARCH=x86_64 appimagetool maxima-squashfs
