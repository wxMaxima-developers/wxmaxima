FROM gcr.io/oss-fuzz-base/base-builder

# Build dependencies. wxWidgets 3.2 is not packaged on the base image, so we
# build it from source in build.sh. GTK3 + GL are its backend dependencies.
# Xvfb is needed because the harnesses start a virtual X display at run time.
RUN apt-get update && apt-get install -y --no-install-recommends \
        cmake ninja-build gettext pkg-config \
        libgtk-3-dev libgl1-mesa-dev libglu1-mesa-dev \
        xvfb pandoc po4a \
    && rm -rf /var/lib/apt/lists/*

# wxWidgets source (built in build.sh so it picks up OSS-Fuzz's $CXXFLAGS).
RUN git clone --depth 1 --branch v3.2.6 \
        https://github.com/wxWidgets/wxWidgets.git $SRC/wxWidgets

RUN git clone --depth 1 \
        https://github.com/wxMaxima-developers/wxmaxima.git $SRC/wxmaxima

WORKDIR $SRC/wxmaxima
COPY build.sh $SRC/
