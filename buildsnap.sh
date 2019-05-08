#!/bin/sh
lxc launch ubuntu:lts
export SNAPCRAFT_BUILD_ENVIRONMENT=lxd
snapcraft
