#!/bin/sh
set -eux
docker run --rm -v $(pwd):/build -w /build \
  images.home.mtaylor.io/haskell \
  ./bin/docker-build.sh
