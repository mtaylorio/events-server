#!/bin/sh
set -eux
apk add --no-cache libpq-dev zlib-dev
stack build --system-ghc --copy-bins --local-bin-path .
