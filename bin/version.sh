#!/bin/sh
set -eux
cat package.yaml | sed -e "s/version:\(\s\+\).*/version:\1$1/g" > package.yaml.tmp
mv package.yaml.tmp package.yaml
