#!/bin/sh
set -eux

export tag=${tag:-$1}

cd deploy
kustomize edit set image images.home.mtaylor.io/events-mtaylor-io:${tag}
cd ../

git commit -a -m v${tag}
git tag ${tag}
git push origin ${tag}
