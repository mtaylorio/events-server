#!/bin/sh
set -eux

git fetch origin --tags
export tag=$(get-release-tag)

cd deploy
kustomize edit set image images.home.mtaylor.io/events-mtaylor-io:${tag}
cd ../

git commit -a -m v${tag}
git tag ${tag}
git push origin ${tag}
