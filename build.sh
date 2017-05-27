#!/usr/bin/env bash
args=("run")
while [ $# -ne 0 ]
do
  args+=($1)
  shift
done

repoFolder="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
pushd $repoFolder
source ./scripts/fake-install.sh
popd

Fake "${args[@]}"
