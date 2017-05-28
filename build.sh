#!/usr/bin/env bash
repoFolder="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
pushd $repoFolder
source ./scripts/fake-install.sh
if [[ $# -eq 0 ]] ; then
  exec_fake run
else
  exec_fake "$@"
fi
popd
