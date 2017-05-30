if ! [[ "$TRAVIS_PULL_REQUEST" == "false" ]]; then
  git checkout -B "pr-$TRAVIS_PULL_REQUEST"
fi