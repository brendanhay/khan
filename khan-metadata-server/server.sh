#!/usr/bin/env bash

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export BUNDLE_GEMFILE=${DIR}/Gemfile

if [ ! -e "${BUNDLE_GEMFILE}.lock" ]; then
    bundle install
fi

exec bundle exec ruby ${DIR}/lib/server.rb "$@"
