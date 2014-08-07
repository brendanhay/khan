#!/usr/bin/env bash

export BUNDLE_GEMFILE=./metadata/Gemfile

if [ ! -e "${BUNDLE_GEMFILE}.lock" ]; then
    bundle install
fi

exec bundle exec ruby metadata/server.rb "$@"
