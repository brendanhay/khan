#!/usr/bin/env bash

if [ ! -e "Gemfile.lock" ]; then
    bundle install
fi

exec bundle exec ruby metadata.rb
