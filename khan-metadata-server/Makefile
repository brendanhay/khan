SHELL  := /usr/bin/env bash
BUNDLE := .bundle

default: $(BUNDLE)

$(BUNDLE):
	bundle install --path=$@

clean:
	rm -rf $(BUNDLE)
