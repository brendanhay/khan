SHELL  := /usr/bin/env bash
BUNDLE := .bundle

install: $(BUNDLE)

$(BUNDLE):
	bundle install --path=$@

clean:
	rm -rf $(BUNDLE)
