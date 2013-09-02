DEPS := vendor/options

.PHONY: test lint doc

all: build

build: .conf
	cabal-dev build

install: $(DEPS)
	cabal-meta install --dev -j \
	 --disable-documentation \
	 --disable-library-coverage

clean:
	-rm -rf .conf dist
	cabal-dev clean

test:
	cabal-dev install --enable-tests

lint:
	hlint src

doc:
	cabal-dev haddock

.conf:
	cabal-dev configure && touch $@

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
