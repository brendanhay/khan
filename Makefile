DEPS  := vendor/options
FLAGS := -j --disable-documentation --disable-library-coverage

.PHONY: test lint doc

all: build

build: cabal.sandbox.config
	cabal build

install: $(DEPS) add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

add-sources: cabal.sandbox.config
	cabal sandbox add-source vendor/options
	cabal sandbox add-source ../hexpat-pickle-generic
	cabal sandbox add-source ../querystring-pickle
	cabal sandbox add-source ../aws-haskell

cabal.sandbox.config:
	cabal sandbox init

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
