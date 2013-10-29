SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
DEPS  := vendor/options
BIN   := dist/build/khan/khan

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS))) && cp $(BIN) .

strip: build
	strip -o dist/khan $(BIN) && upx dist/khan

install: $(DEPS) cabal.sandbox.config add-sources
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

add-sources: cabal.sandbox.config
	cabal sandbox add-source vendor/options
	cabal sandbox add-source ../aws-haskell

cabal.sandbox.config:
	cabal sandbox init

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
