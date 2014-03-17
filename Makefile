SHELL := /usr/bin/env bash
FLAGS := --disable-documentation --disable-library-coverage
BIN   := dist/build/khan/khan
DEPS  := vendor/amazonka vendor/ede

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS))) && cp -f $(BIN) .

strip: build
	strip -o dist/khan $(BIN) && \
	 upx dist/khan

install: cabal.sandbox.config add-sources
	cabal install -j $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/amazonka
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/%:
	git clone git@github.com:brendanhay/$*.git $@
