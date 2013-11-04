SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage
BIN   := dist/build/khan/khan
AMZ   := ../amazonka

.PHONY: test lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS))) && cp $(BIN) .

strip: build
	strip -o dist/khan $(BIN) && upx dist/khan

install: cabal.sandbox.config add-sources
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

add-sources: cabal.sandbox.config $(AMZ)
	cabal sandbox add-source $(AMZ)

cabal.sandbox.config:
	cabal sandbox init

$(AMZ):
	$(error Path '$@' doesn't exist - try Makefile.jenkins instead.)
