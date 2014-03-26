SHELL        := /usr/bin/env bash
NAME         := khan
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
FLAGS        := --disable-documentation --disable-library-coverage
DEPS         := vendor/amazonka vendor/ede
BIN          := dist/build/$(NAME)/$(NAME)
OUT          := dist/$(NAME)

.PHONY: $(BIN) clean test lint

all: deps $(NAME)

dist: deps dist/$(DEB)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor $(OUT)
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

lint:
	hlint src

$(NAME): $(BIN)
	ln -fs $< $@

$(BIN):
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

$(OUT): $(BIN)
	strip -o $(OUT) $< && upx $<

%.deb: $(OUT)
	makedeb --name=$(NAME) \
	 --version=$(VERSION) \
	 --debian-dir=deb \
	 --build=$(BUILD_NUMBER) \
	 --architecture=amd64 \
	 --output-dir=dist

deps: add-sources
	cabal install -j $(FLAGS) --only-dependencies

add-sources: cabal.sandbox.config $(DEPS)
	cabal sandbox add-source vendor/amazonka
	cabal sandbox add-source vendor/ede

cabal.sandbox.config:
	cabal sandbox init

vendor/%:
	git clone https://github.com/brendanhay/$*.git $@
