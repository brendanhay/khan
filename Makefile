SHELL        := /usr/bin/env bash
NAME         := khan
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := $(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
FLAGS        := --disable-documentation --disable-library-coverage
DEPS         := vendor/amazonka vendor/ede
BIN          := dist/build/$(NAME)/$(NAME)
OUT          := dist/$(NAME)
SDIST        := dist/$(NAME)-$(VERSION).tar.gz

.PHONY: $(BIN) clean test

all: build

build: $(BIN) link

install: add-sources
	cabal install -j $(FLAGS) --only-dependencies

test:
	cabal install --enable-tests $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox vendor $(OUT) bin
	cabal clean

dist: install dist/$(DEB) $(SDIST)

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

$(SDIST):
	cabal sdist

add-sources: cabal.sandbox.config $(DEPS)
	$(foreach dir,$(DEPS),cabal sandbox add-source $(dir);)

cabal.sandbox.config:
	cabal sandbox init

vendor/%:
	git clone https://github.com/brendanhay/$*.git $@

link: bin/khan bin/metadata-sync bin/metadata-server

bin/khan: bin
	ln -fs ../$(BIN) $@

bin/metadata-sync: bin
	ln -fs ../dist/build/khan-metadata-sync/khan-metadata-sync $@

bin/metadata-server: bin
	ln -fs ../khan-metadata-server/script/instance-data.sh $@

bin:
	-mkdir $@
