Bin := bin
GHCOpts := -Wall -O -XBangPatterns -threaded

.PHONY: all
all: build


.PHONY: config-dev
config-dev:
	cabal configure --enable-tests --enable-benchmarks -fenable-test-modules

.PHONY: build
build:
	cabal build
	mkdir -p $(Bin) && touch $(Bin)/.lib

.PHONY: copy
copy:
	cabal copy
	cabal register


.PHONY: apps
apps: $(patsubst apps/%.hs, $(Bin)/%, $(wildcard apps/*.hs))

$(Bin)/.lib:
	cabal copy
	cabal register
	mkdir -p $(Bin) && touch $(Bin)/.lib

$(Bin)/%: apps/%.hs $(Bin)/.lib
	ghc --make $(GHCOpts) -outputdir=$(Bin) -o $@ $<
