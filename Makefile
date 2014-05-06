GHC_OPTS := -Wall -O -XBangPatterns -threaded

.PHONY: all
all: build

.PHONY: build
build:
	cabal build

.PHONY: copy
copy: build
	cabal copy
	cabal register

.PHONY: config-dev
config-dev:
	cabal configure --enable-tests --enable-benchmarks -fenable-test-modules

.PHONY: apps
apps: bin/DrawTangleStarGlues bin/GenerateVirtualLinks bin/PrintVirtualLinksTable bin/DrawKnot

bin/%: apps/%.hs copy
	mkdir -p bin
	ghc --make $(GHC_OPTS) -o $@ $<
