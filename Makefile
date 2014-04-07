
.PHONY: all
all: build

.PHONY: build
build:
	cabal build

.PHONY: config-dev
config-dev:
	cabal configure --enable-tests --enable-benchmarks -fenable-test-modules
