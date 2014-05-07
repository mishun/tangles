Bin := bin
Obj := obj
GHCOpts := -Wall -O -XBangPatterns -threaded

.PHONY: all
all: build


.PHONY: config-dev
config-dev:
	cabal configure --enable-tests -fenable-test-modules

.PHONY: build
build:
	cabal build

.PHONY: copy
copy:
	cabal copy
	cabal register
	mkdir -p $(Obj) && touch $(Obj)/.lib


AppsBinaries := $(patsubst apps/%.hs,$(Bin)/%, $(wildcard apps/*.hs))

.PHONY: apps
apps: $(AppsBinaries)

$(Obj)/.lib:
	cabal copy
	cabal register
	mkdir -p $(Obj) && touch $(Obj)/.lib

$(Bin)/%: apps/%.hs $(Obj)/.lib
	mkdir -p $(dir $@)
	ghc -fforce-recomp $(GHCOpts) -outputdir=$(Obj) -osuf=$(patsubst apps/%.hs,%.o, $<) -hisuf=$(patsubst apps/%.hs,%.hi, $<) -o $@ $<
