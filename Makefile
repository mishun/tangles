Bin := bin
Obj := dist/build
GHCOpts := -Wall -O -XBangPatterns -threaded

.PHONY: all
all: build


.PHONY: config-dev
config-dev:
	cabal-dev configure --enable-tests -fenable-test-modules

.PHONY: build
build:
	cabal-dev build


AppsBinaries := $(patsubst apps/%.hs,$(Bin)/%, $(wildcard apps/*.hs))

.PHONY: apps
apps: $(AppsBinaries)


$(Bin)/%: apps/%.hs dist/package.conf.inplace
	mkdir -p $(dir $@)
	ghc --make -package-db $(wildcard cabal-dev/packages-?.?.?.conf) -package-db dist/package.conf.inplace $(GHCOpts) -outputdir=$(Obj) -osuf=$(patsubst apps/%.hs,%.o, $<) -hisuf=$(patsubst apps/%.hs,%.hi, $<) -o $@ $<
