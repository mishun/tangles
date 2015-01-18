Cabal := cabal-dev
Bin := bin
Obj := dist/build
GHCOpts := -Wall -O -XBangPatterns -threaded

.PHONY: all
all: build


.PHONY: deps-dev
deps-dev:
	$(Cabal) install HaskellForMaths \
	                 IfElse \
	                 diagrams \
	                 diagrams-cairo \
	                 disjoint-set \
	                 test-framework \
	                 test-framework-hunit \
	                 test-framework-quickcheck2

.PHONY: config-dev
config-dev:
	$(Cabal) configure --enable-tests -fenable-test-modules

.PHONY: build
build:
	$(Cabal) build

.PHONY: clean
clean:
	$(Cabal) clean

.PHONY: test
test:
	$(Cabal) test


AppsBinaries := $(patsubst apps/%.hs,$(Bin)/%, $(wildcard apps/*.hs))

.PHONY: apps
apps: $(AppsBinaries)

$(Bin)/%: apps/%.hs dist/package.conf.inplace
	mkdir -p $(dir $@)
	ghc --make -package-db $(wildcard cabal-dev/packages-?.?.?.conf) \
	           -package-db dist/package.conf.inplace $(GHCOpts) \
	           -outputdir=$(Obj) \
	           -osuf=$(patsubst apps/%.hs,%.o, $<) \
	           -hisuf=$(patsubst apps/%.hs,%.hi, $<) \
	           -o $@ $<
