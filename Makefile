Bin := bin


.PHONY: all
all: build


.PHONY: deps
deps:
	cabal install --dependencies-only

.PHONY: deps-dev
deps-dev:
	cabal install --dependencies-only --enable-tests


.PHONY: config
config:
	cabal configure

.PHONY: config-dev
config-dev:
	cabal configure --enable-tests -finternal-tests


.PHONY: build
build:
	cabal build


.PHONY: clean
clean:
	cabal clean


.PHONY: test
test:
	cabal test


.PHONY: apps
apps: $(patsubst apps/%.hs,$(Bin)/%, $(wildcard apps/*.hs))

$(Bin)/%: apps/%.hs dist/package.conf.inplace
	mkdir -p $(dir $@)
	ghc --make -package-db dist/package.conf.inplace \
	           -Wall -O -XBangPatterns -threaded \
	           -outputdir=dist/build \
	           -osuf=$(patsubst apps/%.hs,%.o, $<) \
	           -hisuf=$(patsubst apps/%.hs,%.hi, $<) \
	           -o $@ $<
