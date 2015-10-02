Bin := bin


.PHONY: all
all: build


.PHONY: deps
deps:
	cabal install --dependencies-only --enable-tests

.PHONY: deps-prof
deps-prof:
	cabal install --dependencies-only --enable-tests --enable-library-profiling --enable-executable-profiling


.PHONY: config
config:
	cabal configure

.PHONY: config-dev
config-dev:
	cabal configure --enable-tests -finternal-tests

.PHONY: config-prof
config-prof:
	cabal configure --enable-tests -finternal-tests --enable-library-profiling --enable-executable-profiling --disable-optimization

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

# TODO: replace this dirty hack with 'cabal exec' afrer cabal update
$(Bin)/%: apps/%.hs dist/package.conf.inplace
	mkdir -p $(dir $@)
	ghc --make -package-db='.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d' \
	           -package-db='dist/package.conf.inplace' \
	           -Wall -O -XBangPatterns -threaded \
	           -outputdir=dist/build \
	           -osuf=$(patsubst apps/%.hs,%.o, $<) \
	           -hisuf=$(patsubst apps/%.hs,%.hi, $<) \
	           -i=test/ \
	           -o $@ $<
