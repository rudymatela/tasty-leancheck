# Makefile for tasty-leancheck
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  test/test
EGS = \
  eg/minimal
BENCHS =
GHCIMPORTDIRS = src
GHCFLAGS = -v0 -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic -package tasty)
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release \
       && echo '--optghc=-dynamic' \
               '--optghc="-package tasty"' \
               '--optghc="-package leancheck"')
INSTALL_DEPS = tasty leancheck

all: mk/toplibs

all-all: mk/All.o

test: $(patsubst %,%.test,$(TESTS))

%.test: %
	./$<

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

ghci: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test test

test-via-stack:
	stack test tasty-leancheck:test:test --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-via-everything: test test-via-cabal test-via-stack

legacy-test: # needs ghc-8.0 .. ghc-7.8 installed as such
	make clean  &&  make test GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare:
	cabal update  &&  cabal install --only-dependencies

prepare-legacy-test:
	cabal-ghc-8.2  update  &&  cabal-ghc-8.2  install --only-dependencies
	cabal-ghc-8.0  update  &&  cabal-ghc-8.0  install --only-dependencies
	cabal-ghc-7.10 update  &&  cabal-ghc-7.10 install --only-dependencies
	cabal-ghc-7.8  update  &&  cabal-ghc-7.8  install --only-dependencies

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
