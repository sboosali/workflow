PACKAGE = windows-playground

HC = cabal exec -- ghc

CODE = sources # tests

default: check

all: build test run document style

configure:
	cabal configure --enable-tests

build: configure
	cabal build

run:
	cabal run

test:
	cabal configure --enable-tests
	cabal test && echo && cat dist/test/*-tests.log

bench:
	cabal configure --enable-benchmarks && cabal bench

document:
	cabal haddock
	open dist/doc/html/$(PACKAGE)/index.html

hyperlink: 
	cabal haddock --with-haddock="$(HOME)/haddock/.cabal-sandbox/bin/haddock" --haddock-options="--hyperlinked-source"
	open dist/doc/html/$(PACKAGE)/index.html

style:
	hlint --hint=HLint.hs  *.hs $(CODE)

fix:
	git vogue fix --all

check:
#	cabal build --ghc-options="-fforce-recomp -fno-code"
	cabal build --ghc-options="-fno-code"

clean:
	cabal clean 
	rm -f Main *.{o,hi,dyn_o,dyn_hi} *.{prof,hp,aux,ps} 

fresh: clean
	rm -fr dist

.PHONY: default clean fresh all configure build test document hyperlink style check fix bench
