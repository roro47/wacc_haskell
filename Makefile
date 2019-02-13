main:
	ghc -v
	stack build
	cabal build

all: main

clean:
	cabal clean

.PHONY: all
