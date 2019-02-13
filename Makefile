main:
	ghc -v
	cabal build

all: main

clean:
	cabal clean

.PHONY: all
