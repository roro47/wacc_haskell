main:
	ghc --version
	stack build --system-ghc

all: main

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all
