main:
	ghc --version
	stack build --only-dependencies
	stack build --system-ghc
	ghc-pkg check

all: main

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all
