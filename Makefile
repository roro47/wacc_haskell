main:
	ghc --version

all: main

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all
