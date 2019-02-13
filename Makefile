main:
	stack --resolver ghc-7.10.3 setup
	stack build --system-ghc

all: main

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all
