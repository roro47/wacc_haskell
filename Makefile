main:
	stack build

all: main

clean:
	stack clean
	rm -rf dist
	rm -rf .stack-work

.PHONY: all
