main:
	stack build

all: main

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all
