install:
	@ stack install

build:
	@ stack build

lint:
	@ hlint src

.PHONY: setup install build
