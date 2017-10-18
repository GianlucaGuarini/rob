setup:
	LDFLAGS=-L/usr/local/opt/readline/lib \
	CFLAGS=-I/usr/local/opt/readline/include \
	stack install readline \
	--extra-include-dirs=/usr/local/opt/readline/include \
	--extra-lib-dirs=/usr/local/opt/readline/lib

install:
	@ stack install

build:
	@ stack build

.PHONY: setup install build
