VERSION:=$(v)

install:
	@ stack install

test:
	@ stack test

build:
	@ stack build

release:
	@ sed -i '' 's/\(^version:\)[^\n].*/\1'$(VERSION)'/' *.cabal
	@ stack sdist
	@ stack upload .
	@ git add .
	@ git commit -m 'v$(VERSION)'
	@ git tag $(VERSION)
	@ git push && git push --tags

.PHONY: install test build release
