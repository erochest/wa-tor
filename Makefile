
SRC=$(shell find src -name '*.hs')

CABAL=cabal
FLAGS=--enable-tests

all: init test docs package

init:
	${CABAL} sandbox init
	make deps

test: build
	${CABAL} test --test-option=--color

specs: build
	./dist/build/wa-tor-specs/wa-tor-specs

run:
	${CABAL} run -- --initial-sharks 0.15 --initial-fish 0.15 --initial-shark-energy 30 --fish-energy 5 --reproduce-shark 30 --reproduce-fish 50 --speed 50


# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

tags: ${SRC}
	hasktags --ctags *.hs src

hlint:
	hlint *.hs src specs

clean:
	${CABAL} clean

distclean: clean
	${CABAL} sandbox delete

configure: clean
	${CABAL} configure ${FLAGS}

deps: clean
	${CABAL} install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	${CABAL} build

restart: distclean init build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
