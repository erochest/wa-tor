
SRC=$(shell find src -name '*.hs')

CABAL=cabal
FLAGS=--enable-tests

TS=$(shell timestamp)

all: init test docs package

init:
	${CABAL} sandbox init
	make deps

test: build
	${CABAL} test --test-option=--color

specs: build
	./dist/build/wa-tor-specs/wa-tor-specs

run:
	${CABAL} run -- --height 185 --width 300 --scaling 4 --initial-sharks 0.025 --initial-fish 0.05 --initial-shark-energy 8 --fish-energy 4 --reproduce-shark 30 --reproduce-fish 10 --quiet +RTS -N


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

cleanoutput:
	rm -f *.log *.tsv

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
