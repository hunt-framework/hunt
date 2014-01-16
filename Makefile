# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

N       = 1
H       = 500
A       = 8
K       = 200
RTSPROF =
RUNOPTS = +RTS -N$(N) -s $(RTSPROF) -K$(K)M -A$(A)M -H$(H)M -RTS
PATTERN =

SERVER  = http://localhost:3000
EXE     = $(shell [ -d ".cabal-sandbox" ] && echo ".cabal-sandbox/bin/holumbusServer" || echo "holumbusServer")
PROFSH  = ./prof.sh

PROFOPTS=--enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all


# RandomData options
# min/max size of a document
SIZEMIN = 200
SIZEMAX = 200
# number of documents
NUMDOCS = 1000

# set test pattern
ifdef PATTERN
	ifeq ($(action),test)
        pattern = --test-options='-t $(PATTERN)'
	endif
endif


action		= install

all:		install
profiling:      searchengine-profiling server-profiling

clean: 		; $(MAKE) target action=clean
configure: 	; $(MAKE) target action=configure
build:		; $(MAKE) target action=build
install:	; $(MAKE) target action=install
test:		; $(MAKE) target action=test

target: searchengine server

sandbox:
	cabal sandbox init --sandbox .cabal-sandbox
	cd searchengine   && cabal sandbox init --sandbox ../.cabal-sandbox
	cd server         && cabal sandbox init --sandbox ../.cabal-sandbox
	cd server         && cabal sandbox add-source ../searchengine/
	cd hayooCrawler   && cabal sandbox init --sandbox ../.cabal-sandbox

# TODO: move bench stuff to separate Makefile
membench-sandbox:
	cd bench && cabal sandbox init --sandbox .cabal-sandbox
	cd bench && cabal sandbox add-source ../searchengine/

membench-configure:
	cd bench && \
		cabal configure --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all

membench-install:
	cd bench && \
		cabal install --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all

membench: membench-install

membench-gen:
	cd bench && (\
		./.cabal-sandbox/bin/holumbusMemBenchBin '../data/jokes/FussballerSprueche.js'; \
		./.cabal-sandbox/bin/holumbusMemBenchBin '../data/random/RandomData.js'         \
		)

bench-%:
# XXX: dataset hardcoded
# 	./.cabal-sandbox/bin/holumbusMemBench $*
	cd bench && \
		$(PROFSH) 1 $*

bench:
	for i in $$(seq 0 4); do \
		$(MAKE) bench-$$i; \
	done

searchengine:
	cd searchengine && cabal $(action) $(pattern)

server: stopServer
	cd server       && cabal $(action) $(pattern)

searchengine-profiling:
	cd searchengine && cabal $(action) --enable-library-profiling --ghc-option=-auto-all $(pattern)

server-profiling: stopServer
	cd server       && cabal $(action) --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all $(pattern)

hayooCrawler:
	$(MAKE) -C hayooCrawler

hayooCrawler-data: hayooCrawler
	$(MAKE) -C hayooCrawler/data
hayooCrawler-cache: hayooCrawler
	$(MAKE) -C hayooCrawler/data cache
hayooCrawler-pkg: hayooCrawler
	$(MAKE) -C hayooCrawler/data pkg
hayooCrawler-rnk: hayooCrawler
	$(MAKE) -C hayooCrawler/data rnk
hayooCrawler-fct: hayooCrawler
	$(MAKE) -C hayooCrawler/data fct
hayooCrawler-clean:
	$(MAKE) -C hayooCrawler/data clean

startServer: stopServer
	$(EXE) $(RUNOPTS) &

stopServer:
	-killall $(notdir $(EXE))

insertJokes:
	curl -X POST -d @data/jokes/contexts.js $(SERVER)/eval
	curl -X POST -d @data/jokes/FussballerSprueche.js $(SERVER)/document/insert

hayooFrontend/functions.js:
	cd hayooFrontend && ./convertJSON.py

insertHayoo: hayooFrontend/functions.js
	curl -X POST -d @hayooFrontend/hayooContexts.js $(SERVER)/eval
	curl -X POST -d @hayooFrontend/functions.js $(SERVER)/document/insert

random:
	$(MAKE) -C data/random

generateRandom:
	-$(MAKE) -C data/random cleanData
	$(MAKE) -C data/random generate SIZEMIN=$(SIZEMIN) SIZEMAX=$(SIZEMAX) NUMDOCS=$(NUMDOCS)

insertRandom: generateRandom
	curl -X POST -d @data/random/contexts.js $(SERVER)/eval
	curl -X POST -d @data/random/RandomData.js $(SERVER)/document/insert

benchmark: generateRandom
	ab -k -n 1000 -c 5 http://localhost:3000/search/esta

benchmark2: generateRandom
	siege -c50 -d10 -t3M -f data/random/urls

# able to read heap profile at runtime
runtimeHeapProfile:
	head -`fgrep -n END_SAMPLE holumbusServer.hp | tail -1 | cut -d : -f 1` holumbusServer.hp | hp2ps -d -c > holumbusServer.ps
	ps2pdf holumbusServer.ps

# github data-stringmap install
stringmap:
	git clone https://github.com/sebastian-philipp/StringMap.git tmpstringmap && cd searchengine && cabal install ../tmpstringmap && cd .. && rm -rf tmpstringmap

membench-sandbox-delete:
	cd bench && cabal sandbox delete

# requires ghc-profiling libs
#   e.g. apt-get install ghc-prof
# profiling enabled
# text < 1, github data-size, github data-stringmap
# data-size 0.1.3.0 not on hackage yet
membench-deps:
	cd bench && \
		cabal install $(PROFOPTS) text --constraint=text\<1
	git clone https://github.com/UweSchmidt/data-size.git tmpdatasize \
		&& cd bench && cabal install $(PROFOPTS) ../tmpdatasize \
		; cd .. && rm -rf tmpdatasize
	git clone https://github.com/sebastian-philipp/StringMap.git tmpstringmap \
		&& cd bench \
		&& cabal install $(PROFOPTS) ../tmpstringmap \
		; cd .. && rm -rf tmpstringmap

.PHONY: target clean configure build install test all searchengine server insertJokes startServer \
		stopServer sandbox hayooCrawler benchmark benchmark2 runtimeHeapProfile startServer \
		profiling searchengine-profiling server-profiling stringmap \
		membench-sandbox membench-configure membench-deps membench-install membench \
		membench-sandbox-delete bench bench-*
