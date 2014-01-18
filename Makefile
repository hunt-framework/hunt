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
# profiling on/off
export PROF=0


# RandomData options
# min/max size of a document
export RD_SIZEMIN = 200
export RD_SIZEMAX = 200
# number of documents
export RD_NUMDOCS = 1000

# set test pattern
ifdef PATTERN
	ifeq ($(action),test)
        pattern = --test-options='-t $(PATTERN)'
	endif
endif

# default profiling options
PROFOPTS_DEFAULT=--enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all

ifndef PROFOPTS
	ifeq ($(PROF),1)
	    PROFOPTS=$(PROFOPTS_DEFAULT)
	else
	    PROFOPTS=
	endif
endif


action		= install

all:		install
profiling:      searchengine-profiling server-profiling

clean:
	$(MAKE) -e -C data/random clean
	$(MAKE) target action=clean PROFOPTS=''

delete: clean
	- rm -rf .cabal-sandbox/

configure: 	; $(MAKE) -e target action=configure
build:		; $(MAKE) -e target action=build PROFOPTS=''
install:	; $(MAKE) -e target action=install
test:		; $(MAKE) -e target action=test PROFOPTS=''

target: searchengine server

sandbox:
	cabal sandbox init --sandbox .cabal-sandbox
	cd searchengine   && cabal sandbox init --sandbox ../.cabal-sandbox
	cd server         && cabal sandbox init --sandbox ../.cabal-sandbox
	cd server         && cabal sandbox add-source ../searchengine/
	cd hayooCrawler   && cabal sandbox init --sandbox ../.cabal-sandbox

searchengine:
	cd searchengine && cabal $(action) $(PROFOPTS) $(pattern)

server: stopServer
	cd server       && cabal $(action) $(PROFOPTS) $(pattern)

searchengine-profiling server-profiling: PROFOPTS=$(PROFOPTS_DEFAULT)
searchengine-profiling: searchengine
server-profiling: server

hayooCrawler:
	$(MAKE) -C hayooCrawler

hayooCrawler-%: hayooCrawler
	$(MAKE) -C hayooCrawler/data $*

startServer: stopServer
	$(EXE) $(RUNOPTS) &

stopServer:
	-killall $(notdir $(EXE)) \
		&& sleep 1 # wait for shutdown and socket release

insertJokes:
	curl -X POST -d @data/jokes/contexts.js $(SERVER)/eval
	curl -X POST -d @data/jokes/FussballerSprueche.js $(SERVER)/document/insert

# NOTE: profiling does not use the makefile target to start the server (RUNOPTS is discarded)
profServer: stopServer
	./server/prof.sh

profServer-fb: stopServer
	./server/prof.sh "make insertJokes"

profServer-rd: stopServer
	./server/prof.sh "make insertRandom"

hayooFrontend/functions.js:
	cd hayooFrontend && ./convertJSON.py

insertHayoo: hayooFrontend/functions.js
	curl -X POST -d @hayooFrontend/hayooContexts.js $(SERVER)/eval
	curl -X POST -d @hayooFrontend/functions.js $(SERVER)/document/insert

random:
	$(MAKE) -C data/random

generateRandom:
	$(MAKE) -e -C data/random cleanData generate

data/random/RandomData.js:
	$(MAKE) -e -C data/random generate

insertRandom:  data/random/RandomData.js
	curl -X POST -d @data/random/contexts.js $(SERVER)/eval
	curl -X POST -d @data/random/RandomData.js $(SERVER)/document/insert

# ab - Apache HTTP server benchmarking tool
benchmark-ab:
	ab -k -n 1000 -c 5 http://localhost:3000/search/esta

# siege - http load testing and benchmarking utility
benchmark-siege:
	siege -c50 -d10 -t3M -f data/random/urls

# able to read heap profile at runtime
runtimeHeapProfile:
	head -`fgrep -n END_SAMPLE holumbusServer.hp | tail -1 | cut -d : -f 1` holumbusServer.hp | hp2ps -d -c > holumbusServer.ps
	ps2pdf holumbusServer.ps

# github data-stringmap install
stringmap:
	git clone https://github.com/sebastian-philipp/StringMap.git tmpgithubdir && cd searchengine && cabal install $(PROFOPTS) ../tmpgithubdir && cd .. && rm -rf tmpgithubdir

membench:
	$(MAKE) -C bench $@
membench-%:
	$(MAKE) -C bench $*
bench-%:
	$(MAKE) -C bench $@

installwithbs:
	rm -rf .cabal-sandbox
	$(MAKE) sandbox
	cd searchengine \
	 	&& cabal install $(PROFOPTS) bytestring \
		&& cabal install $(PROFOPTS) text --constraint=text\<1
	git clone https://github.com/UweSchmidt/data-size.git tmpgithubdir \
		&& ( cd searchengine && cabal install $(PROFOPTS) ../tmpgithubdir ) \
		; rm -rf tmpgithubdir
	git clone https://github.com/sebastian-philipp/StringMap.git tmpgithubdir \
		&& ( cd searchengine && cabal install $(PROFOPTS) ../tmpgithubdir ) \
		; rm -rf tmpgithubdir
	cd searchengine && cabal install $(PROFOPTS) --reinstall --force-reinstalls


.PHONY: target clean configure build install test all searchengine server insertJokes startServer \
		stopServer sandbox benchmark-ab benchmark-siege runtimeHeapProfile startServer \
		hayooCrawler hayooCrawler-% \
		profiling searchengine-profiling server-profiling stringmap \
		bench bench-* membench membench-*
