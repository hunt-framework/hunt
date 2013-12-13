# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

N       = 1
H       = 500
A       = 8
K       = 200
RUNOPTS = +RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -RTS
RUNOPTSP = +RTS -N$(N) -s -hc -K$(K)M -A$(A)M -H$(H)M -RTS

SERVER  = http://localhost:3000
EXE     = $(shell [ -d ".cabal-sandbox" ] && echo ".cabal-sandbox/bin/holumbusServer" || echo "holumbusServer")


# RandomData options
# min/max size of a document
SIZEMIN = 200
SIZEMAX = 200
# number of documents
NUMDOCS = 1000



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

searchengine:
	cd searchengine && cabal $(action)

server: stopServer
	cd server       && cabal $(action)

searchengine-profiling:
	cd searchengine && cabal $(action) --enable-library-profiling --enable-executable-profiling

server-profiling: stopServer
	cd server       && cabal $(action) --enable-library-profiling --enable-executable-profiling

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

startServer-profiling: stopServer
	$(EXE) $(RUNOPTSP)

stopServer:
	-killall $(notdir $(EXE))

insertJokes: startServer
	curl -X POST -d @data/jokes/contexts.js $(SERVER)/eval
	curl -X POST -d @data/jokes/FussballerSprueche.js $(SERVER)/document/insert

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

.PHONY: target clean configure build install test all searchengine server insertJokes startServer stopServer sandbox hayooCrawler benchmark
