# disable prarallel builds. 
.NOTPARALLEL:

# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M
# more info on rts opts below

N       = 1
H       = 500
A       = 8
K       = 200
RTSPROF =
#RUNOPTS = +RTS -N$(N) -s $(RTSPROF) -K$(K)M -A$(A)M -H$(H)M -RTS
RUNOPTS = +RTS -s$(RTSPROF) -K$(K)M -RTS
PATTERN =

SERVER  = http://localhost:3000
EXE     = $(shell [ -d ".cabal-sandbox" ] && echo ".cabal-sandbox/bin/hunt-server" || echo "hunt-server")
PROFSH  = ./prof.sh
# profiling on/off
export PROF=1


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

# RTS options
# http://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html
# ------------------
# -N[<n>]   Use <n> processors (default: 1,	-N alone determines the number of
#           processors to use automatically)
# -H<size> Sets the minimum heap size (default 0M)   Egs: -H24m  -H1G
# -A<size> Sets the minimum allocation area size (default 512k) Egs: -A1m -A10k
# -K<size> Sets the maximum stack size (default 8M)  Egs: -K32k   -K512k
# -t[<file>] One-line GC statistics (if <file> omitted, uses stderr)
# -s[<file>] Summary  GC statistics (if <file> omitted, uses stderr)
# -S[<file>] Detailed GC statistics (if <file> omitted, uses stderr)

# default profiling options
PROFOPTS_DEFAULT=--enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all

ifndef PROFOPTS
	ifeq ($(PROF),1)
	    PROFOPTS=$(PROFOPTS_DEFAULT)
	else
	    PROFOPTS=
	endif
endif

# comma to use in functions
comma := ,


action		= install

all:		install

first-install: delete sandbox install

first-install-bs: delete sandbox cabal-bytestring cabal-text data-size stringmap searchengine-force server

clean:
	$(MAKE) -e -C hunt-test/data/random clean
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
	cd hunt-searchengine   && cabal sandbox init --sandbox ../.cabal-sandbox
	cd hunt-server         && cabal sandbox init --sandbox ../.cabal-sandbox
	cd hunt-client         && cabal sandbox init --sandbox ../.cabal-sandbox
	cabal sandbox add-source hunt-searchengine
	cd ../hayoo/hayooFrontend  && cabal sandbox init --sandbox $(CURDIR)/.cabal-sandbox
	cd hunt-demos/geoFrontend && cabal sandbox init --sandbox ../../.cabal-sandbox
	
searchengine:
	cd hunt-searchengine && cabal $(action) $(PROFOPTS) $(pattern)

server: stopServer
	cd hunt-server       && cabal $(action) $(PROFOPTS) $(pattern)

client:
	cd hunt-client && cabal $(action) $(PROFOPTS) $(pattern)

startServer: stopServer
	$(EXE) $(RUNOPTS) &

stopServer:
	-killall $(notdir $(EXE)) \
		&& sleep 1 # wait for shutdown and socket release

insertJokes:
	curl -X POST -d @hunt-test/data/jokes/contexts.js $(SERVER)/eval
	curl -X POST -d @hunt-test/data/jokes/FussballerSprueche.js $(SERVER)/document/insert

# NOTE: profiling does not use the makefile target to start the server (RUNOPTS is discarded)
profServer: stopServer
	./hunt-server/prof.sh

profServer-fb: stopServer
	./hunt-server/prof.sh "make insertJokes"

profServer-rd: stopServer
	./hunt-server/prof.sh "make insertRandom"

insertHayoo: ../hayoo/hayooFrontend/functions.js
	$(MAKE) -e -C ../hayoo/hayoo-json insert SERVER=$(SERVER)

startHayoo: hayooFrontend/functions.js insertHayoo
	cd ../hayoo/hayooFrontend && cabal run &

random:
	$(MAKE) -C hunt-test/data/random

generateRandom:
	$(MAKE) -e -C hunt-test/data/random cleanData generate

hunt-test/data/random/RandomData.js:
	$(MAKE) -e -C hunt-test/data/random generate

insertRandom:  hunt-test/data/random/RandomData.js
	curl -X POST -d @hunt-test/data/random/contexts.js $(SERVER)/eval
	curl -X POST -d @hunt-test/data/random/RandomData.js $(SERVER)/document/insert

hunt-test/data/dict/en_US.dict.js:
	$(MAKE) -e -C hunt-test/data/dict

insertenUSDict: hunt-test/data/dict/en_US.dict.js
	curl -X POST -d @hunt-test/data/dict/en_US.dict.js $(SERVER)/eval
	
# ab - Apache HTTP server benchmarking tool
benchmark-ab:
	ab -k -n 1000 -c 5 http://localhost:3000/search/esta

# siege - http load testing and benchmarking utility
benchmark-siege:
	siege -c50 -d10 -t3M -f hunt-test/data/random/urls

# able to read heap profile at runtime
runtimeHeapProfile:
	head -`fgrep -n END_SAMPLE hunt-server.hp | tail -1 | cut -d : -f 1` hunt-server.hp | hp2ps -d -c > hunt-server.ps
	ps2pdf hunt-server.ps

cabal-%:
	cd hunt-searchengine \
		&& 	cabal install $(PROFOPTS) $* --constraint=text\<1

github-%:
	git clone https://github.com/$(subst !,/,$*).git tmpgithubdir \
		&& ( cd hunt-searchengine && cabal install $(PROFOPTS) ../tmpgithubdir ) \
		; rm -rf tmpgithubdir

# github data-stringmap install
stringmap: github-sebastian-philipp!StringMap
# github data-size install
data-size: github-UweSchmidt!data-size
# use this on ghc 7.8 until version is bumped
murmur78: github-chrisreu!murmur-hash

membench:
	$(MAKE) -C hunt-test/bench $@
membench-%:
	$(MAKE) -C hunt-test/bench $*
bench:
	$(MAKE) -C hunt-test/bench $@
bench-%:
	$(MAKE) -C hunt-test/bench $@

searchengine-force:
	cd hunt-searchengine \
		&& cabal install $(PROFOPTS) --reinstall --force-reinstalls

.PHONY: target clean configure build install test all searchengine server insertJokes startServer \
		first-install first-install-bs \
		stopServer sandbox benchmark-ab benchmark-siege runtimeHeapProfile startServer \
		bench bench-* membench membench-* \
		profServer profServer-fb profServer-rd
		searchengine-force
		github-* stringmap data-size cabal-*
