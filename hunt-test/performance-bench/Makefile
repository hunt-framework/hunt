# ######################################
# Membench
# ######################################
# requires ghc-profiling libs
#   apt: ghc-prof
#
# First installation:
#   > make first-install
#
# Rebuild:
#   > make
#
# Benchmark:
#   > make bench
#   > make bench-{dataset}-{index}
#   dataset=fb|fbb|rd|rdd
#   index=0-4
#
# Clean:
#   > make clean
#
# Clean incl. sandbox:
#   > make delete
#
# ######################################

EXE     = $(shell [ which .cabal-sandbox/bin/hunt-permbench hunt-permbench | head -1)
PROFSH  = ./prof.sh

PROFOPTS=--disable-library-profiling --disable-executable-profiling --ghc-option=-auto-all

N       = 1
H       = 500
A       = 8
K       = 200

IXMAX = 4

# RandomData options
# min/max size of a document
export RD_SIZEMIN = 200
export RD_SIZEMAX = 200
# number of documents
export RD_NUMDOCS = 1000

# ######################################

membench install all: sandbox
	cabal install $(PROFOPTS)

first-install: delete sandbox deps force-install

sandbox: .cabal-sandbox

.cabal-sandbox:
	cabal sandbox init --sandbox .cabal-sandbox && \
	cabal sandbox add-source ../../hunt-searchengine/

run:
	./.cabal-sandbox/bin/hunt-perbench +RTS -N$(N) -RTS

deps: sandbox
#   bytestring
	cabal install $(PROFOPTS) bytestring --constraint=bytestring\>=0.10.4.0
#   text
	cabal install $(PROFOPTS) text --constraint=text\<1
#   data-size
	git clone https://github.com/UweSchmidt/data-size.git tmpgithubdir \
		&& cabal install $(PROFOPTS) tmpgithubdir/ \
		; rm -rf tmpgithubdir
#   stringmap
	git clone https://github.com/sebastian-philipp/StringMap.git tmpgithubdir \
		&& cabal install $(PROFOPTS) tmpgithubdir/ \
		; rm -rf tmpgithubdir

force-install: sandbox
	cabal install $(PROFOPTS) --reinstall --force-reinstalls

clean:
	cabal clean
	- rm -rf tmpgithubdir/
	- rm -f ../data/jokes/FussballerSprueche.bin
	- rm -f ../data/random/RandomData.bin

# generate .bin from .js
%.bin: %.js
	./.cabal-sandbox/bin/hunt-membench-bin '$*.js'

../data/random/RandomData.js:
	$(MAKE) -e -C ../data/random all generate clean

generate: gen-bin

generate-bin: ../data/jokes/FussballerSprueche.bin ../data/random/RandomData.bin

# bench targets with different datasets
bench-fb-%: ../data/jokes/FussballerSprueche.js
		$(PROFSH) $(shell echo \'$<\' $*)

bench-fbb-%: ../data/jokes/FussballerSprueche.bin
		$(PROFSH) $(shell echo \'$<\' $*)

bench-rd-%: ../data/random/RandomData.js
		$(PROFSH) $(shell echo \'$<\' $*)

bench-rdb-%: ../data/random/RandomData.bin
		$(PROFSH) $(shell echo \'$<\' $*)

bench bench-rd: membench
# XXX: random json dataset hardcoded
	for i in $$(seq 0 $(IXMAX)); do \
		$(MAKE) bench-rd-$$i; \
	done

delete: clean
	- cabal sandbox delete

configure:
	cabal configure $(PROFOPTS)

runtimeHeapProfile:
		head -`fgrep -n END_SAMPLE hunt-perbench.hp | tail -1 | cut -d : -f 1` hunt-perbench.hp | hp2ps -d -c > hunt-perbench.ps
			ps2pdf hunt-perbench.ps

.PHONY: sandbox deps all install clean configure membench gen gen-bin bench bench-*
