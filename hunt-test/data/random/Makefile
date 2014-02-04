GHC     = ghc
MAIN    = RandomData
OUT     = $(MAIN)

# hardcoded in RandomData.hs
GENOUT  = RandomData.js
# min/max size of a document
RD_SIZEMIN = 200
RD_SIZEMAX = 200
# number of documents
RD_NUMDOCS = 1000


all: install

install:
	cabal install

clean:
	- rm -f $(OUT) $(MAIN).hi $(MAIN).o

cleanData:
	- rm -f $(GENOUT)

$(GENOUT): install
	./../../../.cabal-sandbox/bin/$(OUT) $(RD_NUMDOCS) $(RD_SIZEMAX) $(RD_SIZEMIN)

generate: $(GENOUT)

.PHONY: all install clean generate cleanData
