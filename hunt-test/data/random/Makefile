OUT     = rd2js

# hardcoded in RandomData.hs
GENOUT  = RandomData.js
# min/max size of a document
RD_SIZEMIN = 200
RD_SIZEMAX = 200
# number of documents
RD_NUMDOCS = 1000


all: install

install:
	cabal sandbox init
	cabal install

clean:
	cabal clean

delete: clean
	- cabal sandbox delete

cleanData:
	- rm -f $(GENOUT)

$(GENOUT): install
	./.cabal-sandbox/bin/$(OUT) $(RD_NUMDOCS) $(RD_SIZEMAX) $(RD_SIZEMIN)

generate: $(GENOUT)

.PHONY: all install clean generate cleanData
