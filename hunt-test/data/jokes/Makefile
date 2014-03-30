OUT     = fb2js

# hardcoded in Fussball2JSON.hs
GENOUT  = FussballerSprueche.js

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
	./.cabal-sandbox/bin/$(OUT)

generate: $(GENOUT)

.PHONY: all install clean delete generate cleanData
