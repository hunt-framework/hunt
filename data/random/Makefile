GHC     = ghc
MAIN    = RandomData
OUT     = $(MAIN)

# hardcoded in RandomData.hs
GENOUT  = RandomData.js
# min/max size of a document
SIZEMIN = 200
SIZEMAX = 200
# number of documents
NUMDOCS = 1000


all: install

install: $(OUT)

clean:
	rm $(OUT) $(MAIN).hi $(MAIN).o $(GENOUT)

cleanData:
	rm $(GENOUT)

$(OUT): $(MAIN).hs
	$(GHC) $(MAIN) -main-is $(MAIN) -o $(OUT)

$(GENOUT): $(OUT)
	./$(OUT) $(NUMDOCS) $(SIZEMAX) $(SIZEMIN)

generate: $(GENOUT)

.PHONY: all install clean generate cleanData
