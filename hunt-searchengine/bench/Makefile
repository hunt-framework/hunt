all: init install run

install:
	cabal sandbox init --sandbox ../../.cabal-sandbox
	cabal install

init:
	mkdir -p results
	cp ./src/index.html ./results/index.html
	wget -nc https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css -O ./results/bootstrap.css
	wget -nc http://code.jquery.com/jquery-1.11.0.min.js -O ./results/jquery.js
	wget -nc http://cdnjs.cloudflare.com/ajax/libs/flot/0.8.2/jquery.flot.min.js -O ./results/flot.js

run:
	./../../.cabal-sandbox/bin/hunt-bench-memory +RTS -N4 -T -RTS
	mv *.js ./results
	mv *.json ./results

clean:
	cabal clean
	rm -rf *.json
	rm -rf *.js

.PHONY: install clean run init all
