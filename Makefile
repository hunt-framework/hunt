action		= install

all		: install

clean		: ; $(MAKE) target action=clean
configure	: ; $(MAKE) target action=configure
build		: ; $(MAKE) target action=build	
install		: ; $(MAKE) target action=install

target	        : searchengine server

searchengine: 
	( cd searchengine           && cabal $(action) )

server:
	( cd server                 && cabal $(action))

startServer: stopServer
	( holumbusServer & )

stopServer:
	-killall holumbusServer
 
insertJokes: startServer
	( cd data/jokes/ && curl -X POST -d @FussballerSprueche.js http://localhost:3000/document/insert)

.PHONY	: target clean configure build install all searchengine server insertJokes startServer stopServer
