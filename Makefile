# the -A option is important for garbage collection performance,
# a good value is about the size of the L2 cache of the cpu
# the default is set to 8M

N       = 1
H       = 500
A       = 8
K       = 200
RUNOPTS = +RTS -N$(N) -s -K$(K)M -A$(A)M -H$(H)M -RTS




action		= install

all		: install

clean		: ; $(MAKE) target action=clean
configure	: ; $(MAKE) target action=configure
build		: ; $(MAKE) target action=build	
install		: ; $(MAKE) target action=install

target	        : searchengine server

searchengine:
	( cd searchengine           && cabal $(action) )

server: stopServer
	( cd server                 && cabal $(action))

startServer: stopServer
	( holumbusServer $(RUNOPTS) & )

stopServer:
	-killall holumbusServer
 
insertJokes: startServer
	( cd data/jokes/ && curl -X POST -d @FussballerSprueche.js http://localhost:3000/document/insert)

.PHONY	: target clean configure build install all searchengine server insertJokes startServer stopServer
