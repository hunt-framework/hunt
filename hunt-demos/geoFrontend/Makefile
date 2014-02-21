data/map.xml:
	mkdir -p data
	wget -O data/map.xml http://overpass-api.de/api/map?bbox=9.6960,53.5681,9.7496,53.5854

runWithInsert: data/map.xml
	cabal run -- --load-index $PWD/data/map.xml

run:
	cabal run
