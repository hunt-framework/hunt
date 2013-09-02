# load and parse all hackage pages
# and store them in binary format in
# dir cache. Usefull for test runs
# reduces web trafic.

cache	:
	hayooCrawler --cache

# crawl all packages and store .js data per package in subdir packages
# 0000-ranks.js contains the json data for updating the packages
# with the package rank. Packages with default value (1.0) for rank
# are not updated

pkg	:
	[ -d tmp ]      || mkdir tmp
	[ -d packages ] || mkdir packages
	hayooCrawler --pkg-index -j --ranking

# crawl all haddock pages and store .js data per function in subdir
# functions/<package>/<module>.<function>.js

fct	:
	[ -d tmp ]       || mkdir tmp
	[ -d functions ] || mkdir functions
	hayooCrawler --fct-index -j --ranking

# save and compress the package files

tar	: pkg.tgz fct.tgz

# restore saved package files

untar	:
	tar xvzf pkg.tgz fct.tgz

clean	:
	rm -rf tmp packages

.PHONY	: pkg tar untar cache clean

pkg.tgz	:
	tar cvzf pkg.tgz packages

fct.tgz	:
	tar cvzf fct.tgz functions

