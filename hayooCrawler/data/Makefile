
# crawl all packages and store .json data per package in subdir packages
# 0000-ranks.json contains the json data for updating the packages
# with the package rank. Packages with default value (1.0) for rank
# are not updated

pkg	:
	[ -d tmp ]      || mkdir tmp
	[ -d packages ] || mkdir packages
	hayooCrawler --pkg-index -j --ranking

# save and compress the package files

tar	: pkg.tgz

# restore saved package files

untar	:
	tar xvzf pkg.tgz

# load and parse all hackage pages
# and store them in binary format in
# dir cache. Usefull for test runs
# reduces web trafic.

cache	:
	hayooCrawler --cache

clean	:
	rm -rf tmp packages

.PHONY	: pkg tar untar cache clean

pkg.tgz	:
	tar cvzf pkg.tgz packages

