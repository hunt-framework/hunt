all: en_US.dict.js


.PHONY: clean

clean:
	rm -f *.dict *.dict.js


%.dict:
	aspell -l $* dump master | aspell munch | aspell expand | tr ' ' '\n' | sort -u  > $@

%.dict.js: %.dict
	./makeJs.py $*.dict