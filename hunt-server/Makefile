HUNT	= http://localhost:3000
Q       = abc

start	:
	$(MAKE) stop
	../.cabal-sandbox/bin/hunt-server +RTS -T -N2 -S -A8M -H100M -I0 -RTS &

stop	:
	killall hunt-server || true

gc	:
	curl $(HUNT)/status/gc | aeson-pretty

abc	:
	curl $(HUNT)/search/$(Q) | aeson-pretty

dt	:
	curl $(HUNT)/status/doctable | aeson-pretty

weight	:
	curl $(HUNT)/weight/$(Q) | aeson-pretty

cmp	:
	curl $(HUNT)/completion/$(Q)/10 | aeson-pretty



.PHONY	: start stop gc abc dt weight cmp

