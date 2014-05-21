HUNT	= http://localhost:3000
Q       = abc

N	= 1
H	= 500
A	= 8
K	= 200
RTSPROF =
#RUNOPTS = +RTS -N$(N) -s $(RTSPROF) -K$(K)M -A$(A)M -H$(H)M -RTS
RUNOPTS = +RTS -N$(N) -s $(RTSPROF) -K$(K)M -G2 -c -I3 -RTS

start	:
	$(MAKE) stop
	../.cabal-sandbox/bin/hunt-server $(RUNOPTS) &

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

