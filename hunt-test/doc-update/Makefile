HCI		= ../../.cabal-sandbox/bin/hunt-server-cli
JSP		= aeson-pretty 
HUNT-START	= make -C ../../hunt-server start

test	: schema.js original.js update.js update2.js update3.js
	$(HUNT-START)
	$(HCI) eval schema.js
	$(HCI) eval original.js
	$(HCI) search this | tee result0.js
	$(HCI) eval update.js
	$(HCI) search this | tee result1.js
	$(HCI) eval update2.js
	$(HCI) search this | tee result2.js
	$(HCI) eval update3.js
	$(HCI) search this | tee result3.js
	for i in 0 1 2 3; \
	  do diff result$$i.js expected$$i.js || echo "test failed"; \
	done

clean	:
	rm -f schema.js result*.js

.PHONY	: clean test

schema.js	: original.js
	$(HCI) make-schema original.js | $(JSP) > schema.js

