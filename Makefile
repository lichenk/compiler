default: l4c
	cp bin/l4c bin/c0c

all: l4c
	cp bin/l4c bin/l1c
	cp bin/l4c bin/l2c
	cp bin/l4c bin/l3c
	cp bin/l4c bin/c0c

l1c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
l2c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
l3c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
l4c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
c0c:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
	rm -rf bin/
	cp -r shellbins/ bin/
	chmod +x bin/*
clean:
	rm -rf bin/
	rm -rf dist/
