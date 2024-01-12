default: build

build:
	cabal build
	cabal install --enable-static --installdir=bin/ --overwrite-policy=always
	mv bin/img2cbr bin/img2cbr-$(shell uname -m)
