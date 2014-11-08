HSC2HS = hsc2hs
GHC    = ghc
CABAL  = cabal

.PHONY: dist/build/Example.o
dist/build/Example.o:
	$(CABAL) configure && $(CABAL) build

.PHONY: wrapper
wrapper: cbits/wrapper.c dist/build/Example.o
	$(GHC) --make -no-hs-main -optc-O cbits/wrapper.c ./dist/build/Example.hs -I./dist/build/ -I./include -o wrapper

clean:
	rm -fr *.o */*.o dist wrapper *.out *.so

all: wrapper
	./wrapper
