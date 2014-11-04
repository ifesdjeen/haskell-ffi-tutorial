HSC2HS = hsc2hs
GHC    = ghc

GHC_RUNTIME_LINKER_FLAG = -lHSrts-ghc7.8.3

.PHONY: src/Example.hs
src/Example.hs: src/Example.hsc
	$(HSC2HS) src/Example.hsc -I./include

libffi_example.so: Example.o wrapper.o
	$(GHC) -o $@ -shared -dynamic -fPIC $^ $(GHC_RUNTIME_LINKER_FLAG)

Example.o: src/Example.hs
	$(GHC) -c -shared -dynamic -fPIC src/Example.hs

.PHONY: wrapper
wrapper: cbits/wrapper.c Example.o
	$(GHC) --make -no-hs-main -optc-O cbits/wrapper.c src/Example.hs -I./src -I./include -o wrapper

clean:
	rm -f *.hi *.o *_stub.[ch] wrapper *.out

clean-all:
	rm -f *.hi *.o *_stub.[ch] *.so

all: wrapper
	./wrapper
