HSC2HS=hsc2hs
GHC=ghc
GHC_RUNTIME_LINKER_FLAG=-lHSrts-ghc7.8.3

.PHONY: Example.hs
Example.hs: Example.hsc
	$(HSC2HS) Example.hsc

libffi_example.so: Example.o wrapper.o
	$(GHC) -o $@ -shared -dynamic -fPIC $^ $(GHC_RUNTIME_LINKER_FLAG)

Example.o: Example.hs
	$(GHC) -c -shared -dynamic -fPIC Example.hs

.PHONY: wrapper
wrapper: wrapper.c Example.o
	$(GHC) --make -no-hs-main -optc-O wrapper.c Example.hs -o wrapper

clean:
	rm -f *.hi *.o *_stub.[ch]

clean-all:
	rm -f *.hi *.o *_stub.[ch] *.so

# Runs the example Python program
all: libffi_example.so
	./libffi_example.so
