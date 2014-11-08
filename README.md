# Haskell FFI Tutorial

This is a demo repository to help out with Haskell FFI, namely, with
nested structures. Everything I've found on the subject up till that 
point contained only partial information for what I've needed, 
so I decided to compose a complete tutorial together.

# Covered subjects 

You'll learn how to:

  * (expressively) represent C `struct` in Haskell code
  * call C code from Haskell
  * call Haskell code from C
  * operate on nested `struct`s 
  * operate on `struct` arrays
  * decode `unions` 
  * read and write C fixed-length strings and Pointer-type Strings
  * how to import functions from, for example, stdlib

# Motivation

There are many C bindings written for different Haskell projects, and 
every one seems to have it's own style. Also, because every C program
is written in a very different style (some use fixed-length strings,
some use `*char`, some have nested structs, some do not, some have
`unions`, some do not), it's somewhat difficult to jump in and start
writing your own C bindings in Haskell, even though all the functionality
is available for you to use.

I've collected all I've learned about writing Haskell bindings into
a single repository, and will document it all part by part to have 
more detailed descriptions, explanation and motivation about how
to do things.

If you're a seasoned Haskell developer, and everything here is obvious
for you, you can go through the concepts introduced here and give
your feedback, since I am by no means an expert in this area, and 
may have misunderstood some things.

## Calling Haskell from C

In order to call Haskell from C, you'd have to:

  * create a C wrapper where you initialize Haskell Runtime
  * write a callback function in Haskell, that will be called from C
  
Let's start with a callback function in Haskell, since it'll be used
within the C code that we'll write later on. Open up a file, call it
`Example.hsc` (hsc extension is used for the files that are interfacing
C, it will be passed through hsc2hs preprocessor that will unwrap all
the macros. We haven't used any for now, but we will later on).

```haskell
{-# LANGUAGE CPP                         #-}
{-# LANGUAGE ForeignFunctionInterface    #-}

module Example where

foreign export ccall entrypoint :: IO ()

entrypoint :: IO ()
entrypoint = do
  print "Hello from Haskell"
  
  return ()
```

So far so good. Now, in order to convert `hsc` file to regular `hs` file,
you have to run 

```
hsc2hs Example.hsc
```

Preprocessor will create `Example.hs` file that unwraps all the macros.

In order to get a stub file that contains all the functions exported from
Haskell to C, you have to run 

```
ghc Example.hs
```

`Example_stub.h` will contain a valid signature for our `entrypoint` function.
Namely, something like that (with some C boilerplate that's ommited for readability):

```
extern void entrypoint(void);
``` 
  
In order to create a C wrapper, just create a `wrapper.c` file:

```c
// Include Haskell FFI file, which we will use to initialize a Haskell runtime
#include "HsFFI.h"

/* #ifdef __GLASGOW_HASKELL__ */
#include "Example_stub.h"
/* #endif */

int main( int argc, char *argv[] )
{
  // Initialize Haskell Runtime _before_ any calls to the Haskell code
  hs_init (&argc, &argv);
  
  // Make a call to Haskell code
  entrypoint();
}
```

Great, now you can actually compile the `wrapper.c` and run the resulting 
binary

```
ghc -no-hs-main wrapper.c Example.hs -o wrapper
```

You'll get a `wrapper` binary, that you can run and see the result:

```
➜  haskell-ffi-tutorial  ./wrapper
"Hello from Haskell"
```

Perfect, now you know how to call Haskell code from C.
  
# License

Copyright (c) 2014 Alex Petrov

Licensed under MIT License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
