Build and installation instructions
-----------------------------------

Please don't think of these as canonical build instructions yet, as
this work is rather early along.  Let me tell you what's working for
*me*, and hopefully this information will be enough to get you going.


Prerequisites
-------------

Firstly, you'll need to have LLVM.  I recommend installing LLVM
version 2.9 (from [llvm.org](http://llvm.org/releases/)), which is
what this package has been tested with.

It's easy to install LLVM itself from source:

    cd llvm
    ./configure --prefix=$SOMEWHERE
    make
    make install

It's a good idea to have `$SOMEWHERE/bin` in your path.

Installing from source on Windows requires MinGW.


Building
--------

This is a normal Haskell package, but needs a `configure` script to
configure some system-specific details of LLVM.

    cabal install --configure-option --with-llvm-prefix=$SOMEWHERE

If you have LLVM installed in a fairly normal location (`/usr` or
`/usr/local`), you don't need the `--configure-option` bits.


Building examples
-----------------

In the examples directory are a few example programs.  There's a GNU
Makefile in there, so running `make` in that directory will build the
examples, as will `make examples` in the top-level directory.  Doing
`make run` will build and run the examples.

Note: On older versions of MacOS X you may see a lot of "atom sorting
error" warnings.  They seem to be harmless.
