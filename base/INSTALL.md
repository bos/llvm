Build and installation instructions
-----------------------------------

Please don't think of these as canonical build instructions yet, as
this work is rather early along.  Let me tell you what's working for
*me*, and hopefully this information will be enough to get you going.


Prerequisites
-------------

Firstly, you'll need to have LLVM.  I recommend installing LLVM
version 2.9 (from [llvm.org](http://llvm.org/releases/)), which is
what this package has mostly been tested with.

We try to work with both the current and one previous release of LLVM.

It's easy to install LLVM itself from source:

    cd llvm
    ./configure --prefix=$SOMEWHERE
    make
    make install

It's a good idea to have `$SOMEWHERE/bin` in your path.

Installing from source on Windows requires MinGW.


Building
--------

(*Note*: If you're building from a clone of the `git` repository
rather than a release, you *must* run `autoreconf` before you can
build!)

This is a normal Haskell package, but needs a `configure` script to
configure some system-specific details of LLVM.

If you have LLVM installed in a fairly normal location (`/usr` or
`/usr/local`), the usual install command should just work:

    cabal install

On the other hand, if you've installed LLVM in an unusual place,
you'll need some `--configure-option` magic to tell the build where to
find it:

    cabal install --configure-option --with-llvm-prefix=$SOMEWHERE


Building examples
-----------------

In the `examples` directory are a few example programs.  There's a GNU
Makefile in there, so running `make` in that directory will build the
examples, as will `make examples` in the top-level directory.  Doing
`make run` will build and run the examples.

Note: On older versions of MacOS X you may see a lot of "atom sorting
error" warnings.  They seem to be harmless.
