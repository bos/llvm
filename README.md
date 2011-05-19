Haskell LLVM bindings
---------------------

This package provides Haskell bindings for the popular
[LLVM](http://llvm.org/) compiler infrastructure project.


Configuration
-------------

By default, when you run `cabal install`, the Haskell bindings will be
configured to look for LLVM in `/usr/local`.

If you have LLVM installed in a different location, e.g. `/usr`, you
can tell the `configure` script where to find it as follows:

    cabal install --configure-option=--with-llvm-prefix=/usr


Package status - what to expect
-------------------------------

This package is still under development.

The high level bindings are currently incomplete, so there are some
limits on what you can do.  Adding new functions is generally easy,
though, so don't be afraid to get your hands dirty.

The high level interface is mostly safe, but the type system cannot
protect against everything that can go wrong, so take care.  And, of
course, there's no way to guarantee anything about the generated code.


Jump in and help!
-----------------

We welcome your comments and contributions.  You can send email to us
at <bos@serpentine.com> or <lennart@augustsson.net>.  If you want to
send patches, please clone a copy of the
[git repository](https://github.com/bos/llvm):

    git clone git://github.com/bos/llvm

Thanks!
