Haskell LLVM bindings
---------------------

This package provides Haskell bindings for the popular
[LLVM](http://llvm.org/) compiler infrastructure project.


Compatibility
-------------

We try to stay up to date with LLVM releases.  The current version of
this package is compatible with LLVM 2.9 and 2.8.  Please understand
that the package may or may not work against older LLVM releases; we
don't have the time or resources to test across multiple releases.


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


Staying in touch
----------------

There is a low-volume mailing list named
[haskell-llvm@projects.haskellorg](http://projects.haskell.org/cgi-bin/mailman/listinfo/haskell-llvm).
If you use the LLVM bindings, you should think about joining.

If you want to contribute patches, please clone a copy of the
[git repository](https://github.com/bos/llvm):

    git clone git://github.com/bos/llvm

Patches are best submitted via the github "pull request" interface.

To file a bug or a request for an enhancement, please use the
[github issue tracker](https://github.com/bos/llvm/issues).
