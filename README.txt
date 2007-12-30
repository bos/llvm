Haskell LLVM bindings
---------------------

This package provides Haskell bindings for the popular LLVM compiler
infrastructure project.  If you don't know what LLVM is, the main LLVM
home page is here:

  http://llvm.org/


Package status - what to expect
-------------------------------

This package is under heavy development.  I've released it quite early
in order to solicit comments and help from interested parties.

The bindings are currently incomplete, so there are some severe limits
on what you can do.  Adding new functions is generally easy, though,
so don't be afraid to get your hands dirty.

Also, the type safety of various functions is a bit dubious.  The
underlying C bindings to LLVM throw away almost all type information,
so we have to reconstruct types in Haskell.  I'm still working on
straightening things out.

Please expect the sands to shift under your feet quite rapidly for a
little while as I add functionality, improve the interfaces, and
generally flesh the bindings out to be thoroughly useful.


Jump in and help!
-----------------

I welcome your comments and contributions.  You can send email to me
at <bos@serpentine.com>.  If you want to send patches, please get a
copy of the darcs repository:

  darcs get http://darcs.serpentine.com/llvm

Thanks!
