Known problems
--------------

If you have solutions to any of the problems listed below, please let
me know, or better yet, send a patch.  Thanks!


Can't use LLVM bindings from ghci
---------------------------------

ghci versions < 7.7 have their own special linker that dynamically
links static libraries rather than using the system dynamic linker.
This is the source of a long history of ffi + ghci bugs, and fundamentally unfixable.

If you have troubles using llvm with ghci versions >= 7.7 , that is a
bug on the GHCI or llvm-hs sides, please file a bug report so we can resolve it.
