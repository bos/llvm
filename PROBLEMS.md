Known problems
--------------

If you have solutions to any of the problems listed below, please let
me know, or better yet, send a patch.  Thanks!


Can't use LLVM bindings from ghci
---------------------------------

When I try to use the LLVM bindings in `ghci`, on Linux, loading the
bindings succeeds, but trying to do anything fails:

    $ ghci
    Prelude> :m +LLVM.Core
    Prelude LLVM.Core> m <- createModule "foo"
    can't load .so/.DLL for: stdc++ (libstdc++.so: cannot open shared
      object file: No such file or directory)

I don't know why this happens, but it looks like a `ghci` bug.
