Haskell LLVM bindings
---------------------

This package provides Haskell bindings for the popular LLVM compiler
infrastructure project.  If you don't know what LLVM is, the main LLVM
home page is here:

  http://llvm.org/


Package status - what to expect
-------------------------------

This package is still under development.

The high level bindings are currently incomplete, so there are some
limits on what you can do.  Adding new functions is generally easy,
though, so don't be afraid to get your hands dirty.

The high level interface is mostly safe, but the type system does not
protect against anything that can go wrong, so take care.
And, of course, there's no way to guarantee anything about the
generated code.


Jump in and help!
-----------------

We welcome your comments and contributions.  You can send email to us
at <bos@serpentine.com> or <lennart@augustsson.net>.  If you want to
send patches, please get a copy of the darcs repository:

  darcs get http://darcs.serpentine.com/llvm

Thanks!
