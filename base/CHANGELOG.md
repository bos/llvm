#Changes

### 06-06-2013 Scott West <scott.gregory.west@gmail.com>
  -  Updated to support more of the LLVM C API, including some global
     functions, type functions, and more.
  -  Added some of the exception handling mechanisms including invoke and 
     friends.
  -  Removed LLVM 2.x functions such as TypeHandles which have been removed
     in more recent versions of LLVM in favour of Struct types.
  -  Added check to configure.ac to accept only LLVM 3.x.
