AC_INIT([HaskLLVM],[0.1],[bos@serpentine.com], [llvm])

AC_CONFIG_SRCDIR([LLVM/ExecutionEngine.hs])

AC_CONFIG_FILES([llvm.buildinfo])

AC_PROG_CXX

AC_ARG_WITH(llvm_prefix,
  [AS_HELP_STRING([--with-llvm-prefix],
    [use the version of LLVM at the given location])],
  llvm_prefix="$withval",
  llvm_prefix=/usr/local)dnl

AC_ARG_WITH(llvm_includedir,
  [AS_HELP_STRING([--with-llvm-includedir],
    [use LLVM headers at the given location])],
  llvm_includedir="$withval",
  llvm_includedir="$llvm_prefix/include")dnl

AC_ARG_WITH(llvm_libdir,
  [AS_HELP_STRING([--with-llvm-libdir],
    [use LLVM libraries at the given location])],
  llvm_libdir="$withval",
  llvm_libdir="$llvm_prefix/lib")dnl

CPPFLAGS="-I$llvm_includedir $CPPFLAGS"

AC_CHECK_HEADERS([llvm-c/Core.h], [],
  [AC_MSG_ERROR(could not find LLVM C bindings in $llvm_includedir/llvm-c)])

LIBS="-lLLVMSupport -lLLVMSystem $LIBS"
LDFLAGS="-L$llvm_libdir $LDFLAGS"

# We have to link using the C++ compiler.
CC=$CXX

AC_CHECK_LIB(LLVMCore, LLVMModuleCreateWithName)

AC_SUBST([llvm_libdir])

AC_OUTPUT
