AC_INIT([Haskell LLVM bindings], [0.1], [bos@serpentine.com], [llvm])

AC_CONFIG_SRCDIR([LLVM/ExecutionEngine.hs])

AC_CONFIG_FILES([llvm.buildinfo])

AC_PROG_CXX

AC_ARG_WITH(llvm_prefix,
  [AS_HELP_STRING([--with-llvm-prefix],
    [use the version of LLVM at the given location])],
  llvm_prefix="$withval",
  llvm_prefix=/usr/local)dnl

AC_ARG_WITH(llvm_bindir,
  [AS_HELP_STRING([--with-llvm-bindir],
    [use LLVM binaries at the given location])],
  llvm_bindir="$withval",
  llvm_bindir="$llvm_prefix/bin")dnl

AC_PATH_PROG(llvm_config, llvm-config,
  [AC_MSG_ERROR(could not find llvm-config in $llvm_bindir)],
  [$llvm_bindir])

llvm_cppflags="`$llvm_config --cppflags`"
llvm_includedir="`$llvm_config --includedir`"
llvm_ldflags="`$llvm_config --ldflags`"

llvm_engine_libs="`$llvm_config --libs engine`"

CPPFLAGS="$llvm_cppflags $CPPFLAGS"
LDFLAGS="$llvm_ldflags $LDFLAGS"

AC_CHECK_HEADERS([llvm-c/Core.h], [],
  [AC_MSG_ERROR(could not find LLVM C bindings)])

LIBS="-lLLVMSupport -lLLVMSystem $LIBS"

# We have to link using the C++ compiler.
CC=$CXX

AC_CHECK_LIB(LLVMCore, LLVMModuleCreateWithName, [],
  [AC_MSG_ERROR(could not find LLVM C bindings)])

AC_SUBST([llvm_cppflags llvm_engine_libs llvm_includedir llvm_ldflags])

AC_OUTPUT
