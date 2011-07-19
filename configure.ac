AC_INIT([Haskell LLVM bindings], [0.9.1.2], [bos@serpentine.com], [llvm])

AC_CONFIG_SRCDIR([LLVM/ExecutionEngine.hs])

AC_CONFIG_FILES([llvm.buildinfo])

AC_CONFIG_HEADERS([include/hs_llvm_config.h])

AC_PROG_CXX

AC_LANG(C++)

AC_ARG_WITH(compiler,
  [AS_HELP_STRING([--with-compiler],
    [use the given Haskell compiler])],
  compiler="$withval",
  compiler=ghc)dnl

AC_ARG_WITH(llvm_prefix,
  [AS_HELP_STRING([--with-llvm-prefix],
    [use the version of LLVM at the given location])],
  llvm_prefix="$withval",
  llvm_prefix="$prefix")dnl

AC_ARG_WITH(llvm_bindir,
  [AS_HELP_STRING([--with-llvm-bindir],
    [use LLVM binaries at the given location])],
  llvm_bindir="$withval",
  llvm_bindir="$llvm_prefix/bin")dnl

AC_PATH_PROGS(llvm_config, [llvm-config],
  [AC_MSG_ERROR(could not find llvm-config in $llvm_bindir)],
  ["$llvm_bindir:$PATH"])
  
dnl * Choose target platform
dnl
dnl We don't use the standard autoconf macros for this, but instead
dnl ask GHC what platform it is for.  Why?  We need to generate a library
dnl matching the compiler.
dnl NB: This code is from GHC's configure (where the corresponding code for
dnl guessing host and build variables can be found, too)

dnl Guess target platform if necessary.
m4_divert_once([HELP_CANON],
[[
System types:
  --target=TARGET   configure for building compilers for TARGET [guessed]]])dnl

if test "$target" = ""
then
    if test "${compiler}" != ""
    then
        target=`${compiler} +RTS --info | grep '^ ,("Target platform"' | sed -e 's/.*, "//' -e 's/")//' | tr -d '\r'`
        echo "Target platform inferred as: $target"
    else
        echo "Can't work out target platform"
        exit 1
    fi
fi

dnl Determine target-specific options
dnl This is important as Snow Leopard (Mac OS X 10.6) defaults to generating
dnl 64-bit code.
case $target in
i386-apple-darwin)
    TARGET_CPPFLAGS="-m32"
    TAGRET_LDFLAGS="-m32"
    ;;
x86_64-apple-darwin)
    TARGET_CPPFLAGS="-m64"
    TAGRET_LDFLAGS="-m64"
    ;;
esac

llvm_version="`$llvm_config --version`"
llvm_cppflags="`$llvm_config --cppflags`"
llvm_includedir="`$llvm_config --includedir`"
llvm_ldflags="`$llvm_config --ldflags`"

llvm_all_libs="`$llvm_config --libs all`"
llvm_target="`$llvm_config --libs engine | sed 's/.*LLVM\(.[[^ ]]*\)CodeGen.*/\1/'`"

dnl We need to separate libraries that need to be linked from other linker options.
llvm_extra_libs=""
llvm_extra_libdirs=""
llvm_extra_ghci_libs=""
llvm_ldoptions=""
for opt in $llvm_all_libs $llvm_ldflags; do
  case $opt in
    -l*) llvm_extra_libs="$llvm_extra_libs `echo $opt | sed 's/^-l//'`";;
    -L*) llvm_extra_libdirs="$llvm_extra_libdirs `echo $opt | sed 's/^-L//'`";;
    */lib*.so|*/lib*.dylib)
         llvm_extra_ghci_libs="$llvm_extra_ghci_libs `echo $opt | sed -e 's,^.*/lib\([^/]*\),\1,' -e 's/\.so$//' -e 's/\.dylib$//'`";;
    *)   llvm_ldoptions="$llvm_ldoptions $opt";;
  esac
done

CPPFLAGS="$llvm_cppflags $CPPFLAGS $TARGET_CPPFLAGS"
LDFLAGS="$llvm_ldflags $LDFLAGS $TARGET_LDFLAGS"

AC_CHECK_HEADERS([llvm-c/Core.h], [],
  [AC_MSG_ERROR(could not find LLVM C bindings)])

AC_CHECK_HEADERS([llvm/Support/DynamicLibrary.h])

save_LIBS="$LIBS"
LIBS="-lLLVMSupport -lLLVMSystem -lpthread -ldl $LIBS"

AC_CHECK_LIB(LLVMCore, LLVMModuleCreateWithName, [], [])
if test "$ac_cv_lib_LLVMCore_LLVMModuleCreateWithName" = "no"; then
  unset ac_cv_lib_LLVMCore_LLVMModuleCreateWithName
  LIBS="-lLLVMSupport $save_LIBS"
  AC_CHECK_LIB(LLVMCore, LLVMModuleCreateWithName, [],
    [AC_MSG_ERROR(could not find LLVM C bindings)])
fi

AC_SUBST([llvm_version])
AC_SUBST([llvm_cppflags])
AC_SUBST([llvm_extra_libs])
AC_SUBST([llvm_extra_libdirs])
AC_SUBST([llvm_extra_ghci_libs])
AC_SUBST([llvm_target])
AC_SUBST([llvm_includedir])
AC_SUBST([llvm_ldoptions])

AC_OUTPUT
