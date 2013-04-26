{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module LLVM.FFI.Linker where
import LLVM.FFI.Core
import Foreign.C.String(CString)
import Foreign.C.Types(CUInt(..))
import Foreign.Ptr(Ptr)

#include <llvm-c/Linker.h>

data LinkerMode = DestroySource | PreserveSource
                  deriving (Show, Eq)

fromLinkerMode :: LinkerMode -> CUInt
fromLinkerMode DestroySource = (#const LLVMLinkerDestroySource)
fromLinkerMode PreserveSource = (#const LLVMLinkerDestroySource)

toLinkerMode :: CUInt -> LinkerMode
toLinkerMode c | c == (#const LLVMLinkerDestroySource) = DestroySource
toLinkerMode c | c == (#const LLVMLinkerPreserveSource) = PreserveSource
toLinkerMode c = error $ "LLVM.FFI.Linker.toLinkerMode: unrecognized linker mode" ++ show c

foreign import ccall unsafe "LLVMLinkModules" linkModules
    :: ModuleRef -> ModuleRef -> CUInt -> Ptr CString -> IO Bool