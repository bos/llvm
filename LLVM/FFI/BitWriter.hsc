{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.BitWriter where
import Foreign.C.String(CString)
import Foreign.C.Types(CInt)

import LLVM.FFI.Core

foreign import ccall unsafe "LLVMWriteBitcodeToFile" writeBitcodeToFile
    :: ModuleRef -> CString -> IO CInt
foreign import ccall unsafe "LLVMWriteBitcodeToFileHandle" writeBitcodeToFileHandle
    :: ModuleRef -> CInt -> IO CInt
