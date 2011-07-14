{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.FFI.Support
    (
      createStandardModulePasses
    , createStandardFunctionPasses
    ) where

import Foreign.C.Types (CInt, CUInt)
import LLVM.FFI.Core (PassManagerRef)

foreign import ccall unsafe "LLVMCreateStandardFunctionPasses" createStandardFunctionPasses
    :: PassManagerRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCreateStandardModulePasses" createStandardModulePasses
    :: PassManagerRef -> CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
