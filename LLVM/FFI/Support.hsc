{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.FFI.Support
    (
      createStandardFunctionPasses
    ) where

import Foreign.C.Types (CUInt)
import LLVM.FFI.Core

foreign import ccall unsafe "LLVMCreateStandardFunctionPasses" createStandardFunctionPasses
    :: PassManagerRef -> CUInt -> IO ()
