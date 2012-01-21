{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module LLVM.FFI.Support
    (
      createStandardModulePasses
    , createStandardFunctionPasses
    ) where

#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..), CUInt(..))
#else
import Foreign.C.Types (CInt, CUInt)
#endif
import LLVM.FFI.Core (PassManagerRef)

foreign import ccall unsafe "LLVMCreateStandardFunctionPasses" createStandardFunctionPasses
    :: PassManagerRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCreateStandardModulePasses" createStandardModulePasses
    :: PassManagerRef -> CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
