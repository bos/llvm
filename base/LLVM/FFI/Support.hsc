{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module LLVM.FFI.Support
    (
      createStandardModulePasses
    , createStandardFunctionPasses
#if HS_LLVM_VERSION >= 300
    , addEmitObjectPass
#endif
    , disablePrettyStackTrace
    ) where

#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CInt(..), CUInt(..))
#else
import Foreign.C.Types (CInt, CUInt)
#endif
import Foreign.C.String (CString)
import LLVM.FFI.Core (PassManagerRef, ModuleRef)

foreign import ccall unsafe "LLVMCreateStandardFunctionPasses" createStandardFunctionPasses
    :: PassManagerRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMCreateStandardModulePasses" createStandardModulePasses
    :: PassManagerRef -> CUInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

#if HS_LLVM_VERSION >= 300
foreign import ccall unsafe "LLVMAddEmitObjectPass" addEmitObjectPass
    :: ModuleRef -> CString -> IO CUInt    
#endif

foreign import ccall unsafe "LLVMDisablePrettyStackTrace" disablePrettyStackTrace
    :: IO ()
