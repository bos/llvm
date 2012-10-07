{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Analysis where
import Foreign.C.String(CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types(CInt(..))
#else
import Foreign.C.Types(CInt)
#endif
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

type VerifierFailureAction = CInt

foreign import ccall unsafe "LLVMVerifyFunction" verifyFunction
    :: ValueRef -> VerifierFailureAction -> IO Bool
foreign import ccall unsafe "LLVMVerifyModule" verifyModule
    :: ModuleRef -> VerifierFailureAction -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMViewFunctionCFG" viewFunctionCFG
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMViewFunctionCFGOnly" viewFunctionCFGOnly
    :: ValueRef -> IO ()
