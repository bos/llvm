{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.Analysis where
import Foreign.C.String(CString)
import Foreign.C.Types(CInt)
import Foreign.Ptr(Ptr)

import LLVM.FFI.Core

type VerifierFailureAction = CInt

foreign import ccall unsafe "LLVMVerifyFunction" verifyFunction
    :: ValueRef -> VerifierFailureAction -> IO CInt
foreign import ccall unsafe "LLVMVerifyModule" verifyModule
    :: ModuleRef -> VerifierFailureAction -> (Ptr CString) -> IO CInt
foreign import ccall unsafe "LLVMViewFunctionCFG" viewFunctionCFG
    :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMViewFunctionCFGOnly" viewFunctionCFGOnly
    :: ValueRef -> IO ()
