{-# LANGUAGE EmptyDataDecls #-}

module LLVM.ExecutionEngine.FFI
    (
    -- * Execution engines
      ExecutionEngine
    , createExecutionEngine
    , disposeExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    , runFunction

    -- * Generic values
    , GenericValue
    ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr (Ptr)

import LLVM.Core.FFI (ModuleProviderRef, ValueRef)

#include <llvm-c/ExecutionEngine.h>

data ExecutionEngine
type ExecutionEngineRef = Ptr ExecutionEngine

foreign import ccall unsafe "LLVMCreateExecutionEngine" createExecutionEngine
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> Ptr CString
    -> IO CInt

foreign import ccall unsafe "LLVMDisposeExecutionEngine" disposeExecutionEngine
    :: ExecutionEngineRef -> IO ()

foreign import ccall unsafe "LLVMRunStaticConstructors" runStaticConstructors
    :: ExecutionEngineRef -> IO ()

foreign import ccall unsafe "LLVMRunStaticDestructors" runStaticDestructors
    :: ExecutionEngineRef -> IO ()


data GenericValue
type GenericValueRef = Ptr GenericValue

foreign import ccall unsafe "LLVMRunFunction" runFunction
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr GenericValueRef -> IO GenericValueRef
