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
    , GenericValueRef
    , createGenericValueOfInt
    , genericValueToInt
    , createGenericValueOfFloat
    , genericValueToFloat
    , disposeGenericValue
    ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble, CInt, CUInt, CULLong)
import Foreign.Ptr (Ptr)

import LLVM.Core.FFI (ModuleProviderRef, TypeRef, ValueRef)

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

foreign import ccall unsafe "LLVMCreateGenericValueOfInt"
    createGenericValueOfInt :: TypeRef -> CULLong -> CInt
                            -> IO GenericValueRef

foreign import ccall unsafe "LLVMGenericValueToInt" genericValueToInt
    :: GenericValueRef -> CInt -> CULLong

foreign import ccall unsafe "LLVMCreateGenericValueOfFloat"
    createGenericValueOfFloat :: TypeRef -> CDouble -> IO GenericValueRef

foreign import ccall unsafe "LLVMGenericValueToFloat" genericValueToFloat
    :: GenericValueRef -> CDouble

foreign import ccall unsafe "LLVMDisposeGenericValue" disposeGenericValue
    :: GenericValueRef -> IO ()

foreign import ccall unsafe "LLVMRunFunction" runFunction
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr GenericValueRef -> IO GenericValueRef
