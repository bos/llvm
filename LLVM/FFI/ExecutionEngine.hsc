{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module LLVM.FFI.ExecutionEngine
    (
    -- * Execution engines
      ExecutionEngine
    , createExecutionEngine
    , disposeExecutionEngine
    , createInterpreter
    , createJITCompiler
    , addModuleProvider
    , removeModuleProvider
    , findFunction
    , freeMachineCodeForFunction
    , runStaticConstructors
    , runStaticDestructors
    , runFunction
    , runFunctionAsMain
    , getExecutionEngineTargetData
    , addGlobalMapping

    -- * Generic values
    , GenericValue
    , GenericValueRef
    , createGenericValueOfInt
    , genericValueToInt
    , genericValueIntWidth
    , createGenericValueOfFloat
    , genericValueToFloat
    , createGenericValueOfPointer
    , genericValueToPointer
    , disposeGenericValue
    ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble, CInt, CUInt, CULLong)
import Foreign.Ptr (Ptr)

import LLVM.FFI.Core (ModuleRef, ModuleProviderRef, TypeRef, ValueRef)
import LLVM.FFI.Target(TargetDataRef)

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
    :: TypeRef -> GenericValueRef -> CDouble

foreign import ccall unsafe "LLVMDisposeGenericValue" disposeGenericValue
    :: GenericValueRef -> IO ()

foreign import ccall unsafe "LLVMRunFunction" runFunction
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr GenericValueRef -> IO GenericValueRef

foreign import ccall unsafe "LLVMAddModuleProvider" addModuleProvider
    :: ExecutionEngineRef -> ModuleProviderRef -> IO ()
foreign import ccall unsafe "LLVMCreateGenericValueOfPointer"
    createGenericValueOfPointer :: Ptr a -> IO GenericValueRef
foreign import ccall unsafe "LLVMCreateInterpreter" createInterpreter
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMCreateJITCompiler" createJITCompiler
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMFindFunction" findFunction
    :: ExecutionEngineRef -> CString -> Ptr ValueRef -> IO CInt
foreign import ccall unsafe "LLVMFreeMachineCodeForFunction"
    freeMachineCodeForFunction :: ExecutionEngineRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMGenericValueIntWidth" genericValueIntWidth
    :: GenericValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGenericValueToPointer" genericValueToPointer
    :: GenericValueRef -> IO (Ptr a)
foreign import ccall unsafe "LLVMRemoveModuleProvider" removeModuleProvider
    :: ExecutionEngineRef -> ModuleProviderRef -> Ptr ModuleRef -> Ptr CString
    -> IO CInt
foreign import ccall unsafe "LLVMRunFunctionAsMain" runFunctionAsMain
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr CString              -- ^ argv
    -> Ptr CString              -- ^ envp
    -> IO CInt

foreign import ccall unsafe "LLVMGetExecutionEngineTargetData" getExecutionEngineTargetData
    :: ExecutionEngineRef -> IO TargetDataRef
foreign import ccall unsafe "LLVMAddGlobalMapping" addGlobalMapping
    :: ExecutionEngineRef -> ValueRef -> Ptr () -> IO ()
