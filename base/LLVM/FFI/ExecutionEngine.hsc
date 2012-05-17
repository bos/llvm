{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module LLVM.FFI.ExecutionEngine
    (
    -- * Linking
      linkInInterpreter
    , linkInJIT

    -- * Generic values
    , GenericValue
    , GenericValueRef
    , createGenericValueOfInt
    , createGenericValueOfPointer
    , createGenericValueOfFloat
    , genericValueIntWidth
    , genericValueToInt
    , genericValueToPointer
    , genericValueToFloat
    , ptrDisposeGenericValue

    -- * Execution engines
    , ExecutionEngine
    , createExecutionEngineForModule
    , createInterpreterForModule
    , createJITCompilerForModule
    , createExecutionEngine
    , createInterpreter
    , createJITCompiler
    , ptrDisposeExecutionEngine
    , disposeExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    , runFunctionAsMain
    , freeMachineCodeForFunction
    , addModule
    , addModuleProvider
    , removeModule
    , removeModuleProvider
    , findFunction
    , recompileAndRelinkFunction
    , runFunction
    , getExecutionEngineTargetData
    , addGlobalMapping
    , getPointerToGlobal

    ) where
import Data.Typeable
import Foreign.C.String (CString)
#if __GLASGOW_HASKELL__ >= 704
import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CULLong(..))
#else
import Foreign.C.Types (CDouble, CInt, CUInt, CULLong)
#endif
import Foreign.Ptr (Ptr, FunPtr)

import LLVM.FFI.Core (ModuleRef, ModuleProviderRef, TypeRef, ValueRef)
import LLVM.FFI.Target(TargetDataRef)

data ExecutionEngine
    deriving (Typeable)
type ExecutionEngineRef = Ptr ExecutionEngine

data GenericValue
    deriving (Typeable)
type GenericValueRef = Ptr GenericValue

-- ** Linking
foreign import ccall unsafe "LLVMLinkInInterpreter" linkInInterpreter
    :: IO ()
foreign import ccall unsafe "LLVMLinkInJIT" linkInJIT
    :: IO ()

-- ** Generic values
foreign import ccall unsafe "LLVMCreateGenericValueOfInt"
    createGenericValueOfInt :: TypeRef -> CULLong -> CInt
                            -> IO GenericValueRef
foreign import ccall unsafe "LLVMCreateGenericValueOfPointer"
    createGenericValueOfPointer :: Ptr a -> IO GenericValueRef
foreign import ccall unsafe "LLVMCreateGenericValueOfFloat"
    createGenericValueOfFloat :: TypeRef -> CDouble -> IO GenericValueRef
foreign import ccall unsafe "LLVMGenericValueIntWidth" genericValueIntWidth
    :: GenericValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGenericValueToInt" genericValueToInt
    :: GenericValueRef -> CInt -> CULLong
foreign import ccall unsafe "LLVMGenericValueToPointer" genericValueToPointer
    :: GenericValueRef -> IO (Ptr a)
foreign import ccall unsafe "LLVMGenericValueToFloat" genericValueToFloat
    :: TypeRef -> GenericValueRef -> CDouble
foreign import ccall unsafe "&LLVMDisposeGenericValue" ptrDisposeGenericValue
    :: FunPtr (GenericValueRef -> IO ())

-- ** Execution engines
foreign import ccall unsafe "LLVMCreateExecutionEngineForModule" createExecutionEngineForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMCreateInterpreterForModule" createInterpreterForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMCreateJITCompilerForModule" createJITCompilerForModule
    :: (Ptr ExecutionEngineRef) -> ModuleRef -> CUInt -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMCreateExecutionEngine" createExecutionEngine
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> Ptr CString
    -> IO CInt
foreign import ccall unsafe "LLVMCreateInterpreter" createInterpreter
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMCreateJITCompiler" createJITCompiler
    :: Ptr ExecutionEngineRef -> ModuleProviderRef -> CUInt -> Ptr CString -> IO CInt
foreign import ccall unsafe "LLVMDisposeExecutionEngine" disposeExecutionEngine
    :: ExecutionEngineRef -> IO ()
foreign import ccall unsafe "&LLVMDisposeExecutionEngine" ptrDisposeExecutionEngine
    :: FunPtr (ExecutionEngineRef -> IO ())
foreign import ccall unsafe "LLVMRunStaticConstructors" runStaticConstructors
    :: ExecutionEngineRef -> IO ()
foreign import ccall unsafe "LLVMRunStaticDestructors" runStaticDestructors
    :: ExecutionEngineRef -> IO ()
{-
safe call is important, since the running LLVM code may call back into Haskell code

See
http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffise3.html#x6-130003.3 says:

"Optionally, an import declaration can specify,
after the calling  convention,
the safety level that should be used when invoking an external entity.
..."
-}
foreign import ccall safe "LLVMRunFunctionAsMain" runFunctionAsMain
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr CString              -- ^ argv
    -> Ptr CString              -- ^ envp
    -> IO CInt
foreign import ccall safe "LLVMRunFunction" runFunction
    :: ExecutionEngineRef -> ValueRef -> CUInt
    -> Ptr GenericValueRef -> IO GenericValueRef
foreign import ccall unsafe "LLVMFreeMachineCodeForFunction"
    freeMachineCodeForFunction :: ExecutionEngineRef -> ValueRef -> IO ()
foreign import ccall unsafe "LLVMAddModule" addModule
    :: ExecutionEngineRef -> ModuleRef -> IO ()
foreign import ccall unsafe "LLVMAddModuleProvider" addModuleProvider
    :: ExecutionEngineRef -> ModuleProviderRef -> IO ()
foreign import ccall unsafe "LLVMRemoveModule" removeModule
    :: ExecutionEngineRef -> ModuleRef -> (Ptr ModuleRef) -> (Ptr CString) -> IO Bool
foreign import ccall unsafe "LLVMRemoveModuleProvider" removeModuleProvider
    :: ExecutionEngineRef -> ModuleProviderRef -> Ptr ModuleRef -> Ptr CString
    -> IO CInt
foreign import ccall unsafe "LLVMFindFunction" findFunction
    :: ExecutionEngineRef -> CString -> Ptr ValueRef -> IO CInt
foreign import ccall unsafe "LLVMRecompileAndRelinkFunction" recompileAndRelinkFunction
    :: ExecutionEngineRef -> ValueRef -> IO (FunPtr a)
foreign import ccall unsafe "LLVMGetExecutionEngineTargetData" getExecutionEngineTargetData
    :: ExecutionEngineRef -> IO TargetDataRef
foreign import ccall unsafe "LLVMAddGlobalMapping" addGlobalMapping
    :: ExecutionEngineRef -> ValueRef -> Ptr () -> IO ()
foreign import ccall unsafe "LLVMGetPointerToGlobal" getPointerToGlobal
    :: ExecutionEngineRef -> ValueRef -> IO (FunPtr a)
