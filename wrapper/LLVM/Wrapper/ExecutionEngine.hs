module LLVM.Wrapper.ExecutionEngine
    ( module LLVM.FFI.ExecutionEngine
    -- * Execution engines
    , ExecutionEngine
    , findFunction
    , runFunction
    , runFunctionAsMain

    , createExecutionEngineForModule
    , createInterpreterForModule
    , createJITCompilerForModule

    -- * Generic values
    , createGenericValueOfInt
    , genericValueToInt
    ) where

import LLVM.FFI.ExecutionEngine
    ( runStaticConstructors
    , runStaticDestructors
    , genericValueToFloat
    , createGenericValueOfFloat
    , disposeExecutionEngine
    , addModuleProvider
    , getExecutionEngineTargetData
    , freeMachineCodeForFunction
    , genericValueIntWidth
    , linkInJIT
    , addModule
    )

import qualified LLVM.FFI.ExecutionEngine as FFI.EE
import qualified LLVM.FFI.Core as FFI

import Control.Monad

import Foreign.Ptr (Ptr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

type ModuleProvider  = FFI.ModuleProviderRef
type ExecutionEngine = Ptr FFI.EE.ExecutionEngine -- FFI.EE.ExecutionEngineRef
type GenericValue    = Ptr FFI.EE.GenericValue    -- FFI.EE.GenericValueRef

type Type       = FFI.TypeRef
type Module     = FFI.ModuleRef
type Value      = FFI.ValueRef
type Builder    = FFI.BuilderRef
type BasicBlock = FFI.BasicBlockRef
type Context    = FFI.ContextRef

createGenericValueOfInt :: Type -> CULLong -> Bool -> IO GenericValue
createGenericValueOfInt ty n isSigned
    = FFI.EE.createGenericValueOfInt ty n (fromBool isSigned)

genericValueToInt :: GenericValue -> Bool -> CULLong
genericValueToInt genVal isSigned = FFI.EE.genericValueToInt genVal (fromBool isSigned)

runFunction :: ExecutionEngine -> Value -> CUInt -> [GenericValue] -> IO GenericValue
runFunction ee f numArgs args
    = withArray args $ \ptr -> FFI.EE.runFunction ee f numArgs ptr

findFunction :: ExecutionEngine -> String -> IO (Maybe Value)
findFunction ee name
    = alloca $ \funPtr ->
        withCString name $ \s -> do
          r <- liftM toBool (FFI.EE.findFunction ee s funPtr)
          if r
              then return Nothing
              else liftM Just (peek funPtr)

runFunctionAsMain :: ExecutionEngine -> Value -> [String] -> [String] -> IO Bool
runFunctionAsMain ee val argv envp
    = do argcstrs <- argcstrings
         envcstrs <- envcstrings
         withArray argcstrs $ \args ->
           withArray envcstrs $ \env -> do
             liftM toBool (FFI.EE.runFunctionAsMain ee val (fromIntegral $ length argv) args env)
  where argcstrings = mapM newCString argv
        envcstrings = mapM newCString envp

createJITCompilerForModule :: Module -> CUInt -> IO ExecutionEngine
createJITCompilerForModule mod optlvl
    = alloca $ \msgPtr ->
        alloca $ \eeref -> do
          r <- FFI.EE.createJITCompilerForModule eeref mod optlvl msgPtr
          if r
              then peek msgPtr >>= peekCString >>= fail
              else peek eeref

createInterpreterForModule :: Module -> IO ExecutionEngine
createInterpreterForModule mod
    = alloca $ \msgPtr ->
        alloca $ \eeref -> do
          r <- FFI.EE.createInterpreterForModule eeref mod msgPtr
          if r
              then peek msgPtr >>= peekCString >>= fail
              else peek eeref

createExecutionEngineForModule :: Module -> IO ExecutionEngine
createExecutionEngineForModule mod
    = alloca $ \msgPtr ->
        alloca $ \eeref -> do
          r <- FFI.EE.createExecutionEngineForModule eeref mod msgPtr
          if r
              then peek msgPtr >>= peekCString >>= fail
              else peek eeref
