{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, UndecidableInstances, OverlappingInstances, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module LLVM.ExecutionEngine.Engine(
       EngineAccess,
       runEngineAccess,
{-
       ExecutionEngine,
-}
       createExecutionEngine, addModuleProvider, addModule,
       {- runStaticConstructors, runStaticDestructors, -}
       getExecutionEngineTargetData,
       getPointerToFunction,
       runFunction, getRunFunction,
       GenericValue, Generic(..)
       ) where
import Control.Monad.State
import Control.Concurrent.MVar
import Data.Typeable
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (fromBool)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr)
import Foreign.Ptr (FunPtr)
import LLVM.Core.CodeGen(Value(..), Function)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

import LLVM.Core.Util(Module, ModuleProvider, withModuleProvider, createModule, createModuleProviderForExistingModule)
import qualified LLVM.FFI.ExecutionEngine as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.Core.Util(Function)
import LLVM.Core.Type(IsFirstClass, typeRef)

{-
-- |The type of the JITer.
newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine = withForeignPtr . fromExecutionEngine

-- |Create an execution engine for a module provider.
-- Warning, do not call this function more than once.
createExecutionEngine :: ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- FFI.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then do err <- peek errPtr
                    errStr <- peekCString err
                    free err
                    ioError . userError $ errStr
            else do ptr <- peek eePtr
                    liftM ExecutionEngine $ newForeignPtr FFI.ptrDisposeExecutionEngine ptr

addModuleProvider :: ExecutionEngine -> ModuleProvider -> IO ()
addModuleProvider ee prov =
    withExecutionEngine ee $ \ eePtr ->
      withModuleProvider prov $ \ provPtr ->
        FFI.addModuleProvider eePtr provPtr

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors

getExecutionEngineTargetData :: ExecutionEngine -> IO FFI.TargetDataRef
getExecutionEngineTargetData ee = withExecutionEngine ee FFI.getExecutionEngineTargetData

getPointerToFunction :: ExecutionEngine -> Function f -> IO (FunPtr f)
getPointerToFunction ee (Value f) =
    withExecutionEngine ee $ \ eePtr ->
      FFI.getPointerToGlobal eePtr f
-}

-- This global variable holds the one and only execution engine.
-- It may be missing, but it never dies.
-- XXX We could provide a destructor, what about functions obtained by runFunction?
{-# NOINLINE theEngine #-}
theEngine :: MVar (Maybe (Ptr FFI.ExecutionEngine))
theEngine = unsafePerformIO $ newMVar Nothing

createExecutionEngine :: ModuleProvider -> IO (Ptr FFI.ExecutionEngine)
createExecutionEngine prov =
    withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- FFI.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then do
                err <- peek errPtr
                errStr <- peekCString err
                free err
                ioError . userError $ errStr
            else
                peek eePtr

getTheEngine :: IO (Ptr FFI.ExecutionEngine)
getTheEngine = do
    mee <- takeMVar theEngine
    case mee of
        Just ee -> do putMVar theEngine mee; return ee
        Nothing -> do
            m <- createModule "__empty__"
            mp <- createModuleProviderForExistingModule m
            ee <- createExecutionEngine mp
            putMVar theEngine (Just ee)
            return ee

data EAState = EAState {
    ea_engine :: Ptr FFI.ExecutionEngine,
    ea_providers :: [ModuleProvider]
    }
    deriving (Show, Typeable)

newtype EngineAccess a = EA (StateT EAState IO a)
    deriving (Functor, Monad, MonadState EAState, MonadIO)

-- |The LLVM execution engine is encapsulated so it cannot be accessed directly.
-- The reason is that (currently) there must only ever be one engine,
-- so access to it is wrapped in a monad.
runEngineAccess :: EngineAccess a -> IO a
runEngineAccess (EA body) = do
    eePtr <- getTheEngine
    let ea = EAState { ea_engine = eePtr, ea_providers = [] }
    (a, _ea') <- runStateT body ea
    -- XXX should remove module providers again
    return a

addModuleProvider :: ModuleProvider -> EngineAccess ()
addModuleProvider prov = do
    ea <- get
    put ea{ ea_providers = prov : ea_providers ea }
    liftIO $ withModuleProvider prov $ \ provPtr ->
                 FFI.addModuleProvider (ea_engine ea) provPtr

getExecutionEngineTargetData :: EngineAccess FFI.TargetDataRef
getExecutionEngineTargetData = do
    eePtr <- gets ea_engine
    liftIO $ FFI.getExecutionEngineTargetData eePtr

getPointerToFunction :: Function f -> EngineAccess (FunPtr f)
getPointerToFunction (Value f) = do
    eePtr <- gets ea_engine
    liftIO $ FFI.getPointerToGlobal eePtr f

addModule :: Module -> EngineAccess ()
addModule m = do
    mp <- liftIO $ createModuleProviderForExistingModule m
    addModuleProvider mp

--------------------------------------

newtype GenericValue = GenericValue {
      fromGenericValue :: ForeignPtr FFI.GenericValue
    }

withGenericValue :: GenericValue -> (FFI.GenericValueRef -> IO a) -> IO a
withGenericValue = withForeignPtr . fromGenericValue

createGenericValueWith :: IO FFI.GenericValueRef -> IO GenericValue
createGenericValueWith f = do
  ptr <- f
  liftM GenericValue $ newForeignPtr FFI.ptrDisposeGenericValue ptr

withAll :: [GenericValue] -> (Int -> Ptr FFI.GenericValueRef -> IO a) -> IO a
withAll ps a = go [] ps
    where go ptrs (x:xs) = withGenericValue x $ \ptr -> go (ptr:ptrs) xs
          go ptrs _ = withArrayLen (reverse ptrs) a
                   
runFunction :: LLVM.Core.Util.Function -> [GenericValue] -> EngineAccess GenericValue
runFunction func args = do
    eePtr <- gets ea_engine
    liftIO $ withAll args $ \argLen argPtr ->
                 createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr
getRunFunction :: EngineAccess (LLVM.Core.Util.Function -> [GenericValue] -> IO GenericValue)
getRunFunction = do
    eePtr <- gets ea_engine
    return $ \ func args -> 
             withAll args $ \argLen argPtr ->
                 createGenericValueWith $ FFI.runFunction eePtr func
                                              (fromIntegral argLen) argPtr

class Generic a where
    toGeneric :: a -> GenericValue
    fromGeneric :: GenericValue -> a

instance Generic () where
    toGeneric _ = error "toGeneric ()"
    fromGeneric _ = ()

toGenericInt :: (Integral a, IsFirstClass a) => Bool -> a -> GenericValue
toGenericInt signed val = unsafePerformIO $ createGenericValueWith $
    FFI.createGenericValueOfInt (typeRef val) (fromIntegral val) (fromBool signed)

fromGenericInt :: (Integral a, IsFirstClass a) => Bool -> GenericValue -> a
fromGenericInt signed val = unsafePerformIO $
    withGenericValue val $ \ref ->
      return . fromIntegral $ FFI.genericValueToInt ref (fromBool signed)

--instance Generic Bool where
--    toGeneric = toGenericInt False . fromBool
--    fromGeneric = toBool . fromGenericInt False

instance Generic Int8 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int16 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Int32 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

{-
instance Generic Int where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True
-}

instance Generic Int64 where
    toGeneric = toGenericInt True
    fromGeneric = fromGenericInt True

instance Generic Word8 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word16 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word32 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

instance Generic Word64 where
    toGeneric = toGenericInt False
    fromGeneric = fromGenericInt False

toGenericReal :: (Real a, IsFirstClass a) => a -> GenericValue
toGenericReal val = unsafePerformIO $ createGenericValueWith $
    FFI.createGenericValueOfFloat (typeRef val) (realToFrac val)

fromGenericReal :: forall a . (Fractional a, IsFirstClass a) => GenericValue -> a
fromGenericReal val = unsafePerformIO $
    withGenericValue val $ \ ref ->
      return . realToFrac $ FFI.genericValueToFloat (typeRef (undefined :: a)) ref

instance Generic Float where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic Double where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic (Ptr a) where
    toGeneric = unsafePerformIO . createGenericValueWith . FFI.createGenericValueOfPointer
    fromGeneric val = unsafePerformIO . withGenericValue val $ FFI.genericValueToPointer

