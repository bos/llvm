{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module LLVM.ExecutionEngine.Engine(
       ExecutionEngine,
       createExecutionEngine, runStaticConstructors, runStaticDestructors,
       runFunction,
       GenericValue, Generic(..)
       ) where
import Control.Monad
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.Marshal.Utils (fromBool)
import Foreign.C.String (peekCString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

import LLVM.Core.Util(ModuleProvider, withModuleProvider)
import qualified LLVM.FFI.ExecutionEngine as FFI
import LLVM.Core.Util(Function)
import LLVM.Core.Type(IsFirstClass, IsType(..))

newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine ee = withForeignPtr (fromExecutionEngine ee)

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
                    final <- h2c_ee FFI.disposeExecutionEngine
                    liftM ExecutionEngine $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_ee
    :: (Ptr FFI.ExecutionEngine -> IO ()) -> IO (FinalizerPtr a)

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors

--------------------------------------

newtype GenericValue = GenericValue {
      fromGenericValue :: ForeignPtr FFI.GenericValue
    }

withGenericValue :: GenericValue -> (FFI.GenericValueRef -> IO a) -> IO a
withGenericValue = withForeignPtr . fromGenericValue

createGenericValueWith :: IO FFI.GenericValueRef -> IO GenericValue
createGenericValueWith f = do
  final <- h2c_genericValue FFI.disposeGenericValue
  ptr <- f
  liftM GenericValue $ newForeignPtr final ptr

foreign import ccall "wrapper" h2c_genericValue
    :: (FFI.GenericValueRef -> IO ()) -> IO (FinalizerPtr a)

withAll :: [GenericValue] -> (Int -> Ptr FFI.GenericValueRef -> IO a) -> IO a
withAll ps a = go [] ps
    where go ptrs (x:xs) = withGenericValue x $ \ptr -> go (ptr:ptrs) xs
          go ptrs _ = withArrayLen (reverse ptrs) a
                   
runFunction :: ExecutionEngine -> LLVM.Core.Util.Function -> [GenericValue]
            -> IO GenericValue
runFunction ee func args =
    withExecutionEngine ee $ \eePtr ->
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

fromGenericReal :: (Fractional a, IsFirstClass a) => GenericValue -> a
fromGenericReal val = unsafePerformIO $
    withGenericValue val $ \ ref ->
      return . realToFrac $ FFI.genericValueToFloat ref

instance Generic Float where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal

instance Generic Double where
    toGeneric = toGenericReal
    fromGeneric = fromGenericReal
