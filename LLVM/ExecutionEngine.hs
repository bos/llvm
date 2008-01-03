{-# LANGUAGE
   DeriveDataTypeable
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.ExecutionEngine
    (
    -- * Execution engines
      ExecutionEngine
    , createExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    , runFunction

    -- * Generic values
    , GenericValue
    , Generic(..)
    ) where

import Control.Applicative ((<$>))
import Control.Exception (ioError)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.ExecutionEngine.FFI as FFI
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V

newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine ee = withForeignPtr (fromExecutionEngine ee)

createExecutionEngine :: T.ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    T.withModuleProvider prov $ \provPtr ->
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
                    ExecutionEngine <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_ee
    :: (Ptr FFI.ExecutionEngine -> IO ()) -> IO (FinalizerPtr a)

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors


newtype GenericValue t = GenericValue {
      fromGenericValue :: ForeignPtr FFI.GenericValue
    }
    deriving (Typeable)

withGenericValue :: GenericValue t -> (FFI.GenericValueRef -> IO a) -> IO a
withGenericValue = withForeignPtr . fromGenericValue

createGenericValueWith :: IO FFI.GenericValueRef -> IO (GenericValue t)
createGenericValueWith f = do
  final <- h2c_genericValue FFI.disposeGenericValue
  ptr <- f
  GenericValue <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_genericValue
    :: (FFI.GenericValueRef -> IO ()) -> IO (FinalizerPtr a)

withAll :: [GenericValue t] -> (Int -> Ptr FFI.GenericValueRef -> IO a) -> IO a
withAll ps a = go [] ps
    where go ptrs (x:xs) = withGenericValue x $ \ptr -> go (ptr:ptrs) xs
          go ptrs _ = withArrayLen (reverse ptrs) a
                   
runFunction :: ExecutionEngine -> V.Function t -> [GenericValue t]
            -> IO (GenericValue t)
runFunction ee func args =
    withExecutionEngine ee $ \eePtr ->
      withAll args $ \argLen argPtr ->
        createGenericValueWith $ FFI.runFunction eePtr (V.valueRef func)
                                        (fromIntegral argLen) argPtr

class Generic a t | a -> t where
    createGeneric :: a -> IO (GenericValue t)
    fromGeneric :: GenericValue t -> a

toGenericInt :: (Integral a, T.Type t) => t -> Bool -> a -> IO (GenericValue t)
toGenericInt typ signed val = createGenericValueWith $
    FFI.createGenericValueOfInt (T.typeRef typ)
           (fromIntegral val) (fromBool signed)

fromGenericInt :: (Integral a, T.Type t) => Bool -> GenericValue t -> a
fromGenericInt signed val = unsafePerformIO $
    withGenericValue val $ \ref ->
      return . fromIntegral $ FFI.genericValueToInt ref (fromBool signed)

instance Generic Bool T.Int1 where
    createGeneric = toGenericInt undefined False . fromBool
    fromGeneric = toBool . fromGenericInt False

instance Generic Int8 T.Int8 where
    createGeneric = toGenericInt undefined True . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt True

instance Generic Int16 T.Int16 where
    createGeneric = toGenericInt undefined True . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt True

instance Generic Int32 T.Int32 where
    createGeneric = toGenericInt undefined True . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt True

instance Generic Int64 T.Int64 where
    createGeneric = toGenericInt undefined True . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt True

instance Generic Word8 T.Int8 where
    createGeneric = toGenericInt undefined False . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt False

instance Generic Word16 T.Int16 where
    createGeneric = toGenericInt undefined False . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt False

instance Generic Word32 T.Int32 where
    createGeneric = toGenericInt undefined False . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt False

instance Generic Word64 T.Int64 where
    createGeneric = toGenericInt undefined False . fromIntegral
    fromGeneric = fromIntegral . fromGenericInt False

toGenericReal :: (Real a, T.Type t) => t -> a -> IO (GenericValue t)
toGenericReal typ val = createGenericValueWith $
    FFI.createGenericValueOfFloat (T.typeRef typ) (realToFrac val)

fromGenericReal :: (Fractional a, T.Type t) => GenericValue t -> a
fromGenericReal val = unsafePerformIO $
    withGenericValue val $ \ref ->
      return . realToFrac $ FFI.genericValueToFloat ref

instance Generic Float T.Float where
    createGeneric = toGenericReal undefined
    fromGeneric = fromGenericReal

instance Generic Double T.Double where
    createGeneric = toGenericReal undefined
    fromGeneric = fromGenericReal
