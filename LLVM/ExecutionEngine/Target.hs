{-# LANGUAGE Rank2Types, DeriveDataTypeable #-}
module LLVM.ExecutionEngine.Target(TargetData(..), getTargetData, targetDataFromString, withIntPtrType) where
import Data.Typeable
import Data.TypeLevel(Nat, reifyIntegral)
import Foreign.C.String
import System.IO.Unsafe(unsafePerformIO)

import LLVM.Core.Data(WordN)
import LLVM.ExecutionEngine.Engine(runEngineAccess, getExecutionEngineTargetData)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

type Type = FFI.TypeRef

data TargetData = TargetData {
    aBIAlignmentOfType         :: Type -> Int,
    aBISizeOfType              :: Type -> Int,
    littleEndian               :: Bool,
    callFrameAlignmentOfType   :: Type -> Int,
--  elementAtOffset            :: Type -> Word64 -> Int,
    intPtrType                 :: Type,
--  offsetOfElements           :: Int -> Word64,
    pointerSize                :: Int,
--  preferredAlignmentOfGlobal :: Value a -> Int,
    preferredAlignmentOfType   :: Type -> Int,
    sizeOfTypeInBits           :: Type -> Int,
    storeSizeOfType            :: Type -> Int
    }
    deriving (Typeable)

withIntPtrType :: (forall n . (Nat n) => WordN n -> a) -> a
withIntPtrType f = reifyIntegral sz (\ n -> f (g n))
  where g :: n -> WordN n
        g _ = error "withIntPtrType: argument used"
        sz = pointerSize $ unsafePerformIO getTargetData

-- Gets the target data for the JIT target.
getEngineTargetDataRef :: IO FFI.TargetDataRef
getEngineTargetDataRef = runEngineAccess getExecutionEngineTargetData

-- Normally the TargetDataRef never changes, so the operation
-- are really pure functions.
makeTargetData :: FFI.TargetDataRef -> TargetData
makeTargetData r = TargetData {
    aBIAlignmentOfType       = fromIntegral . FFI.aBIAlignmentOfType r,
    aBISizeOfType            = fromIntegral . FFI.aBISizeOfType r,
    littleEndian             = FFI.byteOrder r /= 0,
    callFrameAlignmentOfType = fromIntegral . FFI.callFrameAlignmentOfType r,
    intPtrType               = FFI.intPtrType r,
    pointerSize              = fromIntegral $ FFI.pointerSize r,
    preferredAlignmentOfType = fromIntegral . FFI.preferredAlignmentOfType r,
    sizeOfTypeInBits         = fromIntegral . FFI.sizeOfTypeInBits r,
    storeSizeOfType          = fromIntegral . FFI.storeSizeOfType r
    }

getTargetData :: IO TargetData
getTargetData = fmap makeTargetData getEngineTargetDataRef

targetDataFromString :: String -> TargetData
targetDataFromString s = makeTargetData $ unsafePerformIO $ withCString s FFI.createTargetData
