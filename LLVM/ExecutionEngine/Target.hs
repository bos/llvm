module LLVM.ExecutionEngine.Target(TargetData(..), ourTargetData) where
import Data.Word
import System.IO.Unsafe(unsafePerformIO)

--import LLVM.Core
import LLVM.ExecutionEngine.Engine(runEngineAccess, getExecutionEngineTargetData)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

type Type = FFI.TypeRef

data TargetData = TargetData {
    aBIAlignmentOfType         :: Type -> Word32,
    aBISizeOfType              :: Type -> Word32,
    littleEndian               :: Bool,
    callFrameAlignmentOfType   :: Type -> Word32,
--  elementAtOffset            :: Type -> Word64 -> Word32,
    intPtrType                 :: Type,
--  offsetOfElements           :: Word32 -> Word64,
    pointerSize                :: Word32,
--  preferredAlignmentOfGlobal :: Value a -> Word32,
    preferredAlignmentOfType   :: Type -> Word32,
    sizeOfTypeInBits           :: Type -> Word32,
    storeSizeOfType            :: Type -> Word32
    }

un :: IO a -> a
un = unsafePerformIO

-- Gets the target data for the JIT target.
-- This is really constant, so unsafePerformIO is safe.
ourEngineTargetDataRef :: FFI.TargetDataRef
ourEngineTargetDataRef = un $
    runEngineAccess getExecutionEngineTargetData

-- Normally the TargetDataRef never changes, so the operation
-- are really pure functions.
makeTargetData :: FFI.TargetDataRef -> TargetData
makeTargetData r = TargetData {
    aBIAlignmentOfType       = fromIntegral . un . FFI.aBIAlignmentOfType r,
    aBISizeOfType            = fromIntegral . un . FFI.aBISizeOfType r,
    littleEndian             = un (FFI.byteOrder r) /= 0,
    callFrameAlignmentOfType = fromIntegral . un . FFI.callFrameAlignmentOfType r,
    intPtrType               = un $ FFI.intPtrType r,
    pointerSize              = fromIntegral $ un $ FFI.pointerSize r,
    preferredAlignmentOfType = fromIntegral . un . FFI.preferredAlignmentOfType r,
    sizeOfTypeInBits         = fromIntegral . un . FFI.sizeOfTypeInBits r,
    storeSizeOfType          = fromIntegral . un . FFI.storeSizeOfType r
    }

ourTargetData :: TargetData
ourTargetData = makeTargetData ourEngineTargetDataRef
