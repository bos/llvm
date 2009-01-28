module LLVM.ExecutionEngine.Target(TargetData(..), ourTargetData, targetDataFromString) where
--import Data.Word
import Foreign.C.String
import System.IO.Unsafe(unsafePerformIO)

--import LLVM.Core
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

targetDataFromString :: String -> TargetData
targetDataFromString s = makeTargetData $ un $ withCString s FFI.createTargetData
