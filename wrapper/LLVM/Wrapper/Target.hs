module LLVM.Wrapper.Target
    ( module LLVM.FFI.Target
    , TargetData
    , ByteOrdering(..)
    , byteOrder
    , copyStringRepOfTargetData
    , createTargetData
    , withTargetData
    ) where

import LLVM.FFI.Target
    ( addTargetData
    , disposeTargetData
    , intPtrType
    , callFrameAlignmentOfType
    , aBIAlignmentOfType
    , aBISizeOfType
    , pointerSize
    , preferredAlignmentOfGlobal 
    , preferredAlignmentOfType 
    , sizeOfTypeInBits 
    , storeSizeOfType
    , elementAtOffset
    , offsetOfElement
    )

import qualified LLVM.FFI.Target as FFI.T
import qualified LLVM.FFI.Core as FFI

import Foreign.C.String (peekCString, withCString)
import Control.Exception (finally)
import Control.Monad (liftM)
import Foreign.C.Types

type Type       = FFI.TypeRef
type Value      = FFI.ValueRef
type TargetData = FFI.T.TargetDataRef

data ByteOrdering = BigEndian | LittleEndian deriving Eq

byteOrder :: TargetData -> ByteOrdering
byteOrder td = if FFI.T.byteOrder td == 0 then BigEndian else LittleEndian

copyStringRepOfTargetData :: TargetData -> IO String
copyStringRepOfTargetData td = do s <- FFI.T.copyStringRepOfTargetData td
                                  peekCString s

createTargetData :: String -> IO TargetData
createTargetData str = withCString str $ \p -> FFI.T.createTargetData p

withTargetData :: String -> (TargetData -> IO a) -> IO a
withTargetData str f = do t <- createTargetData str
                          finally (f t) (FFI.T.disposeTargetData t)
