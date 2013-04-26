module LLVM.Wrapper.BitReader (parseBitcodeInContext) where

import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.C.String (peekCString, withCString)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (FilePath)
import Control.Exception.Base (bracket, finally)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr.Safe (withForeignPtr)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitReader as FFI

import LLVM.Wrapper.Internal
import LLVM.Wrapper.Core

parseBitcodeInContext :: Context -> MemoryBuffer -> IO (Either String Module)
parseBitcodeInContext ctx buf =
    alloca $ \msgPtr ->
    alloca $ \modPtr ->
    withForeignPtr ctx $ \ctx' ->
    withForeignPtr buf $ \buf' -> do
      errOccurred <- FFI.parseBitcodeInContext ctx' buf' modPtr msgPtr
      case errOccurred of
        True -> fmap Left $ peek msgPtr >>= peekCString
        False -> fmap Right $ peek modPtr >>= initModule

-- TODO: Work out the memory dynamics of this
getBitcodeModuleInContext :: Context -> MemoryBuffer -> IO (Either String Module)
getBitcodeModuleInContext ctx buf =
    alloca $ \msgPtr ->
    alloca $ \modPtr ->
    withForeignPtr ctx $ \ctx' ->
    withForeignPtr buf $ \buf' -> do
      errOccurred <- FFI.getBitcodeModuleInContext ctx' buf' modPtr msgPtr
      case errOccurred of
        True -> fmap Left $ peek msgPtr >>= peekCString
        False -> fmap Right $ peek modPtr >>= initModule
