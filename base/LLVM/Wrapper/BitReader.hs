module LLVM.Wrapper.BitReader (parseBitcodeInContext) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.C.String (peekCString)
import Foreign.Storable (peek)
import Foreign.ForeignPtr.Safe (withForeignPtr)

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
      if toBool errOccurred
        then fmap Left $ peek msgPtr >>= peekCString
        else fmap Right $ peek modPtr >>= initModule

-- TODO: Work out the memory dynamics of this
getBitcodeModuleInContext :: Context -> MemoryBuffer -> IO (Either String Module)
getBitcodeModuleInContext ctx buf =
    alloca $ \msgPtr ->
    alloca $ \modPtr ->
    withForeignPtr ctx $ \ctx' ->
    withForeignPtr buf $ \buf' -> do
      errOccurred <- FFI.getBitcodeModuleInContext ctx' buf' modPtr msgPtr
      if toBool errOccurred
        then fmap Left $ peek msgPtr >>= peekCString
        else fmap Right $ peek modPtr >>= initModule
