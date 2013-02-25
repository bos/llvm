module LLVM.Wrapper.BitReader (parseBitcodeInContext, parseBitcodeFromFileInContext) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.C.String (peekCString, withCString)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (FilePath)
import Control.Exception.Base (finally)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitReader as FFI

import LLVM.Wrapper.Internal

parseBitcodeInContext :: FFI.ContextRef -> ByteString -> IO (Either String Module)
parseBitcodeInContext ctx bs =
    unsafeUseAsCStringLen bs $ \(str, len) ->
    withMemoryBuffer "bitcode" str len (parseFromBuf ctx)

parseBitcodeFromFileInContext :: FFI.ContextRef -> FilePath -> IO (Either String Module)
parseBitcodeFromFileInContext ctx path =
    alloca $ \bufPtr ->
    alloca $ \msgPtr -> do
      errOccurred <- withCString path $ \cpath ->
                     FFI.createMemoryBufferWithContentsOfFile cpath bufPtr msgPtr
      case errOccurred of
        True -> peek msgPtr >>= peekCString >>= fail
        False -> do buf <- peek bufPtr
                    finally (parseFromBuf ctx buf)
                            (FFI.disposeMemoryBuffer buf)

parseFromBuf :: FFI.ContextRef -> FFI.MemoryBufferRef -> IO (Either String Module)
parseFromBuf ctx buf =
    alloca $ \msgPtr ->
    alloca $ \modPtr -> do
      errOccurred <- FFI.parseBitcodeInContext ctx buf modPtr msgPtr
      case errOccurred of
        True -> fmap Left $ peek msgPtr >>= peekCString
        False -> fmap Right $ peek modPtr >>= initModule
