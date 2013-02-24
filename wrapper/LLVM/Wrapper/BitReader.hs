module LLVM.Wrapper.BitReader (parseBitcode, parseBitcodeFromFile) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.C.String (peekCString, withCString)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (FilePath)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.BitReader as FFI

import LLVM.Wrapper.Internal

parseBitcode :: ByteString -> Either String Module
parseBitcode bs =
    unsafePerformIO $
    unsafeUseAsCStringLen bs $ \(str, len) ->
    withMemoryBuffer "bitcode" str len parseFromBuf

parseBitcodeFromFile :: FilePath -> IO (Either String Module)
parseBitcodeFromFile path =
    alloca $ \bufPtr ->
    alloca $ \msgPtr -> do
      errOccurred <- withCString path $ \cpath ->
                     FFI.createMemoryBufferWithContentsOfFile cpath bufPtr msgPtr
      case errOccurred of
        True -> peek msgPtr >>= peekCString >>= fail
        False -> peek bufPtr >>= parseFromBuf 
          

parseFromBuf :: FFI.MemoryBufferRef -> IO (Either String Module)
parseFromBuf buf =
    alloca $ \msgPtr ->
    alloca $ \modPtr -> do
      errOccurred <- FFI.parseBitcode buf modPtr msgPtr
      case errOccurred of
        True -> fmap Left $ peek msgPtr >>= peekCString
        False -> fmap Right $ peek modPtr >>= initModule
