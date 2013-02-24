{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Wrapper.Internal where

import Foreign.ForeignPtr.Safe (ForeignPtr, newForeignPtrEnv)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Control.Exception.Base

import qualified LLVM.FFI.Core as FFI

data Module = MkModule (ForeignPtr FFI.Module) (Ptr Bool)

withMemoryBuffer :: String -> Ptr a -> Int -> (FFI.MemoryBufferRef -> IO b) -> IO b
withMemoryBuffer name p len =
    bracket (withCString name $ \str -> FFI.createMemoryBufferWithMemoryRange p (fromIntegral len) str False)
            FFI.disposeMemoryBuffer

type EnvFinalizer env a = Ptr env -> Ptr a -> IO ()
foreign import ccall "wrapper"
   mkFinalizer :: EnvFinalizer env a -> IO (FunPtr (EnvFinalizer env a))

{-# NOINLINE moduleFinalizer #-}
moduleFinalizer :: FunPtr (EnvFinalizer Bool FFI.Module)
moduleFinalizer = unsafePerformIO . mkFinalizer $ \ours m -> do
                    isOurs <- peek ours
                    free ours
                    when isOurs $ FFI.disposeModule m

initModule :: Ptr FFI.Module -> IO Module
initModule ptr = do
  ours <- malloc
  poke ours True
  ptr <- newForeignPtrEnv moduleFinalizer ours ptr
  return $ MkModule ptr ours
