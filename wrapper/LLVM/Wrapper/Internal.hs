{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Wrapper.Internal where

import Foreign.ForeignPtr.Safe (ForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Data.IORef

import qualified LLVM.FFI.Core as FFI

data Module = MkModule (ForeignPtr FFI.Module) (IORef Bool)
              deriving Eq

moduleFinalizer :: Ptr FFI.Module -> IORef Bool -> IO ()
moduleFinalizer m ours = do
  isOurs <- readIORef ours
  when isOurs $ FFI.disposeModule m

initModule :: Ptr FFI.Module -> IO Module
initModule ptr = do
  ours <- newIORef True
  ptr <- newForeignPtr ptr (moduleFinalizer ptr ours)
  return $ MkModule ptr ours
