{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Wrapper.Internal where

import Foreign.ForeignPtr.Safe (ForeignPtr, newForeignPtr)
import qualified Foreign.Concurrent as FC (newForeignPtr)
import Foreign.Ptr (Ptr)

import Control.Monad
import Data.IORef

import qualified LLVM.FFI.Core as FFI

data Module = MkModule (ForeignPtr FFI.Module) (IORef Bool)
              deriving Eq

data PassManager = MkPassManager (ForeignPtr FFI.PassManager)
                 deriving Eq

moduleFinalizer :: Ptr FFI.Module -> IORef Bool -> IO ()
moduleFinalizer m ours = do
  isOurs <- readIORef ours
  when isOurs $ FFI.disposeModule m

initModule :: Ptr FFI.Module -> IO Module
initModule ptr = do
  ours <- newIORef True
  fptr <- FC.newForeignPtr ptr (moduleFinalizer ptr ours)
  return $ MkModule fptr ours

initPassManager :: Ptr FFI.PassManager -> IO PassManager
initPassManager ptr = fmap MkPassManager (newForeignPtr FFI.ptrDisposePassManager ptr)
