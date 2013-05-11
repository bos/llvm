{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module LLVM.Target.Native(initializeNativeTarget) where
import Control.Monad
import Control.Concurrent.MVar
import System.IO.Unsafe

import Foreign.C.Types

-- TARGET is expanded by CPP to the native target architecture.
import LLVM.Target.TARGET

foreign import ccall unsafe "LLVMInitNativeTarget"
        llvmInitializeNativeTarget :: IO CUInt

-- | Initialize jitter to the native target.
-- The operation is idempotent.
initializeNativeTarget :: IO ()
initializeNativeTarget = do
    done <- takeMVar refDone
    when (not done) (llvmInitializeNativeTarget >> return ()) -- initializeTarget
    putMVar refDone True

-- UNSAFE: global variable to keep track of initialization state.
refDone :: MVar Bool
refDone = unsafePerformIO $ newMVar False
