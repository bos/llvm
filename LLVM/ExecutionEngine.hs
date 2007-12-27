module LLVM.ExecutionEngine
    (
      ExecutionEngine
    , createExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    ) where

import Control.Applicative ((<$>))
import Control.Exception (finally, ioError)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Error (userError)

import qualified LLVM
import qualified LLVM.ExecutionEngine.Base as EE
import LLVM.Internal (withModuleProvider)

newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr EE.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr EE.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine ee = withForeignPtr (fromExecutionEngine ee)

createExecutionEngine :: LLVM.ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- EE.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then finally (peek errPtr >>= peekCString >>= ioError . userError)
                 (free errPtr)
            else do ptr <- peek eePtr
                    final <- h2c_ee EE.disposeExecutionEngine
                    ExecutionEngine <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_ee
    :: (Ptr EE.ExecutionEngine -> IO ()) -> IO (FinalizerPtr a)

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee EE.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee EE.runStaticDestructors
