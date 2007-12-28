module LLVM.ExecutionEngine
    (
    -- * Execution engines
      ExecutionEngine
    , createExecutionEngine
    , runStaticConstructors
    , runStaticDestructors
    , runFunction

    -- * Generic values
    , GenericValue
    ) where

import Control.Applicative ((<$>))
import Control.Exception (ioError)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr,
                           withForeignPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO.Error (userError)

import qualified LLVM.ExecutionEngine.Base as EE
import LLVM.Internal (ModuleProvider, withModuleProvider, Value(..))

newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr EE.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr EE.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine ee = withForeignPtr (fromExecutionEngine ee)

createExecutionEngine :: ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- EE.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then do err <- peek errPtr
                    errStr <- peekCString err
                    free err
                    ioError . userError $ errStr
            else do ptr <- peek eePtr
                    final <- h2c_ee EE.disposeExecutionEngine
                    ExecutionEngine <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_ee
    :: (Ptr EE.ExecutionEngine -> IO ()) -> IO (FinalizerPtr a)

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee EE.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee EE.runStaticDestructors


newtype GenericValue = GenericValue {
      fromGenericValue :: Ptr EE.GenericValue
    }

runFunction :: ExecutionEngine -> Value -> [GenericValue] -> IO GenericValue
runFunction ee func args =
    withExecutionEngine ee $ \eePtr ->
      withArrayLen (map fromGenericValue args) $ \argLen argPtr ->
        GenericValue <$> EE.runFunction eePtr (fromValue func)
                                        (fromIntegral argLen) argPtr
