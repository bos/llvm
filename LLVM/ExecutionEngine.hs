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

import qualified LLVM.ExecutionEngine.FFI as FFI
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V

newtype ExecutionEngine = ExecutionEngine {
      fromExecutionEngine :: ForeignPtr FFI.ExecutionEngine
    }

withExecutionEngine :: ExecutionEngine -> (Ptr FFI.ExecutionEngine -> IO a)
                    -> IO a
withExecutionEngine ee = withForeignPtr (fromExecutionEngine ee)

createExecutionEngine :: T.ModuleProvider -> IO ExecutionEngine
createExecutionEngine prov =
    T.withModuleProvider prov $ \provPtr ->
      alloca $ \eePtr ->
        alloca $ \errPtr -> do
          ret <- FFI.createExecutionEngine eePtr provPtr errPtr
          if ret == 1
            then do err <- peek errPtr
                    errStr <- peekCString err
                    free err
                    ioError . userError $ errStr
            else do ptr <- peek eePtr
                    final <- h2c_ee FFI.disposeExecutionEngine
                    ExecutionEngine <$> newForeignPtr final ptr

foreign import ccall "wrapper" h2c_ee
    :: (Ptr FFI.ExecutionEngine -> IO ()) -> IO (FinalizerPtr a)

runStaticConstructors :: ExecutionEngine -> IO ()
runStaticConstructors ee = withExecutionEngine ee FFI.runStaticConstructors

runStaticDestructors :: ExecutionEngine -> IO ()
runStaticDestructors ee = withExecutionEngine ee FFI.runStaticDestructors


newtype GenericValue = GenericValue {
      fromGenericValue :: Ptr FFI.GenericValue
    }

runFunction :: ExecutionEngine -> V.Function a -> [GenericValue] -> IO GenericValue
runFunction ee func args =
    withExecutionEngine ee $ \eePtr ->
      withArrayLen (map fromGenericValue args) $ \argLen argPtr ->
        GenericValue <$> FFI.runFunction eePtr (V.valueRef func)
                                        (fromIntegral argLen) argPtr
