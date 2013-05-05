module LLVM.Wrapper.Linker ( LinkerMode(..)
                           , linkModules ) where

import qualified LLVM.FFI.Linker as FFI
import LLVM.FFI.Linker (LinkerMode(..))
import qualified LLVM.FFI.Core as FFI
import LLVM.Wrapper.Core
import LLVM.Wrapper.Internal

import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.ForeignPtr.Safe (withForeignPtr)

import Data.IORef

linkModules :: Module -> Module -> LinkerMode -> IO (Maybe String)
linkModules (MkModule dest _) (MkModule src srcOurs) mode =
    withForeignPtr dest $ \dest' ->
    withForeignPtr src $ \src' ->
    alloca $ \msgPtr -> do
               result <- FFI.linkModules dest' src' (FFI.fromLinkerMode mode) msgPtr
               writeIORef srcOurs False
               msg <- peek msgPtr
               if not result
                 then return Nothing
                 else do str <- peekCString msg
                         FFI.disposeMessage msg
                         return (Just str)
