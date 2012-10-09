module LLVM.Wrapper.Analysis where

import qualified LLVM.FFI.Analysis as FFI
import qualified LLVM.FFI.Core as FFI
import LLVM.Wrapper.Core

import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Control.Monad (liftM)

-- VerifierFailureAction 2 is 'no side effects'
verifyFunction :: Value -> IO Bool
verifyFunction f = FFI.verifyFunction f 2

verifyModule :: Module -> IO (Maybe String)
verifyModule m = alloca (\msgPtr -> do
                           result <- FFI.verifyModule m 2 msgPtr
                           msg <- peek msgPtr
                           case result of
                             False -> return Nothing
                             True -> do str <- peekCString msg
                                        FFI.disposeMessage msg
                                        return $ Just str)
