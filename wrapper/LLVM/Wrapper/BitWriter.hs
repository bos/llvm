module LLVM.Wrapper.BitWriter where

import Foreign.C.String
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Control.Monad

import qualified LLVM.FFI.BitWriter as FFI
import LLVM.Wrapper.Core

writeBitcodeToFile :: Module -> FilePath -> IO ()
writeBitcodeToFile m p = do result <- withForeignPtr m (\m' -> withCString p (FFI.writeBitcodeToFile m'))
                            unless (result == False) $
                                   fail $ "Failed to write bitcode to " ++ p
