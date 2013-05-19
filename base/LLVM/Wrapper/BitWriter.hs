module LLVM.Wrapper.BitWriter where

import Foreign.C.String
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Control.Monad

import qualified LLVM.FFI.BitWriter as FFI
import LLVM.Wrapper.Core
import LLVM.Wrapper.Internal

writeBitcodeToFile :: Module -> FilePath -> IO ()
writeBitcodeToFile (MkModule m _) p = do
  result <- withForeignPtr m (withCString p . FFI.writeBitcodeToFile)
  when (result /= 0) $
         fail $ "Failed to write bitcode to " ++ p
