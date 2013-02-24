module LLVM.Wrapper.Internal where

import Foreign.ForeignPtr.Safe (ForeignPtr)
import Foreign.Ptr (Ptr)

import qualified LLVM.FFI.Core as FFI

data Module = MkModule (ForeignPtr FFI.Module) (Ptr Bool)
