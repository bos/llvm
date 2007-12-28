module LLVM.Core.Types
    (
      Module(..)
    , withModule
    , ModuleProvider(..)
    , withModuleProvider
    , Type(..)
    , Value(..)
    ) where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Prelude hiding (mod)

import qualified LLVM.Core.FFI as FFI


newtype Module = Module {fromModule :: ForeignPtr FFI.Module}

withModule :: Module -> (FFI.ModuleRef -> IO a) -> IO a
withModule mod = withForeignPtr (fromModule mod)

newtype ModuleProvider = ModuleProvider {
      fromModuleProvider :: ForeignPtr FFI.ModuleProvider
    }

withModuleProvider :: ModuleProvider -> (FFI.ModuleProviderRef -> IO a)
                   -> IO a
withModuleProvider prov = withForeignPtr (fromModuleProvider prov)

newtype Type = Type {
      fromType :: FFI.TypeRef
    }
                          
newtype Value = Value {
      fromValue :: FFI.ValueRef
    }
