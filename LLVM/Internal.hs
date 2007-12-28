module LLVM.Internal
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

import qualified LLVM.Base as Base


newtype Module = Module {fromModule :: ForeignPtr Base.Module}

withModule :: Module -> (Base.ModuleRef -> IO a) -> IO a
withModule mod = withForeignPtr (fromModule mod)

newtype ModuleProvider = ModuleProvider {
      fromModuleProvider :: ForeignPtr Base.ModuleProvider
    }

withModuleProvider :: ModuleProvider -> (Base.ModuleProviderRef -> IO a)
                   -> IO a
withModuleProvider prov = withForeignPtr (fromModuleProvider prov)

newtype Type = Type {fromType :: Base.TypeRef}
                          
newtype Value = Value {fromValue :: Base.ValueRef}
