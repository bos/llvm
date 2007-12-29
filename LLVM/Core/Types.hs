{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, ExistentialQuantification #-}
module LLVM.Core.Types
    (
      Module(..)
    , withModule
    , ModuleProvider(..)
    , withModuleProvider
    , AnyType
    , Value(..)
    , BasicBlock(..)
    , Builder(..)
    , Type(..)
    , Any(..)
    , mkAny
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

class Type a where
    fromType :: a -> FFI.TypeRef

instance Type FFI.TypeRef where
    fromType = id

data AnyType = forall a. Type a => AnyType a

class Any a where
    toAny :: a -> AnyType
    fromAny :: AnyType -> a

instance Any AnyType where
    toAny = id
    fromAny = id

mkAny :: Type a => a -> AnyType
mkAny = AnyType

instance Type AnyType where
    fromType (AnyType a) = fromType a

newtype Value = Value {
      fromValue :: FFI.ValueRef
    }

newtype BasicBlock = BasicBlock {
      fromBasicBlock :: FFI.BasicBlockRef
    }

newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }
