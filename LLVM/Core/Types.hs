{-# LANGUAGE ExistentialQuantification #-}

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

    -- ** Integer types
    , Integer
    , Int1(..)
    , Int8(..)
    , Int16(..)
    , Int32(..)
    , Int64(..)
    , IntWidth(..)

    -- ** Real types
    , Real
    , Float(..)
    , Double(..)

    -- *** Machine-specific real types
    , X86Float80(..)
    , Float128(..)
    , PPCFloat128(..)

    -- ** Array, pointer, and vector types
    , Sequence
    , Array(..)
    , Pointer(..)
    , Vector(..)

    -- ** Function-related types
    , Function(..)

    -- ** Other types
    , Void(..)
    ) where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Prelude hiding (Double, Float, Integer, Real, mod)

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

instance Show AnyType where
    show a = "AnyType " ++ show (fromType a)

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

class Type a => Integer a

instance Integer AnyType

newtype Int1 = Int1 AnyType
    deriving (Any, Integer, Show, Type)

newtype Int8 = Int8 AnyType
    deriving (Any, Integer, Show, Type)

newtype Int16 = Int16 AnyType
    deriving (Any, Integer, Show, Type)

newtype Int32 = Int32 AnyType
    deriving (Any, Integer, Show, Type)

newtype Int64 = Int64 AnyType
    deriving (Any, Integer, Show, Type)

newtype IntWidth a = IntWidth AnyType
    deriving (Any, Integer, Show, Type)

class Type a => Real a

instance Real AnyType

newtype Float = Float AnyType
    deriving (Any, Real, Show, Type)

newtype Double = Double AnyType
    deriving (Any, Real, Show, Type)

newtype X86Float80 = X86Float80 AnyType
    deriving (Any, Real, Show, Type)

newtype Float128 = Float128 AnyType
    deriving (Any, Real, Show, Type)

newtype PPCFloat128 = PPCFloat128 AnyType
    deriving (Any, Real, Show, Type)

class Type a => Sequence a

instance Sequence AnyType

newtype Array a = Array AnyType
    deriving (Any, Sequence, Type)

newtype Pointer a = Pointer AnyType
    deriving (Any, Sequence, Type)

newtype Vector a = Vector AnyType
    deriving (Any, Sequence, Type)

newtype Void = Void AnyType
    deriving (Any, Show, Type)

newtype Function p = Function AnyType
    deriving (Type, Any)
             
