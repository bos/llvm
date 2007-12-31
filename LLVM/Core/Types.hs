{-# LANGUAGE
    ExistentialQuantification
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.Core.Types
    (
      Module(..)
    , withModule
    , ModuleProvider(..)
    , withModuleProvider

    -- * Types
    , Type(..)
    , AnyType
    , HasAnyType(..)
    , mkAnyType

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
    , arrayElement
    , Pointer(..)
    , pointerElement
    , Vector(..)
    , vectorElement

    -- ** Function-related types
    , Function(..)
    , functionParams
    , Params(..)
    , (:->)
    , car
    , cdr

    -- ** Other types
    , Void(..)
    ) where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Prelude hiding (Double, Float, Integer, Real, mod)

import qualified LLVM.Core.FFI as FFI


newtype Module = Module {
      fromModule :: ForeignPtr FFI.Module
    }

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

class HasAnyType a where
    toAnyType :: a -> AnyType
    fromAnyType :: AnyType -> a

instance Type FFI.TypeRef where
    fromType = id

data AnyType = forall a. Type a => AnyType a

instance Show AnyType where
    show a = "AnyType " ++ show (fromType a)

mkAnyType :: Type a => a -> AnyType
mkAnyType = AnyType

instance Type AnyType where
    fromType (AnyType a) = fromType a

instance HasAnyType AnyType where
    toAnyType = id
    fromAnyType = id

class Params l where
    listValue :: l -> [AnyType]

class Type a => Integer a

instance Integer AnyType

newtype Int1 = Int1 AnyType
    deriving (HasAnyType, Integer, Type)

instance Show Int1 where
    show _ = "Int1"

newtype Int8 = Int8 AnyType
    deriving (HasAnyType, Integer, Type)

instance Show Int8 where
    show _ = "Int8"

newtype Int16 = Int16 AnyType
    deriving (HasAnyType, Integer, Type)

instance Show Int16 where
    show _ = "Int16"

newtype Int32 = Int32 AnyType
    deriving (HasAnyType, Integer, Type)

instance Show Int32 where
    show _ = "Int32"

newtype Int64 = Int64 AnyType
    deriving (HasAnyType, Integer, Type)

instance Show Int64 where
    show _ = "Int64"

newtype IntWidth a = IntWidth AnyType
    deriving (HasAnyType, Integer, Type)

instance Show (IntWidth a) where
    show _ = "IntWidth"

class Type a => Real a

instance Real AnyType

newtype Float = Float AnyType
    deriving (HasAnyType, Real, Type)

instance Show Float where
    show _ = "Float"

newtype Double = Double AnyType
    deriving (HasAnyType, Real, Type)

instance Show Double where
    show _ = "Double"

newtype X86Float80 = X86Float80 AnyType
    deriving (HasAnyType, Real, Type)

instance Show X86Float80 where
    show _ = "X86Float80"

newtype Float128 = Float128 AnyType
    deriving (HasAnyType, Real, Type)

instance Show Float128 where
    show _ = "Float128"

newtype PPCFloat128 = PPCFloat128 AnyType
    deriving (HasAnyType, Real, Type)

instance Show PPCFloat128 where
    show _ = "PPCFloat128"

class Type a => Sequence a

instance Sequence AnyType

newtype Array a = Array AnyType
    deriving (HasAnyType, Sequence, Type)

arrayElement :: Array a -> a
arrayElement _ = undefined

instance (Show a) => Show (Array a) where
    show a = "Array " ++ show (arrayElement a)

newtype Pointer a = Pointer AnyType
    deriving (HasAnyType, Sequence, Type)

pointerElement :: Pointer a -> a
pointerElement _ = undefined

instance (Show a) => Show (Pointer a) where
    show a = "Pointer " ++ show (pointerElement a)

newtype Vector a = Vector AnyType
    deriving (HasAnyType, Sequence, Type)

vectorElement :: Vector a -> a
vectorElement _ = undefined

instance (Show a) => Show (Vector a) where
    show a = "Vector " ++ show (vectorElement a)

newtype Void = Void AnyType
    deriving (HasAnyType, Type)

instance Show Void where
    show _ = "Void"

newtype Function p = Function AnyType
    deriving (HasAnyType, Type)
             
functionParams :: Function p -> p
functionParams _ = undefined

data a :-> b
infixr 6 :->

car :: (a :-> b) -> a
car _ = undefined

cdr :: (a :-> b) -> b
cdr _ = undefined

instance (Show a, Show b) => Show (a :-> b) where
    show a = show (car a) ++ " :-> " ++ show (cdr a)
