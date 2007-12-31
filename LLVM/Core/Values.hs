{-# LANGUAGE
    ExistentialQuantification
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.Core.Values
    (
    -- * Values

    -- * Opaque wrapper for LLVM's basic value type
      AnyValue
    , HasAnyValue(..)
    , mkAnyValue

    -- ** Type classes

    , Value(..)
    , ConstValue
    , GlobalValue
    , GlobalVariable

    , Global(..)
    , GlobalVar(..)
    , Function(..)
    , TypedValue(..)
    , BasicBlock(..)

    -- ** KILLME
    , Val(..)

    -- ** Constants
    , ConstInt(..)
    , ConstReal(..)
    , ConstArray(..)

    -- ** Instructions
    , Instruction
    , CallInst(..)
    , GetElementPtrInst(..)
    , ReturnInst(..)

    -- * Operations on values
    , Builder(..)
    ) where

import Foreign.ForeignPtr (ForeignPtr)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Types as T


class Value a where
    fromValue :: a -> FFI.ValueRef

class HasAnyValue a where
    toAnyValue :: a -> AnyValue
    fromAnyValue :: AnyValue -> a

data AnyValue = forall a. Value a => AnyValue a

instance HasAnyValue AnyValue where
    toAnyValue = id
    fromAnyValue = id

instance Value FFI.ValueRef where
    fromValue = id

mkAnyValue :: Value a => a -> AnyValue
mkAnyValue = AnyValue

class Value a => ConstValue a
class ConstValue a => GlobalValue a
class GlobalValue a => GlobalVariable a
class Value a => Instruction a

instance Value AnyValue where
    fromValue (AnyValue a) = fromValue a

instance ConstValue AnyValue
instance GlobalValue AnyValue
instance GlobalVariable AnyValue
instance Instruction AnyValue

newtype Val t = Val {
      fromVal :: FFI.ValueRef
    }

instance Value (Val t) where
    fromValue = fromVal

newtype BasicBlock = BasicBlock AnyValue
    deriving (HasAnyValue, Value)

newtype Builder = Builder {
      fromBuilder :: ForeignPtr FFI.Builder
    }

newtype CallInst = CallInst AnyValue
    deriving (HasAnyValue, Instruction, Value)

newtype GetElementPtrInst = GetElementPtrInst AnyValue
    deriving (HasAnyValue, Instruction, Value)

newtype ReturnInst = ReturnInst AnyValue
    deriving (HasAnyValue, Instruction, Value)

newtype Global t = Global AnyValue
    deriving (ConstValue, GlobalValue, HasAnyValue, Value)

newtype GlobalVar t = GlobalVar AnyValue
    deriving (ConstValue, GlobalValue, GlobalVariable, HasAnyValue, Value)

newtype Function = Function AnyValue
    deriving (ConstValue, GlobalValue, GlobalVariable, HasAnyValue, Value)

newtype ConstInt t = ConstInt AnyValue
    deriving (ConstValue, HasAnyValue, Value)

newtype ConstArray t = ConstArray AnyValue
    deriving (ConstValue, HasAnyValue, Value)

newtype ConstReal t = ConstReal AnyValue
    deriving (ConstValue, HasAnyValue, Value)

class T.Type t => TypedValue a t | a -> t where
    valueType :: a -> t
