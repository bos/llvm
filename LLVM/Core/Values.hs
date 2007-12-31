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
    , typeOfDyn

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

    -- * Constants
    , ConstInt(..)
    , ConstReal(..)
    , ConstArray(..)

    -- ** Useful functions
    , Const(..)

    -- *** Scalar constants
    , constInt
    , constWord
    , constReal

    -- *** Composite constants
    , constString
    , constStringNul

    -- *** Constant expressions
    , constBitCast

    -- * Instructions
    , Instruction
    , CallInst(..)
    , GetElementPtrInst(..)
    , ReturnInst(..)

    -- * Operations on values
    , Builder(..)
    ) where

import Control.Applicative ((<$>))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (withCStringLen)
import Foreign.ForeignPtr (ForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Types as T


class Value a where
    valueRef :: a -> FFI.ValueRef

class HasAnyValue a where
    toAnyValue :: a -> AnyValue
    fromAnyValue :: AnyValue -> a

data AnyValue = forall a. Value a => AnyValue a

instance HasAnyValue AnyValue where
    toAnyValue = id
    fromAnyValue = id

instance Value FFI.ValueRef where
    valueRef = id

mkAnyValue :: Value a => a -> AnyValue
mkAnyValue = AnyValue

class Value a => ConstValue a
class ConstValue a => GlobalValue a
class GlobalValue a => GlobalVariable a
class Value a => Instruction a

instance Value AnyValue where
    valueRef (AnyValue a) = valueRef a

instance ConstValue AnyValue
instance GlobalValue AnyValue
instance GlobalVariable AnyValue
instance Instruction AnyValue

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

newtype Function t = Function AnyValue
    deriving (ConstValue, GlobalValue, GlobalVariable, HasAnyValue, Value)

functionType :: Function a -> T.Function a
functionType = T.fromAnyType . T.mkAnyType . typeOfDyn

newtype ConstInt t = ConstInt AnyValue
    deriving (ConstValue, HasAnyValue, Value)

newtype ConstArray t = ConstArray AnyValue
    deriving (ConstValue, HasAnyValue, Value)

constArrayType :: ConstArray a -> T.Array a
constArrayType = T.fromAnyType . T.mkAnyType . typeOfDyn

newtype ConstReal t = ConstReal AnyValue
    deriving (ConstValue, HasAnyValue, Value)

-- | Recover the type of a value in a manner that preserves static
-- type safety.
class T.Type t => TypedValue a t | a -> t where
    typeOf :: a -> t

constWord :: (T.Integer t, Integral a) => t -> a -> ConstInt t
constWord typ val =
    ConstInt . mkAnyValue $ FFI.constInt (T.typeRef typ) (fromIntegral val) 0

constInt :: (T.Integer t, Integral a) => t -> a -> ConstInt t
constInt typ val =
    ConstInt . mkAnyValue $ FFI.constInt (T.typeRef typ) (fromIntegral val) 1

constReal :: (T.Real t, RealFloat a) => t -> a -> ConstReal t
constReal typ val = ConstReal . mkAnyValue $ FFI.constReal (T.typeRef typ) (realToFrac val)

constString :: String -> ConstArray T.Int8
constString s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . ConstArray . mkAnyValue $ FFI.constString sPtr (fromIntegral sLen) 1

constStringNul :: String -> ConstArray T.Int8
constStringNul s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . ConstArray . mkAnyValue $ FFI.constString sPtr (fromIntegral sLen) 0

constBitCast :: (ConstValue a, ConstValue b, HasAnyValue b, T.Type t) => t -> a -> b
constBitCast typ val =
    fromAnyValue . mkAnyValue $ FFI.constBitCast (valueRef val) (T.typeRef typ)

class ConstValue t => Const a t | a -> t where
    const :: a -> t

instance Const String (ConstArray T.Int8) where
    const = constStringNul

instance Const Float (ConstReal T.Float) where
    const = constReal T.float . fromRational . toRational

instance Const Double (ConstReal T.Double) where
    const = constReal T.double

instance Const Int8 (ConstInt T.Int8) where
    const = constInt T.int8 . fromIntegral

instance Const Int16 (ConstInt T.Int16) where
    const = constInt T.int16 . fromIntegral

instance Const Int32 (ConstInt T.Int32) where
    const = constInt T.int32 . fromIntegral

instance Const Int64 (ConstInt T.Int64) where
    const = constInt T.int64

instance Const Word8 (ConstInt T.Int8) where
    const = constWord T.int8 . fromIntegral

instance Const Word16 (ConstInt T.Int16) where
    const = constWord T.int16 . fromIntegral

instance Const Word32 (ConstInt T.Int32) where
    const = constWord T.int32 . fromIntegral

instance Const Word64 (ConstInt T.Int64) where
    const = constWord T.int64 . fromIntegral

typeOfDyn :: Value a => a -> T.AnyType
typeOfDyn val = unsafePerformIO $ T.mkAnyType <$> FFI.typeOf (valueRef val)

instance TypedValue AnyValue T.AnyType where
    typeOf = T.mkAnyType . typeOfDyn

instance TypedValue (ConstArray a) (T.Array a) where
    typeOf = constArrayType

instance TypedValue (Function a) (T.Function a) where
    typeOf = functionType
