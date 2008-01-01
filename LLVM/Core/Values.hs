{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.Core.Values
    (
    -- * Values

    -- * Opaque wrapper for LLVM's basic value type
      AnyValue
    , DynamicValue(..)
    , mkAnyValue
    , typeOfDyn

    -- ** Type classes
    , Value(..)
    , ConstValue
    , GlobalValue
    , GlobalVariable
    , Arithmetic
    , Integer
    , Real
    , Vector

    , Global(..)
    , GlobalVar(..)
    , Function(..)
    , TypedValue(..)

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
    ) where

import Control.Applicative ((<$>))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (withCStringLen)
import Foreign.Marshal.Utils (fromBool)
import Prelude hiding (Integer, Real)
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Types as T

-- import Debug.Trace


class Value a where
    valueRef :: a -> FFI.ValueRef

class DynamicValue a where
    fromAnyValue :: AnyValue -> a

-- | Recover the type of a value in a manner that preserves static
-- type safety.
class T.Type t => TypedValue a t | a -> t where
    typeOf :: a                 -- ^ value is not inspected
           -> t

data AnyValue = forall a. Value a => AnyValue a
                deriving (Typeable)

instance DynamicValue AnyValue where
    fromAnyValue = id

instance Value FFI.ValueRef where
    valueRef = id

mkAnyValue :: Value a => a -> AnyValue
mkAnyValue = AnyValue

class Value a => ConstValue a
class Value a => Arithmetic a
class Arithmetic a => Integer a
class Arithmetic a => Real a
class Arithmetic a => Vector a
class ConstValue a => GlobalValue a
class GlobalValue a => GlobalVariable a

instance Value AnyValue where
    valueRef (AnyValue a) = valueRef a

instance ConstValue AnyValue
instance GlobalValue AnyValue
instance GlobalVariable AnyValue
instance Arithmetic AnyValue
instance Integer AnyValue
instance Real AnyValue

newtype Global t = Global AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, Typeable, Value)

newtype GlobalVar t = GlobalVar AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, GlobalVariable,
              Typeable, Value)

newtype Function t = Function AnyValue
    deriving (ConstValue, DynamicValue, GlobalValue, GlobalVariable,
              Typeable, Value)

instance T.Params p => TypedValue (Function p) (T.Function p) where
    typeOf _ = T.function undefined

newtype ConstInt t = ConstInt AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Integer, Typeable, Value)

instance TypedValue (ConstInt T.Int1) T.Int1 where
    typeOf = T.int1

instance TypedValue (ConstInt T.Int8) T.Int8 where
    typeOf = T.int8

instance TypedValue (ConstInt T.Int16) T.Int16 where
    typeOf = T.int16

instance TypedValue (ConstInt T.Int32) T.Int32 where
    typeOf = T.int32

instance TypedValue (ConstInt T.Int64) T.Int64 where
    typeOf = T.int64

newtype ConstArray t = ConstArray AnyValue
    deriving (ConstValue, DynamicValue, Typeable, Value)

instance (T.DynamicType a) => TypedValue (ConstArray a) (T.Array a) where
    typeOf _ = T.array undefined 0

newtype ConstReal t = ConstReal AnyValue
    deriving (Arithmetic, ConstValue, DynamicValue, Real, Typeable, Value)

instance TypedValue (ConstReal T.Float) T.Float where
    typeOf = T.float

instance TypedValue (ConstReal T.Double) T.Double where
    typeOf = T.double

instance TypedValue (ConstReal T.X86Float80) T.X86Float80 where
    typeOf = T.x86Float80

instance TypedValue (ConstReal T.Float128) T.Float128 where
    typeOf = T.float128

instance TypedValue (ConstReal T.PPCFloat128) T.PPCFloat128 where
    typeOf = T.ppcFloat128

constWord :: (T.Integer t, Integral a) => (b -> t) -> a -> ConstInt t
constWord typ val =
    ConstInt . mkAnyValue $ FFI.constInt (T.typeRef (typ undefined)) (fromIntegral val) 0

constInt :: (T.Integer t, Integral a) => (b -> t) -> a -> ConstInt t
constInt typ val =
    ConstInt . mkAnyValue $ FFI.constInt (T.typeRef (typ undefined)) (fromIntegral val) 1

constReal :: (T.Real t, RealFloat a) => (b -> t) -> a -> ConstReal t
constReal typ val = ConstReal . mkAnyValue $ FFI.constReal (T.typeRef (typ undefined)) (realToFrac val)

constStringInternal :: Bool -> String -> ConstArray T.Int8
constStringInternal nulTerm s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . ConstArray . mkAnyValue $
      FFI.constString sPtr (fromIntegral sLen) (fromBool (not nulTerm))

constString :: String -> ConstArray T.Int8
constString = constStringInternal False

constStringNul :: String -> ConstArray T.Int8
constStringNul = constStringInternal True

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
