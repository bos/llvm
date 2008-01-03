{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  #-}

module LLVM.Core.Constant
    (
    -- * Constant expressions
      ConstExpr(..)

    -- ** Arithmetic
    , neg
    , not
    , add
    , sub
    , mul
    , udiv
    , sdiv
    , fdiv
    , urem
    , srem
    , frem
    , and
    , or
    , xor
    , shl
    , lshr
    , ashr

    -- ** Memory
    , gep

    -- ** Conversions
    , trunc
    , sExt
    , zExt
    , fpTrunc
    , fpExt
    , uiToFP
    , siToFP
    , fpToUI
    , fpToSI
    , ptrToInt
    , intToPtr
    , bitCast

    -- ** Comparisons
    , icmp
    , fcmp

    -- ** Miscellaneous operations
    , select
    , extractElement
    , insertElement
    , shuffleVector

    -- * Constant values
    , Const(..)

    -- ** Scalar constants
    , constInt
    , constWord
    , constReal

    -- ** Composite constants
    , constString
    , constStringNul
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.String (withCStringLen)
import Foreign.Marshal.Utils (fromBool)
import Prelude hiding (and, const, not, or)
import qualified Prelude as Prelude
import System.IO.Unsafe (unsafePerformIO)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Instruction as I
import qualified LLVM.Core.Type as T
import qualified LLVM.Core.Value as V


newtype ConstExpr t = ConstExpr V.AnyValue
    deriving (V.ConstValue, V.DynamicValue, Typeable, V.Value)

unary :: (V.ConstValue v) =>
         (FFI.ValueRef -> FFI.ValueRef) -> v -> ConstExpr t
unary ffi = ConstExpr . V.mkAnyValue . ffi . V.valueRef

neg :: (V.ConstValue v, V.Arithmetic v, T.Arithmetic t) => v -> ConstExpr t
neg = unary FFI.constNeg

not :: (V.ConstValue v, T.Integer t, V.TypedValue v t) => v -> ConstExpr t
not = unary FFI.constNot

binary :: (V.ConstValue a, V.ConstValue b)
          => (FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef) -> a -> b
          -> ConstExpr t
binary ffi a b = ConstExpr . V.mkAnyValue $ ffi (V.valueRef a) (V.valueRef b)

add :: (T.Arithmetic t, V.ConstValue a, V.TypedValue a t,
        V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
add = binary FFI.constAdd

sub :: (T.Arithmetic t, V.ConstValue a, V.TypedValue a t,
        V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
sub = binary FFI.constSub

mul :: (T.Arithmetic t, V.ConstValue a, V.TypedValue a t,
        V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
mul = binary FFI.constMul

udiv :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
udiv = binary FFI.constUDiv

sdiv :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
sdiv = binary FFI.constSDiv

fdiv :: (T.Real t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
fdiv = binary FFI.constFDiv

urem :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
urem = binary FFI.constURem

srem :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
srem = binary FFI.constURem

frem :: (T.Real t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
frem = binary FFI.constFRem

and :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
        V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
and = binary FFI.constAnd

or :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
       V.ConstValue b, V.TypedValue b t)
      => a -> b -> ConstExpr t
or = binary FFI.constOr

xor :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
        V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
xor = binary FFI.constXor

icmp :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
        => I.IntPredicate -> a -> b -> ConstExpr T.Int1
icmp p = binary (FFI.constICmp (I.fromIP p))

fcmp :: (T.Real t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
        => I.RealPredicate -> a -> b -> ConstExpr T.Int1
fcmp p = binary (FFI.constFCmp (I.fromRP p))

shl :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
shl = binary FFI.constShl

lshr :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
lshr = binary FFI.constLShr

ashr :: (T.Integer t, V.ConstValue a, V.TypedValue a t,
         V.ConstValue b, V.TypedValue b t)
       => a -> b -> ConstExpr t
ashr = binary FFI.constAShr

gep :: (V.ConstValue v, T.Integer t, V.TypedValue v t)
       => a -> b -> ConstExpr t
gep = undefined

typed :: (V.ConstValue v, T.Type s, V.ConstValue w, V.DynamicValue w)
         => (FFI.ValueRef -> FFI.TypeRef -> FFI.ValueRef) -> v -> s -> w
typed ffi a b = V.fromAnyValue . V.mkAnyValue $ ffi (V.valueRef a) (T.typeRef b)

trunc :: (V.ConstValue v, T.Integer s, V.TypedValue v s,
          V.ConstValue w, V.DynamicValue w, T.Integer t, V.TypedValue w t)
         => v -> t -> w
trunc = typed FFI.constTrunc

sExt :: (V.ConstValue v, T.Integer t, V.TypedValue v t)
        => v -> t -> ConstExpr t
sExt = typed FFI.constSExt

zExt :: (V.ConstValue v, T.Integer t, V.TypedValue v t)
        => v -> t -> ConstExpr t
zExt = typed FFI.constZExt

fpTrunc :: (V.ConstValue v, V.Real v, T.Real t)
         => v -> t -> ConstExpr t
fpTrunc = typed FFI.constFPTrunc

fpExt :: (V.ConstValue v, V.Real v, T.Real t)
         => v -> t -> ConstExpr t
fpExt = typed FFI.constFPExt

-- XXX How to express the inability to cast from scalar to vector?

uiToFP :: (V.ConstValue v, T.Integer s, V.TypedValue v s, T.Real t)
          => v -> s -> ConstExpr t
uiToFP = typed FFI.constUIToFP

siToFP :: (V.ConstValue v, V.Integer v, T.Integer s, T.Real t)
          => v -> s -> ConstExpr t
siToFP = typed FFI.constSIToFP

fpToUI :: (V.ConstValue v, V.Real v, T.Real s, T.Integer t)
          => v -> s -> ConstExpr t
fpToUI = typed FFI.constFPToUI

fpToSI :: (V.ConstValue v, V.Real v, T.Real s, T.Integer t)
          => v -> s -> ConstExpr t
fpToSI = typed FFI.constFPToSI

ptrToInt :: (V.ConstValue v, T.Integer t)
            => v -> T.Pointer a -> ConstExpr t
ptrToInt = typed FFI.constPtrToInt

intToPtr :: (V.ConstValue v, T.Integer t)
            => v -> t -> ConstExpr (T.Pointer a)
intToPtr = typed FFI.constIntToPtr

-- XXX How to express pointer/non-pointer and bit-width constraints?
bitCast :: (V.ConstValue v, T.Type t,
            V.ConstValue w, V.DynamicValue w)
           => v -> t -> w
bitCast = typed FFI.constBitCast

select :: (V.TypedValue k T.Int1,
           V.ConstValue a, V.TypedValue a t, V.ConstValue b, V.TypedValue b t)
          => k -> a -> b -> ConstExpr t
select k = binary (FFI.constSelect (V.valueRef k))

extractElement :: (V.ConstValue v, V.TypedValue v (T.Vector a),
                   V.ConstValue i, V.Integer i) => v -> i -> ConstExpr a
extractElement = binary FFI.constExtractElement

ternary :: (V.ConstValue a, V.ConstValue b, V.ConstValue c)
           => (FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef)
           -> a -> b -> c -> ConstExpr t
ternary ffi a b c = ConstExpr . V.mkAnyValue $
                    ffi (V.valueRef a) (V.valueRef b) (V.valueRef c)

insertElement :: (V.ConstValue v, V.TypedValue v (T.Vector a),
                  V.ConstValue e, V.TypedValue e a,
                  V.ConstValue i, V.Integer i)
                 => v -> e -> i -> ConstExpr (T.Vector a)
insertElement = ternary FFI.constInsertElement

shuffleVector :: (V.ConstValue v1, V.TypedValue v1 (T.Vector a),
                  V.ConstValue v2, V.TypedValue v2 (T.Vector a),
                  V.ConstValue m, V.TypedValue m (T.Vector T.Int32))
                 => v1 -> v2 -> m -> ConstExpr (T.Vector a)
shuffleVector = ternary FFI.constShuffleVector

constWord :: (T.Integer t, Integral a) => (b -> t) -> a -> V.ConstInt t
constWord typ val =
    V.ConstInt . V.mkAnyValue $ FFI.constInt (T.typeRef (typ undefined))
         (fromIntegral val) 0

constInt :: (T.Integer t, Integral a) => (b -> t) -> a -> V.ConstInt t
constInt typ val =
    V.ConstInt . V.mkAnyValue $ FFI.constInt (T.typeRef (typ undefined))
                 (fromIntegral val) 1

constReal :: (T.Real t, RealFloat a) => (b -> t) -> a -> V.ConstReal t
constReal typ val = V.ConstReal . V.mkAnyValue $ FFI.constReal
                    (T.typeRef (typ undefined)) (realToFrac val)

constStringInternal :: Bool -> String -> V.ConstArray T.Int8
constStringInternal nulTerm s = unsafePerformIO $
    withCStringLen s $ \(sPtr, sLen) ->
      return . V.ConstArray . V.mkAnyValue $
      FFI.constString sPtr (fromIntegral sLen) (fromBool (Prelude.not nulTerm))

constString :: String -> V.ConstArray T.Int8
constString = constStringInternal False

constStringNul :: String -> V.ConstArray T.Int8
constStringNul = constStringInternal True

class V.ConstValue t => Const a t | a -> t where
    const :: a -> t

instance Const String (V.ConstArray T.Int8) where
    const = constStringNul

instance Const Float (V.ConstReal T.Float) where
    const = constReal T.float . fromRational . toRational

instance Const Double (V.ConstReal T.Double) where
    const = constReal T.double

instance Const Int8 (V.ConstInt T.Int8) where
    const = constInt T.int8 . fromIntegral

instance Const Int16 (V.ConstInt T.Int16) where
    const = constInt T.int16 . fromIntegral

instance Const Int32 (V.ConstInt T.Int32) where
    const = constInt T.int32 . fromIntegral

instance Const Int64 (V.ConstInt T.Int64) where
    const = constInt T.int64

instance Const Word8 (V.ConstInt T.Int8) where
    const = constWord T.int8 . fromIntegral

instance Const Word16 (V.ConstInt T.Int16) where
    const = constWord T.int16 . fromIntegral

instance Const Word32 (V.ConstInt T.Int32) where
    const = constWord T.int32 . fromIntegral

instance Const Word64 (V.ConstInt T.Int64) where
    const = constWord T.int64 . fromIntegral
