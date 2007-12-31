{-# LANGUAGE FlexibleContexts #-}

module LLVM.Core.Instructions
    (
      RealPredicate(..)
    , fromRP

    -- * Constant expressions
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
    , icmp
    , fcmp
    , shl
    , lshr
    , ashr
    , gep
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
    , select
    , extractElement
    , insertElement
    , shuffleVector
    ) where

import Foreign.C.Types (CInt)
import Prelude hiding (and, not, or)

import qualified LLVM.Core.FFI as FFI
import qualified LLVM.Core.Types as T
import qualified LLVM.Core.Values as V


data RealPredicate =
    RealFalse           -- ^ Always false (always folded)
  | RealOEQ             -- ^ True if ordered and equal
  | RealOGT             -- ^ True if ordered and greater than
  | RealOGE             -- ^ True if ordered and greater than or equal
  | RealOLT             -- ^ True if ordered and less than
  | RealOLE             -- ^ True if ordered and less than or equal
  | RealONE             -- ^ True if ordered and operands are unequal
  | RealORD             -- ^ True if ordered (no nans)
  | RealUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | RealUEQ             -- ^ True if unordered or equal
  | RealUGT             -- ^ True if unordered or greater than
  | RealUGE             -- ^ True if unordered, greater than, or equal
  | RealULT             -- ^ True if unordered or less than
  | RealULE             -- ^ True if unordered, less than, or equal
  | RealUNE             -- ^ True if unordered or not equal
  | RealT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show)

fromRP :: RealPredicate -> CInt
fromRP = fromIntegral . fromEnum

unary :: (V.ConstValue v) =>
         (FFI.ValueRef -> FFI.ValueRef) -> v -> V.ConstExpr t
unary ffi = V.ConstExpr . V.mkAnyValue . ffi . V.valueRef

neg :: (V.ConstValue v, V.Arithmetic v, T.Arithmetic t) => v -> V.ConstExpr t
neg = unary FFI.constNeg

not :: (V.ConstValue v, V.Integer v, T.Integer t) => v -> V.ConstExpr t
not = unary FFI.constNot

binary :: (V.ConstValue a, V.ConstValue b)
          => (FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef) -> a -> b
          -> V.ConstExpr t
binary ffi a b = V.ConstExpr . V.mkAnyValue $ ffi (V.valueRef a) (V.valueRef b)

add :: (V.ConstValue v, V.Arithmetic v, T.Arithmetic t)
       => v -> v -> V.ConstExpr t
add = binary FFI.constAdd

sub :: (V.ConstValue v, V.Arithmetic v, T.Arithmetic t)
       => v -> v -> V.ConstExpr t
sub = binary FFI.constSub

mul :: (V.ConstValue v, V.Arithmetic v, T.Arithmetic t)
       => v -> v -> V.ConstExpr t
mul = binary FFI.constMul

udiv :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
udiv = binary FFI.constUDiv

sdiv :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
sdiv = binary FFI.constSDiv

fdiv :: (V.ConstValue v, V.Real v, T.Real t)
       => v -> v -> V.ConstExpr t
fdiv = binary FFI.constFDiv

urem :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
urem = binary FFI.constURem

srem :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
srem = binary FFI.constURem

frem :: (V.ConstValue v, V.Real v, T.Real t)
       => v -> v -> V.ConstExpr t
frem = binary FFI.constFRem

and :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
and = binary FFI.constAnd

or :: (V.ConstValue v, V.Integer v, T.Integer t)
      => v -> v -> V.ConstExpr t
or = binary FFI.constOr

xor :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
xor = binary FFI.constXor

icmp :: (V.ConstValue v, V.Integer v, T.Integer t)
        => RealPredicate -> v -> v -> V.ConstExpr t
icmp p a b = V.ConstExpr . V.mkAnyValue $
             FFI.constFCmp (fromRP p) (V.valueRef a) (V.valueRef b)

fcmp :: (V.ConstValue v, V.Real v, T.Real t)
        => RealPredicate -> v -> v -> V.ConstExpr t
fcmp p a b = V.ConstExpr . V.mkAnyValue $
             FFI.constFCmp (fromRP p) (V.valueRef a) (V.valueRef b)

shl :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
shl = binary FFI.constShl

lshr :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
lshr = binary FFI.constLShr

ashr :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
ashr = binary FFI.constAShr

gep :: (V.ConstValue v, V.Integer v, T.Integer t)
       => v -> v -> V.ConstExpr t
gep = undefined

typed :: (V.ConstValue v, T.Type s, V.ConstValue w, V.DynamicValue w)
         => (FFI.ValueRef -> FFI.TypeRef -> FFI.ValueRef) -> v -> s -> w
typed ffi a b = V.fromAnyValue . V.mkAnyValue $ ffi (V.valueRef a) (T.typeRef b)

trunc :: (V.ConstValue v, T.Integer s, V.TypedValue v s,
          V.ConstValue w, V.DynamicValue w, T.Integer t, V.TypedValue w t)
         => v -> t -> w
trunc = typed FFI.constTrunc

sExt :: (V.ConstValue v, V.Integer v, T.Integer t)
        => v -> t -> V.ConstExpr t
sExt = typed FFI.constSExt

zExt :: (V.ConstValue v, V.Integer v, T.Integer t)
        => v -> t -> V.ConstExpr t
zExt = typed FFI.constZExt

fpTrunc :: (V.ConstValue v, V.Real v, T.Real t)
         => v -> t -> V.ConstExpr t
fpTrunc = typed FFI.constFPTrunc

fpExt :: (V.ConstValue v, V.Real v, T.Real t)
         => v -> t -> V.ConstExpr t
fpExt = typed FFI.constFPExt

-- XXX How to express the inability to cast from scalar to vector?

uiToFP :: (V.ConstValue v, V.Integer v, T.Integer s, T.Real t)
          => v -> s -> V.ConstExpr t
uiToFP = typed FFI.constUIToFP

siToFP :: (V.ConstValue v, V.Integer v, T.Integer s, T.Real t)
          => v -> s -> V.ConstExpr t
siToFP = typed FFI.constSIToFP

fpToUI :: (V.ConstValue v, V.Real v, T.Real s, T.Integer t)
          => v -> s -> V.ConstExpr t
fpToUI = typed FFI.constFPToUI

fpToSI :: (V.ConstValue v, V.Real v, T.Real s, T.Integer t)
          => v -> s -> V.ConstExpr t
fpToSI = typed FFI.constFPToSI

ptrToInt :: (V.ConstValue v, T.Integer t)
            => v -> T.Pointer a -> V.ConstExpr t
ptrToInt = typed FFI.constPtrToInt

intToPtr :: (V.ConstValue v, T.Integer t)
            => v -> t -> V.ConstExpr (T.Pointer a)
intToPtr = typed FFI.constIntToPtr

-- XXX How to express pointer/non-pointer and bit-width constraints?
bitCast :: (V.ConstValue v, T.Type t,
            V.ConstValue w, V.DynamicValue w)
           => v -> t -> w
bitCast = typed FFI.constBitCast

select :: (V.Value k, V.TypedValue k T.Int1, V.ConstValue v, V.TypedValue v t)
          => k -> v -> v -> V.ConstExpr t
select k = binary (FFI.constSelect (V.valueRef k))

extractElement :: (V.ConstValue v, V.TypedValue v (T.Vector a),
                   V.ConstValue i, V.Integer i) => v -> i -> V.ConstExpr a
extractElement = binary FFI.constExtractElement

ternary :: (V.ConstValue a, V.ConstValue b, V.ConstValue c)
           => (FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef -> FFI.ValueRef)
           -> a -> b -> c -> V.ConstExpr t
ternary ffi a b c = V.ConstExpr . V.mkAnyValue $
                    ffi (V.valueRef a) (V.valueRef b) (V.valueRef c)

insertElement :: (V.ConstValue v, V.TypedValue v (T.Vector a),
                  V.ConstValue e, V.TypedValue e a,
                  V.ConstValue i, V.Integer i)
                 => v -> e -> i -> V.ConstExpr (T.Vector a)
insertElement = ternary FFI.constInsertElement

shuffleVector :: (V.ConstValue v1, V.TypedValue v1 (T.Vector a),
                  V.ConstValue v2, V.TypedValue v2 (T.Vector a),
                  V.ConstValue m, V.TypedValue m (T.Vector T.Int32))
                 => v1 -> v2 -> m -> V.ConstExpr (T.Vector a)
shuffleVector = ternary FFI.constShuffleVector
